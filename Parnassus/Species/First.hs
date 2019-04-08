{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE NamedFieldPuns #-}

module Parnassus.Species.First where

import Algorithm.Search (dijkstraM)
import Control.Monad (join)
import Control.Monad.Random (evalRandIO, Rand)
import Data.List (sortOn, zip4)
import Data.Maybe (fromJust, fromMaybe, isJust, listToMaybe)
import Data.Range.Range (fromRanges)
import qualified Data.Vector as V
import System.Random (RandomGen)

import Euterpea (absPitch, Control (..), InstrumentName (..), Mode (..), pitch, Pitch, PitchClass (..))
import Parnassus.Utils (ngrams)
import Parnassus.Dist (discreteDist, DiscreteDist (..), getLogProb, sample, samplesWithoutReplacement, trainDiscrete, uniformDiscreteDist)
import Parnassus.Markov (boundedIntegerRandomWalk, markovConditionOn)
import Parnassus.MusicBase (Key, modify, play, (/=/))
import Parnassus.MusicD (extractTied, MusicD (..), Tied (..))
import Parnassus.Search (beamSearchM, dfsM, greedySearchM, NeighborGenM)
import Parnassus.Species.Base

import Debug.Trace


data FirstSpecies = First { key :: Key, cantusFirmus :: VoiceLine, counterpoint :: VoiceLine}
    deriving (Show)

-- convenience constructor from strings
firstSpecies :: Key -> (Voice, String) -> (Voice, String) -> FirstSpecies
firstSpecies (pc, mode) (cfVoice, cfStr) (cptVoice, cptStr) = spec
    where
        key' = (pc, convertMode mode)
        validateNote :: Voice -> Tied Pitch -> Bool
        validateNote voice (Untied _ p) = voiceCanSing voice p
        validateNote voice (Tied p)     = voiceCanSing voice p
        validateNote _ Rest             = False
        cf = parseLine cfStr
        cpt = parseLine cptStr
        (cfPitches, _) = foldTied cf
        cadencePitchClass = fst $ cfPitches !! (length cfPitches - 2)
        scale = scaleForKey key' False
        doCheck :: String -> Bool -> RuleCheck
        doCheck _ True = Passed
        doCheck s False = Failed s
        checks = [
            doCheck "length of C.F. must be >= 3 notes" (length cfPitches >= 3),
            doCheck "note out of vocal range in C.F." (all (validateNote cfVoice) cf),
            doCheck "note out of vocal range in counterpoint" (all (validateNote cptVoice) cpt),
            doCheck "length mismatch between C.F. and counterpoint" (length cf == length cpt),
            doCheck "C.F. must start with the tonic" ((fst <$> extractTied (head cf)) == Just pc),
            doCheck "C.F. must end with the tonic" ((fst <$> extractTied (last cf)) == Just pc),
            doCheck "penultimate note of C.F. must be the second note of the scale" (equivPitchClass (scale !! 1) cadencePitchClass)]
        firstFailed = dropWhile (== Passed) checks
        spec = case firstFailed of
            (x:_)     -> error s where (Failed s) = x
            otherwise -> First {key = key', cantusFirmus = (cfVoice, cf), counterpoint = (cptVoice, cpt)}

instance Species FirstSpecies where
    toMusicD :: FirstSpecies -> MusicD Pitch
    toMusicD First {cantusFirmus = (_, cf), counterpoint = (_, cpt)} = modify (Tempo 3) $ (MusicD 1 [Instrument VoiceOohs] (pure <$> cf)) /=/ (MusicD 1 [Instrument VoiceOohs] (pure <$> cpt))


-- global data for first species
data FirstSpeciesConstants = FirstSpecConsts {
    s1Length :: Int,    -- number of bars (ignoring ties)
    s1Key :: Key,       -- key (fundamental)
    s1Ordering :: Bool  -- True if CF <= CPT, False if CF > CPT
}
    deriving (Eq, Show)

-- local context of a note in first species
-- this is the minimal data needed to determine if a violation is present locally
data FirstSpeciesContext = FirstSpecContext {
    s1Constants :: FirstSpeciesConstants,
    s1Index :: Int,                        -- index of the present note
    s1Interval :: Interval,                -- the present (vertical) interval
    s1IntervalWindow :: [Maybe Interval],  -- up to 7-long window of surrounding intervals
    s1Motions :: [Maybe PairwiseMotion]    -- 2-long list containing motion into present interval & into next interval
}
    deriving (Eq, Show)

data FirstSpeciesRule = FirstSpecRule {
    s1RuleCheck :: FirstSpeciesContext -> Bool,     -- returns True if the rule is satisfied
    s1RuleDescr :: String,                          -- description of the rule
    s1RuleIsEssential :: Bool                       -- flag indicating whether the rule is "essential" (crucial)
}

-- given a rule and a context, checks the rule
checkFirstSpeciesRule :: FirstSpeciesRule -> FirstSpeciesContext -> RuleCheck
checkFirstSpeciesRule (FirstSpecRule {s1RuleCheck, s1RuleDescr}) context@(FirstSpecContext {s1Index}) = result
    where
        passed = s1RuleCheck context
        result = case passed of
            True  -> Passed
            False -> Failed $ "Bar " ++ show s1Index ++ ": " ++ s1RuleDescr

-- given a list of rules, returns True if the given context passes all the rules
passesFirstSpeciesRules :: [FirstSpeciesRule] -> FirstSpeciesContext -> Bool
passesFirstSpeciesRules rules context = all (== Passed) [checkFirstSpeciesRule rule context | rule <- rules]

-- FIRST SPECIES RULES --

fsRuleCheck0 (FirstSpecContext {s1Constants = FirstSpecConsts {s1Key = (pc, _)}, s1Index, s1Interval}) =
    (s1Index /= 0) || 
    (harmonicIntervalType pc s1Interval == PerfectConsonance)

fsRule0 = FirstSpecRule {
    s1RuleCheck = fsRuleCheck0,
    s1RuleDescr = "First interval is a perfect consonance.",
    s1RuleIsEssential = True
}

fsRuleCheck1 (FirstSpecContext {s1Constants = FirstSpecConsts {s1Length, s1Key = (pc, _)}, s1Index, s1Interval}) =
    (s1Index /= s1Length - 1) || 
    (harmonicIntervalType pc s1Interval == PerfectConsonance)

fsRule1 = FirstSpecRule {
    s1RuleCheck = fsRuleCheck1,
    s1RuleDescr = "Last interval is a perfect consonance.",
    s1RuleIsEssential = True
}

fsRuleCheck2 (FirstSpecContext {s1Constants = FirstSpecConsts {s1Key = (pc, _)}, s1Motions}) =
    all (fromMaybe True . fmap check) s1Motions
    where
        check :: PairwiseMotion -> Bool
        check motion = (motionType motion /= Parallel) || (harmonicIntervalType pc (snd motion) /= PerfectConsonance)

fsRule2 = FirstSpecRule {
    s1RuleCheck = fsRuleCheck2,
    s1RuleDescr = "No parallel motion into a perfect consonance.",
    s1RuleIsEssential = True
}

fsRuleCheck3 (FirstSpecContext {s1Constants = FirstSpecConsts {s1Length, s1Ordering}, s1Index, s1Interval}) =
    (s1Index /= s1Length - 2) || 
    (s1Ordering && intervalDisplacement s1Interval == 9) ||
    (not s1Ordering && intervalDisplacement s1Interval == -3)

fsRule3 = FirstSpecRule {
    s1RuleCheck = fsRuleCheck3,
    s1RuleDescr = "The cadence interval must be a major sixth (if C.F. is the lower part) or a minor third (if C.F. is the higher part)",
    s1RuleIsEssential = True
}

fsRuleCheck4 (FirstSpecContext {s1Constants = FirstSpecConsts {s1Key = (pc, _)}, s1Index, s1Interval = ((pc1, oct1), (pc2, oct2))}) = (s1Index /= 0) || (equivPitchClass pc' pc)
    where pc' = if (absPitch (pc1, oct1) <= absPitch (pc2, oct2)) then pc1 else pc2                      

fsRule4 = FirstSpecRule {
    s1RuleCheck = fsRuleCheck4,
    s1RuleDescr = "The bottom note of the first interval must be the tonic.",
    s1RuleIsEssential = True
}

fsRuleCheck5 (FirstSpecContext {s1Motions}) = all (fromMaybe True . fmap check) s1Motions
    where
        check :: PairwiseMotion -> Bool
        check (interval1, interval2) = not $
            (abs (intervalNumber interval1) == 10) && (intervalDistance interval2 == 12) &&
            (abs (intervalNumber motion1) == 2) && (abs (intervalNumber motion2) == 2)
            where (motion1, motion2) = motionTranspose (interval1, interval2)

fsRule5 = FirstSpecRule {
    s1RuleCheck = fsRuleCheck5,
    s1RuleDescr = "A tenth cannot proceed into an octave via stepwise contrary motion (battuta).",
    s1RuleIsEssential = False
}

fsRuleCheck6 (FirstSpecContext {s1Motions}) = all (fromMaybe True . fmap check) s1Motions
    where
        check :: PairwiseMotion -> Bool
        check (interval1, interval2) = not $
            (intervalDistance interval1 > 12) && (intervalDistance interval2 == 12) &&
            ((abs (intervalNumber motion1) > 2) || (abs (intervalNumber motion2) > 2))
            where (motion1, motion2) = motionTranspose (interval1, interval2)

fsRule6 = FirstSpecRule {
    s1RuleCheck = fsRuleCheck6,
    s1RuleDescr = "An interval greater than an octave cannot proceed into an octave by a skip.",
    s1RuleIsEssential = True
}

fsRuleCheck7 (FirstSpecContext {s1Constants = FirstSpecConsts {s1Length}, s1Index, s1Interval}) =
    (s1Index == 0) || (s1Index == s1Length - 1) ||
    (intervalDisplacement s1Interval /= 0)

fsRule7 = FirstSpecRule {
    s1RuleCheck = fsRuleCheck7,
    s1RuleDescr = "A unison may only occur as the first or last interval.",
    s1RuleIsEssential = True
}

fsRuleCheck8 (FirstSpecContext {s1Motions}) = all (fromMaybe True . fmap check) s1Motions
    where
        check :: PairwiseMotion -> Bool
        check (interval1, interval2) = not $
            (intervalDisplacement interval2 == 0) &&
            ((abs (intervalNumber motion1) > 2) || (abs (intervalNumber motion2) > 2))
            where (motion1, motion2) = motionTranspose (interval1, interval2)

fsRule8 = FirstSpecRule {
    s1RuleCheck = fsRuleCheck8,
    s1RuleDescr = "A unison interval cannot be entered via a skip.",
    s1RuleIsEssential = True
}

fsRuleCheck9 (FirstSpecContext {s1Constants = FirstSpecConsts {s1Key = (pc, _)}, s1Motions}) =
    all (fromMaybe True . fmap check) s1Motions
    where
        check :: PairwiseMotion -> Bool
        check (interval1, interval2) = not $
            (intervalDisplacement interval1 == 0) &&              -- first interval is a unison
            (harmonicIntervalType pc interval2 /= Dissonance) &&  -- proceeds into a consonance
            (abs (intervalNumber motion2) > 2)                    -- counterpoint motion is a skip
            where (_, motion2) = motionTranspose (interval1, interval2)

fsRule9 = FirstSpecRule {
    s1RuleCheck = fsRuleCheck9,
    s1RuleDescr = "A unison interval cannot progress into another consonance with the counterpoint moving by a skip.",
    s1RuleIsEssential = True
}

fsRuleCheck10 (FirstSpecContext {s1Constants = FirstSpecConsts {s1Key = (pc, _)}, s1Motions}) =
    all (fromMaybe True . fmap check) s1Motions
    where
        check :: PairwiseMotion -> Bool
        check pm = not $
            (abs (intervalNumber motion1) > 2) &&
            (melodicIntervalType pc motion1 == Dissonance)
            where (motion1, _) = motionTranspose pm

fsRule10 = FirstSpecRule {
    s1RuleCheck = fsRuleCheck10,
    s1RuleDescr = "C.F. should not proceed with a skip that is a dissonant interval.",
    s1RuleIsEssential = False
}

fsRuleCheck11 (FirstSpecContext {s1Constants = FirstSpecConsts {s1Key = (pc, _)}, s1Motions}) =
    all (fromMaybe True . fmap check) s1Motions
    where
        check :: PairwiseMotion -> Bool
        check pm = not $
            (abs (intervalNumber motion2) > 2) &&
            (melodicIntervalType pc motion2 == Dissonance)
            where (_, motion2) = motionTranspose pm

fsRule11 = FirstSpecRule {
    s1RuleCheck = fsRuleCheck11,
    s1RuleDescr = "Counterpoint should not proceed with a skip that is a dissonant interval.",
    s1RuleIsEssential = True
}

fsRuleCheck12 (FirstSpecContext {s1Constants = FirstSpecConsts {s1Key = (pc, _)}, s1Interval, s1Motions}) =
    (intervalDistance s1Interval <= 16) &&
    all (fromMaybe True . fmap check) s1Motions &&
    (harmonicIntervalType pc s1Interval /= Dissonance)
    where
        check :: PairwiseMotion -> Bool
        check pm = (intervalDistance motion1 <= 16) && (intervalDistance motion2 <= 16)
            where (motion1, motion2) = motionTranspose pm

fsRule12 = FirstSpecRule {
    s1RuleCheck = fsRuleCheck12,
    s1RuleDescr = "Harmonic intervals must be consonant, and both harmonic and melodic intervals must be no larger than a major 10th.",
    s1RuleIsEssential = True
}

fsRuleCheck13 (FirstSpecContext {s1Constants = FirstSpecConsts {s1Key = (pc, mode)}, s1Motions}) =
    all (fromMaybe True . fmap check) s1Motions
    where
        fa = shiftPitch (firstDegree (pc, mode)) 5
        checkMotion :: Interval -> Bool
        checkMotion (p1@(pc1, _), p2) = not $
            (equivPitchClass pc1 fa) &&     -- first pitch is a 'fa' in the scale
            (intervalNumber (p1, p2) == 2)  -- motion is an ascending step
        check :: PairwiseMotion -> Bool
        check pm = (checkMotion motion1) && (checkMotion motion2)
            where (motion1, motion2) = motionTranspose pm

-- NB: this seems to be violated often
fsRule13 = FirstSpecRule {
    s1RuleCheck = fsRuleCheck13,
    s1RuleDescr = "Undesirable to progress upward by a step from the 'fa' scale degree.",
    s1RuleIsEssential = False
}

fsRuleCheck14 (FirstSpecContext {s1Constants = FirstSpecConsts {s1Key = (pc, mode)}, s1Motions}) =
    all (fromMaybe True . fmap check) s1Motions
    where
        mi = shiftPitch (firstDegree (pc, mode)) 4
        checkMotion :: Interval -> Bool
        checkMotion (p1@(pc1, _), p2) = not $
            (equivPitchClass pc1 mi) &&      -- first pitch is a 'mi' in the scale
            (intervalNumber (p1, p2) == -2)  -- motion is a descending step
        check :: PairwiseMotion -> Bool
        check pm = (checkMotion motion1) && (checkMotion motion2)
            where (motion1, motion2) = motionTranspose pm

-- NB: this seems to be violated often
fsRule14 = FirstSpecRule {
    s1RuleCheck = fsRuleCheck13,
    s1RuleDescr = "Undesirable to progress downward by a step from the 'mi' scale degree.",
    s1RuleIsEssential = False
}

fsRuleCheck15 (FirstSpecContext {s1Motions}) =
    all (fromMaybe True . fmap check) s1Motions
    where
        check :: PairwiseMotion -> Bool
        check pm = (intervalDisplacement motion1 /= 0) || (intervalDisplacement motion2 /= 0)
            where (motion1, motion2) = motionTranspose pm

fsRule15 = FirstSpecRule {
    s1RuleCheck = fsRuleCheck15,
    s1RuleDescr = "Intervals should not be repeated consecutively.",
    s1RuleIsEssential = True
}

fsRuleCheck16 (FirstSpecContext {s1IntervalWindow}) = maxRpt <= 3
    where
        counterpoint = fmap (absPitch . snd) <$> s1IntervalWindow
        maxRpt = maxRepeats counterpoint

fsRule16 = FirstSpecRule {
    s1RuleCheck = fsRuleCheck16,
    s1RuleDescr = "Counterpoint cannot have four consecutive repeated notes.",
    s1RuleIsEssential = True
}

fsRuleCheck17 (FirstSpecContext {s1IntervalWindow}) = maxRpt <= 3
    where
        numbers = fmap intervalNumber <$> s1IntervalWindow
        maxRpt = maxRepeats numbers

fsRule17 = FirstSpecRule {
    s1RuleCheck = fsRuleCheck17,
    s1RuleDescr = "Cannot have four consecutive intervals of the same number.",
    s1RuleIsEssential = True
}

fsRuleCheck18 (FirstSpecContext {s1Constants = FirstSpecConsts {s1Length, s1Key = (pc, mode)}, s1Index, s1Interval = (_, (pc', _))}) =
    (s1Index == s1Length - 2) ||
    (not $ equivPitchClass pc (shiftPitch pc' 1)) ||
    (mode `elem` [Ionian, Major, Lydian])

fsRule18 = FirstSpecRule {
    s1RuleCheck = fsRuleCheck18,
    s1RuleDescr = "An accidental leading tone cannot occur in the counterpoint, except in the cadence.",
    s1RuleIsEssential = True
}

fsRuleCheck19 (FirstSpecContext {s1Constants = FirstSpecConsts {s1Length, s1Ordering}, s1Index, s1Interval = (p1, p2)}) =
    ((s1Index /= 0) && (s1Index /= s1Length - 1)) ||
    (s1Ordering && (absPitch p1 <= absPitch p2)) ||
    ((not s1Ordering) && (absPitch p1 >= absPitch p2))

fsRule19 = FirstSpecRule {
    s1RuleCheck = fsRuleCheck19,
    s1RuleDescr = "Voice crossover cannot occur in the first or last note.",
    s1RuleIsEssential = True
}

firstSpeciesRules =
    [fsRule12] ++  -- uses current interval
    [fsRule0, fsRule1, fsRule3, fsRule4, fsRule7, fsRule19, fsRule18] ++  -- uses current index & interval
    [fsRule2, fsRule5, fsRule6, fsRule8, fsRule9, fsRule10, fsRule11, fsRule12, fsRule13, fsRule14, fsRule15] ++  -- uses motions
    [fsRule16, fsRule17]  -- uses 7-long interval window

firstSpeciesEssentialRules = filter s1RuleIsEssential firstSpeciesRules

-- VALIDITY CHECKING --

-- gets the sequence of intervals for first species (eliminating ties)
firstSpeciesIntervals :: FirstSpecies -> [Interval]
firstSpeciesIntervals (First {cantusFirmus, counterpoint}) = intervals
    where
        (cfPitches, _) = foldTied (snd cantusFirmus)
        (cptPitches, _) = foldTied (snd counterpoint)
        intervals
            | length cfPitches == length cptPitches = zip cfPitches cptPitches
            | otherwise                             = error "mismatch between number of notes in C.F. and counterpoint"

-- converts a FirstSpecies into a list of contexts for evaluating rules
firstSpeciesContexts :: FirstSpecies -> [FirstSpeciesContext]
firstSpeciesContexts firstSpec@(First {key, cantusFirmus, counterpoint}) = contexts
    where
        intervals = firstSpeciesIntervals firstSpec
        voiceOrdering = (fst cantusFirmus <= fst counterpoint)
        consts = FirstSpecConsts {s1Length = length intervals, s1Key = key, s1Ordering = voiceOrdering}
        windows = map Just <$> getWindows 3 intervals
        listToPair (x:y:_) = (x, y)
        motionss = (ngrams 2 $ [Nothing] ++ (Just <$> (listToPair <$> ngrams 2 intervals)) ++ [Nothing])
        contexts = [FirstSpecContext {s1Constants = consts, s1Index = i, s1Interval = interval, s1IntervalWindow = window, s1Motions = motions} | (i, interval, window, motions) <- zip4 [0..] intervals windows motionss]

-- checks all the rules against the first species
-- returns either Passed or Failed (with the first violation encountered)
checkFirstSpecies :: [FirstSpeciesRule] -> FirstSpecies -> RuleCheck
checkFirstSpecies rules fs = result
    where
        contexts = firstSpeciesContexts fs
        results = (checkFirstSpeciesRule <$> rules) <*> contexts  -- nested for loop (rules are outer loop)
        firstFailed = dropWhile (== Passed) results
        result = case firstFailed of
            (x:_)     -> x       -- first failure
            otherwise -> Passed  -- no failures, so we've passed

-- MODELING --

data FirstSpeciesModel = FirstSpecModel {
    harmonicModel :: DiscreteDist String Int,  -- distribution on harmonic displacements (cpt vs. c.f.)
    melodicModel :: DiscreteDist String Int  -- distribution on melodic displacements
}
    deriving (Eq, Show)

-- trains a FirstSpeciesModel given a list of first species counterpoints
trainFirstSpeciesModel :: [FirstSpecies] -> Double -> FirstSpeciesModel
trainFirstSpeciesModel fss smooth = FirstSpecModel {harmonicModel = harmonicModel, melodicModel = melodicModel}
    where
        intervalss = firstSpeciesIntervals <$> fss
        vertDisplacements = concatMap (map intervalDisplacement) intervalss
        vmin = min (-16) (minimum vertDisplacements)
        vmax = max 16 (maximum vertDisplacements)
        vRange = [vmin..vmax]
        harmonicModel = trainDiscrete "harmonic displacement" (Just vRange) smooth vertDisplacements
        (cfs, cpts) = unzip (unzip <$> intervalss)
        melodies = cfs ++ cpts  -- just combine
        bigramDisplacement (p1:p2:_) = intervalDisplacement (p1, p2)
        horizDisplacements = (map bigramDisplacement) . ngrams 2 <$> melodies
        allHorizDisplacements = concat horizDisplacements
        hmin = min (-16) (minimum allHorizDisplacements)
        hmax = max 16 (maximum allHorizDisplacements)
        hRange = [hmin..hmax]
        melodicModel = trainDiscrete "melodic displacement" (Just hRange) smooth allHorizDisplacements

-- gets log2 probability of the FirstSpecies under the model
-- model is: P(cpt_i | cpt_{i-1},cf_{i})
-- that is, scores only the counterpoint, conditioned on the cantus firmus
scoreFirstSpecies :: FirstSpeciesModel -> FirstSpecies -> Double
scoreFirstSpecies (FirstSpecModel {harmonicModel, melodicModel}) fs = harmonicScore + melodicScore
    where
        -- just get the notes
        intervals = firstSpeciesIntervals fs
        (_, cpt) = unzip intervals
        cpt' = absPitch <$> cpt
        horizDisplacements = zipWith (-) (tail cpt') cpt'
        harmonicScore = sum $ getLogProb harmonicModel . intervalDisplacement <$> intervals
        melodicScore = sum $ getLogProb melodicModel <$> horizDisplacements


-- GENERATION --

-- position and vector of counterpoint pitches (some yet unfilled)
type FirstSpeciesState = (Int, V.Vector (Maybe Pitch))
type TransitionCostFunc a = a -> a -> Double
type FirstSpeciesTransitionCostFunc = TransitionCostFunc FirstSpeciesState
-- order of note generation (forward, backward, random)
data GenerationPolicy = ForwardPolicy | BackwardPolicy | RandomPolicy
    deriving (Eq, Show)

data FirstSpeciesSetup = FirstSpeciesSetup {
    fsModel :: FirstSpeciesModel,
    fsKey :: Key,
    fsCf :: VoiceLine,
    fsCptVoice :: Voice,
    genPolicy :: GenerationPolicy
}
    deriving Show

-- cost (-log prob) of transitioning from one state to another
firstSpeciesNeighborCost :: FirstSpeciesSetup -> FirstSpeciesTransitionCostFunc
firstSpeciesNeighborCost (FirstSpeciesSetup {fsModel = FirstSpecModel {harmonicModel, melodicModel}, fsKey, fsCf = (_, cf), fsCptVoice}) _ (i2, cpt2) = cost
    where
        scale = scaleForKey fsKey True  -- scale with leading tones
        validPitches = [(pc, n) | (pc, n) <- pitch <$> fromRanges [voiceRange fsCptVoice], pc `elem` scale]
        medianValidPitch = absPitch $ validPitches !! (length validPitches `quot` 2)
        (cfPitches, _) = foldTied cf
        cfPitchVec = V.fromList cfPitches
        n = V.length cfPitchVec
        cptNote = fromJust $ cpt2 V.! i2
        cptNoteDist = absPitch cptNote - medianValidPitch
        interval = (cfPitchVec V.! i2, cptNote)
        harmonicCost = -(getLogProb harmonicModel $ intervalDisplacement interval)
        -- distances from median pitch for voice
        distances = V.map (fmap ((\p -> p - medianValidPitch) . absPitch)) cpt2
        extract (j, d) = (j, fromJust d)
        pair1 = listToMaybe $ extract <$> (filter (isJust . snd) $ zip [(1::Int)..] (reverse $ V.toList $ V.slice 0 i2 distances))
        pair2 = listToMaybe $ extract <$> (filter (isJust . snd) $ zip [(1::Int)..] (V.toList $ V.slice (i2 + 1) (n - i2 - 1) distances))
        melodicRandomWalk = boundedIntegerRandomWalk melodicModel (-32, 32)
        revMelodicModel = negate <$> melodicModel
        revMelodicRandomWalk = boundedIntegerRandomWalk revMelodicModel (-32, 32)
        (i, rwalk, pair1', pair2') = case (pair1, pair2) of
            -- backward transition
            (Nothing, Just (j, d)) -> (n - i2, revMelodicRandomWalk, Just (j, d), Nothing)
            -- forward transition
            otherwise              -> (i2, melodicRandomWalk, pair1, pair2)
        condDist =  markovConditionOn rwalk (toInteger i) (pair1', pair2')
        melodicCost = case (pair1, pair2) of
            (Nothing, Nothing) -> 0.0  -- indifferent to the first pitch chosen
            otherwise          -> -(getLogProb condDist cptNoteDist)
        cost = harmonicCost + melodicCost
        --cost = (trace (show (i, cptNoteDist, pair1', pair2') ++ "\n" ++ (show condDist) ++ "\n" ++ show (harmonicCost, melodicCost, harmonicCost + melodicCost))) $ harmonicCost + melodicCost

firstSpeciesNeighbors :: RandomGen g => FirstSpeciesSetup -> FirstSpeciesState -> Rand g [FirstSpeciesState]
firstSpeciesNeighbors (FirstSpeciesSetup {fsKey, fsCf = (cfVoice, cf), fsCptVoice, genPolicy}) state@(i, cpt) = do
    i' <- case genPolicy of
        ForwardPolicy  -> return (i + 1)
        BackwardPolicy -> return (i - 1)
        RandomPolicy   -> sample idxDist
            where
                validIndices = fst <$> filter (not . isJust . snd) (zip [0..] (V.toList cpt))
                idxDist = uniformDiscreteDist () validIndices
    let states = case (cpt V.! i') of
                    Just p  -> [(i', cpt)]  -- no nontrivial neighbors
                    Nothing -> [(i', cpt V.// [(i', Just p)]) | p <- validPitches]
    let neighbors = filter (passesEssentialRules . getContext) states
    return neighbors   
    where
        scale = scaleForKey fsKey True  -- scale with leading tones
        validPitches = [(pc, n) | (pc, n) <- pitch <$> fromRanges [voiceRange fsCptVoice], pc `elem` scale]
        voiceOrdering = cfVoice <= fsCptVoice
        (cfPitches, cfBook) = foldTied cf
        cfPitchVec = V.fromList cfPitches
        n = V.length cfPitchVec
        consts = FirstSpecConsts {s1Length = n, s1Key = fsKey, s1Ordering = voiceOrdering}
        -- given current state, gets the context
        getContext :: FirstSpeciesState -> FirstSpeciesContext
        getContext (i, cpt) = FirstSpecContext {s1Constants = consts, s1Index = i, s1Interval = interval, s1IntervalWindow = window, s1Motions = motions}
            where
                interval = (cfPitchVec V.! i, fromJust $ cpt V.! i)
                start = max 0 (i - 3)
                len = min 7 (n - start)
                window = maybePair <$> V.toList (V.zip (V.map Just $ V.slice start len cfPitchVec) cpt)
                leftInterval = (\x -> (cfPitchVec V.! (i - 1), x)) <$> join (cpt V.!? (i - 1))
                rightInterval = (\x -> (cfPitchVec V.! (i + 1), x)) <$> join (cpt V.!? (i + 1))
                motions = [(\x -> (x, interval)) <$> leftInterval, (\x -> (interval, x)) <$> rightInterval]
        passesEssentialRules = passesFirstSpeciesRules firstSpeciesEssentialRules  

-- returns probability distribution on neighbor states (Nothing if the set is empty)
firstSpeciesNeighborDist :: RandomGen g => FirstSpeciesSetup -> FirstSpeciesState -> FirstSpeciesTransitionCostFunc -> Rand g (Maybe (DiscreteDist () FirstSpeciesState))
firstSpeciesNeighborDist setup state costFunc = do
    neighbors <- firstSpeciesNeighbors setup state
    let costs = costFunc state <$> neighbors
    let pairs = sortOn fst (zip costs neighbors)
    let (costs', neighbors') = unzip $ trace ((unlines $ show <$> pairs) ++ "-----------------------") pairs
    --let (costs', neighbors') = unzip pairs
    let probs = exp . negate <$> costs'
    let neighborDist = case neighbors of
                            []        -> Nothing
                            otherwise -> Just $ discreteDist () neighbors' probs
    return neighborDist

-- generates list of random neighbors via sampling without replacement
firstSpeciesRandomNeighborStates :: (RandomGen g) => FirstSpeciesSetup -> FirstSpeciesTransitionCostFunc -> NeighborGenM (Rand g) FirstSpeciesState
firstSpeciesRandomNeighborStates setup costFunc state = do
    neighborDist <- firstSpeciesNeighborDist setup state costFunc
    case neighborDist of
        Nothing   -> return []
        Just dist -> samplesWithoutReplacement dist

type MonadicSearcher m s c = NeighborGenM m s -> (s -> s -> c) -> (s -> Bool) -> s -> m (Maybe [s])

-- workhorse for first species counterpoint generation
generateFirstSpecies :: RandomGen g => FirstSpeciesSetup -> NeighborGenM (Rand g) FirstSpeciesState -> FirstSpeciesTransitionCostFunc -> MonadicSearcher (Rand g) FirstSpeciesState Double -> Rand g (Maybe FirstSpecies)
generateFirstSpecies (FirstSpeciesSetup {fsKey, fsCf = (cfVoice, cf), fsCptVoice, genPolicy}) neighborGen costFunc searcher = do
    startIdx <- case genPolicy of
                    ForwardPolicy  -> return $ -1
                    BackwardPolicy -> return n
                    RandomPolicy   -> sample idxDist
    let startState = (startIdx, V.replicate n Nothing)
    maybeStates <- searcher neighborGen costFunc solutionFound startState
    return $ do
        states <- maybeStates
        let (_, cptPitches) = head states
        let cpt = unfoldTied (fromJust <$> V.toList cptPitches, cfBook)
        return $ First {key = fsKey, cantusFirmus = (cfVoice, cf), counterpoint = (fsCptVoice, cpt)}
    where
        (cfPitches, cfBook) = foldTied cf
        n = length cfPitches
        idxDist = uniformDiscreteDist () [0..(n-1)]
        solutionFound :: FirstSpeciesState -> Bool
        solutionFound (_, cpt) = all isJust $ V.toList cpt

randomFirstSpecies :: RandomGen g => FirstSpeciesSetup -> Rand g (Maybe FirstSpecies)
randomFirstSpecies setup = generateFirstSpecies setup neighborGen costFunc searcher
    where
        costFunc = firstSpeciesNeighborCost setup
        neighborGen = firstSpeciesRandomNeighborStates setup costFunc
        searcher nbrGen _ solutionFound startState = dfsM nbrGen solutionFound startState

greedyFirstSpecies :: RandomGen g => FirstSpeciesSetup -> Rand g (Maybe FirstSpecies)
greedyFirstSpecies setup = generateFirstSpecies setup neighborGen costFunc searcher
    where
        costFunc = firstSpeciesNeighborCost setup
        neighborGen = firstSpeciesNeighbors setup
        searcher nbrGen costFn solutionFound startState = fmap snd <$> greedySearchM nbrGen costFn solutionFound startState

beamSearchFirstSpecies :: RandomGen g => Int -> FirstSpeciesSetup -> Rand g (Maybe FirstSpecies)
beamSearchFirstSpecies width setup = generateFirstSpecies setup neighborGen costFunc searcher
    where
        costFunc = firstSpeciesNeighborCost setup
        neighborGen = firstSpeciesNeighbors setup
        searcher nbrGen costFn solutionFound startState = fmap snd . listToMaybe <$> beam
            where beam = beamSearchM width nbrGen costFn solutionFound startState

dijkstraFirstSpecies :: RandomGen g => FirstSpeciesSetup -> Rand g (Maybe FirstSpecies)
dijkstraFirstSpecies setup = generateFirstSpecies setup neighborGen costFunc searcher
    where
        costFunc = firstSpeciesNeighborCost setup
        neighborGen = firstSpeciesNeighbors setup
        searcher nbrGen costFn solutionFound startState = fmap (reverse . snd) <$> dijkstraM nbrGen costFn' (return . solutionFound) startState
            where
                costFn' state1 state2 = return $ costFn state1 state2

-- extracts setup information from an existing FirstSpecies
getFirstSpeciesSetup :: FirstSpeciesModel -> GenerationPolicy -> FirstSpecies -> FirstSpeciesSetup
getFirstSpeciesSetup model policy (First {key, cantusFirmus, counterpoint = (cptVoice, _)}) = FirstSpeciesSetup {fsModel = model, fsKey = key, fsCf = cantusFirmus, fsCptVoice = cptVoice, genPolicy = policy}


-- Fux --

fig5 = firstSpecies (D, Dorian) (Alto, "D4 F4 E4 D4 G4 F4 A4 G4 F4 E4 D4 ~D4") (Soprano, "A4 A4 G4 A4 B4 C5 C5 B4 D5 Cs5 D5 ~D5")
fig6Bad = firstSpecies (D, Dorian) (Alto, "D4 F4 E4 D4 G4 F4 A4 G4 F4 E4 D4 ~D4") (Tenor, "G3 D4 A3 F3 E3 D3 F3 C4 D4 Cs4 D4 ~D4")
fig6Good = firstSpecies (D, Dorian) (Alto, "D4 F4 E4 D4 G4 F4 A4 G4 F4 E4 D4 ~D4") (Tenor, "D3 D3 A3 F3 E3 D3 F3 C4 D4 Cs4 D4 ~D4")
fig11 = firstSpecies (E, Phrygian) (Alto, "E4 C4 D4 C4 A3 A4 G4 E4 F4 E4 ~E4") (Soprano, "B4 C5 F4 G4 A4 C5 B4 E5 D5 E5 ~E5")
fig12Bad = firstSpecies (E, Phrygian) (Alto, "E4 C4 D4 C4 A3 A4 G4 E4 F4 E4 ~E4") (Tenor, "E3 A3 D3 E3 F3 F3 B3 C4 D4 E4 ~E4")
fig12Good = firstSpecies (E, Phrygian) (Alto, "E4 C4 D4 C4 A3 A4 G4 E4 F4 E4 ~E4") (Tenor, "E3 A3 D3 E3 F3 F3 C4 C4 D4 E4 ~E4")
fig13 = firstSpecies (F, Lydian) (Tenor, "F3 G3 A3 F3 D3 E3 F3 C4 A3 F3 G3 F3 ~F3") (Alto, "F4 E4 C4 F4 F4 G4 A4 G4 C4 F4 E4 F4 ~F4")
fig14 = firstSpecies (F, Lydian) (Tenor, "F3 G3 A3 F3 D3 E3 F3 C4 A3 F3 G3 F3 ~F3") (Bass, "F3 E3 F3 A3 Bf3 G3 A3 E3 F3 D3 E3 F3 ~F3")
fig15Bad = firstSpecies (G, Mixolydian) (Alto, "G3 C4 B3 G3 C4 E4 D4 G4 E4 C4 D4 B3 A3 G3 ~G3") (Soprano, "G4 E4 D4 G4 G4 G4 A4 B4 G4 E5 D5 G4 Fs4 G4 ~G4")
fig15Bad' = firstSpecies (G, Mixolydian) (Alto, "G3 C4 B3 G3 C4 E4 D4 G4 E4 C4 D4 B3 A3 G3 ~G3") (Soprano, "G4 E4 D4 G4 G4 G4 A4 B4 G4 F5 D5 G4 Fs4 G4 ~G4")
fig15Good = firstSpecies (G, Mixolydian) (Alto, "G3 C4 B3 G3 C4 E4 D4 G4 E4 C4 D4 B3 A3 G3 ~G3") (Soprano, "G4 E4 D4 G4 G4 G4 A4 B4 G4 C5 A4 G4 Fs4 G4 ~G4")
fig21 = firstSpecies (G, Mixolydian) (Alto, "G3 C4 B3 G3 C4 E4 D4 G4 E4 C4 D4 B3 A3 G3 ~G3") (Tenor, "G3 A3 G3 E3 E3 C3 G3 B3 C4 A3 Fs3 G3 Fs3 G3 ~G3")
fig22 = firstSpecies (A, Aeolian) (Alto, "A3 C4 B3 D4 C4 E4 F4 E4 D4 C4 B3 A3 ~A3") (Soprano, "A4 E4 G4 F4 E4 C5 A4 B4 B4 A4 Gs4 A4 ~A4")
fig23 = firstSpecies (A, Aeolian) (Alto, "A3 C4 B3 D4 C4 E4 F4 E4 D4 C4 B3 A3 ~A3") (Tenor, "A3 A3 G3 F3 E3 E3 D3 C3 G3 A3 Gs3 A3 ~A3")

fuxFirstSpeciesBad = [fig6Bad, fig12Bad, fig15Bad, fig15Bad']
fuxFirstSpeciesGood = [fig5, fig6Good, fig11, fig12Good, fig13, fig14, fig15Good, fig21, fig22, fig23]
fuxFirstSpeciesModelNoSmooth = trainFirstSpeciesModel fuxFirstSpeciesGood 0.0
fuxFirstSpeciesModelSmooth = trainFirstSpeciesModel fuxFirstSpeciesGood 1.0

--testFsGen = fromJust $ generateFirstSpecies' fuxFirstSpeciesModelSmooth (D, Dorian) (cantusFirmus fig5) Soprano

fig5Dijkstra = First {key = (D,Dorian), cantusFirmus = (Alto,[Untied [] (D,4),Untied [] (F,4),Untied [] (E,4),Untied [] (D,4),Untied [] (G,4),Untied [] (F,4),Untied [] (A,4),Untied [] (G,4),Untied [] (F,4),Untied [] (E,4),Untied [] (D,4),Tied (D,4)]), counterpoint = (Soprano,[Untied [] (D,5),Untied [] (C,5),Untied [] (C,5),Untied [] (D,5),Untied [] (E,5),Untied [] (D,5),Untied [] (C,5),Untied [] (B,4),Untied [] (C,5),Untied [] (Cs,5),Untied [] (D,5),Tied (D,5)])}

fig6Dijkstra = First {key = (D,Dorian), cantusFirmus = (Alto,[Untied [] (D,4),Untied [] (F,4),Untied [] (E,4),Untied [] (D,4),Untied [] (G,4),Untied [] (F,4),Untied [] (A,4),Untied [] (G,4),Untied [] (F,4),Untied [] (E,4),Untied [] (D,4),Tied (D,4)]), counterpoint = (Tenor,[Untied [] (D,4),Untied [] (D,4),Untied [] (C,4),Untied [] (B,3),Untied [] (E,4),Untied [] (D,4),Untied [] (F,4),Untied [] (E,4),Untied [] (D,4),Untied [] (Cs,4),Untied [] (D,4),Tied (D,4)])}

fig11Dijkstra = First {key = (E,Phrygian), cantusFirmus = (Alto,[Untied [] (E,4),Untied [] (C,4),Untied [] (D,4),Untied [] (C,4),Untied [] (A,3),Untied [] (A,4),Untied [] (G,4),Untied [] (E,4),Untied [] (F,4),Untied [] (E,4),Tied (E,4)]), counterpoint = (Soprano,[Untied [] (B,4),Untied [] (A,4),Untied [] (B,4),Untied [] (A,4),Untied [] (A,4),Untied [] (C,5),Untied [] (D,5),Untied [] (E,5),Untied [] (D,5),Untied [] (E,5),Tied (E,5)])}

fig12Dijkstra = First {key = (E,Phrygian), cantusFirmus = (Alto,[Untied [] (E,4),Untied [] (C,4),Untied [] (D,4),Untied [] (C,4),Untied [] (A,3),Untied [] (A,4),Untied [] (G,4),Untied [] (E,4),Untied [] (F,4),Untied [] (E,4),Tied (E,4)]), counterpoint = (Tenor,[Untied [] (E,4),Untied [] (E,4),Untied [] (F,4),Untied [] (G,4),Untied [] (A,4),Untied [] (F,4),Untied [] (E,4),Untied [] (C,4),Untied [] (D,4),Untied [] (E,4),Tied (E,4)])}

fig13Dijkstra = First {key = (F,Lydian), cantusFirmus = (Tenor,[Untied [] (F,3),Untied [] (G,3),Untied [] (A,3),Untied [] (F,3),Untied [] (D,3),Untied [] (E,3),Untied [] (F,3),Untied [] (C,4),Untied [] (A,3),Untied [] (F,3),Untied [] (G,3),Untied [] (F,3),Tied (F,3)]), counterpoint = (Alto,[Untied [] (F,4),Untied [] (D,4),Untied [] (C,4),Untied [] (C,4),Untied [] (D,4),Untied [] (B,3),Untied [] (A,3),Untied [] (A,3),Untied [] (C,4),Untied [] (D,4),Untied [] (E,4),Untied [] (F,4),Tied (F,4)])}

fig15Dijkstra = First {key = (G,Mixolydian), cantusFirmus = (Alto,[Untied [] (G,3),Untied [] (C,4),Untied [] (B,3),Untied [] (G,3),Untied [] (C,4),Untied [] (E,4),Untied [] (D,4),Untied [] (G,4),Untied [] (E,4),Untied [] (C,4),Untied [] (D,4),Untied [] (B,3),Untied [] (A,3),Untied [] (G,3),Tied (G,3)]), counterpoint = (Soprano,[Untied [] (D,4),Untied [] (E,4),Untied [] (D,4),Untied [] (G,4),Untied [] (A,4),Untied [] (G,4),Untied [] (F,4),Untied [] (E,4),Untied [] (B,4),Untied [] (C,5),Untied [] (A,4),Untied [] (G,4),Untied [] (Fs,4),Untied [] (G,4),Tied (G,4)])}




--FirstSpeciesSetup {fsModel = model, fsKey = key, fsCf = cantusFirmus, fsCptVoice = cptVoice, genPolicy = policy}
twinkleSetup policy cptVoice = FirstSpeciesSetup {fsModel = fuxFirstSpeciesModelSmooth, fsKey = (C, Ionian), fsCf = (Alto, parseLine "C4 C4 G4 G4 A4 A4 G4 ~G4 F4 F4 E4 E4 D4 D4 C4 ~C4"), fsCptVoice = cptVoice, genPolicy = policy}

macdonaldSetup policy cptVoice = FirstSpeciesSetup {fsModel = fuxFirstSpeciesModelSmooth, fsKey = (G, Ionian), fsCf = (Alto, parseLine "G4 G4 G4 D4 E4 E4 D4 ~D4 B4 B4 A4 A4 G4 ~G4 ~G4 ~G4"), fsCptVoice = cptVoice, genPolicy = policy}

castlevaniaSetup policy cptVoice = FirstSpeciesSetup {fsModel = fuxFirstSpeciesModelSmooth, fsKey = (Bf, Aeolian), fsCf = (Soprano, parseLine "Bf4 F5 Ef5 Df5 C5 Df5 C5 Bf4 C5 Df5 Ef5 Df5 C5 Af4 C5 Bf4 ~Bf4"), fsCptVoice = cptVoice, genPolicy = policy}
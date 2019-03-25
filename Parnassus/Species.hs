-- Species Counterpoint --

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE NamedFieldPuns #-}

module Parnassus.Species where

import Algorithm.Search (dijkstraM)
import Control.Exception.Base (assert)
import Control.Monad (join)
import Control.Monad.Random (evalRandIO, Rand)
import Data.Char (isDigit, ord)
import Data.List (inits, sortOn, zip4)
import Data.List.Split (splitOn)
import qualified Data.Map as M
import Data.Maybe (fromJust, fromMaybe, isJust, listToMaybe)
import Data.Range.Range (Range (SpanRange), fromRanges, inRange)
import Data.Ratio ((%))
import qualified Data.Vector as V
import System.Random (RandomGen)

import Euterpea (AbsPitch, Control (..), InstrumentName (..), Mode (..), Pitch, PitchClass (..), absPitch, pitch)

import Parnassus.Utils (ngrams)
import Parnassus.Dist (discreteDist, DiscreteDist (..), getLogProb, sample, samplesWithoutReplacement, trainDiscrete, uniformDiscreteDist)
import Parnassus.Markov (boundedIntegerRandomWalk, markovConditionOn)
import Parnassus.MusicBase (Key, MusicT (..), Pitched (getPitch), (/=/))
import Parnassus.MusicD (MusicD (MusicD), Tied (..), extractTied, isTied)
import Parnassus.Search (beamSearchM, dfsM, greedySearchM, NeighborGenM)

import Debug.Trace


-- HELPER FUNCTIONS --

maybePair :: (Maybe a, Maybe b) -> Maybe (a, b)
maybePair (Just x, Just y) = Just (x, y)
maybePair _                = Nothing

-- gets windows of a list
-- n is the amount to the left and right of the current element
getWindows :: Int -> [a] -> [[a]]
getWindows n xs = (drop (n + 1) $ take (2 * n + 1) $ inits xs) ++ ngrams (2 * n + 1) xs ++ reverse (reverse <$> (drop (n + 1) $ take (2 * n + 1) $ inits $ reverse xs))

-- folds a list of Tied a into a list of a and some bookkeeping that will allow us to reconstruct the original list
-- (loses all Control information)
foldTied :: [Tied a] -> ([a], [(Bool, Int)])
foldTied xs = (reverse items, reverse pairs)
    where
        foldTied' []                = ([], [])
        foldTied' ((Untied _ x):xs) = ((x:ys), (True, 1):pairs)
            where (ys, pairs) = foldTied' xs
        foldTied' ((Tied x):xs)     = (ys, (True, n + 1):(tail pairs))
            where
                (ys, pairs) = foldTied' xs
                (_, n) = head pairs
        foldTied' (Rest:xs)         = (ys, (False, 1):pairs)
            where (ys, pairs) = foldTied' xs
        (items, pairs) = foldTied' $ reverse xs

-- given folded representation of [Tied a], converts back to the original representation
unfoldTied :: ([a], [(Bool, Int)]) -> [Tied a]
unfoldTied (_, [])                = []
unfoldTied (xs, (False, n):pairs) = replicate n Rest ++ unfoldTied (xs, pairs)
unfoldTied (x:xs, (True, n):pairs) = Untied [] x : (replicate (n - 1) (Tied x) ++ unfoldTied (xs, pairs))

-- given a list of items, returns a list of (a, count) pairs, where count counts the number of successive identical items
foldRepeats :: (Eq a) => [a] -> [(a, Int)]
foldRepeats xs = reverse $ foldRepeats' $ reverse xs
    where
        foldRepeats' [] = []
        foldRepeats' [x] = [(x, 1)]
        foldRepeats' (x:xs) = if (x == head xs) then ((x, n + 1) : tail ys) else ((x, 1) : foldRepeats' xs)
            where
                ys = foldRepeats' xs
                (_, n) = head ys

-- gets the max number of repeated elements in a list (that are not Nothing) and the element itself
maxRepeats :: (Eq a) => [Maybe a] -> Int
maxRepeats xs = maximum $ [0] ++ (snd <$> filter (isJust . fst) (foldRepeats xs))


-- VOICES --

data Voice = Bass | Tenor | Alto | Soprano
    deriving (Eq, Ord, Show)

-- vocal range for each voice part
voiceRange :: Voice -> Range AbsPitch
voiceRange Bass    = SpanRange (absPitch (E, 2)) (absPitch (E, 4))
voiceRange Tenor   = SpanRange (absPitch (C, 3)) (absPitch (A, 4))
voiceRange Alto    = SpanRange (absPitch (F, 3)) (absPitch (F, 5))
voiceRange Soprano = SpanRange (absPitch (C, 4)) (absPitch (A, 5))

-- returns True if the voice can sing the note
voiceCanSing :: Voice -> Pitch -> Bool
voiceCanSing voice p = inRange (voiceRange voice) (absPitch p)

-- SCALES & MODES --

-- only church modes are acceptable for species counterpoint
convertMode :: Mode -> Mode
convertMode Major = Ionian
convertMode Minor = Aeolian
convertMode Locrian = error "Locrian is an invalid mode for species counterpoint"
convertMode (CustomMode _) = error "CustomMode is an invalid mode for species counterpoint"
convertMode mode = mode

-- returns True if two pitch classes are equivalent
equivPitchClass :: PitchClass -> PitchClass -> Bool
equivPitchClass pc1 pc2 = (absPitch (pc1, 4)) == (absPitch (pc2, 4))

-- transposes a pitch class by some number of semitones
shiftPitch :: PitchClass -> Int -> PitchClass
shiftPitch pc shift = fst $ pitch $ absPitch (pc, 4) + shift

-- gets the first degree of the key
firstDegree :: Key -> PitchClass
firstDegree (pc, mode) = shiftPitch pc i
    where i = case mode of
            Dorian     -> 10
            Phrygian   -> 8
            Lydian     -> 7
            Mixolydian -> 5
            Aeolian    -> 3
            Minor      -> 3
            Locrian    -> 1
            otherwise  -> 0

-- gets the pitch class scale for a given key
-- extra boolean flag indicating whether to include "extra" nodes (e.g. leading tone) sometimes permitted in the scale
scaleForKey :: Key -> Bool -> [PitchClass]
scaleForKey (pc, mode) extra = [shiftPitch pc j | j <- scale]
    where
        scale = case mode of
            Dorian -> if extra then [0, 2, 3, 5, 7, 9, 10, 11] else [0, 2, 3, 5, 7, 9, 10]
            Phrygian -> [0, 1, 3, 5, 7, 8, 10]
            Lydian -> if extra then [0, 2, 4, 5, 6, 7, 9, 11] else [0, 2, 4, 6, 7, 9, 11]
            Mixolydian -> if extra then [0, 2, 4, 5, 7, 9, 10, 11] else [0, 2, 4, 5, 7, 9, 10]
            Aeolian -> if extra then [0, 2, 3, 5, 7, 8, 10, 11] else [0, 2, 3, 5, 7, 8, 10]
            Minor -> if extra then [0, 2, 3, 5, 7, 8, 10, 11] else [0, 2, 3, 5, 7, 8, 10]
            Locrian -> [0, 1, 3, 5, 6, 8, 10]
            otherwise -> [0, 2, 4, 5, 7, 9, 11]

-- INTERVALS --

type Interval = (Pitch, Pitch) -- a pair of notes

-- (signed) number of semitones between the two pitches
intervalDisplacement :: Interval -> Int
intervalDisplacement (p1, p2) = absPitch p2 - absPitch p1

-- (unsigned) number of semitones between the two pitches
intervalDistance :: Interval -> Int
intervalDistance = abs . intervalDisplacement

-- returns the ordering of the two pitches
intervalOrdering :: Interval -> Ordering
intervalOrdering (p1, p2) = compare p1 p2

-- three broad categories of intervals, according to Fux
data IntervalType = PerfectConsonance | ImperfectConsonance | Dissonance
    deriving (Eq, Show)

-- gets the type of interval for two adjacent notes
melodicIntervalType :: PitchClass -> Interval -> IntervalType
melodicIntervalType root interval
    | (dist `elem` [0, 7])                             = PerfectConsonance
    | (dist `elem` [3, 4, 5])                          = ImperfectConsonance
    | (dist == 8) && (intervalOrdering interval /= LT) = ImperfectConsonance  -- descent by a minor 6th is consonant
    | otherwise                                        = Dissonance
    where dist = intervalDistance interval `mod` 12

-- gets the type of interval for two notes in parallel (main voice is the first note, harmony the second)
harmonicIntervalType :: PitchClass -> Interval -> IntervalType
harmonicIntervalType root interval@(p1, p2)
    | (dist `elem` [0, 7])                           = PerfectConsonance
    | (dist `elem` [3, 4, 8, 9])                     = ImperfectConsonance
    -- Fux calls the fourth a consonance if the top note is the fundamental
    | (dist == 5) && ((fst $ snd interval') == root) = ImperfectConsonance
    | otherwise                                      = Dissonance
    where
        dist = intervalDistance interval `mod` 12
        interval' = if (intervalOrdering interval == GT) then (p2, p1) else (p1, p2)

-- gets the position of a Pitch in the diatonic staff (i.e. mod 7)
diatonicPosition :: Pitch -> Int
diatonicPosition (pc, oct) = 7 * oct + (posTable M.! (head $ show pc))
    where posTable = M.fromList $ zip "CDEFGAB" [0..]

-- gets the (signed) interval number (staff distance) of an interval, e.g. "first", "third", etc.
intervalNumber :: Interval -> Int
intervalNumber (p1, p2) = if (diff >= 0) then (diff + 1) else (diff - 1)
    where diff = diatonicPosition p2 - diatonicPosition p1

-- a pair of adjacent harmonic intervals
type PairwiseMotion = (Interval, Interval)

data MotionType = Parallel | Contrary | Oblique
    deriving (Eq, Show)

-- gets the type of motion for a pair of intervals
motionType :: PairwiseMotion -> MotionType
motionType ((p1, p2), (p1', p2'))
    | (n1 == n1') || (n2 == n2') = Oblique
    | (n1 < n1') && (n2 > n2')   = Contrary
    | (n1 > n1') && (n2 < n2')   = Contrary
    | otherwise                  = Parallel
    where [n1, n2, n1', n2'] = absPitch <$> [p1, p2, p1', p2']

-- converts PairwiseMotion from adjacent harmonies to concurrent motions
motionTranspose :: PairwiseMotion -> (Interval, Interval)
motionTranspose ((p1, p2), (p1', p2')) = ((p1, p1'), (p2, p2'))
        
-- PARSING --

-- parses a note from a string: starts with the pitch, followed by an octave, followed by ~ if it is tied; a rest is simply a *
parseNote :: String -> Tied Pitch
parseNote "*" = Rest
parseNote s   = tp
    where
        (tie, s') = span (== '~') s
        (pc, s'') = span (not . isDigit) s'
        (oct, _) = span isDigit s''
        tp = case tie of
            ""        -> Untied [] (read pc, read oct)
            "~"       -> Tied (read pc, read oct)
            otherwise -> error "parse error"

parseLine :: String -> [Tied Pitch]
parseLine s = parseNote <$> filter (not . null) (splitOn " " s)

-- SPECIES --

data RuleCheck = Failed String | Passed
    deriving (Eq, Show)

class Species a where
    toMusicD :: a -> MusicD Pitch

-- First Species --

type VoiceLine = (Voice, [Tied Pitch])

data FirstSpecies = FirstSpecies { key :: Key, cantusFirmus :: VoiceLine, counterpoint :: VoiceLine}
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
            otherwise -> FirstSpecies {key = key', cantusFirmus = (cfVoice, cf), counterpoint = (cptVoice, cpt)}

instance Species FirstSpecies where
    toMusicD :: FirstSpecies -> MusicD Pitch
    toMusicD FirstSpecies {cantusFirmus = (_, cf), counterpoint = (_, cpt)} = modify (Tempo 3) $ (MusicD 1 [Instrument VoiceOohs] (pure <$> cf)) /=/ (MusicD 1 [Instrument VoiceOohs] (pure <$> cpt))

-- COUNTERPOINT RULES --

-- applies a sequence of rules to something, short-circuiting as soon as the first rule fails
-- for efficiency, it is best to put the filters in order of decreasing cheapness
checkRules :: [a -> RuleCheck] -> a -> RuleCheck
checkRules [] x     = Passed
checkRules (f:fs) x = case f x of
    fx@(Failed _) -> fx
    otherwise     -> checkRules fs x

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
firstSpeciesIntervals (FirstSpecies {cantusFirmus, counterpoint}) = intervals
    where
        (cfPitches, _) = foldTied (snd cantusFirmus)
        (cptPitches, _) = foldTied (snd counterpoint)
        intervals
            | length cfPitches == length cptPitches = zip cfPitches cptPitches
            | otherwise                             = error "mismatch between number of notes in C.F. and counterpoint"

-- converts a FirstSpecies into a list of contexts for evaluating rules
firstSpeciesContexts :: FirstSpecies -> [FirstSpeciesContext]
firstSpeciesContexts firstSpec@(FirstSpecies {key, cantusFirmus, counterpoint}) = contexts
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
        pair1 = listToMaybe $ filter (isJust . snd) $ zip [(1::Int)..] (reverse $ V.toList $ V.slice 0 i2 distances)
        pair2 = listToMaybe $ filter (isJust . snd) $ zip [(1::Int)..] (V.toList $ V.slice (i2 + 1) (n - i2 - 1) distances)
        extract (j, d) = (j, fromJust d)
        melodicRandomWalk = boundedIntegerRandomWalk melodicModel (-32, 32)
        condDist = markovConditionOn melodicRandomWalk (toInteger i2) (extract <$> pair1, extract <$> pair2)
        melodicCost = case (pair1, pair2) of
            (Nothing, Nothing) -> 0.0  -- indifferent to the first pitch chosen
            otherwise          -> -(getLogProb condDist cptNoteDist)
        --cost = harmonicCost + melodicCost
        cost = trace (show (i2, cptNote, (extract <$> pair1, extract <$> pair2), harmonicCost, melodicCost, harmonicCost + melodicCost)) $ harmonicCost + melodicCost

firstSpeciesNeighbors :: RandomGen g => FirstSpeciesSetup -> FirstSpeciesState -> Rand g [FirstSpeciesState]
firstSpeciesNeighbors (FirstSpeciesSetup {fsKey, fsCf = (cfVoice, cf), fsCptVoice, genPolicy}) state@(i, cpt) = do
    i' <- case genPolicy of
        ForwardPolicy  -> return (i + 1)
        BackwardPolicy -> return (i - 1)
        RandomPolicy   -> error "RandomPolicy not yet supported"
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
        return $ FirstSpecies {key = fsKey, cantusFirmus = (cfVoice, cf), counterpoint = (fsCptVoice, cpt)}
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
getFirstSpeciesSetup model policy (FirstSpecies {key, cantusFirmus, counterpoint = (cptVoice, _)}) = FirstSpeciesSetup {fsModel = model, fsKey = key, fsCf = cantusFirmus, fsCptVoice = cptVoice, genPolicy = policy}


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

fig5Dijkstra = FirstSpecies {key = (D,Dorian), cantusFirmus = (Alto,[Untied [] (D,4),Untied [] (F,4),Untied [] (E,4),Untied [] (D,4),Untied [] (G,4),Untied [] (F,4),Untied [] (A,4),Untied [] (G,4),Untied [] (F,4),Untied [] (E,4),Untied [] (D,4),Tied (D,4)]), counterpoint = (Soprano,[Untied [] (D,5),Untied [] (C,5),Untied [] (C,5),Untied [] (D,5),Untied [] (E,5),Untied [] (D,5),Untied [] (C,5),Untied [] (B,4),Untied [] (C,5),Untied [] (Cs,5),Untied [] (D,5),Tied (D,5)])}

fig6Dijkstra = FirstSpecies {key = (D,Dorian), cantusFirmus = (Alto,[Untied [] (D,4),Untied [] (F,4),Untied [] (E,4),Untied [] (D,4),Untied [] (G,4),Untied [] (F,4),Untied [] (A,4),Untied [] (G,4),Untied [] (F,4),Untied [] (E,4),Untied [] (D,4),Tied (D,4)]), counterpoint = (Tenor,[Untied [] (D,4),Untied [] (D,4),Untied [] (C,4),Untied [] (B,3),Untied [] (E,4),Untied [] (D,4),Untied [] (F,4),Untied [] (E,4),Untied [] (D,4),Untied [] (Cs,4),Untied [] (D,4),Tied (D,4)])}

fig11Dijkstra = FirstSpecies {key = (E,Phrygian), cantusFirmus = (Alto,[Untied [] (E,4),Untied [] (C,4),Untied [] (D,4),Untied [] (C,4),Untied [] (A,3),Untied [] (A,4),Untied [] (G,4),Untied [] (E,4),Untied [] (F,4),Untied [] (E,4),Tied (E,4)]), counterpoint = (Soprano,[Untied [] (B,4),Untied [] (A,4),Untied [] (B,4),Untied [] (A,4),Untied [] (A,4),Untied [] (C,5),Untied [] (D,5),Untied [] (E,5),Untied [] (D,5),Untied [] (E,5),Tied (E,5)])}

fig12Dijkstra = FirstSpecies {key = (E,Phrygian), cantusFirmus = (Alto,[Untied [] (E,4),Untied [] (C,4),Untied [] (D,4),Untied [] (C,4),Untied [] (A,3),Untied [] (A,4),Untied [] (G,4),Untied [] (E,4),Untied [] (F,4),Untied [] (E,4),Tied (E,4)]), counterpoint = (Tenor,[Untied [] (E,4),Untied [] (E,4),Untied [] (F,4),Untied [] (G,4),Untied [] (A,4),Untied [] (F,4),Untied [] (E,4),Untied [] (C,4),Untied [] (D,4),Untied [] (E,4),Tied (E,4)])}

fig13Dijkstra = FirstSpecies {key = (F,Lydian), cantusFirmus = (Tenor,[Untied [] (F,3),Untied [] (G,3),Untied [] (A,3),Untied [] (F,3),Untied [] (D,3),Untied [] (E,3),Untied [] (F,3),Untied [] (C,4),Untied [] (A,3),Untied [] (F,3),Untied [] (G,3),Untied [] (F,3),Tied (F,3)]), counterpoint = (Alto,[Untied [] (F,4),Untied [] (D,4),Untied [] (C,4),Untied [] (C,4),Untied [] (D,4),Untied [] (B,3),Untied [] (A,3),Untied [] (A,3),Untied [] (C,4),Untied [] (D,4),Untied [] (E,4),Untied [] (F,4),Tied (F,4)])}

fig15Dijkstra = FirstSpecies {key = (G,Mixolydian), cantusFirmus = (Alto,[Untied [] (G,3),Untied [] (C,4),Untied [] (B,3),Untied [] (G,3),Untied [] (C,4),Untied [] (E,4),Untied [] (D,4),Untied [] (G,4),Untied [] (E,4),Untied [] (C,4),Untied [] (D,4),Untied [] (B,3),Untied [] (A,3),Untied [] (G,3),Tied (G,3)]), counterpoint = (Soprano,[Untied [] (D,4),Untied [] (E,4),Untied [] (D,4),Untied [] (G,4),Untied [] (A,4),Untied [] (G,4),Untied [] (F,4),Untied [] (E,4),Untied [] (B,4),Untied [] (C,5),Untied [] (A,4),Untied [] (G,4),Untied [] (Fs,4),Untied [] (G,4),Tied (G,4)])}



twinkle = firstSpecies (C, Ionian) (Alto, "C4 C4 G4 G4 A4 A4 G4 ~G4 F4 F4 E4 E4 D4 D4 C4 ~C4") (Soprano, "C4 C4 G4 G4 A4 A4 G4 ~G4 F4 F4 E4 E4 D4 D4 C4 ~C4")
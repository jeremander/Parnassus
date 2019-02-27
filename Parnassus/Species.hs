-- Species Counterpoint --

{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE NamedFieldPuns #-}

module Parnassus.Species where

import Control.Exception.Base (assert)
import Data.Char (isDigit, ord)
import Data.List (inits, zip4)
import Data.List.Split (splitOn)
import qualified Data.Map as M
import Data.Maybe (fromMaybe, isJust)
import Data.Range.Range (Range (SpanRange), inRange)
import Data.Ratio ((%))
import qualified Data.Vector.Unboxed as V

import Euterpea (AbsPitch, Control (..), InstrumentName (..), Mode (..), Pitch, PitchClass (..), absPitch, pitch)

import Parnassus.MusicBase (Key, MusicT (..), Pitched (getPitch), (/=/))
import Parnassus.MusicD (MusicD (MusicD), Tied (..), extractTied, isTied)
import Parnassus.Markov (ngrams)


-- HELPER FUNCTIONS --

-- gets windows of a list
-- n is the amount to the left and right of the current element
getWindows :: Int -> [a] -> [[a]]
getWindows n xs = (drop (n + 1) $ take (2 * n + 1) $ inits xs) ++ ngrams (2 * n + 1) xs ++ reverse (reverse <$> (drop (n + 1) $ take (2 * n + 1) $ inits $ reverse xs))

-- TODO: improve normalization from [Tied a] to [a] (make it perfectly reversible)
-- given a list of Tied a, returns a list of (a, count) pairs, where count counts the number of successive tied items
-- rests are not permitted
foldTied :: [Tied a] -> [(a, Int)]
foldTied xs = reverse $ foldTied' $ reverse xs
    where
        foldTied' [] = []
        foldTied' ((Untied _ x):xs) = (x, 1) : foldTied' xs
        foldTied' ((Tied x):xs)     = (x, n + 1) : tail ys
            where
                ys = foldTied' xs
                (y, n) = head ys
        foldTied' (Rest:xs)         = error "Rest is invalid"

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
scaleForKey :: Key -> [PitchClass]
scaleForKey (pc, mode) = take 7 [shiftPitch pc' j | j <- drop i (cycle majorScale)]
    where
        majorScale = [0, 2, 4, 5, 7, 9, 11]
        i = case mode of
            Dorian -> 1
            Phrygian -> 2
            Lydian -> 3
            Mixolydian -> 4
            Aeolian -> 5
            Minor -> 5
            Locrian -> 6
            otherwise -> 0
        pc' = shiftPitch pc (-(majorScale !! i))

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

-- a pair of adjacent intervals
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
        cfPitches  = fst <$> foldTied cf
        cadencePitchClass = fst $ cfPitches !! (length cfPitches - 2)
        scale = scaleForKey key'
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
    s1Ordering :: Bool  -- False if CF <= CPT, True if CF > CPT
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

fsRuleCheck12 (FirstSpecContext {s1Constants = FirstSpecConsts {s1Key = (pc, _)}, s1Interval}) =
    (intervalDistance s1Interval <= 16) &&
    (harmonicIntervalType pc s1Interval /= Dissonance)

fsRule12 = FirstSpecRule {
    s1RuleCheck = fsRuleCheck12,
    s1RuleDescr = "Intervals must be consonant, and no larger than a major 10th.",
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

firstSpeciesRules =
    [fsRule12] ++  -- uses current interval
    [fsRule0, fsRule1, fsRule3, fsRule4, fsRule7] ++  -- uses current index & interval
    [fsRule2, fsRule5, fsRule6, fsRule8, fsRule9, fsRule10, fsRule11, fsRule13, fsRule14, fsRule15] ++  -- uses motions
    [fsRule16, fsRule17]  -- uses 7-long interval window

firstSpeciesEssentialRules = filter s1RuleIsEssential firstSpeciesRules

-- gets the sequence of intervals for first species (eliminating ties)
firstSpeciesIntervals :: FirstSpecies -> [Interval]
firstSpeciesIntervals (FirstSpecies {cantusFirmus, counterpoint}) = intervals
    where
        cfPitches  = fst <$> foldTied (snd cantusFirmus)
        cptPitches = fst <$> foldTied (snd counterpoint)
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

-- Generation --

type FirstSpeciesState = (V.Vector (Maybe Pitch), V.Vector (Maybe Pitch))


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
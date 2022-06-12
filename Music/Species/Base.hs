{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}

module Music.Species.Base where

import Data.Char (isDigit)
import qualified Data.Map as M
import Data.Maybe (fromJust, isJust)
import Data.Range (Bound(..), BoundType(..), inRange, Range (SpanRange))
import Data.List (inits)
import Data.List.Split (splitOn)

import Euterpea (absPitch, AbsPitch, Control (..), InstrumentName (..), Mode (..), pitch, Pitch, PitchClass (..))
import Misc.Utils (justOrError, ngrams, safeHead)
import Music.Pitch (Key)
import Music.Types.MusicT (MusicT(..))
import Music.Types.MusicD (extractTied, MusicD (MusicD), Tied (..), ToMusicD (..))


-- * Helper Functions

maybePair :: (Maybe a, Maybe b) -> Maybe (a, b)
maybePair (Just x, Just y) = Just (x, y)
maybePair _                = Nothing

-- | Gets windows of a list.
--   \(n\) is the amount to the left and right of the current element.
getWindows :: Int -> [a] -> [[a]]
getWindows n xs = (drop (n + 1) $ take (2 * n + 1) $ inits xs) ++ ngrams (2 * n + 1) xs ++ reverse (reverse <$> (drop (n + 1) $ take (2 * n + 1) $ inits $ reverse xs))

-- | Folds a list of @Tied a@ into a list of a and some bookkeeping that will allow us to reconstruct the original list (loses all 'Control' information).
foldTied :: [Tied a] -> ([a], [(Bool, Int)])
foldTied xs = (reverse items, reverse pairs)
    where
        foldTied' []                = ([], [])
        foldTied' ((Untied _ x):xs) = ((x:ys), (True, 1):pairs)
            where (ys, pairs) = foldTied' xs
        foldTied' ((Tied _):xs)     = (ys, (True, n + 1):(tail pairs))
            where
                (ys, pairs) = foldTied' xs
                (_, n) = head pairs
        foldTied' (RestD:xs)        = (ys, (False, 1):pairs)
            where (ys, pairs) = foldTied' xs
        (items, pairs) = foldTied' $ reverse xs

-- | Given folded representation of @[Tied a]@, converts back to the original representation.
unfoldTied :: ([a], [(Bool, Int)]) -> [Tied a]
unfoldTied (_, [])                 = []
unfoldTied (xs, (False, n):pairs)  = replicate n RestD ++ unfoldTied (xs, pairs)
unfoldTied (x:xs, (True, n):pairs) = Untied [] x : (replicate (n - 1) (Tied x) ++ unfoldTied (xs, pairs))
unfoldTied _                       = error "unexpected pattern"

-- | Given a list of items, returns a list of (a, count) pairs, where count counts the number of successive identical items.
foldRepeats :: (Eq a) => [a] -> [(a, Int)]
foldRepeats xs = reverse $ foldRepeats' $ reverse xs
    where
        foldRepeats' [] = []
        foldRepeats' [x] = [(x, 1)]
        foldRepeats' (x:xs) = if (x == head xs) then ((x, n + 1) : tail ys) else ((x, 1) : foldRepeats' xs)
            where
                ys = foldRepeats' xs
                (_, n) = head ys

-- | Gets the max number of repeated elements in a list (that are not Nothing) and the element itself.
maxRepeats :: (Eq a) => [Maybe a] -> Int
maxRepeats xs = maximum $ 0 : (snd <$> filter (isJust . fst) (foldRepeats xs))


-- * Voices

data Voice = Bass | Tenor | Alto | Soprano
    deriving (Enum, Eq, Ord, Show)

incBnd :: a -> Bound a
incBnd bnd = Bound bnd Inclusive

-- | Vocal range for each voice part.
voiceRange :: Voice -> Range AbsPitch
voiceRange Bass    = SpanRange (incBnd $ absPitch (E, 2)) (incBnd $ absPitch (E, 4))
voiceRange Tenor   = SpanRange (incBnd $ absPitch (C, 3)) (incBnd $ absPitch (A, 4))
voiceRange Alto    = SpanRange (incBnd $ absPitch (F, 3)) (incBnd $ absPitch (F, 5))
voiceRange Soprano = SpanRange (incBnd $ absPitch (C, 4)) (incBnd $ absPitch (A, 5))

-- | Returns True if the voice can sing the note.
voiceCanSing :: Voice -> Pitch -> Bool
voiceCanSing voice p = inRange (voiceRange voice) (absPitch p)

-- * Scales & Modes

-- | Only church modes are acceptable for species counterpoint.
convertMode :: Mode -> Mode
convertMode Major = Ionian
convertMode Minor = Aeolian
convertMode Locrian = error "Locrian is an invalid mode for species counterpoint"
convertMode (CustomMode _) = error "CustomMode is an invalid mode for species counterpoint"
convertMode mode = mode

-- | Returns True if two pitch classes are equivalent.
equivPitchClass :: PitchClass -> PitchClass -> Bool
equivPitchClass pc1 pc2 = (absPitch (pc1, 4)) == (absPitch (pc2, 4))

-- | Transposes a pitch class by some number of semitones.
shiftPitch :: PitchClass -> Int -> PitchClass
shiftPitch pc shift = fst $ pitch $ absPitch (pc, 4) + shift

-- | Gets the first degree of the key.
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
            _          -> 0

-- | Gets the pitch class scale for a given key.
--   An extra boolean flag indicating whether to include "extra" nodes (e.g. leading tone) is sometimes permitted in the scale.
scaleForKey :: Key -> Bool -> [PitchClass]
scaleForKey (pc, mode) extra = [shiftPitch pc j | j <- scale]
    where
        scale = case mode of
            Dorian     -> if extra then [0, 2, 3, 5, 7, 9, 10, 11] else [0, 2, 3, 5, 7, 9, 10]
            Phrygian   -> [0, 1, 3, 5, 7, 8, 10]
            Lydian     -> if extra then [0, 2, 4, 5, 6, 7, 9, 11] else [0, 2, 4, 6, 7, 9, 11]
            Mixolydian -> if extra then [0, 2, 4, 5, 7, 9, 10, 11] else [0, 2, 4, 5, 7, 9, 10]
            Aeolian    -> if extra then [0, 2, 3, 5, 7, 8, 10, 11] else [0, 2, 3, 5, 7, 8, 10]
            Minor      -> if extra then [0, 2, 3, 5, 7, 8, 10, 11] else [0, 2, 3, 5, 7, 8, 10]
            Locrian    -> [0, 1, 3, 5, 6, 8, 10]
            _          -> [0, 2, 4, 5, 7, 9, 11]

-- * Intervals

type Interval = (Pitch, Pitch) -- a pair of notes

-- | (Signed) number of semitones between the two pitches
intervalDisplacement :: Interval -> Int
intervalDisplacement (p1, p2) = absPitch p2 - absPitch p1

-- | (Unsigned) number of semitones between the two pitches
intervalDistance :: Interval -> Int
intervalDistance = abs . intervalDisplacement

-- | Returns the ordering of the two pitches.
intervalOrdering :: Interval -> Ordering
intervalOrdering (p1, p2) = compare p1 p2

-- | Three broad categories of intervals, according to Fux.
data IntervalType = PerfectConsonance | ImperfectConsonance | Dissonance
    deriving (Eq, Show)

-- | Gets the type of interval for two adjacent notes.
melodicIntervalType :: PitchClass -> Interval -> IntervalType
melodicIntervalType _ interval
    | (dist `elem` [0, 7])                             = PerfectConsonance
    | (dist `elem` [3, 4, 5])                          = ImperfectConsonance
    | (dist == 8) && (intervalOrdering interval /= LT) = ImperfectConsonance  -- descent by a minor 6th is consonant
    | otherwise                                        = Dissonance
    where dist = intervalDistance interval `mod` 12

-- | Gets the type of interval for two notes in parallel (main voice is the first note, harmony the second).
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

-- | Gets the position of a Pitch in the diatonic staff (i.e. mod 7).
diatonicPosition :: Pitch -> Int
diatonicPosition (pc, oct) = 7 * oct + (posTable M.! (head $ show pc))
    where posTable = M.fromList $ zip "CDEFGAB" [0..]

-- | Gets the (signed) interval number (staff distance) of an interval, e.g. "first", "third", etc.
intervalNumber :: Interval -> Int
intervalNumber (p1, p2) = if (diff >= 0) then (diff + 1) else (diff - 1)
    where diff = diatonicPosition p2 - diatonicPosition p1

-- | A pair of adjacent harmonic intervals.
type PairwiseMotion = (Interval, Interval)

data MotionType = Parallel | Contrary | Oblique
    deriving (Eq, Show)

-- | Gets the type of motion for a pair of intervals.
motionType :: PairwiseMotion -> MotionType
motionType ((p1, p2), (p1', p2'))
    | (n1 == n1') || (n2 == n2') = Oblique
    | (n1 < n1') && (n2 > n2')   = Contrary
    | (n1 > n1') && (n2 < n2')   = Contrary
    | otherwise                  = Parallel
    where [n1, n2, n1', n2'] = absPitch <$> [p1, p2, p1', p2']

-- | Converts 'PairwiseMotion' from adjacent harmonies to concurrent motions.
motionTranspose :: PairwiseMotion -> (Interval, Interval)
motionTranspose ((p1, p2), (p1', p2')) = ((p1, p1'), (p2, p2'))

-- * Parsing

-- | Parses a note from a string: starts with the pitch, followed by an octave, followed by ~ if it is tied; a rest is simply a *.
parseNote :: String -> Tied Pitch
parseNote "*" = RestD
parseNote s   = tp
    where
        (tie, s') = span (== '~') s
        (pc, s'') = break isDigit s'
        (oct, _) = span isDigit s''
        tp = case tie of
            ""  -> Untied [] (read pc, read oct)
            "~" -> Tied (read pc, read oct)
            _   -> error "parse error"

parseLine :: String -> [Tied Pitch]
parseLine s = parseNote <$> filter (not . null) (splitOn " " s)

-- * Species

type VoiceLineT a = (Voice, [Tied a])
type VoiceLine = VoiceLineT Pitch

data RuleCheck = Failed String | Passed
    deriving (Eq, Show)

data SpeciesT a = Species { key :: Key, cantusFirmus :: VoiceLineT a, counterpoint :: VoiceLineT a}
    deriving (Show)

type Species = SpeciesT Pitch

-- | Convert 'Species' to/from 'MusicD Pitch'.
instance ToMusicD SpeciesT Pitch where
    toMusicD Species {cantusFirmus = (_, cf), counterpoint = (_, cpt)} = modify (Tempo 3) $ (MusicD 1 [Instrument VoiceOohs] (pure <$> cf)) /=/ (MusicD 1 [Instrument VoiceOohs] (pure <$> cpt))
    fromMusicD (MusicD _ _ [cf, cpt]) = Species {key = key, cantusFirmus = (getVoice cfPitches, cf), counterpoint = (getVoice cptPitches, cpt)}
        where
            (cfPitches, _) = foldTied cf
            (cptPitches, _) = foldTied cpt
            getVoice :: [Pitch] -> Voice
            getVoice pitches = justOrError voice ("no voice can sing vocal line: " ++ show pitches)
                where voice = safeHead $ filter (\voice -> all (voiceCanSing voice) pitches) [(Bass)..(Soprano)]
            (pc, _) = fromJust $ extractTied $ head cf  -- first pitch of C.F. is the key
            validKeys = [(pc, mode) | mode <- [Ionian, Dorian, Phrygian, Lydian, Mixolydian, Aeolian]]
            pitchesFitKey :: Key -> [Pitch] -> Bool
            pitchesFitKey key = all (\(pc, _) -> pc `elem` scaleForKey key True)
            key = justOrError (safeHead $ filter (\k -> pitchesFitKey k (cfPitches ++ cptPitches)) validKeys) "no key could be found to accommodate all the notes"
    fromMusicD _ = error "input MusicD must consist of two voice lines"

-- | Applies a sequence of rules to something, short-circuiting as soon as the first rule fails.
--   For efficiency, it is best to put the filters in order of decreasing cheapness.
checkRules :: [a -> RuleCheck] -> a -> RuleCheck
checkRules [] _     = Passed
checkRules (f:fs) x = case f x of
    fx@(Failed _) -> fx
    _             -> checkRules fs x

-- | Convenience constructor from strings.
species :: Key -> (Voice, String) -> (Voice, String) -> Species
species (pc, mode) (cfVoice, cfStr) (cptVoice, cptStr) = spec
    where
        key' = (pc, convertMode mode)
        validateNote :: Voice -> Tied Pitch -> Bool
        validateNote voice (Untied _ p) = voiceCanSing voice p
        validateNote voice (Tied p)     = voiceCanSing voice p
        validateNote _ RestD            = False
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
            (x:_)  -> error s where (Failed s) = x
            _      -> Species {key = key', cantusFirmus = (cfVoice, cf), counterpoint = (cptVoice, cpt)}

-- | Global data for first species.
data SpeciesConstants = SpecConsts {
    specLength :: Int,    -- number of bars
    specKey :: Key,       -- key (fundamental)
    specOrdering :: Bool  -- True if CF <= CPT, False if CF > CPT
} deriving (Eq, Show)

-- | Local context of a note in a species.
--   This is the minimal data needed to determine if a violation is present locally.
data SpeciesContext = SpecContext {
    specConsts :: SpeciesConstants,
    specIndex :: Int,                        -- index of the present note
    specInterval :: Interval,                -- the present (vertical) interval
    specIntervalWindow :: [Maybe Interval],  -- up to 7-long window of surrounding intervals
    specMotions :: [Maybe PairwiseMotion]    -- 2-long list containing motion into present interval & into next interval
} deriving (Eq, Show)

data SpeciesRule = SpecRule {
    specRuleCheck :: SpeciesContext -> Bool,  -- returns True if the rule is satisfied
    specRuleDescr :: String,                  -- description of the rule
    specRuleIsEssential :: Bool               -- flag indicating whether the rule is "essential" (crucial)
}

-- | Given a rule and a context, checks the rule.
checkSpeciesRule :: SpeciesRule -> SpeciesContext -> RuleCheck
checkSpeciesRule (SpecRule {specRuleCheck, specRuleDescr}) context@(SpecContext {specIndex}) = result
    where
        passed = specRuleCheck context
        result = if passed then Passed else (Failed $ "Bar " ++ show specIndex ++ ": " ++ specRuleDescr)

-- | Given a list of rules, returns True if the given context passes all the rules.
passesFirstSpeciesRules :: [SpeciesRule] -> SpeciesContext -> Bool
passesFirstSpeciesRules rules context = all (== Passed) [checkSpeciesRule rule context | rule <- rules]

-- | Checks all of the given rules against all the given contexts.
--   Returns either Passed or Failed (with the first violation encountered).
checkSpeciesContexts :: [SpeciesRule] -> [SpeciesContext] -> RuleCheck
checkSpeciesContexts rules contexts = result
    where
        results = (checkSpeciesRule <$> rules) <*> contexts  -- nested for loop (rules are outer loop)
        firstFailed = dropWhile (== Passed) results
        result = case firstFailed of
            (x:_) -> x       -- first failure
            _     -> Passed  -- no failures, so we've passed

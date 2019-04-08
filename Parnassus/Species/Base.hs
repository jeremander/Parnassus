-- Species Counterpoint --

module Parnassus.Species.Base where

import Data.Char (isDigit)
import qualified Data.Map as M
import Data.Maybe (isJust)
import Data.Range.Range (inRange, Range (SpanRange))
import Data.List (inits)
import Data.List.Split (splitOn)

import Euterpea (absPitch, AbsPitch, Mode (..), pitch, Pitch, PitchClass (..))

import Parnassus.Utils (ngrams)
import Parnassus.MusicBase (Key)
import Parnassus.MusicD (MusicD (MusicD), Tied (..))


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

-- COUNTERPOINT RULES --

-- applies a sequence of rules to something, short-circuiting as soon as the first rule fails
-- for efficiency, it is best to put the filters in order of decreasing cheapness
checkRules :: [a -> RuleCheck] -> a -> RuleCheck
checkRules [] x     = Passed
checkRules (f:fs) x = case f x of
    fx@(Failed _) -> fx
    otherwise     -> checkRules fs x

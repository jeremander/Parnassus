{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}

module Music.Tuning where

import Data.Array ((!), (//), elems, listArray)
import Data.Counter (count, Counter)
import Data.Default (Default(..))
import Data.List (foldl', intersperse)
import qualified Data.Map as M
import Data.Maybe (catMaybes)
import Data.Range (Bound(..), BoundType(..), Range(..))
import Data.Ratio ((%))
import qualified Data.Set as S
import Data.Sort (sort, sortOn)
import qualified Data.Vector as V

import Euterpea (AbsPitch, Music(..), Music1, Note1, NoteAttribute(..), Pitch, PitchClass(..), ToMusic1, absPitch, applyControls, hn, mMap, note, pitch, qn, rest, shiftPitches)

import Misc.Utils (ToDouble(..), log2, mkArray, pairwise, pow2)
import Math.Optimize ((^-^), FinDiffParams(..), fdsaGradientDescent, mkFinDiffFuncs)
import Music.Pitch (ToPitch(..))
import Music.Types (MusicD(..), MusicT(..), Tied(..), ToMusicD(..))
import Music.Types.MusicD (extractTied, isTied)


-- * Types

-- | Frequency in Hz
type Freq = Double
-- | Pitch basis for a tuning system (e.g. tune A4 to 440 Hz)
type StdPitch = (Pitch, Freq)
-- | log2 of a ratio
type LogRatio = Double
-- | Represents tuning as a list of freqs mapping @[0..127]@ to Hz
type Tuning = [Freq]
-- | Interval in a scale, given by the two scale degrees
type ScaleInterval = (Int, Int)
-- | Ideal intervals @[0..11]@ (can allow multiple possibilities)
type IdealScale a = [[a]]
-- | Actual intervals @[0..11]@
type Scale a = [a]
-- | A named tuning system
type TuningSystem a = (String, Scale Double)
-- | A set of named tuning systems
type TuningPackage = [TuningSystem Double]


-- * Helper Functions

-- | Takes reciprocal of a rational if necessary so that it is \( \geq 1 \).
posRatio :: RealFrac a => a -> a
posRatio r = if r >= 1 then r else 1 / r

-- | Multiplies or divides by the right power of 2 until the rational is in [1, 2).
normalize2pow :: RealFrac a => a -> a
normalize2pow r
    | r < 1     = head $ dropWhile (< 1) [r * (2 ^ i) | i <- [1..]]
    | r >= 2    = head $ dropWhile (>= 2) [r / (2 ^ i) | i <- [1..]]
    | otherwise = r


-- * Intervals/Frequencies

a440 :: StdPitch
a440 = ((A, 4), 440)

semitone :: LogRatio
semitone = 1 / 12

cent :: LogRatio
cent = 1 / 1200

perfectFifth :: LogRatio
perfectFifth = log2 $ 3 / 2

syntonicComma :: LogRatio
syntonicComma = log2 $ 81 / 80

pythagoreanComma :: LogRatio
pythagoreanComma = 12 * log2 3 - 19

-- | Converts a ratio to a number of cents.
toCents :: (ToDouble a) => a -> Double
toCents x = log2 (toDouble x) / cent

-- | Shifts a frequency by a given log-ratio.
shiftFreq :: Freq -> LogRatio -> Freq
shiftFreq f shift = pow2 $ log2 f + shift

-- | Converts pitch to frequency, given a base frequency.
freq :: StdPitch -> Pitch -> Freq
freq (basePitch, baseFreq) pc = shiftFreq baseFreq (fromIntegral (absPitch pc - absPitch basePitch) * semitone)

-- | Converts pitch to frequency with A4 = 440 Hz base frequency.
stdFreq :: Pitch -> Freq
stdFreq = freq ((A, 4), 440)

-- | Standard 88-key piano range.
pianoRange :: Range AbsPitch
pianoRange = SpanRange (Bound (absPitch (A, 0)) Inclusive) (Bound (absPitch (C, 8)) Inclusive)


-- * Tuning Construction

-- | Transposes a tuning by a ratio.
transposeTuning :: Tuning -> Double -> Tuning
transposeTuning tuning shift = (shift *) <$> tuning

-- | Given a base pitch and a tuning of a full-octave chromatic scale starting with the base pitch, extends it to the entire set of pitches [0..127].
extendScaleTuning :: Pitch -> Tuning -> Tuning
extendScaleTuning pc tuning = freqs
    where
        ap = absPitch pc
        (octave, diff) = ap `divMod` 12
        diff' = if diff == 0 then 12 else 12 - diff
        baseFreqs = cycle tuning
        octaves = concat $ replicate 12 <$> [-(octave + 1)..]
        freqs = take 128 $ drop diff' [freq * (2 ** fromIntegral oct) | (freq, oct) <- zip baseFreqs octaves]

-- | Extends a scale to a full tuning [0..127], given a base pitch & frequency.
makeTuning :: (ToDouble a) => Scale a -> StdPitch -> Tuning
makeTuning scale (basePitch, baseFreq) = extendScaleTuning basePitch tuning
    where tuning = (baseFreq *) . toDouble <$> scale

-- | Measures interval ratios between each pair of notes in a scale.
--   Always expresses ratio between a lower note and the higher note.
--   If the first note is higher, uses the octave above the lower note.
intervalRatios :: Fractional a => Scale a -> [(ScaleInterval, a)]
intervalRatios scale = [((i, j), if i <= j then s2 / s1 else 2 * s2 / s1) | (i, s1) <- zip [0..] scale, (j, s2) <- zip [0..] scale]

-- | Measures deviations between a scale's intervals and some "ideal" intervals, as ratios.
intervalDevs :: (Fractional a, Ord a, RealFrac a) => [Int] -> IdealScale a -> Scale a -> [(ScaleInterval, a)]
intervalDevs intervals ideal scale = devs
    where
        intervalSet = S.fromList intervals
        idealArr = listArray (0, length ideal - 1) ideal
        idealIntervals i j = idealArr ! ((j - i) `mod` 12)
        bestInterval r xs = head $ sortOn posRatio [r / x | x <- xs]
        devs = [((i, j), bestInterval r (idealIntervals i j)) | ((i, j), r) <- intervalRatios scale, ((j - i) `mod` 12) `S.member` intervalSet]

-- | Measures deviations between a scale's intervals and some "ideal" intervals, in cents.
--   Sorts by decreasing abs deviation.
intervalDevCents :: (RealFrac a, ToDouble a) => [Int] -> IdealScale a -> Scale a -> [(ScaleInterval, Double)]
intervalDevCents intervals ideal scale = sortOn (negate . abs . snd) [((i, j), toCents dev) | ((i, j), dev) <- intervalDevs intervals ideal scale]

-- | Constructs a 12-tone scale by tuning each scale interval in succession.
--   Issues an error if the entire scale is not covered.
constructScale :: RealFrac a => [(ScaleInterval, a)] -> Scale a
constructScale pairs = extract <$> elems finalScale
    where
        initScale = listArray (0, 11) $ Just 1 : replicate 11 Nothing
        update arr ((i, j), r) = arr // [(j, normalize2pow . (r *) <$> (arr ! i))]
        finalScale = foldl' update initScale pairs
        extract (Just r) = r
        extract Nothing  = error "scale intervals not fully specified"

circleOfFifths :: [Int]
circleOfFifths = [7 * i `mod` 12 | i <- [0..12]]

-- | Creates 12-tone scale based on a stack of fifths of a given size.
--   Takes the base note to be the center of the stack.
stackedFifthScale :: RealFrac a => a -> Scale a
stackedFifthScale r = constructScale $ zip pairs1 (repeat r) ++ zip pairs2 (repeat $ 1 / r)
    where
        pairs1 = pairwise $ take 7 circleOfFifths
        pairs2 = pairwise $ take 6 $ reverse circleOfFifths


-- * Temperaments

-- ** Equal Temperament

stdScale :: Scale Double
stdScale = pow2 . (/ 12) <$> [0..11]

stdTuning :: Tuning
stdTuning = extendScaleTuning (A, 4) ((440.0 *) <$> stdScale)

centsFromStd :: Tuning -> [Double]
centsFromStd tuning = toCents <$> zipWith (/) tuning stdTuning

eqTmp440 = stdTuning
eqTmp432 = transposeTuning eqTmp440 (432 / 440)

-- ** Just Temperament

-- *** 5-limit temperament

just5Sym1aScale :: Scale Rational
just5Sym1aScale = [1, 16 % 15, 9 % 8, 6 % 5, 5 % 4, 4 % 3, 45 % 32, 3 % 2, 8 % 5, 5 % 3, 16 % 9, 15 % 8]

just5Sym1bScale :: Scale Rational
just5Sym1bScale = [1, 16 % 15, 9 % 8, 6 % 5, 5 % 4, 4 % 3, 64 % 45, 3 % 2, 8 % 5, 5 % 3, 16 % 9, 15 % 8]

just5Sym2aScale :: Scale Rational
just5Sym2aScale = [1, 16 % 15, 10 % 9, 6 % 5, 5 % 4, 4 % 3, 45 % 32, 3 % 2, 8 % 5, 5 % 3, 9 % 5, 15 % 8]

just5Sym2bScale :: Scale Rational
just5Sym2bScale = [1, 16 % 15, 10 % 9, 6 % 5, 5 % 4, 4 % 3, 64 % 45, 3 % 2, 8 % 5, 5 % 3, 9 % 5, 15 % 8]

just5Asym1aScale :: Scale Rational
just5Asym1aScale = [1, 16 % 15, 9 % 8, 6 % 5, 5 % 4, 4 % 3, 45 % 32, 3 % 2, 8 % 5, 5 % 3, 9 % 5, 15 % 8]

just5Asym1bScale :: Scale Rational
just5Asym1bScale = [1, 16 % 15, 9 % 8, 6 % 5, 5 % 4, 4 % 3, 64 % 45, 3 % 2, 8 % 5, 5 % 3, 9 % 5, 15 % 8]

-- NB: this is the "justest" of 5-limit scales
just5Asym2aScale :: Scale Rational
just5Asym2aScale = [1, 16 % 15, 9 % 8, 6 % 5, 5 % 4, 4 % 3, 25 % 18, 3 % 2, 8 % 5, 5 % 3, 9 % 5, 15 % 8]

just5Asym2bScale :: Scale Rational
just5Asym2bScale = [1, 16 % 15, 9 % 8, 6 % 5, 5 % 4, 4 % 3, 36 % 25, 3 % 2, 8 % 5, 5 % 3, 9 % 5, 15 % 8]

strict5Limit :: IdealScale Rational
strict5Limit = pure <$> just5Asym2aScale

lax5Limit :: IdealScale Rational
lax5Limit = [[1], [16 % 15], [9 % 8, 10 % 9], [6 % 5], [5 % 4], [4 % 3], [25 % 18, 45 % 32, 36 % 25, 64 % 45], [3 % 2], [8 % 5], [5 % 3], [9 % 5, 16 % 9], [15 % 8]]

-- ** Pythagorean

pythagoreanScale :: Scale Rational
pythagoreanScale = stackedFifthScale (3 % 2)

pythagoreanTuning :: StdPitch -> Tuning
pythagoreanTuning = makeTuning pythagoreanScale

-- ** Meantone

nthCommaMeantoneScale :: Int -> Scale Double
nthCommaMeantoneScale n = stackedFifthScale r
    where r = pow2 $ perfectFifth - syntonicComma / fromIntegral n

nthCommaMeantoneTuning :: Int -> StdPitch -> Tuning
nthCommaMeantoneTuning n = makeTuning (nthCommaMeantoneScale n)

quarterCommaMeantoneTuning :: StdPitch -> Tuning
quarterCommaMeantoneTuning = nthCommaMeantoneTuning 4

septimalMeantoneScale :: Scale Double
septimalMeantoneScale = stackedFifthScale (56 ** (1 / 10))

septimalMeantoneTuning :: StdPitch -> Tuning
septimalMeantoneTuning = makeTuning septimalMeantoneScale

-- ** Miscellaneous

kirnberger2Scale :: Scale Double
kirnberger2Scale = constructScale $ zip (pairwise $ init circleOfFifths) [perf, perf, imp, imp, perf, perf, perf, perf, perf, perf, perf]
    where
        perf = 3 / 2
        imp = pow2 $ perfectFifth - (1 / 2) * syntonicComma

kirnberger2Tuning :: StdPitch -> Tuning
kirnberger2Tuning = makeTuning kirnberger2Scale

kirnberger3Scale :: Scale Double
kirnberger3Scale = constructScale $ zip (pairwise $ init circleOfFifths) [imp, imp, imp, imp, perf, perf, perf, perf, perf, perf, perf]
    where
        perf = 3 / 2
        imp = pow2 $ perfectFifth - (1 / 4) * syntonicComma

kirnberger3Tuning :: StdPitch -> Tuning
kirnberger3Tuning = makeTuning kirnberger3Scale

harmonicScale :: Scale Rational
harmonicScale = (% 16) <$> [16, 17, 18, 19, 20, 21, 22, 24, 26, 27, 28, 30]

harmonicTuning :: StdPitch -> Tuning
harmonicTuning = makeTuning harmonicScale

-- * Tuning Packages

equalTuningPackage :: TuningPackage
equalTuningPackage = [("twelveToneEqual", stdScale)]

justTuningPackage :: TuningPackage
justTuningPackage = [(name, toDouble <$> scale) | (name, scale) <- [
    ("just5Sym1a", just5Sym1aScale),
    ("just5Sym1b", just5Sym1bScale),
    ("just5Sym2a", just5Sym2aScale),
    ("just5Sym2b", just5Sym2bScale),
    ("just5Asym1a", just5Asym1aScale),
    ("just5Asym1b", just5Asym1bScale),
    ("just5Asym2a", just5Asym2aScale),
    ("just5Asym2b", just5Asym2bScale)]]

pythagoreanTuningPackage :: TuningPackage
pythagoreanTuningPackage = [("pythagorean", toDouble <$> pythagoreanScale)]

meantoneTuningPackage :: TuningPackage
meantoneTuningPackage = [
    ("thirdCommaMeantone", nthCommaMeantoneScale 3),
    ("quarterCommaMeantone", nthCommaMeantoneScale 4),
    ("septimalMeantone", septimalMeantoneScale)]

miscTuningPackage :: TuningPackage
miscTuningPackage = [
    ("kirnberger2", kirnberger2Scale),
    ("kirnberger3", kirnberger3Scale),
    ("wendyCarlosHarmonic", toDouble <$> harmonicScale)]

allTuningPackage :: TuningPackage
allTuningPackage = equalTuningPackage ++ justTuningPackage ++ pythagoreanTuningPackage ++ meantoneTuningPackage ++ miscTuningPackage

-- * Musical Examples

melodicInterval :: Pitch -> AbsPitch -> Music Pitch
melodicInterval pc i = nt :+: shiftPitches i nt
    where nt = note qn pc

harmonicInterval :: Pitch -> AbsPitch -> Music Pitch
harmonicInterval pc i = nt :=: shiftPitches i nt
    where nt = note hn pc

melodicIntervals :: [(Pitch, AbsPitch)] -> Music Pitch
melodicIntervals pairs = line $ intersperse (rest qn) (uncurry melodicInterval <$> pairs)

harmonicIntervals :: [(Pitch, AbsPitch)] -> Music Pitch
harmonicIntervals pairs = line $ intersperse (rest qn) (uncurry harmonicInterval <$> pairs)

-- | Major scale starting on the given note.
majorScale :: Pitch -> Music Pitch
majorScale pc = line $ note qn . pitch . (+ absPitch pc) <$> [0, 2, 4, 5, 7, 9, 11, 12]

-- | Minor scale starting on the given note.
minorScale :: Pitch -> Music Pitch
minorScale pc = line $ note qn . pitch . (+ absPitch pc) <$> [0, 2, 3, 5, 7, 8, 10, 12]

-- | Instances of every interval from each note in [i..(i + 11)].
allIntervals :: Pitch -> AbsPitch -> Music Pitch
allIntervals pc i = line [shiftPitches i interval | (i, interval) <- zip [0..11] (repeat $ melodicInterval pc i)]

majCadencePattern :: Pitch -> Music Pitch
majCadencePattern pc = line $ mkChord <$> [[0, 4, 7], [0, 5, 9], [0, 4, 7], [-1, 2, 7], [-1, 5, 7], [0, 4, 7]]
    where
        ap = absPitch pc
        mkChord xs = chord [note qn (pitch $ ap + x) | x <- xs]


-- * MIDI Retuning

-- | Units of MIDI pitch bend per cent.
pitchBendPerCent :: Double
pitchBendPerCent = 8192 / 200

tuneMusic :: (ToMusic1 a) => Tuning -> Music a -> Music1
tuneMusic tuning = mMap tuneNote . applyControls . toMusic1
    where
        centArr = mkArray $ centsFromStd tuning
        tuneNote :: Note1 -> Note1
        tuneNote (pc, attrs) = (pc, PitchShift cts : attrs)
            where
                ap = fromIntegral $ absPitch pc
                cts = centArr ! ap


-- * Tuning Optimization

data TunOpt = TunOpt {
    pnorm :: Double,
    idealScale :: IdealScale Double
} deriving Show

type Interval = (AbsPitch, AbsPitch)

instance Default TunOpt where
    def = TunOpt 2.0 (fmap fromRational <$> strict5Limit)

diatonicIntervals :: [Interval]
diatonicIntervals = [(60, 60 + i) | i <- [0, 2, 4, 5, 7, 9, 11, 12]]

-- | Takes interval notes mod 12.
collapseInterval :: Interval -> Interval
collapseInterval (i, j) = (i `mod` 12, j `mod` 12)

-- | Gives \(p\)-norm distance between 2 scales in log space.
scaleDistance :: Double -> Scale Double -> Scale Double -> Double
scaleDistance p scale1 scale2 = dist
    where
        scale1' = V.fromList $ log2 <$> scale1
        scale2' = V.fromList $ log2 <$> scale2
        dist = (** (1.0 / p)) $ V.sum $ (** p) . abs <$> (scale1' ^-^ scale2')

-- | Computes \(p\)-norm loss between a given scale and an ideal scale.
pNormLoss :: TunOpt -> Counter Interval Int -> PitchClass -> Scale Double -> Double
pNormLoss (TunOpt {pnorm, idealScale}) intervalCtr pc scale = loss
    where
        base = absPitch (pc, 4) `mod` 12  -- base pitch class
        base' = 12 - base
        scaleArr = log2 <$> mkArray scale
        idealDiffArr = mkArray $ fmap log2 <$> idealScale
        err :: Interval -> Int -> Double
        err (i, j) ct = fromIntegral ct * (minAbsErr ** pnorm)
            where
                i' = (i + base') `mod` 12
                iDiff = scaleArr ! i'
                j' = (j + base') `mod` 12
                jDiff = scaleArr ! j'
                actualDiff = if j' >= i' then jDiff - iDiff else 1.0 + jDiff - iDiff
                dist = j - i
                (_, dist') = dist `divMod` 12
                idealDiffs = idealDiffArr ! dist'
                minAbsErr = minimum $ abs . (actualDiff -) <$> idealDiffs
        intervalCtPairs = M.toList intervalCtr
        lossTerms = [err interval ct | (interval, ct) <- intervalCtPairs]
        loss = sum lossTerms ** (1.0 / pnorm)

tuningFinDiffParams = FinDiffParams {a = 0.0001, b = 50.0, alpha = 0.602, c = 0.0001, gamma = 0.101}
tuningFinDiffFuncs = mkFinDiffFuncs tuningFinDiffParams

-- | Optimizes a 12-tone scale to minimize \(p\)-norm loss with an ideal scale.
optimizeScale :: TunOpt -> [Interval] -> PitchClass -> Scale Double
optimizeScale tunOpt intervals pc = vecToScale xf
    where
        vecToScale = fmap (2.0 **) . (0.0 :) . V.toList
        intervalCtr = count $ collapseInterval <$> intervals
        lossFunc = pNormLoss tunOpt intervalCtr pc . vecToScale
        x0 = V.fromList $ log2 <$> tail stdScale  -- start with equal temperament
        (_, xf) = fdsaGradientDescent def tuningFinDiffFuncs lossFunc x0

musicToIntervals :: (ToPitch a, ToMusicD m a) => m a -> [Interval]
musicToIntervals mus = vertIntervals ++ horizIntervals
    where
        (MusicD _ _ arr) = toMusicD mus
        orderPair (i, j) = if i <= j then (i, j) else (j, i)
        ap = absPitch . toPitch
        -- gets interval between each untied note and each other note in a chord
        chordIntervals notes = intervals
            where
                untiedPcs = sort $ ap <$> catMaybes (extractTied <$> filter (not . isTied) notes)
                tiedPcs = sort $ ap <$> catMaybes (extractTied <$> filter isTied notes)
                intervals = [orderPair (untiedPc, tiedPc) | untiedPc <- untiedPcs, tiedPc <- tiedPcs]
        vertIntervals = concat $ chordIntervals <$> arr
        seqInterval (Untied _ p1) (Untied _ p2) = Just (ap p1, ap p2)
        seqInterval (Tied p1)     (Untied _ p2) = Just (ap p1, ap p2)
        seqInterval _             _             = Nothing
        adjChordIntervals (notes1, notes2) = catMaybes [seqInterval note1 note2 | note1 <- notes1, note2 <- notes2]
        horizIntervals = concat $ adjChordIntervals <$> pairwise arr

-- | Optimizes a 12-tone scale (starting on C) for a given piece of music.
optimizeScaleForMusic :: (ToPitch a, ToMusicD m a) => TunOpt -> m a -> Scale Double
optimizeScaleForMusic tunOpt mus = optimizeScale tunOpt (musicToIntervals mus) C

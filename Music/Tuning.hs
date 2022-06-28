{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}

module Music.Tuning where

import Data.Aeson (FromJSON, ToJSON)
import Data.Array ((!), (//), elems, listArray)
import Data.List (foldl', intersperse)
import Data.Range (Bound(..), BoundType(..), Range(..))
import Data.Ratio ((%))
import GHC.Generics (Generic)

import Euterpea (AbsPitch, Music(..), Music1, Note1, NoteAttribute(..), Pitch, PitchClass(..), ToMusic1, absPitch, applyControls, hn, mMap, note, pitch, qn, rest, shiftPitches)

import Music.Types (MusicT(..))
import Misc.Utils (ToDouble(..), log2, mkArray, pairwise, pow2)


-- * Types

-- | Frequency in Hz
type Freq = Double
-- | Pitch basis for a tuning system (e.g. tune A4 to 440 Hz)
type StdPitch = (Pitch, Freq)
-- | log2 of a ratio
type LogRatio = Double
-- a ratio measured in cents
type Cents = Double

-- | Represents tuning as a list of freqs mapping @[0..127]@ to Hz
newtype Tuning = Tuning {unTuning :: [Freq]}
    deriving (Eq, Generic, Show)
    deriving newtype (FromJSON, ToJSON)

-- | Interval in a scale, given by the two scale degrees
type ScaleInterval = (Int, Int)

-- | Actual intervals @[0..11]@
newtype Scale a = Scale {unScale :: [a]}
    deriving (Eq, Show)

-- | A named tuning
data NamedTuning = NamedTuning {
    name :: String,
    tuning :: Tuning
} deriving (Eq, Generic, Show)

instance ToJSON NamedTuning
instance FromJSON NamedTuning

-- | A named temperament (defined by a 12-tone scale which gets extended to all octaves)
type Temperament = (String, Scale Double)


-- * Helper Functions


-- | Multiplies or divides by the right power of 2 until the rational is in [1, 2).
normalize2pow :: RealFrac a => a -> a
normalize2pow r
    | r < 1     = head $ dropWhile (< 1) [r * (2 ^ i) | i <- [1..]]
    | r >= 2    = head $ dropWhile (>= 2) [r / (2 ^ i) | i <- [1..]]
    | otherwise = r


-- * Intervals/Frequencies

a440 :: StdPitch
a440 = ((A, 4), 440)

c523 :: StdPitch
c523 = ((C, 5), 523.3)

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
toCents :: (ToDouble a) => a -> Cents
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
transposeTuning (Tuning tuning) shift = Tuning $ (shift *) <$> tuning

-- | Given a base pitch and a tuning of a full-octave n-tone scale starting with the base pitch, extends it to the entire set of MIDI keys [0..127].
extendScaleTuning :: Int -> Pitch -> Tuning -> Tuning
extendScaleTuning n pc (Tuning tuning) = Tuning freqs
    where
        ap = absPitch pc
        (octave, diff) = ap `divMod` n
        diff' = if diff == 0 then n else n - diff
        baseFreqs = cycle tuning
        octaves = concatMap (replicate n) [-(octave + 1)..]
        freqs = take 128 $ drop diff' [freq * (2 ** fromIntegral oct) | (freq, oct) <- zip baseFreqs octaves]

-- | Extends a scale to a full tuning [0..127], given a base pitch & frequency.
--
--   The scale provided is expected to represent one octave.
makeTuning :: (ToDouble a) => Scale a -> StdPitch -> Tuning
makeTuning (Scale scale) (basePitch, baseFreq) = extendScaleTuning n basePitch (Tuning tuning)
    where
        n = length scale
        tuning = (baseFreq *) . toDouble <$> scale

-- | Measures interval ratios between each pair of notes in a scale.
--   Always expresses ratio between a lower note and the higher note.
--   If the first note is higher, uses the octave above the lower note.
intervalRatios :: Fractional a => Scale a -> [(ScaleInterval, a)]
intervalRatios (Scale scale) = [((i, j), if i <= j then s2 / s1 else 2 * s2 / s1) | (i, s1) <- zip [0..] scale, (j, s2) <- zip [0..] scale]

-- | Constructs a 12-tone scale by tuning each scale interval in succession.
--   Issues an error if the entire scale is not covered.
constructScale :: RealFrac a => [(ScaleInterval, a)] -> Scale a
constructScale pairs = Scale $ extract <$> elems finalScale
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

scaleToDouble :: (ToDouble a) => Scale a -> Scale Double
scaleToDouble (Scale scale) = Scale $ toDouble <$> scale

-- * Temperaments

-- ** Equal Temperament

-- | Scale for \(n\)-tone equal temperament.
nTetScale :: Int -> Scale Double
nTetScale n = Scale $ pow2 . (/ fromIntegral n) <$> [0..(fromIntegral n - 1)]

-- | Scale for 12-tone equal temperament.
twelveTetScale :: Scale Double
twelveTetScale = nTetScale 12

nTetTuning :: Int -> StdPitch -> Tuning
nTetTuning n (pc, freq) = extendScaleTuning n pc (Tuning $ (freq *) <$> unScale (nTetScale n))

twelveTetTuning :: StdPitch -> Tuning
twelveTetTuning = nTetTuning 12

-- | Tuning for 12-tone equal temperament, A440.
stdTuning :: Tuning
stdTuning = twelveTetTuning ((A, 4), 440.0)

-- | Given a 'Tuning', computes the corresponding deviations (in cents) from standard tuning.
centsFromStd :: Tuning -> [Cents]
centsFromStd (Tuning tuning) = toCents <$> zipWith (/) tuning (unTuning stdTuning)

eqTmp440 = stdTuning
eqTmp432 = transposeTuning eqTmp440 (432 / 440)

-- ** Just Temperament

-- *** 5-limit temperament

just5Sym1aScale :: Scale Rational
just5Sym1aScale = Scale [1, 16 % 15, 9 % 8, 6 % 5, 5 % 4, 4 % 3, 45 % 32, 3 % 2, 8 % 5, 5 % 3, 16 % 9, 15 % 8]

just5Sym1bScale :: Scale Rational
just5Sym1bScale = Scale [1, 16 % 15, 9 % 8, 6 % 5, 5 % 4, 4 % 3, 64 % 45, 3 % 2, 8 % 5, 5 % 3, 16 % 9, 15 % 8]

just5Sym2aScale :: Scale Rational
just5Sym2aScale = Scale [1, 16 % 15, 10 % 9, 6 % 5, 5 % 4, 4 % 3, 45 % 32, 3 % 2, 8 % 5, 5 % 3, 9 % 5, 15 % 8]

just5Sym2bScale :: Scale Rational
just5Sym2bScale = Scale [1, 16 % 15, 10 % 9, 6 % 5, 5 % 4, 4 % 3, 64 % 45, 3 % 2, 8 % 5, 5 % 3, 9 % 5, 15 % 8]

just5Asym1aScale :: Scale Rational
just5Asym1aScale = Scale [1, 16 % 15, 9 % 8, 6 % 5, 5 % 4, 4 % 3, 45 % 32, 3 % 2, 8 % 5, 5 % 3, 9 % 5, 15 % 8]

just5Asym1bScale :: Scale Rational
just5Asym1bScale = Scale [1, 16 % 15, 9 % 8, 6 % 5, 5 % 4, 4 % 3, 64 % 45, 3 % 2, 8 % 5, 5 % 3, 9 % 5, 15 % 8]

-- NB: this is the "justest" of 5-limit scales
just5Asym2aScale :: Scale Rational
just5Asym2aScale = Scale [1, 16 % 15, 9 % 8, 6 % 5, 5 % 4, 4 % 3, 25 % 18, 3 % 2, 8 % 5, 5 % 3, 9 % 5, 15 % 8]

just5Asym2bScale :: Scale Rational
just5Asym2bScale = Scale [1, 16 % 15, 9 % 8, 6 % 5, 5 % 4, 4 % 3, 36 % 25, 3 % 2, 8 % 5, 5 % 3, 9 % 5, 15 % 8]

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
harmonicScale = Scale $ (% 16) <$> [16, 17, 18, 19, 20, 21, 22, 24, 26, 27, 28, 30]

harmonicTuning :: StdPitch -> Tuning
harmonicTuning = makeTuning harmonicScale

-- * Temperaments

twelveToneEqualTemperament :: Temperament
twelveToneEqualTemperament = ("12TET", twelveTetScale)

justTemperaments :: [Temperament]
justTemperaments = [(name, scaleToDouble scale) | (name, scale) <- [
    ("just5Sym1a", just5Sym1aScale),
    ("just5Sym1b", just5Sym1bScale),
    ("just5Sym2a", just5Sym2aScale),
    ("just5Sym2b", just5Sym2bScale),
    ("just5Asym1a", just5Asym1aScale),
    ("just5Asym1b", just5Asym1bScale),
    ("just5Asym2a", just5Asym2aScale),
    ("just5Asym2b", just5Asym2bScale)]]

pythagoreanTemperaments :: [Temperament]
pythagoreanTemperaments = [("pythagorean", scaleToDouble pythagoreanScale)]

meantoneTemperaments :: [Temperament]
meantoneTemperaments = [
    ("thirdCommaMeantone", nthCommaMeantoneScale 3),
    ("quarterCommaMeantone", nthCommaMeantoneScale 4),
    ("septimalMeantone", septimalMeantoneScale)]

miscTemperaments :: [Temperament]
miscTemperaments = [
    ("kirnberger2", kirnberger2Scale),
    ("kirnberger3", kirnberger3Scale),
    ("wendyCarlosHarmonic", scaleToDouble harmonicScale)]

allTemperaments :: [Temperament]
allTemperaments = [twelveToneEqualTemperament] ++ justTemperaments ++ pythagoreanTemperaments ++ meantoneTemperaments ++ miscTemperaments

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


-- * Default Tunings

quarterShiftSplits :: Pitch -> NamedTuning -> [NamedTuning]
quarterShiftSplits pc (NamedTuning {name, tuning}) = [namedTuningFlat, namedTuningSharp]
    where
        ap = absPitch pc
        freqs = unTuning tuning
        tuningFlat = Tuning [if (i < ap) then shiftFreq freq (23 / 24) else freq | (i, freq) <- zip [0..] freqs]
        tuningSharp = Tuning [shiftFreq freq (if (i < ap) then 1 else (1 / 24)) | (i, freq) <- zip [0..] freqs]
        -- shiftedTuning =
        namedTuningFlat = NamedTuning (name ++ "-qflat") tuningFlat
        namedTuningSharp = NamedTuning (name ++ "-qsharp") tuningSharp

pentatonicOnly :: StdPitch -> NamedTuning
pentatonicOnly stdPitch = NamedTuning "pentatonic" (makeTuning scale stdPitch)
    where
        scale12 = unScale twelveTetScale
        scale = Scale $ [scale12 !! i | i <- [6, 8, 10]] ++ [2 * scale12 !! i | i <- [1, 3]]

-- | Given a pitch and frequency to "anchor" the tunings, returns a collection of various 'NamedTuning's.
defaultTunings :: StdPitch -> [NamedTuning]
defaultTunings stdPitch = twelveToneTunings ++ [pentatonicOnly stdPitch] ++ nTetTunings ++ qshiftTunings
    where
        fromTemperament (name, scale) = NamedTuning name (makeTuning scale stdPitch)
        twelveToneTunings = fromTemperament <$> allTemperaments
        nTetRange = [i | i <- [6..36], i /= 12]
        nTetTunings = [NamedTuning (show n ++ "TET") (nTetTuning n stdPitch) | n <- nTetRange]
        qshiftTunings = quarterShiftSplits (C, 4) (NamedTuning "12TET" (twelveTetTuning stdPitch))


-- * MIDI Retuning

-- | Units of MIDI pitch bend per cent.
pitchBendPerCent :: Double
pitchBendPerCent = 8192 / 200

-- | Retunes 'Music' to a different tuning.
tuneMusic :: (ToMusic1 a) => Tuning -> Music a -> Music1
tuneMusic tuning = mMap tuneNote . applyControls . toMusic1
    where
        centArr = mkArray $ centsFromStd tuning
        tuneNote :: Note1 -> Note1
        tuneNote (pc, attrs) = (pc, PitchShift cts : attrs)
            where
                ap = fromIntegral $ absPitch pc
                cts = centArr ! ap

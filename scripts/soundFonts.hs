{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeSynonymInstances #-}

import Codec.SoundFont
import Control.Exception (assert)
import Control.Monad.Reader
import Data.Array
import Data.List (foldl', intersperse, partition)
import Data.Maybe (catMaybes, fromJust, isJust)
import Data.Range.Range
import Data.Ratio
import qualified Data.Set as S
import Data.Sort
import qualified Data.Text as T
import System.IO.Unsafe

import Euterpea hiding (exportFile, importFile, preset)


-- HELPER FUNCTIONS --

class ToDouble a where
    toDouble :: a -> Double

instance ToDouble Double where
    toDouble = id

instance ToDouble Rational where
    toDouble = fromRational

pairwise :: [a] -> [(a, a)]
pairwise xs = zip xs (tail xs)

rotateL :: Int -> [a] -> [a]
rotateL n xs = drop n' xs ++ take n' xs
    where n' = n `mod` (length xs)

cumsum :: (Num a) => [a] -> [a]
cumsum = scanl (+) 0

lengthsToSpans :: [Int] -> [(Int, Int)]
lengthsToSpans = pairwise . cumsum

strip :: String -> String
strip = T.unpack . T.strip . T.pack

mkArray :: (Ix i, Integral i) => [e] -> Array i e
mkArray xs = listArray (0, fromIntegral $ length xs - 1) xs

log2 :: Floating a => a -> a
log2 x = log x / (log 2)

pow2 :: Floating a => a -> a
pow2 x = 2 ** x

fracPart :: RealFrac a => a -> a
fracPart x = x - (fromIntegral $ floor x)

posRatio :: RealFrac a => a -> a
posRatio r = if (r >= 1) then r else (1 / r)
    
-- multiplies or divides by the right power of 2 until the rational is in [1, 2)
normalize2pow :: RealFrac a => a -> a
normalize2pow r
    | r < 1     = head $ dropWhile (< 1) [r * (2 ^ i) | i <- [1..]]
    | r >= 2    = head $ dropWhile (>= 2) [r / (2 ^ i) | i <- [1..]]
    | otherwise = r

-- TUNING --

type Freq = Double
type StdPitch = (Pitch, Freq)
type LogRatio = Double  -- NB: using log2
-- represent tuning as a list of freqs mapping [0..127] to hertz
type Tuning = [Freq]
-- difference (in cents) between two tunings
type TuningDiff = [Int]
-- interval in a scale, given by the two scale degrees
type ScaleInterval = (Int, Int)
-- ideal intervals [0..11] (can allow multiple possibilities)
type IdealScale a = [[a]]
-- actual intervals [0..11]
type Scale a = [a]

-- Interval Ratios

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

-- converts a ratio to a number of cents
toCents :: (ToDouble a) => a -> Double
toCents x = log2 (toDouble x) / cent

-- shifts a frequency by a given log-ratio
shiftFreq :: Freq -> LogRatio -> Freq
shiftFreq f shift = pow2 $ log2 f + shift

-- converts pitch to frequency, given a base frequency
freq :: StdPitch -> Pitch -> Freq
freq (basePitch, baseFreq) pc = shiftFreq baseFreq (fromIntegral (absPitch pc - absPitch basePitch) * semitone)

-- converts pitch to frequency with A4 = 440 Hz base frequency
stdFreq :: Pitch -> Freq
stdFreq = freq ((A, 4), 440)

-- standard 88-key piano range
pianoRange :: Range AbsPitch
pianoRange = SpanRange (absPitch (A, 0)) (absPitch (C, 8))

-- transposes a tuning by a ratio
transposeTuning :: Tuning -> Double -> Tuning
transposeTuning tuning shift = (shift *) <$> tuning

-- given a base pitch and a tuning of a full-octave chromatic scale starting with the base pitch, extends it to the entire set of pitches [0..127]
extendScaleTuning :: Pitch -> Tuning -> Tuning
extendScaleTuning pc tuning = freqs
    where
        ap = absPitch pc
        (octave, diff) = ap `divMod` 12
        diff' = if (diff == 0) then 12 else (12 - diff)
        baseFreqs = cycle tuning
        octaves = concat $ replicate 12 <$> [-(octave + 1)..]
        freqs = take 128 $ drop diff' $ [freq * (2 ** fromIntegral oct) | (freq, oct) <- zip baseFreqs octaves]

-- extends a scale to a full tuning [0..127], given a base pitch & frequency        
makeTuning :: (ToDouble a) => Scale a -> StdPitch -> Tuning        
makeTuning scale (basePitch, baseFreq) = extendScaleTuning basePitch tuning
    where tuning = (baseFreq *) . toDouble <$> scale

-- measures interval ratios between each pair of notes in a scale 
-- always expresses ratio between a lower note and the higher note
-- if the first note is higher, uses the octave above the lower note
intervalRatios :: Fractional a => Scale a -> [(ScaleInterval, a)]
intervalRatios scale = [((i, j), if (i <= j) then (s2 / s1) else (2 * s2 / s1)) | (i, s1) <- zip [0..] scale, (j, s2) <- zip [0..] scale] 

-- measures deviations between a scale's intervals and some "ideal" intervals, as ratios
intervalDevs :: (Fractional a, Ord a, RealFrac a) => [Int] -> IdealScale a -> Scale a -> [(ScaleInterval, a)]
intervalDevs intervals ideal scale = devs
    where
        intervalSet = S.fromList intervals
        idealArr = listArray (0, length ideal - 1) ideal
        idealIntervals i j = idealArr ! ((j - i) `mod` 12)
        --idealIntervals i j = normalize2pow <$> idealArr ! ((j - i) `mod` 12)
        --idealIntervals i j = if (i <= j) then (idealArr ! (j - i)) else ((1 /) <$>  (idealArr ! (i - j)))
        bestInterval r xs = head $ sortOn posRatio [r / x | x <- xs]
        devs = [((i, j), bestInterval r (idealIntervals i j)) | ((i, j), r) <- intervalRatios scale, ((j - i) `mod` 12) `S.member` intervalSet]

-- measures deviations between a scale's intervals and some "ideal" intervals, in cents
-- sorts by decreasing abs deviation
intervalDevCents :: (RealFrac a, ToDouble a) => [Int] -> IdealScale a -> Scale a -> [(ScaleInterval, Double)]
intervalDevCents intervals ideal scale = sortOn (negate . abs . snd) [((i, j), toCents dev) | ((i, j), dev) <- intervalDevs intervals ideal scale]

-- constructs a 12-tone scale by tuning each scale interval in succession
-- issues an error if the entire scale is not covered
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
        
-- creates 12-tone scale based on a stack of fifths of a given size
-- takes the base note to be the center of the stack
stackedFifthScale :: RealFrac a => a -> Scale a
stackedFifthScale r = constructScale $ zip pairs1 (repeat r) ++ zip pairs2 (repeat $ 1 / r)
    where
        pairs1 = pairwise $ take 7 circleOfFifths
        pairs2 = pairwise $ take 6 $ reverse circleOfFifths

-- Just Temperament

-- 5-limit temperament

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

-- Equal Temperament

stdScale :: Scale Double
stdScale = pow2 . (/ 12) <$> [0..11]

stdTuning :: Tuning
stdTuning = extendScaleTuning (A, 4) ((440.0 *) <$> stdScale)

centsFromStd :: Tuning -> TuningDiff
centsFromStd tuning = round <$> toCents <$> zipWith (/) tuning stdTuning

eqTmp440 = stdTuning
eqTmp432 = transposeTuning eqTmp440 (432 / 440)

-- Pythagorean Tuning

pythagoreanScale :: Scale Rational
pythagoreanScale = stackedFifthScale (3 % 2)

pythagoreanTuning :: StdPitch -> Tuning
pythagoreanTuning = makeTuning pythagoreanScale

-- Meantone Tuning

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

-- Other Tunings

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

-- Wendy Carlos created this "super-just" scale
harmonicScale :: Scale Rational
harmonicScale = (% 16) <$> [16, 17, 18, 19, 20, 21, 22, 24, 26, 27, 28, 30, 32]

harmonicTuning :: StdPitch -> Tuning
harmonicTuning = makeTuning harmonicScale


-- Musical specimens

melodicInterval :: Pitch -> AbsPitch -> Music Pitch
melodicInterval pc i = nt :+: transpose i nt
    where nt = note qn pc

harmonicInterval :: Pitch -> AbsPitch -> Music Pitch
harmonicInterval pc i = nt :=: transpose i nt
    where nt = note hn pc

melodicIntervals :: [(Pitch, AbsPitch)] -> Music Pitch
melodicIntervals pairs = foldl' (:+:) (rest 0) $ intersperse (rest qn) (uncurry melodicInterval <$> pairs)

harmonicIntervals :: [(Pitch, AbsPitch)] -> Music Pitch
harmonicIntervals pairs = foldl' (:+:) (rest 0) $ intersperse (rest qn) (uncurry harmonicInterval <$> pairs)

-- major scale starting on the given note
majorScale :: Pitch -> Music Pitch
majorScale pc = foldl' (:+:) (rest 0) $ note qn . pitch . (+ absPitch pc) <$> [0, 2, 4, 5, 7, 9, 11, 12]

-- minor scale starting on the given note
minorScale :: Pitch -> Music Pitch
minorScale pc = foldl' (:+:) (rest 0) $ note qn . pitch . (+ absPitch pc) <$> [0, 2, 3, 5, 7, 8, 10, 12]


-- SOUND FONTS --

-- given a number of cents to adjust, returns a list of Generators reflecting the change
centsToGenerators :: Int -> [Generator]
centsToGenerators cts = coarse ++ fine
    where
        (isNeg, absCts) = (cts < 0, abs cts)
        (semis, cts') = divMod absCts 100
        coarse = (if (semis == 0) then [] else [CoarseTune (if isNeg then (-semis) else semis)])
        fine = if (cts' == 0) then [] else [FineTune (if isNeg then (-cts') else cts')]

getKeyRange :: Generator -> Maybe (Int, Int)
getKeyRange (KeyRange kmin kmax) = Just (fromIntegral kmin, fromIntegral kmax)
getKeyRange _                    = Nothing

fixPhdr :: Phdr -> Phdr
fixPhdr hdr@(Phdr {presetName}) = hdr {presetName = strip presetName}

fixInst :: Inst -> Int -> Inst
fixInst instrument@(Inst {instName}) bagIndex = instrument {instName = strip instName, instBagNdx = fromIntegral bagIndex}

-- loads a SoundFont file (unsafely)
loadSoundFont :: FilePath -> SoundFont
loadSoundFont = either (\err -> error "invalid SoundFont") id . unsafePerformIO . importFile

sfPath = "/Users/jeremander/Programming/Music/SoundFonts/"
gmPath = sfPath ++ "gm2.sf2"
gm = loadSoundFont gmPath
testPath = sfPath ++ "test.sf2"


-- GENERAL MIDI --
        
ibagSpans :: [Int] -> Reader SoundFont [(Int, Int)]
ibagSpans instIndices = do
    pdata <- pdta <$> ask
    let instIndices' = fromIntegral <$> instIndices ++ [last instIndices + 1]
    let ibagIndices = fromIntegral <$> [instBagNdx $ insts pdata ! i | i <- instIndices']
    return $ pairwise ibagIndices

igenSpans :: [(Int, Int)] -> Reader SoundFont [[(Int, Int)]]
igenSpans spans = do
    bags <- ibags . pdta <$> ask
    let igenIndices = [[fromIntegral . genNdx $ bags ! fromIntegral i | i <- [fst span .. snd span]] | span <- spans]
    return $ pairwise <$> igenIndices

-- NB: it is important that tuning generators come before the SampleMode generator
insertTuningGens :: [Generator] -> [Generator] -> [Generator]
insertTuningGens tuningGens gens = left ++ tuningGens ++ right
    where
        isSampleMode :: Generator -> Bool
        isSampleMode (SampleMode _) = True
        isSampleMode _              = False
        (left, right) = break isSampleMode gens

-- given a list of igen spans, gets the corresponding instrument zones    
getZones :: [(Int, Int)] -> Reader SoundFont [[Generator]]
getZones spans = do
    gens <- igens . pdta <$> ask
    return [[gens ! fromIntegral i | i <- [fst span .. snd span - 1]] | span <- spans]

-- given a list of igen spans (instrument zones), creates a new set of zones where each distinct pitch (on an 88-key keyboard) gets its own zone, and each pitch is re-tuned according to the given tuning
retuneZones :: Tuning -> [(Int, Int)] -> Reader SoundFont [[Generator]]
retuneZones tuning spans = do
    gens <- igens . pdta <$> ask
    let zones = [[gens ! fromIntegral i | i <- [fst span .. snd span - 1]] | span <- spans]
    let zonePairs = zip zones [0..]
    let (keyPairs, nonKeyPairs) = partition (isJust . getKeyRange . head . fst) zonePairs
    let numKeyPairs = length keyPairs
    let keyRanges = sort [(fromJust $ getKeyRange $ head zone, i) | (zone, i) <- keyPairs]
    let firstPair = head keyRanges
    let (minKey, minIdx) = (fst $ fst firstPair, snd firstPair)
    let prange = fromRanges [pianoRange]
    let (bottomNote, topNote) = (fromIntegral $ minimum prange, fromIntegral $ maximum prange)
    let zoneIndices = [if (k < minKey) then minIdx else (snd $ last $ takeWhile (\pair -> k >= (fst $ fst pair)) keyRanges) | k <- prange]
    let krange = \k -> if (k == bottomNote)
                        then KeyRange 0 k
                        else
                            if (k == topNote)
                                then KeyRange k 127
                                else KeyRange k k
    let tuningGens = centsToGenerators <$> centsFromStd tuning                     
    let newKeyZones = [insertTuningGens (tuningGens !! k) (krange (fromIntegral k) : tail (zones !! i)) | (k, i) <- zip prange zoneIndices]
    let newZones = fst <$> sortOn snd (nonKeyPairs ++ [(zone, minIdx) | zone <- newKeyZones])
    return $ case keyPairs of
        []        -> zones  -- instrument does not have key zones
        otherwise -> newZones

-- retunes melodic instruments
-- for now, only does this to instruments in bank 0 up to preset 63, to avoid int16 overflow issues
-- should go up to at least preset 95        
retuneMelodicInstruments :: Tuning -> Reader SoundFont SoundFont
retuneMelodicInstruments tuning = do
    sf <- ask
    let pdata = pdta sf
    let instIndices = [0..(length $ insts $ pdta $ sf) - 1]
    -- NB: we assume presets and instruments are one-to-one
    let validFlags = assert ((length $ phdrs pdata) == (length instIndices)) [(bank hdr == 0) && (preset hdr <= 63) | hdr <- elems $ phdrs pdata]
    let bagSpans = runReader (ibagSpans $ init instIndices) sf
    let genSpans = runReader (igenSpans bagSpans) sf
    let newZones = [runReader (if flag then (retuneZones tuning spans) else (getZones spans)) sf | (flag, spans) <- zip validFlags genSpans]
    let newBagIndices = cumsum $ length <$> newZones
    let newInsts = mkArray [fixInst instrument i | (instrument, i) <- zip (elems $ insts pdata) newBagIndices]
    let newGenIndices = cumsum $ length <$> concat newZones
    -- for simplicity, require all modulator indices to be 0
    let modIndexSet = S.fromList $ [modNdx bag | bag <- elems $ (ibags pdata)]
    let assertion = assert (modIndexSet == S.singleton 0)
    let newIbags = mkArray [Bag {genNdx = fromIntegral i, modNdx = 0} | i <- newGenIndices]
    let newIgens = mkArray $ concat $ concat newZones
    let newPhdrs = fixPhdr <$> phdrs pdata
    let newPdata = pdata {phdrs = newPhdrs, insts = newInsts, ibags = newIbags, igens = newIgens}
    return $ assertion $ sf {pdta = newPdata}    


-- given a tuning, retunes a SoundFont at one path and saves it to another path    
retuneSoundFont :: Tuning -> FilePath -> FilePath -> IO ()
retuneSoundFont tuning infile outfile = do
    sf <- either (\err -> error "invalid SoundFont") id <$> importFile infile
    let sf' = runReader (retuneMelodicInstruments tuning) sf
    exportFile outfile sf'
{-# LANGUAGE NamedFieldPuns #-}

import Codec.SoundFont
import Control.Exception (assert)
import Control.Monad.Reader
import Data.Array
import Data.List (partition)
import Data.Maybe (catMaybes, fromJust, isJust)
import Data.Range.Range
import qualified Data.Set as S
import Data.Sort
import qualified Data.Text as T
import System.IO.Unsafe

import Euterpea hiding (exportFile, importFile)


-- HELPER FUNCTIONS --

pairwise :: [a] -> [(a, a)]
pairwise xs = zip xs (tail xs)

cumsum :: (Num a) => [a] -> [a]
cumsum = scanl (+) 0

lengthsToSpans :: [Int] -> [(Int, Int)]
lengthsToSpans = pairwise . cumsum

strip :: String -> String
strip = T.unpack . T.strip . T.pack

mkArray :: (Ix i, Integral i) => [e] -> Array i e
mkArray xs = listArray (0, fromIntegral $ length xs - 1) xs


-- PITCH --

type Freq = Double
type LogRatio = Double

semitone :: LogRatio
semitone = (log 2) / 12

cent :: LogRatio
cent = (log 2) / 1200

shiftFreq :: Freq -> LogRatio -> Freq
shiftFreq f shift = exp $ log f + shift

-- converts pitch to frequency, given a base frequency
freq :: (AbsPitch, Freq) -> AbsPitch -> Freq
freq (basePitch, baseFreq) ap = shiftFreq baseFreq (fromIntegral (ap - basePitch) * semitone)

-- converts pitch to frequency with A4 = 440 Hz base frequency
stdFreq :: AbsPitch -> Freq
stdFreq = freq (absPitch (A, 4), 440)

-- standard 88-key piano range
pianoRange :: Range AbsPitch
pianoRange = SpanRange (absPitch (A, 0)) (absPitch (C, 8))


-- TUNING --

-- represent tuning as a list of freqs mapping [0..127] to hertz
type Tuning = [Freq]
-- difference (in cents) between two tunings
type TuningDiff = [Int]

-- 12-tone equal temperament, A4 = 440 Hz
stdTuning :: Tuning
stdTuning = stdFreq <$> [0..127]

centsFromStd :: Tuning -> TuningDiff
centsFromStd tuning = round <$> (/ cent) <$> zipWith (-) (log <$> tuning) (log <$> stdTuning)

transposeTuning :: Tuning -> LogRatio -> Tuning
transposeTuning tuning shift = exp <$> zipWith (+) (log <$> tuning) (repeat shift)



-- SOUND FONTS --

loadSoundFont :: FilePath -> SoundFont
loadSoundFont = either (\err -> error "invalid SoundFont") id . unsafePerformIO . importFile

sfPath = "/Users/jeremander/Programming/Music/SoundFonts/"
gmPath = sfPath ++ "gm2.sf2"
gm = loadSoundFont gmPath
testPath = sfPath ++ "test.sf2"


-- GENERAL MIDI --

-- gets inst indices of all non-percussion instruments
-- melodicInstIndices :: Reader SoundFont [Int]
-- melodicInstIndices = do
--     pdata <- pdta <$> ask
--     let hdrs = phdrs pdata
--     let pbagIndices = fromIntegral <$> concat [[presetBagNdx $ hdrs ! i .. (presetBagNdx $ hdrs ! (i + 1)) - 1] | (i, phdr) <- assocs hdrs, (bank phdr /= 128) && (fromIntegral i < length hdrs - 1)]
--     let pzones = pbags pdata
--     let gens = pgens pdata
--     let melodicIndices = catMaybes $ [getInstIndex $ gens ! (genNdx $ pzones ! i) | i <- pbagIndices]
--     return melodicIndices
--     where
--         getInstIndex :: Generator -> Maybe Int
--         getInstIndex (InstIndex i) = Just $ fromIntegral i
--         getInstIndex _             = Nothing
        
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

-- given a list of igen spans, gets the corresponding instrument zones    
getZones :: [(Int, Int)] -> Reader SoundFont [[Generator]]
getZones spans = do
    gens <- igens . pdta <$> ask
    return [[gens ! fromIntegral i | i <- [fst span .. snd span - 1]] | span <- spans]

-- given a list of igen spans (instrument zones), creates a new set of zones where each distinct pitch (on an 88-key keyboard) gets its own zone
-- only divides instruments in bank 0
divideZones :: [(Int, Int)] -> Reader SoundFont [[Generator]]
divideZones spans = do
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
    let newKeyZones = [krange (fromIntegral k) : tail (zones !! i) | (k, i) <- zip prange zoneIndices]
    let newZones = fst <$> sortOn snd (nonKeyPairs ++ [(zone, minIdx) | zone <- newKeyZones])
    return $ case keyPairs of
        []        -> zones  -- instrument does not have key zones
        otherwise -> newZones
    where
        getKeyRange :: Generator -> Maybe (Int, Int)
        getKeyRange (KeyRange kmin kmax) = Just (fromIntegral kmin, fromIntegral kmax)
        getKeyRange _                    = Nothing

-- melodicIbagSpans = melodicInstIndices >>= ibagSpans
-- melodicIgenSpans = melodicIbagSpans >>= igenSpans
-- melodicDividedZones = melodicIgenSpans >>= (mapM divideZones)

divideMelodicInstruments :: Reader SoundFont SoundFont
divideMelodicInstruments = do
    sf <- ask
    let pdata = pdta sf
    let instIndices = [0..(length $ insts $ pdta $ sf) - 1]
    -- NB: we assume presets and instruments are one-to-one
    let validFlags = assert ((length $ phdrs pdata) == (length instIndices)) [bank hdr == 0 | hdr <- elems $ phdrs pdata]
    let bagSpans = runReader (ibagSpans $ init instIndices) sf
    let genSpans = runReader (igenSpans bagSpans) sf
    let newZones = [runReader (if flag then (divideZones spans) else (getZones spans)) sf | (flag, spans) <- zip validFlags genSpans]
    let newBagIndices = cumsum $ length <$> newZones
    let newInsts = mkArray [fixInst instrument i | (instrument, i) <- zip (elems $ insts pdata) newBagIndices]
    let newGenIndices = cumsum $ length <$> concat newZones
    -- for simplicity, require all modulator indices to be 0
    let modIndexSet = S.fromList $ [modNdx bag | bag <- elems $ (ibags pdata)]
    let assertion = assert (modIndexSet == S.singleton 0)
    let newIbags = mkArray [Bag {genNdx = fromIntegral i, modNdx = 0} | i <- newGenIndices]
    let newIgens = mkArray $ concat $ concat newZones
    let newPhdrs = fixHdr <$> phdrs pdata
    let newPdata = pdata {phdrs = newPhdrs, insts = newInsts, ibags = newIbags, igens = newIgens}
    return $ assertion $ sf {pdta = newPdata}    

fixHdr :: Phdr -> Phdr
fixHdr hdr@(Phdr {presetName}) = hdr {presetName = strip presetName}
fixInst :: Inst -> Int -> Inst
fixInst instrument@(Inst {instName}) bagIndex = instrument {instName = strip instName, instBagNdx = fromIntegral bagIndex}


divideSoundFont :: FilePath -> FilePath -> IO ()
divideSoundFont infile outfile = do
    sf <- either (\err -> error "invalid SoundFont") id <$> importFile infile
    let sf' = runReader divideMelodicInstruments sf
    exportFile outfile sf'
{-# LANGUAGE NamedFieldPuns #-}

module Music.SoundFont where

import Codec.SoundFont (Bag(..), Generator(..), Info(..), Inst(..), Phdr(..), Pdta(..), SoundFont(..), exportFile, importFile)
import Control.Exception (assert)
import Control.Monad.Reader
import Data.Array ((!), elems)
import Data.List (partition)
import Data.Maybe (fromJust, isJust)
import qualified Data.Set as S
import Data.Range.Range (fromRanges)
import Data.Sort (sort, sortOn)
import Data.Time (getCurrentTime)
import System.IO.Unsafe (unsafePerformIO)
import System.FilePath.Posix
import System.Posix.User (getEffectiveUserName)

import Euterpea (PitchClass(..))
import Misc.Utils (cumsum, mkArray, pairwise, strip)
import Music.Tuning (StdPitch, Tuning, TuningPackage, TuningSystem, centsFromStd, makeTuning, pianoRange)


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

sfPath = "/Users/jeremander/Programming/Music/SoundFonts/GM"
gmPath = sfPath </> "gm.sf2"
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
    let tuningGens = centsToGenerators . round <$> centsFromStd tuning
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
    let modIndexSet = S.fromList [modNdx bag | bag <- elems $ (ibags pdata)]
    let assertion = assert (modIndexSet == S.singleton 0)
    let newIbags = mkArray [Bag {genNdx = fromIntegral i, modNdx = 0} | i <- newGenIndices]
    let newIgens = mkArray $ concat $ concat newZones
    let newPhdrs = fixPhdr <$> phdrs pdata
    let newPdata = pdata {phdrs = newPhdrs, insts = newInsts, ibags = newIbags, igens = newIgens}
    return $ assertion $ sf {pdta = newPdata}

-- SOUNDFONT MANIPULATION --

-- -- given a tuning, retunes a SoundFont
-- retuneSoundFont :: Tuning -> SoundFont -> SoundFont
-- retuneSoundFont tuning = runReader (retuneMelodicInstruments tuning)

-- renames a SoundFont's bank name
-- also adds username & creation date to the header
renameSF :: String -> SoundFont -> IO SoundFont
renameSF name sf@(SoundFont {infos}) = liftM2 modifySF (return sf) infos'
    where
        modifyInfo :: Info -> Info
        modifyInfo (BankName _) = BankName name
        modifyInfo info         = info
        infos' = do
            time <- getCurrentTime
            user <- getEffectiveUserName
            return $ mkArray $ (modifyInfo <$> elems infos) ++ [CreationDate $ show time, Authors user]
        modifySF s i = s {infos = i}

-- given a base frequency and tuning system, retunes the given SoundFont
retuneSF :: StdPitch -> TuningSystem a -> SoundFont -> IO SoundFont
retuneSF std (name, scale) = renameSF name . retuner
    where
        tuning = makeTuning scale std
        retuner = runReader (retuneMelodicInstruments tuning)

-- given a tuning system, base frequency, and path to a SoundFont, retunes the SoundFont and saves it to [name].sf2 in the same directory as the input file
retuneSoundFont :: FilePath -> StdPitch -> TuningSystem a -> IO ()
retuneSoundFont infile std (name, scale) = do
    sf <- either (\err -> error $ "invalid SoundFont " ++ "'" ++ infile ++ "'") id <$> importFile infile
    sf' <- retuneSF std (name, scale) sf
    let outfile = takeDirectory infile </> name <.> "sf2"
    putStrLn outfile
    exportFile outfile sf'

-- given a tuning package (list of tuning systems), base frequency, and path to a SoundFont, retunes the SoundFont in each tuning system, and saves them all to different output files
retuneSoundFonts :: FilePath -> StdPitch -> TuningPackage -> IO ()
retuneSoundFonts infile std pkg = forM_ pkg (retuneSoundFont infile std)

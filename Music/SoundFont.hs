{-# LANGUAGE NamedFieldPuns #-}

module Music.SoundFont where

import Codec.SoundFont (Bag(..), Generator(..), Info(..), Inst(..), Phdr(..), Pdta(..), SoundFont(..), exportFile, importFile)
import Control.Exception (assert)
import Control.Monad.Reader (liftM2, forM_, MonadReader(..), asks, runReader, Reader)
import Data.Array ((!), elems)
import Data.List (partition)
import Data.Maybe (fromJust, isJust)
import qualified Data.Set as S
import Data.Range (fromRanges)
import Data.Sort (sort, sortOn)
import Data.Time (getCurrentTime)
import System.IO.Unsafe (unsafePerformIO)
import System.FilePath.Posix ((<.>), (</>), takeDirectory)
import System.Posix.User (getEffectiveUserName)

import Euterpea (PitchClass(..))
import Misc.Utils (cumsum, mkArray, pairwise, strip)
import Music.Tuning (StdPitch, Tuning, TuningPackage, TuningSystem, centsFromStd, makeTuning, pianoRange)


type Span = (Int, Int)


-- * Sound Fonts

-- | Given a number of cents to adjust, returns a list of 'Generator's reflecting the change.
centsToGenerators :: Int -> [Generator]
centsToGenerators cts = coarse ++ fine
    where
        (isNeg, absCts) = (cts < 0, abs cts)
        (semis, cts') = divMod absCts 100
        coarse = [CoarseTune (if isNeg then (-semis) else semis) | semis /= 0]
        fine = [FineTune (if isNeg then (-cts') else cts') | cts' /= 0]

getKeyRange :: Generator -> Maybe (Int, Int)
getKeyRange (KeyRange kmin kmax) = Just (fromIntegral kmin, fromIntegral kmax)
getKeyRange _                    = Nothing

fixPhdr :: Phdr -> Phdr
fixPhdr hdr@(Phdr {presetName}) = hdr {presetName = strip presetName}

fixInst :: Inst -> Int -> Inst
fixInst instrument@(Inst {instName}) bagIndex = instrument {instName = strip instName, instBagNdx = fromIntegral bagIndex}


-- ** Accessors

ibagSpans :: [Int] -> Reader SoundFont [Span]
ibagSpans instIndices = do
    pdata <- asks pdta
    let instIndices' = fromIntegral <$> instIndices ++ [last instIndices + 1]
    let ibagIndices = fromIntegral <$> [instBagNdx $ insts pdata ! i | i <- instIndices']
    return $ pairwise ibagIndices

igenSpans :: [Span] -> Reader SoundFont [[Span]]
igenSpans spans = do
    bags <- asks $ ibags . pdta
    let igenIndices = [[fromIntegral . genNdx $ bags ! fromIntegral i | i <- [start..stop]] | (start, stop) <- spans]
    return $ pairwise <$> igenIndices

-- NB: it is important that tuning generators come before the SampleMode generator
insertTuningGens :: [Generator] -> [Generator] -> [Generator]
insertTuningGens tuningGens gens = left ++ tuningGens ++ right
    where
        isSampleMode :: Generator -> Bool
        isSampleMode (SampleMode _) = True
        isSampleMode _              = False
        (left, right) = break isSampleMode gens

-- | Given a list of @igen@ spans, gets the corresponding instrument zones.
instrumentZones :: [Span] -> Reader SoundFont [[Generator]]
instrumentZones spans = do
    gens <- asks $ igens . pdta
    return [[gens ! fromIntegral i | i <- [fst span .. snd span - 1]] | span <- spans]


-- ** Manipulators

-- | Given a list of @igen@ spans (instrument zones), creates a new set of zones where each distinct pitch (on an 88-key keyboard) gets its own zone, and each pitch is re-tuned according to the given tuning.
retuneZones :: Tuning -> [Span] -> Reader SoundFont [[Generator]]
retuneZones tuning spans = do
    gens <- asks $ igens . pdta
    let zones = [[gens ! fromIntegral i | i <- [fst span .. snd span - 1]] | span <- spans]
    let zonePairs = zip zones [0..]
    let (keyPairs, nonKeyPairs) = partition (isJust . getKeyRange . head . fst) zonePairs
    let numKeyPairs = length keyPairs
    let keyRanges = sort [(fromJust $ getKeyRange $ head zone, i) | (zone, i) <- keyPairs]
    let firstPair = head keyRanges
    let (minKey, minIdx) = (fst $ fst firstPair, snd firstPair)
    let prange = fromRanges [pianoRange]
    let (bottomNote, topNote) = (fromIntegral $ minimum prange, fromIntegral $ maximum prange)
    let zoneIndices = [if k < minKey then minIdx else (snd $ last $ takeWhile (\pair -> k >= (fst $ fst pair)) keyRanges) | k <- prange]
    let krange k
          | k == bottomNote = KeyRange 0 k
          | k == topNote    = KeyRange k 127
          | otherwise       = KeyRange k k
    let tuningGens = centsToGenerators . round <$> centsFromStd tuning
    let newKeyZones = [insertTuningGens (tuningGens !! k) (krange (fromIntegral k) : tail (zones !! i)) | (k, i) <- zip prange zoneIndices]
    let newZones = fst <$> sortOn snd (nonKeyPairs ++ [(zone, minIdx) | zone <- newKeyZones])
    return $ case keyPairs of
        [] -> zones  -- instrument does not have key zones
        _  -> newZones

-- | Retunes all melodic instruments in a SoundFont.
--   For now, only does this to instruments in bank 0 up to preset 63, to avoid int16 overflow issues (should ideally go up to at least preset 95).
retuneMelodicInstruments :: Tuning -> Reader SoundFont SoundFont
retuneMelodicInstruments tuning = do
    sf <- ask
    let pdata = pdta sf
    let numInsts = length $ insts pdata
    let instIndices = [0..(numInsts - 1)]
    -- NB: we assume presets and instruments are one-to-one
    -- let validFlags = assert ((length $ phdrs pdata) == (length instIndices)) [(bank hdr == 0) && (preset hdr <= 63) | hdr <- elems $ phdrs pdata]
    let validFlags = assert (length (phdrs pdata) == numInsts) [bank hdr == 0 && preset hdr == 0 | hdr <- elems $ phdrs pdata]
    bagSpans <- ibagSpans $ init instIndices
    genSpans <- igenSpans bagSpans
    zones' <- sequence [if flag then retuneZones tuning spans else instrumentZones spans | (flag, spans) <- zip validFlags genSpans]
    let newBagIndices = cumsum $ length <$> zones'
    let insts' = mkArray [fixInst instrument i | (instrument, i) <- zip (elems $ insts pdata) newBagIndices]
    let genIndices' = cumsum $ length <$> concat zones'
    -- for simplicity, require all modulator indices to be 0
    let modIndexSet = S.fromList [modNdx bag | bag <- elems $ ibags pdata]
    let assertion = assert (modIndexSet == S.singleton 0)
    let ibags' = mkArray [Bag {genNdx = fromIntegral i, modNdx = 0} | i <- genIndices']
    let igens' = mkArray $ concat $ concat zones'
    let phdrs' = fixPhdr <$> phdrs pdata
    let pdata' = pdata {phdrs = phdrs', insts = insts', ibags = ibags', igens = igens'}
    return $ assertion $ sf {pdta = pdata'}

-- | Renames a SoundFont's bank name.
--   Also adds username & creation date to the header.
renameSF :: String -> SoundFont -> IO SoundFont
renameSF name sf@(SoundFont {infos}) = do
    time <- getCurrentTime
    user <- getEffectiveUserName
    let infos' = mkArray $ (modifyInfo <$> elems infos) ++ [CreationDate $ show time, Authors user]
    return $ sf {infos = infos'}
    where
        modifyInfo :: Info -> Info
        modifyInfo (BankName _) = BankName name
        modifyInfo info         = info

-- | Given a base frequency and tuning system, retunes the given SoundFont.
retuneSF :: StdPitch -> TuningSystem a -> SoundFont -> IO SoundFont
retuneSF std (name, scale) = renameSF name . retuner
    where
        tuning = makeTuning scale std
        retuner = runReader (retuneMelodicInstruments tuning)

-- | Given an input path, output directory, base frequency, and 'TuningSystem', retunes the SoundFont and saves it to @[system name].sf2@.
retuneSoundFont :: FilePath -> FilePath -> StdPitch -> TuningSystem a -> IO ()
retuneSoundFont infile outdir std (name, scale) = do
    sf <- either (\err -> error $ "invalid SoundFont " ++ "'" ++ infile ++ "'") id <$> importFile infile
    sf' <- retuneSF std (name, scale) sf
    let outfile = outdir </> name <.> "sf2"
    putStrLn outfile
    exportFile outfile sf'

-- | Given a 'TuningPackage' (list of 'TuningSystem's), base frequency, and path to a SoundFont, retunes the SoundFont in each tuning system, and saves them all to different output files.
retuneSoundFonts :: FilePath -> FilePath -> StdPitch -> TuningPackage -> IO ()
retuneSoundFonts infile outdir std pkg = forM_ pkg (retuneSoundFont infile outdir std)

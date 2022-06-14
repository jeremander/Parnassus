{-# LANGUAGE NamedFieldPuns #-}

module Music.SoundFont where

import Codec.SoundFont (Bag(..), Generator(..), Info(..), Inst(..), Phdr(..), Pdta(..), Sdta(..), Shdr(..), SoundFont(..), exportFile, importFile)
import Control.Exception (assert)
import Control.Monad (forM_)
import Data.Array ((!), elems)
import qualified Data.Array.IArray as IA
import Data.List ((\\), nub, partition)
import qualified Data.Map as M
import Data.Maybe (catMaybes, fromJust, isJust, mapMaybe)
import Data.Range (fromRanges)
import qualified Data.Set as S
import Data.Sort (sort, sortOn)
import Data.Time (getCurrentTime)
import System.FilePath.Posix ((<.>), (</>))
import System.Posix.User (getEffectiveUserName)

import Misc.Utils (atIndices, cumsum, inverseIndexMap, mkArray, pairwise, strip)
import Music.Tuning (StdPitch, Tuning, TuningPackage, TuningSystem, centsFromStd, makeTuning, pianoRange)


type Span = (Int, Int)
type SFMod = SoundFont -> SoundFont


-- * Sound Fonts

-- ** Helper Functions

-- | Given a number of cents to adjust, returns a list of 'Generator's reflecting the change.
centsToGenerators :: Int -> [Generator]
centsToGenerators cts = coarse ++ fine
    where
        (isNeg, absCts) = (cts < 0, abs cts)
        (semis, cts') = divMod absCts 100
        coarse = [CoarseTune (if isNeg then (-semis) else semis) | semis /= 0]
        fine = [FineTune (if isNeg then (-cts') else cts') | cts' /= 0]

spansToIndices :: [Span] -> [Int]
spansToIndices spans = concat [[start..(stop - 1)] | (start, stop) <- spans]

invIdxMap :: [Int] -> M.Map Word Word
invIdxMap indices = M.fromList [(fromIntegral i, fromIntegral j) | (i, j) <- inverseIndexMap indices]

-- ** Accessors

-- | Given a 'KeyRange' 'Generator', extracts the bounds of the range.
getKeyRange :: Generator -> Maybe (Int, Int)
getKeyRange (KeyRange kmin kmax) = Just (fromIntegral kmin, fromIntegral kmax)
getKeyRange _                    = Nothing

getInstIdx :: Generator -> Maybe Int
getInstIdx (InstIndex i) = Just $ fromIntegral i
getInstIdx _             = Nothing

-- | Given 'Pdta' and a preset index, extracts the corresponding index 'Span' for the preset's @pbag@s.
pbagSpan :: Pdta -> Int -> Span
pbagSpan pdata i = (go i, go $ i + 1)
    where go j = fromIntegral $ presetBagNdx $ phdrs pdata ! (fromIntegral j)

-- | Given 'Pdta' and a @pbag@ 'Span', extracts the corresponding index 'Spans' for the @pbag@'s generators.
pgenSpan :: Pdta -> Span -> [Span]
pgenSpan pdata (start, stop) = pairwise [fromIntegral $ genNdx $ pbags pdata ! fromIntegral i | i <- [start..stop]]

-- | Given 'Pdta' and a @pmod@ 'Span', extracts the corresponding index 'Spans' for the @pbag@'s generators.
pmodSpan :: Pdta -> Span -> [Span]
pmodSpan pdata (start, stop) = pairwise [fromIntegral $ modNdx $ pbags pdata ! fromIntegral i | i <- [start..stop]]

-- | Given 'Pdta' and an instrument index, extracts the corresponding index 'Span' for the instrument's @ibag@s.
ibagSpan :: Pdta -> Int -> Span
ibagSpan pdata i = (go i, go $ i + 1)
    where go j = fromIntegral $ instBagNdx $ insts pdata ! (fromIntegral j)

-- | Given 'Pdta' and an @ibag@ 'Span', extracts the corresponding index 'Spans' for the @ibag@'s generators.
igenSpan :: Pdta -> Span -> [Span]
igenSpan pdata (start, stop) = pairwise [fromIntegral $ genNdx $ ibags pdata ! fromIntegral i | i <- [start..stop]]

-- | Given 'Pdta' and an @imod@ 'Span', extracts the corresponding index 'Spans' for the @ibag@'s generators.
imodSpan :: Pdta -> Span -> [Span]
imodSpan pdata (start, stop) = pairwise [fromIntegral $ modNdx $ ibags pdata ! fromIntegral i | i <- [start..stop]]

-- | Given a preset index, gets the indices of the instruments assigned to it.
instIdxForPhdrIdx :: Pdta -> Int -> [Int]
instIdxForPhdrIdx pdata i = catMaybes [getInstIdx $ pgens pdata ! (fromIntegral stop - 1) | (_, stop) <- genSpans]
    where genSpans = pgenSpan pdata (pbagSpan pdata i)

-- | Inserts tuning 'Generator's ('FineTune' or 'CoarseTune') within a list of existing generators.
--
--   NOTE: it is important that the tuning generators come before the 'SampleMode' generator.
insertTuningGens :: [Generator] -> [Generator] -> [Generator]
insertTuningGens tuningGens gens = left ++ tuningGens ++ right
    where
        isSampleMode :: Generator -> Bool
        isSampleMode (SampleMode _) = True
        isSampleMode _              = False
        (left, right) = break isSampleMode gens

-- | Given 'Pdta' and a list of @igen@ spans, gets the corresponding instrument zones.
instrumentZones :: Pdta -> [Span] -> [[Generator]]
instrumentZones pdata spans = zones
    where
        gens = igens pdata
        zones = [[gens ! fromIntegral i | i <- [fst span .. snd span - 1]] | span <- spans]

-- | Removes all unused samples from a 'SoundFont'.
filterUnusedSamples :: SFMod
filterUnusedSamples sf = sf'
    where
        pdata = pdta sf
        getSampleIdx (SampleIndex i) = Just i
        getSampleIdx _               = Nothing
        allGens = elems (pgens pdata) ++ elems (igens pdata)
        -- filter the sample indices
        sampleIndices = fromIntegral <$> (sort $ nub $ mapMaybe getSampleIdx allGens)
        sampleIdxMap = invIdxMap sampleIndices
        fixGen (SampleIndex i) = SampleIndex $ sampleIdxMap M.! i
        fixGen gen           = gen
        pgens' = fixGen <$> pgens pdata
        igens' = fixGen <$> igens pdata
        -- get indices of the raw samples to keep (plus the terminal shdr index)
        shdrs' = shdrs pdata
        numShdrs = length shdrs'
        shdrList = shdrs' `atIndices` (sampleIndices ++ [numShdrs - 1])
        -- ensure at least 46 zero data points available after each sample, as per the standard
        dataSpans = [(i, j + 46) | (i, j) <- pairwise $ fromIntegral <$> (start <$> init shdrList) ++ [end $ last $ init shdrList]]
        dataIndices = spansToIndices dataSpans
        -- dedupe indices
        dataIndices' = sort $ S.elems $ S.fromList dataIndices
        -- adjust sample offsets
        dataIdxMap = invIdxMap dataIndices'
        fixShdrIdx i = if (i == 0) then 0 else (dataIdxMap M.! i)
        fixShdr shdr@(Shdr {start, end}) = shdr {start = fixShdrIdx start, end = fixShdrIdx end}
        shdrs'' = mkArray $ fixShdr <$> shdrList
        pdata' = pdata {pgens = pgens', igens = igens', shdrs = shdrs''}
        -- filter raw samples
        filterData arr = IA.listArray (0, length dataIndices' - 1) [arr IA.! i | i <- dataIndices']
        sdata = sdta sf
        smpl' = filterData $ smpl sdata
        sm24' = filterData <$> sm24 sdata
        sdata' = Sdta {smpl = smpl', sm24 = sm24'}
        sf' = sf {sdta = sdata', pdta = pdata'}

-- | Given preset indices, filters a 'SoundFont' to include only the specified presets. Also filters out instruments and samples that are not assigned to any of the presets.
filterPresets :: [Int] -> SFMod
filterPresets presetIndices sf = sf'
    where
        pdata = pdta sf
        numPresets = length (phdrs pdata) - 1
        pbagSpans = pbagSpan pdata <$> [0..(numPresets - 1)]
        lastPbagIndex = presetBagNdx $ phdrs pdata ! fromIntegral numPresets
        lastPbag = pbags pdata ! lastPbagIndex
        validPbagSpans = mkArray pbagSpans `atIndices` presetIndices
        -- filter the pmods
        validPmodSpans = concatMap (pmodSpan pdata) validPbagSpans
        validPmodIndices = spansToIndices validPmodSpans ++ [fromIntegral $ modNdx lastPbag]
        pmodIdxMap = invIdxMap validPmodIndices
        pmods' = mkArray $ pmods pdata `atIndices` validPmodIndices
        -- filter the pgens
        validPgenSpans = concatMap (pgenSpan pdata) validPbagSpans
        validPgenIndices = spansToIndices validPgenSpans ++ [fromIntegral $ genNdx lastPbag]
        pgenIdxMap = invIdxMap validPgenIndices
        pgens' = mkArray $ pgens pdata `atIndices` validPgenIndices
        -- filter the pbags, adjusting the pgen and pmod indices
        fixPbag pbag = pbag {genNdx = pgenIdxMap M.! genNdx pbag, modNdx = pmodIdxMap M.! modNdx pbag}
        validPbagIndices = spansToIndices validPbagSpans ++ [fromIntegral lastPbagIndex]
        pbags' = mkArray $ fixPbag <$> pbags pdata `atIndices` validPbagIndices
        -- filter the phdrs, adjusting the pbag indices
        pbagIdxMap = invIdxMap validPbagIndices
        fixPhdr phdr = phdr {presetBagNdx = pbagIdxMap M.! presetBagNdx phdr}
        validPhdrIndices = presetIndices ++ [numPresets]
        phdrs' = mkArray $ fixPhdr <$> phdrs pdata `atIndices` validPhdrIndices
        pdata' = pdata {phdrs = phdrs', pbags = pbags', pmods = pmods', pgens = pgens'}
        sf' = filterUnusedSamples $ sf {pdta = pdata'}

-- | Given instrument indices, filters a 'SoundFont' to include only the specified instruments. Also filters out presets and samples that are not assigned to any of the instruments.
filterInstruments :: [Int] -> SFMod
filterInstruments instIndices sf = sf'''
    where
        pdata = pdta sf
        numInsts = length (insts pdata) - 1
        ibagSpans = ibagSpan pdata <$> [0..(numInsts - 1)]
        lastIbagIndex = instBagNdx $ insts pdata ! fromIntegral numInsts
        lastIbag = ibags pdata ! lastIbagIndex
        validIbagSpans = mkArray ibagSpans `atIndices` instIndices
        -- filter the imods
        validImodSpans = concatMap (imodSpan pdata) validIbagSpans
        validImodIndices = spansToIndices validImodSpans ++ [fromIntegral $ modNdx lastIbag]
        imodIdxMap = invIdxMap validImodIndices
        imods' = mkArray $ pmods pdata `atIndices` validImodIndices
        -- filter the igens
        validIgenSpans = concatMap (igenSpan pdata) validIbagSpans
        validIgenIndices = spansToIndices validIgenSpans ++ [fromIntegral $ genNdx lastIbag]
        igenIdxMap = invIdxMap validIgenIndices
        igens' = mkArray $ igens pdata `atIndices` validIgenIndices
        -- filter the ibags, adjusting the igen and imod indices
        fixIbag ibag = ibag {genNdx = igenIdxMap M.! genNdx ibag, modNdx = imodIdxMap M.! modNdx ibag}
        validIbagIndices = spansToIndices validIbagSpans ++ [fromIntegral lastIbagIndex]
        ibags' = mkArray $ fixIbag <$> ibags pdata `atIndices` validIbagIndices
        -- filter the instruments, adjusting the ibag indices
        ibagIdxMap = invIdxMap validIbagIndices
        fixInst inst = inst {instBagNdx = ibagIdxMap M.! instBagNdx inst}
        validInstIndices = instIndices ++ [numInsts]
        insts' = mkArray $ fixInst <$> insts pdata `atIndices` validInstIndices
        pdata' = pdata {insts = insts', ibags = ibags', imods = imods', igens = igens'}
        sf' = sf {pdta = pdata'}
        -- filter out unused presets, if applicable
        numPresets = length (phdrs pdata) - 1
        instIdxSet = S.fromList instIndices
        allPresetIndices = [0..(numPresets - 1)]
        validPresetIndices = [i | i <- allPresetIndices, any (`S.member` instIdxSet) (instIdxForPhdrIdx pdata i)]
        sf'' = if (allPresetIndices == validPresetIndices)
                    then filterUnusedSamples sf'
                    else filterPresets validPresetIndices sf'
        -- finally, remap the instrument indices stored in the pgens
        instIdxMap = invIdxMap instIndices
        fixGen (InstIndex i) = InstIndex $ instIdxMap M.! i
        fixGen gen           = gen
        pdata'' = pdta sf''
        pgens''' = fixGen <$> pgens pdata''
        pdata''' = pdata'' {pgens = pgens'''}
        sf''' = sf'' {pdta = pdata'''}

-- | Removes any unsed instruments from a 'SoundFont'.
filterUnusedInstruments :: SFMod
filterUnusedInstruments sf = sf'
    where
        pdata = pdta sf
        numInsts = length (insts pdata) - 1
        instIndices = nub $ mapMaybe getInstIdx $ elems $ pgens pdata
        unusedIndices = [0..(numInsts - 1)] \\ instIndices
        sf' = if null unusedIndices then sf else filterInstruments instIndices sf

-- | Resets the bank/preset indices to start from bank 0, preset 0.
reindexPresets :: SFMod
reindexPresets sf = sf'
    where
        pdata = pdta sf
        pairs = (,) <$> [0..127] <*> [0..127]  -- all possible (bank, preset) pairs
        phdrs' = mkArray [phdr {bank = bank, preset = preset} | (phdr, (bank, preset)) <- zip (elems $ phdrs pdata) pairs]
        pdata' = pdata {phdrs = phdrs'}
        sf' = sf {pdta = pdata'}

-- ** Manipulators

-- | Strips whitespace from the names of all presets and instruments in a 'SoundFont'.
stripNames :: SFMod
stripNames sf@(SoundFont {pdta}) = sf'
    where
        stripPhdrName phdr@(Phdr {presetName}) = phdr {presetName = strip presetName}
        stripInstName inst@(Inst {instName}) = inst {instName = strip instName}
        phdrs' = stripPhdrName <$> phdrs pdta
        insts' = stripInstName <$> insts pdta
        sf' = sf {pdta = pdta {phdrs = phdrs', insts = insts'}}

-- | Fixes an 'Inst' to strip whitespace from the instrument's name, and also reassigns the @ibag@ index to a specified value.
fixInst :: Inst -> Int -> Inst
fixInst instrument@(Inst {instName}) bagIndex = instrument {instName = strip instName, instBagNdx = fromIntegral bagIndex}

-- | Given a list of @igen@ spans (instrument zones), creates a new set of zones where each distinct pitch (on an 88-key keyboard) gets its own zone, and each pitch is re-tuned according to the given tuning.
retuneZones :: Tuning -> [Span] -> Pdta -> [[Generator]]
retuneZones tuning spans pdata = zones'
    where
        gens = igens pdata
        zones = [[gens ! fromIntegral i | i <- [fst span .. snd span - 1]] | span <- spans]
        zonePairs = zip zones [0..]
        (keyPairs, nonKeyPairs) = partition (isJust . getKeyRange . head . fst) zonePairs
        keyRanges = sort [(fromJust $ getKeyRange $ head zone, i) | (zone, i) <- keyPairs]
        firstPair = head keyRanges
        (minKey, minIdx) = (fst $ fst firstPair, snd firstPair)
        prange = fromRanges [pianoRange]
        (bottomNote, topNote) = (fromIntegral $ minimum prange, fromIntegral $ maximum prange)
        zoneIndices = [if k < minKey then minIdx else (snd $ last $ takeWhile (\pair -> k >= (fst $ fst pair)) keyRanges) | k <- prange]
        krange k
            | k == bottomNote = KeyRange 0 k
            | k == topNote    = KeyRange k 127
            | otherwise       = KeyRange k k
        tuningGens = centsToGenerators . round <$> centsFromStd tuning
        newKeyZones = [insertTuningGens (tuningGens !! k) (krange (fromIntegral k) : tail (zones !! i)) | (k, i) <- zip prange zoneIndices]
        newZones = fst <$> sortOn snd (nonKeyPairs ++ [(zone, minIdx) | zone <- newKeyZones])
        zones' = case keyPairs of
            [] -> zones  -- instrument does not have key zones
            _  -> newZones

-- | Retunes all melodic instruments in a SoundFont.
--   For now, only does this to instruments in bank 0 up to preset 63, to avoid int16 overflow issues (should ideally go up to at least preset 95).
retuneInstruments :: Tuning -> SFMod
retuneInstruments tuning sf = assertion $ sf {pdta = pdata'}
    where
        pdata = pdta sf
        numInsts = length $ insts pdata
        instIndices = [0..(numInsts - 1)]
        -- NB: the last instrument is a sentinel, so exclude it
        bagSpans = ibagSpan pdata <$> init instIndices
        genSpans = igenSpan pdata <$> bagSpans
        zones' = [retuneZones tuning spans pdata | spans <- genSpans]
        newBagIndices = cumsum $ length <$> zones'
        insts' = mkArray [fixInst instrument i | (instrument, i) <- zip (elems $ insts pdata) newBagIndices]
        genIndices' = cumsum $ length <$> concat zones'
        -- for simplicity, require all modulator indices to be 0
        modIndexSet = S.fromList [modNdx bag | bag <- elems $ ibags pdata]
        assertion = assert (modIndexSet == S.singleton 0)
        ibags' = mkArray [Bag {genNdx = fromIntegral i, modNdx = 0} | i <- genIndices']
        igens' = mkArray $ concat $ concat zones'
        pdata' = pdata {insts = insts', ibags = ibags', igens = igens'}

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
        retuner = retuneInstruments tuning

-- Loads a SoundFont file.
loadSoundFont :: FilePath -> IO SoundFont
loadSoundFont path = do
    sf <- importFile path
    return $ case sf of
        Left err  -> error err
        Right sf' -> sf'

-- | Given an IO function modifying a 'SoundFont', an input path, and an output path, loads a 'SoundFont' from the input file, applies the modification, then saves it to the output file.
modifySoundFont :: (SoundFont -> IO SoundFont) -> FilePath -> FilePath -> IO ()
modifySoundFont f infile outfile = do
    putStrLn $ "Loading " ++ infile
    sf <- loadSoundFont infile
    sf' <- f sf
    putStrLn $ "Saving " ++ outfile
    exportFile outfile sf'

-- | Given a list of instrument indices, modfiies a SoundFont file to include only the given instruments.
filterSoundFontInstruments :: [Int] -> FilePath -> FilePath -> IO ()
filterSoundFontInstruments indices = modifySoundFont (return . reindexPresets . filterInstruments indices)

-- | Given a base frequency and 'TuningSystem', retunes a SoundFont file accordingly and uses the tuning system's name as the filename.
retuneSoundFont :: StdPitch -> TuningSystem a -> FilePath -> FilePath -> IO ()
retuneSoundFont std (name, scale) infile outdir = do
    let f = retuneSF std (name, scale)
    let outfile = outdir </> name <.> "sf2"
    modifySoundFont f infile outfile

-- | Given a base frequency, a 'TuningPackage' (list of 'TuningSystem's), path to a SoundFont, and output directory, retunes the SoundFont in each tuning system, and saves them all to different output files.
retuneSoundFonts :: StdPitch -> TuningPackage -> FilePath -> FilePath -> IO ()
retuneSoundFonts std pkg infile outdir = forM_ pkg (\tuning -> retuneSoundFont std tuning infile outdir)

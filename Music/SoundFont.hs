{-# LANGUAGE NamedFieldPuns #-}

module Music.SoundFont where

import Codec.SoundFont (Bag(..), Generator(..), Info(..), Inst(..), Mod(..), Phdr(..), Pdta(..), Sdta(..), Shdr(..), SoundFont(..), exportFile, importFile)
import Control.Exception (assert)
import qualified Data.Array as A
import Data.Array ((!), elems)
import qualified Data.Array.IArray as IA
import Data.List ((\\), nub, partition)
import qualified Data.Map as M
import Data.Maybe (catMaybes, fromJust, isJust, mapMaybe)
import Data.Range (fromRanges)
import qualified Data.Set as S
import Data.Sort (sort, sortOn)
import Data.Time (getCurrentTime)
import System.Posix.User (getEffectiveUserName)

import Misc.Utils (atIndices, Conj, conj, cumsum, inverseIndexMap, mkArray, pairwise, strip)
import Music.Tuning (StdPitch, Tuning, centsFromStd, makeTuning, pianoRange)


type Span = (Int, Int)

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

sliceArr :: (A.Ix i, Num i) => A.Array i a -> Span -> [a]
sliceArr arr (start, stop) = [arr ! fromIntegral i | i <- [start..(stop - 1)]]

-- | Indexes a list at the given indices.
listAtIndices :: [a] -> [Int] -> [a]
listAtIndices xs indices = [x | (i, x) <- zip [0..] xs, i `S.member` idxSet]
    where idxSet = S.fromList indices


-- * Sound Fonts

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

-- | Given 'Pdta' and a @pgen@ 'Span', extracts the corresponding index 'Spans' for the @pbag@'s generators.
pgenSpan :: Pdta -> Span -> [Span]
pgenSpan pdata (start, stop) = pairwise [fromIntegral $ genNdx $ pbags pdata ! fromIntegral i | i <- [start..stop]]

-- | Given 'Pdta' and a @pmod@ 'Span', extracts the corresponding index 'Spans' for the @pbag@'s generators.
pmodSpan :: Pdta -> Span -> [Span]
pmodSpan pdata (start, stop) = pairwise [fromIntegral $ modNdx $ pbags pdata ! fromIntegral i | i <- [start..stop]]

-- | Given 'Pdta' and an instrument index, extracts the corresponding index 'Span' for the instrument's @ibag@s.
ibagSpan :: Pdta -> Int -> Span
ibagSpan pdata i = (go i, go $ i + 1)
    where go j = fromIntegral $ instBagNdx $ insts pdata ! (fromIntegral j)

-- | Given 'Pdta' and an @igen@ 'Span', extracts the corresponding index 'Spans' for the @ibag@'s generators.
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
        zones = [gens `sliceArr` span | span <- spans]

-- ** SFData Type

type SFBag = ([Generator], [Mod])
type SFBags = [SFBag]
type SFPhdr = (Phdr, SFBags)
type SFPhdrs = [SFPhdr]
type SFInst = (String, SFBags)
type SFInsts = [SFInst]
type SFShdrs = [Shdr]

-- | A saner data structure to store the fearsome "hydra" that is the 'SoundFont' 'Pdta'.
data SFPdta = SFPdta {
    sfPhdrs :: SFPhdrs,
    sfInsts :: SFInsts,
    sfShdrs :: SFShdrs
} deriving (Eq, Show)

-- | Alternate representation of a 'SoundFont' that is much easier to manipulate.
data SFData = SFData {
    sfInfos :: [Info],
    sfSdta :: Sdta,
    sfPdta :: SFPdta
} deriving (Eq, Show)

-- ** Conversion

-- | Converts 'Pdta' to 'SFPdta'.
pdtaToSfPdta :: Pdta -> SFPdta
pdtaToSfPdta pdata = sfPdata
    where
        phdrList = [phdr {presetBagNdx = 0} | phdr <- elems $ phdrs pdata]
        numPresets = length phdrList - 1
        pbagSpans = pbagSpan pdata <$> [0..(numPresets - 1)]
        pmodSpans = pmodSpan pdata <$> pbagSpans
        pgenSpans = pgenSpan pdata <$> pbagSpans
        slicePmods = sliceArr (pmods pdata)
        slicePgens = sliceArr (pgens pdata)
        getPbags genSpans modSpans = [(slicePgens genSpan, slicePmods modSpan) | (genSpan, modSpan) <- zip genSpans modSpans]
        allPbags = zipWith getPbags pgenSpans pmodSpans
        sfPhdrs = zip phdrList allPbags
        instList = elems $ insts pdata
        numInsts = length instList - 1
        ibagSpans = ibagSpan pdata <$> [0..(numInsts - 1)]
        imodSpans = imodSpan pdata <$> ibagSpans
        igenSpans = igenSpan pdata <$> ibagSpans
        sliceImods = sliceArr (imods pdata)
        sliceIgens = sliceArr (igens pdata)
        getIbags genSpans modSpans = [(sliceIgens genSpan, sliceImods modSpan) | (genSpan, modSpan) <- zip genSpans modSpans]
        allIbags = zipWith getIbags igenSpans imodSpans
        sfInsts = [(instName inst, bags) | (inst, bags) <- zip instList allIbags]
        sfShdrs = elems $ shdrs pdata
        sfPdata = SFPdta {sfPhdrs, sfInsts, sfShdrs}

-- | Converts 'SFPdta' to 'Pdta'.
sfPdtaToPdta :: SFPdta -> Pdta
sfPdtaToPdta sfPdata = pdata
    where
        -- define terminal (sentinel) elements
        terminalPhdr = Phdr {presetName = "EOP", preset = 255, bank = 255, presetBagNdx = 0, library = 0, genre = 0, morphology = 0}
        terminalGen = StartAddressOffset 0
        terminalMod = Mod {srcOper = 0, destOper = 0, amount = 0, amtSrcOper = 0, transOper = 0}
        -- convert the preset data
        (phdrList, sfPbags) = unzip $ sfPhdrs sfPdata
        pbagOffsets = cumsum $ length <$> sfPbags
        phdrs = mkArray $ [phdr {presetBagNdx = fromIntegral i} | (i, phdr) <- zip pbagOffsets (phdrList ++ [terminalPhdr])]
        pgenZones = map fst <$> sfPbags
        pgenZoneLengths = map length <$> pgenZones
        pbagGenOffsets = cumsum $ fromIntegral . sum <$> pgenZoneLengths
        pmodZones = map snd <$> sfPbags
        pmodZoneLengths = map length <$> pmodZones
        pbagModOffsets = cumsum $ fromIntegral . sum <$> pmodZoneLengths
        pbags = mkArray $ [Bag {genNdx, modNdx} | (genNdx, modNdx) <- zip pbagGenOffsets pbagModOffsets]
        pmods = mkArray $ concatMap concat pmodZones ++ [terminalMod]
        pgens = mkArray $ concatMap concat pgenZones ++ [terminalGen]
        -- convert the instrument data
        (instNames, sfIbags) = unzip $ sfInsts sfPdata
        ibagOffsets = cumsum $ length <$> sfIbags
        insts = mkArray $ [Inst {instName, instBagNdx = fromIntegral i} | (i, instName) <- zip ibagOffsets (instNames ++ ["EOI"])]
        igenZones = map fst <$> sfIbags
        igenZoneLengths = concatMap (map length) igenZones
        ibagGenOffsets = cumsum $ fromIntegral <$> igenZoneLengths
        imodZones = map snd <$> sfIbags
        imodZoneLengths = concatMap (map length) imodZones
        ibagModOffsets = cumsum $ fromIntegral <$> imodZoneLengths
        ibags = mkArray $ [Bag {genNdx, modNdx} | (genNdx, modNdx) <- zip ibagGenOffsets ibagModOffsets]
        imods = mkArray $ concatMap concat imodZones ++ [terminalMod]
        igens = mkArray $ concatMap concat igenZones ++ [terminalGen]
        shdrs = mkArray $ sfShdrs sfPdata
        pdata = Pdta {phdrs, pbags, pmods, pgens, insts, ibags, imods, igens, shdrs}

soundFontToSfData :: SoundFont -> SFData
soundFontToSfData sf = sfd
    where
        pdata = pdtaToSfPdta $ pdta sf
        sfd = SFData {sfInfos = elems $ infos sf, sfSdta = sdta sf, sfPdta = pdata}

sfDataToSoundFont :: SFData -> SoundFont
sfDataToSoundFont sfd = sf
    where
        infos' = mkArray $ sfInfos sfd
        pdata = sfPdtaToPdta $ sfPdta sfd
        sf = SoundFont {infos = infos', sdta = sfSdta sfd, pdta = pdata}

type SFMod = SFData -> SFData
type SFModIO = SFData -> IO SFData

-- | Defines a 'SoundFont' transformation in terms of an 'SFData' transformation ('SFMod').
conjSF :: Conj SoundFont SFData
conjSF = conj soundFontToSfData sfDataToSoundFont

-- | Defines an 'SFMod' in terms of a transformation on 'SFPdta'.
overSfPdta :: SFData -> Conj SFData SFPdta
overSfPdta sf = conj sfPdta (\sfPdata -> sf {sfPdta = sfPdata})


-- ** Manipulators

-- | Strips whitespace from the names of all presets and instruments in a 'SoundFont'.
sfStripNames :: SFMod
sfStripNames sf = sf'
    where
        stripPhdrName phdr@(Phdr {presetName}) = phdr {presetName = strip presetName}
        sfPdata = sfPdta sf
        sfPhdrs' = [(stripPhdrName phdr, bags) | (phdr, bags) <- sfPhdrs sfPdata]
        sfInsts' = [(strip name, bags) | (name, bags) <- sfInsts sfPdata]
        sf' = sf {sfPdta = sfPdata {sfPhdrs = sfPhdrs', sfInsts = sfInsts'}}

-- | Renames a 'SoundFont''s bank name.
sfRename :: String -> SFMod
sfRename name sf = sf {sfInfos = sfInfos'}
    where
        fixInfo (BankName _) = BankName name
        fixInfo info         = info
        sfInfos' = fixInfo <$> sfInfos sf

-- | Adds a username and creation date to the header of a 'SoundFont'.
sfStampWithUserAndTime :: SFModIO
sfStampWithUserAndTime sf = do
    time <- getCurrentTime
    user <- getEffectiveUserName
    let sfInfos' = sfInfos sf ++ [CreationDate $ show time, Authors user]
    return $ sf {sfInfos = sfInfos'}

-- | Resets the bank/preset indices to start from bank 0, preset 0.
sfReindexPresets :: SFMod
sfReindexPresets sf = sf'
    where
        sfPdata = sfPdta sf
        pairs = (,) <$> [0..127] <*> [0..127]  -- all possible (bank, preset) pairs
        sfPhdrs' = [(phdr {bank, preset}, bags) | ((phdr, bags), (bank, preset)) <- zip (sfPhdrs sfPdata) pairs]
        sfPdata' = sfPdata {sfPhdrs = sfPhdrs'}
        sf' = sf {sfPdta = sfPdata'}

-- | Applies a function to all 'Generators' in an 'SFPdta'.
mapGen :: (Generator -> Generator) -> SFPdta -> SFPdta
mapGen f sfPdata = sfPdata'
    where
        fixBags bags = [(f <$> gens, mods) | (gens, mods) <- bags]
        sfPhdrs' = [(phdr, fixBags bags) | (phdr, bags) <- sfPhdrs sfPdata]
        sfInsts' = [(name, fixBags bags) | (name, bags) <- sfInsts sfPdata]
        sfPdata' = sfPdata {sfPhdrs = sfPhdrs', sfInsts = sfInsts'}

-- | Given a list of name suffixes, makes a copy of each instrument, applying each suffix to each instrument.
--   Also copies presets corresponding to each copied instrument (with its corresponding suffix), and reindexes the banks/presets from zero.
sfCopyInstruments :: [String] -> SFMod
sfCopyInstruments suffixes sf = sfReindexPresets $ sf {sfPdta = sfPdata'}
    where
        sfPdata = sfPdta sf
        insts = sfInsts sfPdata
        numInsts = length insts
        -- replicate instruments with each suffix
        sfInsts' = concat [[(name ++ suffix, bags) | (name, bags) <- insts] | suffix <- suffixes]
        -- replicate phdrs with indices of replicated instruments
        fixGen i (InstIndex j) = InstIndex $ fromIntegral (i * numInsts) + j
        fixGen _ gen           = gen
        fixPhdr suffix phdr = phdr {presetName = presetName phdr ++ suffix}
        getNewPhdrs (phdr, bags) = [(fixPhdr suffix phdr, [(fixGen i <$> gens, mods) | (gens, mods) <- bags]) | (i, suffix) <- zip [0..] suffixes]
        sfPhdrs' = concatMap getNewPhdrs $ sfPhdrs sfPdata
        sfPdata' = sfPdata {sfPhdrs = sfPhdrs', sfInsts = sfInsts'}

-- TODO: Monoid instance

-- *** Filtering

-- | Removes unused sample data from 'SFData' and reindexes appropriately.
sfFilterUnusedSampleData :: SFMod
sfFilterUnusedSampleData sf = sf'
    where
        sfPdata = sfPdta sf
        -- get indices of the raw samples to keep
        shdrs = sfShdrs sfPdata
        -- ensure at least 46 zero data points available after each sample, as per the standard
        dataSpans = [(i, j + 46) | (i, j) <- pairwise $ fromIntegral <$> (start <$> shdrs) ++ [end $ last shdrs]]
        -- get indices from spans, and dedupe
        dataIndices = sort $ S.elems $ S.fromList $ spansToIndices dataSpans
        -- adjust sample data offsets
        dataIdxMap = invIdxMap dataIndices
        fixShdrIdx i = if (i == 0) then 0 else (dataIdxMap M.! i)
        fixShdr shdr@(Shdr {start, end, startLoop, endLoop}) = shdr {start = fixShdrIdx start, end = fixShdrIdx end, startLoop = fixShdrIdx startLoop, endLoop = fixShdrIdx endLoop}
        shdrs' = fixShdr <$> shdrs
        sfPdata' = sfPdata {sfShdrs = shdrs'}
        -- filter the raw data
        filterData arr = IA.listArray (0, length dataIndices - 1) [arr IA.! i | i <- dataIndices]
        sfSdata = sfSdta sf
        smpl' = filterData $ smpl sfSdata
        sm24' = filterData <$> sm24 sfSdata
        sfSdata' = Sdta {smpl = smpl', sm24 = sm24'}
        sf' = sf {sfSdta = sfSdata', sfPdta = sfPdata'}

-- | Removes all unused samples from 'SFData'.
sfFilterUnusedSamples :: SFMod
sfFilterUnusedSamples sf = sfFilterUnusedSampleData sf'
    where
        sfPdata = sfPdta sf
        -- identify the sample indices used in any preset/instrument
        getSampleIdx (SampleIndex i) = Just i
        getSampleIdx _               = Nothing
        allBags = concat $ (snd <$> sfPhdrs sfPdata) ++ (snd <$> sfInsts sfPdata)
        allGens = concatMap fst allBags
        sampleIndices = fromIntegral <$> (sort $ nub $ mapMaybe getSampleIdx allGens)
        -- filter the samples to include only the ones used
        sfShdrs' = sfShdrs sfPdata `listAtIndices` sampleIndices
        -- reindex the samples and adjust the indices in all Generators
        sampleIdxMap = invIdxMap sampleIndices
        fixGen (SampleIndex i) = SampleIndex $ sampleIdxMap M.! i
        fixGen gen             = gen
        sfPdata' = mapGen fixGen sfPdata {sfShdrs = sfShdrs'}
        sf' = sf {sfPdta = sfPdata'}

-- | For each preset, gets the list of instrument indices it utilizes.
instIndicesForPresets :: SFPdta -> [[Int]]
instIndicesForPresets sfPdata = mapMaybe getInstIdx <$> gens
    where gens = concatMap fst . snd <$> sfPhdrs sfPdata

-- | Given instrument indices to be kept, filters the instruments by these indices, and reindexes the instruments to start from 0.
sfPdtaFilterInstruments :: [Int] -> SFPdta -> SFPdta
sfPdtaFilterInstruments instIndices sfPdata = (mapGen fixGen sfPdata) {sfInsts = sfInsts'}
    where
        -- filter the instruments
        sfInsts' = sfInsts sfPdata `listAtIndices` instIndices
        -- adjust the instrument indices in the Generators
        instIdxMap = invIdxMap instIndices
        fixGen (InstIndex i) = InstIndex $ instIdxMap M.! i
        fixGen gen           = gen

-- | Filters out any instruments not used in any preset.
sfPdtaFilterUnusedInstruments :: SFPdta -> SFPdta
sfPdtaFilterUnusedInstruments sfPdata = if (length instIndices == numInsts) then sfPdata else sfPdata'
    where
        numInsts = length $ sfInsts sfPdata
        -- get indices of instruments used in any preset
        instIndices = sort $ nub $ concat $ instIndicesForPresets sfPdata
        sfPdata' = sfPdtaFilterInstruments instIndices sfPdata

-- | Given preset indices, filters 'SFData' to include only the specified presets. Then filters out any unused instruments and samples.
sfFilterPresets :: [Int] -> SFMod
sfFilterPresets presetIndices sf = sfFilterUnusedSamples sf'
    where
        sfPdata = sfPdta sf
        sfPhdrs' = sfPhdrs sfPdata `listAtIndices` presetIndices
        sfPdata' = sfPdtaFilterUnusedInstruments $ sfPdata {sfPhdrs = sfPhdrs'}
        sf' = sf {sfPdta = sfPdata'}

-- | Given instrument indices, filters 'SFData' to include only the specified instruments. Also filters out presets and samples that are assigned to any of the remaining instruments.
sfFilterInstruments :: [Int] -> SFMod
sfFilterInstruments instIndices sf = sfFilterPresets presetIndices sf'
    where
        sfPdata = sfPdta sf
        -- filter instruments and relabel Generators
        sfPdata' = sfPdtaFilterInstruments instIndices sfPdata
        sf' = sf {sfPdta = sfPdata'}
        -- determine which presets to keep
        instIdxSet = S.fromList instIndices
        presetIndices = [i | (i, inds) <- zip [0..] (instIndicesForPresets sfPdata), or [j `S.member` instIdxSet | j <- inds]]


-- -- *** Retuning

-- -- | Given a list of @igen@ spans (instrument zones), creates a new set of zones where each distinct pitch (on an 88-key keyboard) gets its own zone, and each pitch is re-tuned according to the given tuning.
-- retuneZones :: Tuning -> Pdta -> [Span] -> [[Generator]]
-- retuneZones tuning pdata spans = zones'
--     where
--         gens = igens pdata
--         zones = [[gens ! fromIntegral i | i <- [fst span .. snd span - 1]] | span <- spans]
--         zonePairs = zip zones [0..]
--         -- partition zones based on whether they contain key ranges
--         (keyPairs, nonKeyPairs) = partition (isJust . getKeyRange . head . fst) zonePairs
--         keyRanges = sort [(fromJust $ getKeyRange $ head zone, i) | (zone, i) <- keyPairs]
--         firstPair = head keyRanges
--         (minKey, minIdx) = (fst $ fst firstPair, snd firstPair)
--         prange = fromRanges [pianoRange]
--         (bottomNote, topNote) = (fromIntegral $ minimum prange, fromIntegral $ maximum prange)
--         -- get the index of the zone for each keyboard key
--         zoneIndices = [if k < minKey then minIdx else (snd $ last $ takeWhile (\pair -> k >= (fst $ fst pair)) keyRanges) | k <- prange]
--         -- assign each key to its own zone, unless it is outside the piano range
--         krange k
--             | k == bottomNote = KeyRange 0 k
--             | k == topNote    = KeyRange k 127
--             | otherwise       = KeyRange k k
--         -- get tuning adjustments for each key in the middle octave
--         tuningGens = centsToGenerators . round <$> centsFromStd tuning
--         newKeyZones = [insertTuningGens (tuningGens !! k) (krange (fromIntegral k) : tail (zones !! i)) | (k, i) <- zip prange zoneIndices]
--         newZones = fst <$> sortOn snd (nonKeyPairs ++ [(zone, minIdx) | zone <- newKeyZones])
--         zones' = if null keyPairs then zones else newZones

-- -- | Retunes instruments in a 'SoundFont' with the given instrument indices.
-- sfRetuneInstruments :: Tuning -> [Int] -> SFMod
-- sfRetuneInstruments tuning instIndices sf = assertion $ sf {pdta = pdata'}
--     where
--         pdata = pdta sf
--         instIdxSet = S.fromList instIndices
--         numInsts = length (insts pdata) - 1
--         ibagSpans = ibagSpan pdata <$> [0..(numInsts - 1)]
--         igenSpans = igenSpan pdata <$> ibagSpans
--         go i = if (i `S.member` instIdxSet) then retuneZones tuning pdata else instrumentZones pdata
--         -- get the new set of igen zones for each instrument
--         zones' = [go i spans | (i, spans) <- zip [0..] igenSpans]
--         bagIndices = cumsum $ length <$> zones'
--         -- adjust the ibag indices to the appropriate zone boundaries
--         fixInst inst bagIdx = inst {instBagNdx = fromIntegral bagIdx}
--         insts' = mkArray [fixInst inst i | (inst, i) <- zip (elems $ insts pdata) bagIndices]
--         genIndices = cumsum $ length <$> concat zones'
--         -- for simplicity, require all modulator indices to be 0
--         modIndexSet = S.fromList [modNdx bag | bag <- elems $ ibags pdata]
--         assertion = assert (modIndexSet == S.singleton 0)
--         ibags' = mkArray [Bag {genNdx = fromIntegral i, modNdx = 0} | i <- genIndices]
--         igens' = mkArray $ concat $ concat zones'
--         pdata' = pdata {insts = insts', ibags = ibags', igens = igens'}

-- -- | Retunes all instruments in a 'SoundFont'.
-- --   WARNING: this can potentially cause integer overflow issues if the number of parameters in the original SoundFont file is large. It may be necessary to filter down to a smaller number of instruments first.
-- sfRetuneAllInstruments :: Tuning -> SFMod
-- sfRetuneAllInstruments tuning sf = sfRetuneInstruments tuning instIndices sf
--     where
--         numInsts = length $ insts $ pdta sf
--         instIndices = [0..(numInsts - 1)]

-- -- | Given a list of 'NamedTuning's, an instrument index, and a 'SoundFont', produces a new 'SoundFont' containing only that single instrument, using all of the given tunings.
-- -- sfRetuneInstrumentMulti :: [NamedTuning] -> Int -> SFMod
-- -- sfRetuneInstrumentMulti pairs i sf = sf'
-- --     where
-- --         sf' = sfReindexPresets $ sfFilterInstruments [i] sf


-- ** I/O

-- | Loads a SoundFont file as a 'SoundFont' object.
loadSoundFont :: FilePath -> IO SoundFont
loadSoundFont path = do
    sf <- importFile path
    return $ case sf of
        Left err  -> error err
        Right sf' -> sf'

loadSfData :: FilePath -> IO SFData
loadSfData = fmap soundFontToSfData . loadSoundFont

saveSoundFont :: FilePath -> SoundFont -> IO ()
saveSoundFont = exportFile

saveSfData :: FilePath -> SFData -> IO ()
saveSfData path = saveSoundFont path . sfDataToSoundFont

-- | Given an IO function modifying an 'SFData', an input path, and an output path, loads a 'SoundFont' from the input file, applies the modification, then saves it to the output file.
modifySfData :: SFModIO -> FilePath -> FilePath -> IO ()
modifySfData f infile outfile = do
    putStrLn $ "Loading " ++ infile
    sf <- loadSfData infile
    sf' <- f sf
    putStrLn $ "Saving " ++ outfile
    saveSfData outfile sf'

-- ** File Operations

-- -- | Given a list of instrument indices, modfiies a SoundFont file to include only the given instruments.
-- filterSoundFontInstruments :: [Int] -> FilePath -> FilePath -> IO ()
-- filterSoundFontInstruments indices = modifySoundFont (return . sfReindexPresets . sfFilterInstruments indices)

-- -- -- | Given a base frequency and 'TuningSystem', retunes a SoundFont file accordingly and uses the tuning system's name as the filename.
-- -- retuneSoundFont :: StdPitch -> TuningSystem a -> FilePath -> FilePath -> IO ()
-- -- retuneSoundFont std (name, scale) infile outdir = do
-- --     let f = sfRetuneMulti std (name, scale)
-- --     let outfile = outdir </> name <.> "sf2"
-- --     modifySoundFont (return . f) infile outfile

-- -- -- | Given a base frequency, a 'TuningPackage' (list of 'TuningSystem's), path to a SoundFont, and output directory, retunes the SoundFont in each tuning system, and saves them all to different output files.
-- -- retuneSoundFonts :: StdPitch -> TuningPackage -> FilePath -> FilePath -> IO ()
-- -- retuneSoundFonts std pkg infile outdir = forM_ pkg (\tuning -> retuneSoundFont std tuning infile outdir)

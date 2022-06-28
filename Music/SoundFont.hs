{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}

module Music.SoundFont where

import Codec.SoundFont (Bag(..), Generator(..), Info(..), Inst(..), Mod(..), Phdr(..), Pdta(..), Sdta(..), Shdr(..), SoundFont(..), exportFile, importFile)
import Control.Monad (forM_)
import qualified Data.Array as A
import Data.Array ((!), elems)
import qualified Data.Array.IArray as IA
import Data.Data (Data(..), cast, toConstr)
import Data.List (nub, sortOn)
import qualified Data.Map as M
import Data.Maybe (catMaybes, fromMaybe, isJust, mapMaybe)
import qualified Data.Set as S
import Data.Sort (sort)
import qualified Data.Text as T
import Data.Time (getCurrentTime)
import System.FilePath.Posix ((<.>), (</>))
import System.Posix.User (getEffectiveUserName)
import qualified Text.Pretty as P

import Misc.Utils (Conj, conj, cumsum, inverseIndexMap, mkArray, mkIArray, pairwise, safeHead, strip)
import Music.Tuning (Cents, centsFromStd, NamedTuning(..), Tuning)


type Span = (Int, Int)
type Generators = [Generator]
type Mods = [Mod]

-- ** Helper Functions

spansToIndices :: [Span] -> [Int]
spansToIndices spans = concat [[start..(stop - 1)] | (start, stop) <- spans]

invIdxMap :: [Int] -> M.Map Word Word
invIdxMap indices = M.fromList [(fromIntegral i, fromIntegral j) | (i, j) <- inverseIndexMap indices]

sliceArray :: (A.Ix i, Num i) => A.Array i a -> Span -> [a]
sliceArray arr (start, stop) = [arr ! fromIntegral i | i <- [start..(stop - 1)]]

sliceIArray :: (IA.IArray arr a, IA.Ix i, Integral i) => arr i a -> (i, i) -> [a]
sliceIArray arr (start, stop) = [arr IA.! fromIntegral i | i <- [start..(stop - 1)]]

-- | Indexes a list at the given indices.
listAtIndices :: [a] -> [Int] -> [a]
listAtIndices xs indices = [x | (i, x) <- zip [0..] xs, i `S.member` idxSet]
    where idxSet = S.fromList indices


-- * Sound Fonts

deriving instance Ord Shdr

-- ** Accessors

-- | Given a 'KeyRange' 'Generator', extracts the bounds of the range.
getKeyRange :: Generator -> Maybe (Int, Int)
getKeyRange (KeyRange kmin kmax) = Just (fromIntegral kmin, fromIntegral kmax)
getKeyRange _                    = Nothing

getInstIdx :: Generator -> Maybe Int
getInstIdx (InstIndex i) = Just $ fromIntegral i
getInstIdx _             = Nothing

getSampleIdx :: Generator -> Maybe Int
getSampleIdx (SampleIndex i) = Just $ fromIntegral i
getSampleIdx _               = Nothing

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

-- | Given 'Pdta' and a list of @igen@ spans, gets the corresponding instrument zones.
instrumentZones :: Pdta -> [Span] -> [Generators]
instrumentZones pdata spans = zones
    where
        gens = igens pdata
        zones = [gens `sliceArray` span | span <- spans]

-- ** SFData Type

type SFBag = (Generators, Mods)
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

-- | For each preset, gets the list of instrument indices it uses.
instIndicesForPresets :: SFPdta -> [[Int]]
instIndicesForPresets sfPdata = mapMaybe getInstIdx <$> gens
    where gens = concatMap fst . snd <$> sfPhdrs sfPdata

-- Gets the indices of all instrument indices used by any preset.
instIndicesUsed :: SFPdta -> [Int]
instIndicesUsed sfPdata = sort $ nub $ concat $ instIndicesForPresets sfPdata

-- | For each preset, gets the list of sample indices it uses.
-- | For each instrument, gets the list of sample indices it uses.
sampleIndicesForPresets :: SFPdta -> [[Int]]
sampleIndicesForPresets sfPdata = mapMaybe getSampleIdx <$> gens
    where gens = fst <$> concatMap snd (sfPhdrs sfPdata)

-- | For each instrument, gets the list of sample indices it uses.
sampleIndicesForInsts :: SFPdta -> [[Int]]
sampleIndicesForInsts sfPdata = mapMaybe getSampleIdx <$> gens
    where gens = fst <$> concatMap snd (sfInsts sfPdata)

-- | Gets the indices of all sample indices used by any preset or instrument.
sampleIndicesUsed :: SFPdta -> [Int]
sampleIndicesUsed sfPdata = sort $ nub $ concat $ sampleIndicesForPresets sfPdata ++ sampleIndicesForInsts sfPdata

-- | Checks whether 'SFData' is valid, issuing an error message if it is not.
validateSfData :: SFData -> Either String ()
validateSfData sf = result
    where
        sfPdata = sfPdta sf
        maxParams = 65536
        presetBags = concat [snd pair | pair <- sfPhdrs sfPdata]
        numPresetGens = sum [length gens | (gens, _) <- presetBags]
        instBags = concat [snd pair | pair <- sfInsts sfPdata]
        numInstGens = sum [length gens | (gens, _) <- instBags]
        result
            | numPresetGens > maxParams = Left $ "number of preset generators = " ++ show numPresetGens ++ " > " ++ show maxParams
            | numInstGens > maxParams   = Left $ "number of instrument generators = " ++ show numInstGens ++ " > " ++ show maxParams
            | otherwise                 = Right ()

-- | Reindexes the presets to start from bank 0, preset 0, with no skipping.
reindexPresets :: SFPhdrs -> SFPhdrs
reindexPresets sfPhdrs = sfPhdrs'
    where
        pairs = (,) <$> [0..127] <*> [0..127]  -- all possible (bank, preset) pairs
        sfPhdrs' = [(phdr {bank, preset}, bags) | ((phdr, bags), (bank, preset)) <- zip sfPhdrs pairs]

-- | Gets the index spans corresponding to each sample.
--
--   Ensures at least 46 zero data points available after each sample, as per the standard.
sampleSpans :: SFData -> [Span]
sampleSpans sf = [(bndStart $ start shdr, bndEnd $ end shdr + 46) | shdr <- sfShdrs $ sfPdta sf]
    where
        maxIdx = snd $ IA.bounds $ smpl $ sfSdta sf
        bndStart i = min (fromIntegral i) maxIdx
        bndEnd i = min (fromIntegral i) (maxIdx + 1)

-- | Removes unused sample data from 'SFData' and reindexes appropriately.
sfFilterUnusedSampleData :: SFMod
sfFilterUnusedSampleData sf = sf'
    where
        sfPdata = sfPdta sf
        sfSdata = sfSdta sf
        -- get indices of the raw samples to keep
        shdrs = sfShdrs sfPdata
        dataSpans = sampleSpans sf
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
        smpl' = filterData $ smpl sfSdata
        sm24' = filterData <$> sm24 sfSdata
        sfSdata' = Sdta {smpl = smpl', sm24 = sm24'}
        sf' = sf {sfSdta = sfSdata', sfPdta = sfPdata'}

-- | Removes all unused samples from 'SFData'.
sfFilterUnusedSamples :: SFMod
sfFilterUnusedSamples sf = sfFilterUnusedSampleData sf'
    where
        sfPdata = sfPdta sf
        sampleIndices = sampleIndicesUsed sfPdata
        -- filter the samples to include only the ones used
        sfShdrs' = sfShdrs sfPdata `listAtIndices` sampleIndices
        -- reindex the samples and adjust the indices in all Generators
        sampleIdxMap = invIdxMap sampleIndices
        fixGen (SampleIndex i) = SampleIndex $ sampleIdxMap M.! i
        fixGen gen             = gen
        sfPdata' = mapGen fixGen sfPdata {sfShdrs = sfShdrs'}
        sf' = sf {sfPdta = sfPdata'}

-- | Dedupes identical samples.
dedupeSamples :: SFMod
dedupeSamples sf = sfFilterUnusedSamples $ sf {sfPdta = sfPdta'}
    where
        sfPdata = sfPdta sf
        sfSdata = sfSdta sf
        -- get the sample data spans for each shdr
        dataSpans = sampleSpans sf
        -- to dedupe spans, get a mapping from sample data to the first span representing it
        sliceSamples span = (smpl sfSdata `sliceIArray` span, (`sliceIArray` span) <$> sm24 sfSdata)
        slices = sliceSamples <$> dataSpans
        spanBySlice = M.fromListWith (\_ span -> span) (zip slices dataSpans)
        -- get index offsets for each shdr
        offsets = fromIntegral <$> [(fst $ spanBySlice M.! slice) - (fst span) | (span, slice) <- zip dataSpans slices]
        fixShdr offset shdr = shdr {start = start shdr + offset, end = end shdr + offset, startLoop = startLoop shdr + offset, endLoop = endLoop shdr + offset}
        -- dedupe identical shdrs
        shdrs' = [fixShdr offset shdr | (offset, shdr) <- zip offsets (sfShdrs sfPdata)]
        sfShdrs' = nub shdrs'
        -- adjust sample indices in instruments
        shdrIdxMap = M.fromListWith (\_ i -> i) (zip shdrs' [0..])
        idxMap = M.fromList [(i, shdrIdxMap M.! shdr) | (i, shdr) <- zip [0..] shdrs']
        fixSampleIndex (SampleIndex i) = SampleIndex $ idxMap M.! i
        fixSampleIndex gen             = gen
        fixInst (name, bags) = (name, [(fixSampleIndex <$> gens, mods) | (gens, mods) <- bags])
        sfInsts' = fixInst <$> sfInsts sfPdata
        sfPdta' = sfPdata {sfInsts = sfInsts', sfShdrs = sfShdrs'}

instance Semigroup SFData where
    sf1 <> sf2 = mconcat [sf1, sf2]

instance Monoid SFData where
    mempty = SFData {sfInfos = [], sfSdta = sfSdta', sfPdta = sfPdta'}
        where
            sfSdta' = Sdta {smpl = mkIArray [], sm24 = Nothing}
            sfPdta' = SFPdta {sfPhdrs = [], sfInsts = [], sfShdrs = []}
    mconcat sfs = dedupeSamples sf'
        where
            -- the first info bundle will take precedence
            sfInfos' = fromMaybe [] (safeHead $ sfInfos <$> sfs)
            -- concatenate sample data
            -- TODO: this could be done more efficiently
            concatArrs arrs = mkIArray arrList
                where
                    arrLists = IA.elems <$> arrs
                    arrList = concat arrLists
            sdtas = sfSdta <$> sfs
            smplArrs = smpl <$> sdtas
            smpl' = concatArrs smplArrs
            arrLen arr = length $ IA.elems arr
            getSm24 sdata = fromMaybe (mkIArray $ replicate numSamples 0) (sm24 sdata)
                where numSamples = arrLen $ smpl sdata
            -- if any sm24s exist, use them (and construct 0 arrays for any that are missing)
            sm24' = if any isJust (sm24 <$> sdtas) then (Just $ concatArrs $ getSm24 <$> sdtas) else Nothing
            sfSdta' = Sdta {smpl = smpl', sm24 = sm24'}
            sfPdtas = sfPdta <$> sfs
            -- adjust sample data indices, then concat shdrs together
            sampleDataOffsets = cumsum $ fromIntegral . arrLen <$> smplArrs
            fixShdr offset shdr = shdr {start = offset + start shdr, end = offset + end shdr, startLoop = offset + startLoop shdr, endLoop = offset + endLoop shdr}
            sfShdrs' = concat [fixShdr offset <$> sfShdrs sfPdata | (offset, sfPdata) <- zip sampleDataOffsets sfPdtas]
            -- adjust sample indices, then concat instruments together
            sampleOffsets = cumsum $ fromIntegral . length . sfShdrs <$> sfPdtas
            fixSampleIndex offset (SampleIndex i) = SampleIndex $ offset + i
            fixSampleIndex _      gen             = gen
            fixInst offset (name, bags) = (name, [(fixSampleIndex offset <$> gens, mods) | (gens, mods) <- bags])
            sfInsts' = concat [fixInst offset <$> sfInsts sfPdata | (offset, sfPdata) <- zip sampleOffsets sfPdtas]
            -- adjust instrument indices, then concat presets together
            instOffsets = cumsum $ fromIntegral . length . sfInsts <$> sfPdtas
            fixInstIndex offset (InstIndex i) = InstIndex $ offset + i
            fixInstIndex _      gen           = gen
            fixPhdr offset (phdr, bags) = (phdr, [(fixInstIndex offset <$> gens, mods) | (gens, mods) <- bags])
            sfPhdrs' = reindexPresets $ concat [fixPhdr offset <$> sfPhdrs sfPdata | (offset, sfPdata) <- zip instOffsets sfPdtas]
            sfPdta' = SFPdta {sfPhdrs = sfPhdrs', sfInsts = sfInsts', sfShdrs = sfShdrs'}
            sf' = SFData {sfInfos = sfInfos', sfSdta = sfSdta', sfPdta = sfPdta'}

-- ** Pretty Printing

deriving instance Data Info

instance P.Pretty Info where
    -- TODO: make this prettier with justification
    pretty info = P.string $ go info
        where
            showArg arg = maybe "" (concat . filter (not . null) . lines) (cast arg :: Maybe String)
            go (Version major minor) = "Version: " ++ show major ++ "." ++ show minor
            go (RomVersion major minor) = "RomVersion: " ++ show major ++ "." ++ show minor
            go info@(ReservedInfo {}) = show info
            go info = (show $ toConstr info) ++ ": " ++ head (gmapQ showArg info)

instance P.Pretty SFPdta where
    pretty sfPdata = P.vcat segments
        where
            justLeft n s = s ++ replicate (n - length s) ' '
            getBagLine bags = P.string $ "\t" ++ show numGens ++ " generator, " ++ show numMods ++ " modulator parameters"
                where
                    numGens = sum $ length . fst <$> bags
                    numMods = sum $ length . snd <$> bags
            getPhdrLine phdr = P.string $ justLeft 6 (show $ bank phdr) ++ justLeft 8 (show $ preset phdr) ++ presetName phdr
            phdrKey phdr = (bank phdr, preset phdr)
            numPresets = length $ sfPhdrs sfPdata
            presetBags = concatMap snd (sfPhdrs sfPdata)
            numInsts = length $ sfInsts sfPdata
            instBags = concatMap snd (sfInsts sfPdata)
            numSamples = length $ sfShdrs sfPdata
            numUnusedInsts = numInsts - (length $ instIndicesUsed sfPdata)
            numUnusedSamples = numSamples - (length $ sampleIndicesUsed sfPdata)
            hdrSeg = P.vcat [
                    P.string $ "Number of presets     = " ++ show numPresets,
                    getBagLine presetBags,
                    P.string $ "Number of instruments = " ++ show numInsts ++ (if numUnusedInsts > 0 then " (" ++ show numUnusedInsts ++ " unused)" else ""),
                    getBagLine instBags,
                    P.string $ "Number of samples     = " ++ show numSamples ++ (if numUnusedSamples > 0 then " (" ++ show numUnusedSamples ++ " unused)" else ""),
                    P.string ""
                ]
            presetSeg = P.vcat $ [
                P.string "Bank  Preset  Name",
                P.string "------------------"
                ] ++ (getPhdrLine <$> sortOn phdrKey (fst <$> sfPhdrs sfPdata))
            segments = [hdrSeg, presetSeg]

instance P.Pretty SFData where
    pretty sf = P.vcat $ (P.pretty <$> sfInfos sf) ++ [P.string "", P.pretty $ sfPdta sf]


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
        slicePmods = sliceArray (pmods pdata)
        slicePgens = sliceArray (pgens pdata)
        getPbags genSpans modSpans = [(slicePgens genSpan, slicePmods modSpan) | (genSpan, modSpan) <- zip genSpans modSpans]
        allPbags = zipWith getPbags pgenSpans pmodSpans
        sfPhdrs = zip phdrList allPbags
        instList = elems $ insts pdata
        numInsts = length instList - 1
        ibagSpans = ibagSpan pdata <$> [0..(numInsts - 1)]
        imodSpans = imodSpan pdata <$> ibagSpans
        igenSpans = igenSpan pdata <$> ibagSpans
        sliceImods = sliceArray (imods pdata)
        sliceIgens = sliceArray (igens pdata)
        getIbags genSpans modSpans = [(sliceIgens genSpan, sliceImods modSpan) | (genSpan, modSpan) <- zip genSpans modSpans]
        allIbags = zipWith getIbags igenSpans imodSpans
        sfInsts = [(instName inst, bags) | (inst, bags) <- zip instList allIbags]
        sfShdrs = init $ elems $ shdrs pdata
        sfPdata = SFPdta {sfPhdrs, sfInsts, sfShdrs}

-- | Converts 'SFPdta' to 'Pdta'.
sfPdtaToPdta :: SFPdta -> Pdta
sfPdtaToPdta sfPdata = pdata
    where
        -- define terminal (sentinel) elements
        terminalPhdr = Phdr {presetName = "EOP", preset = 255, bank = 255, presetBagNdx = 0, library = 0, genre = 0, morphology = 0}
        terminalGen = StartAddressOffset 0
        terminalMod = Mod {srcOper = 0, destOper = 0, amount = 0, amtSrcOper = 0, transOper = 0}
        terminalShdr = Shdr {sampleName = "EOS", start = 0, end = 0, startLoop = 0, endLoop = 0, sampleRate = 0, originalPitch = 0, pitchCorrection = 0, sampleLink = 0, sampleType = 0}
        -- convert the preset data
        (phdrList, sfPbags) = unzip $ sfPhdrs sfPdata
        pbagOffsets = cumsum $ length <$> sfPbags
        phdrs = mkArray $ [phdr {presetBagNdx = fromIntegral i} | (i, phdr) <- zip pbagOffsets (phdrList ++ [terminalPhdr])]
        pgenZones = map fst <$> sfPbags
        pgenZoneLengths = concatMap (map length) pgenZones
        pbagGenOffsets = cumsum $ fromIntegral <$> pgenZoneLengths
        pmodZones = map snd <$> sfPbags
        pmodZoneLengths = concatMap (map length) pmodZones
        pbagModOffsets = cumsum $ fromIntegral <$> pmodZoneLengths
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
        shdrs = mkArray $ sfShdrs sfPdata ++ [terminalShdr]
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
overSfPdta :: Conj SFData SFPdta
overSfPdta go sf = sf {sfPdta = go $ sfPdta sf}

-- ** Manipulators

-- *** Renaming

-- | Renames a 'SoundFont''s bank name.
sfRename :: String -> SFMod
sfRename name sf = sf {sfInfos = sfInfos'}
    where
        fixInfo (BankName _) = BankName name
        fixInfo info         = info
        sfInfos' = fixInfo <$> sfInfos sf

-- | Applies some function to rename all instruments. Will truncate to ensure all names are 20 characters or less.
sfRenameInsts :: (String -> String) -> SFMod
sfRenameInsts rename = overSfPdta go
    where
        go sfPdata = sfPdata {sfPhdrs = sfPhdrs'}
            where sfPhdrs' = [(phdr {presetName = take 20 $ rename $ presetName phdr}, bags) | (phdr, bags) <- sfPhdrs sfPdata]

-- | Applies some function to rename all presets. Will truncate to ensure all names are 20 characters or less.
sfRenamePresets :: (String -> String) -> SFMod
sfRenamePresets rename = overSfPdta go
    where
        go sfPdata = sfPdata {sfInsts = sfInsts'}
            where sfInsts' = [(take 20 $ rename name, bags) | (name, bags) <- sfInsts sfPdata]

-- | Strips whitespace from the names of all presets and instruments in a 'SoundFont'.
sfStripNames :: SFMod
sfStripNames = sfRenamePresets strip . sfRenameInsts strip

-- | Adds a username and creation date to the header of a 'SoundFont'.
sfStampWithUserAndTime :: SFModIO
sfStampWithUserAndTime sf = do
    time <- getCurrentTime
    user <- getEffectiveUserName
    let sfInfos' = sfInfos sf ++ [CreationDate $ show time, Authors user]
    return $ sf {sfInfos = sfInfos'}

-- | Resets the bank/preset indices to start from bank 0, preset 0.
sfReindexPresets :: SFMod
sfReindexPresets = overSfPdta go
    where go sfPdata = sfPdata {sfPhdrs = reindexPresets $ sfPhdrs sfPdata}

-- | Applies a function to all 'Generators' in an 'SFPdta'.
mapGen :: (Generator -> Generator) -> SFPdta -> SFPdta
mapGen f sfPdata = sfPdata'
    where
        fixBags bags = [(f <$> gens, mods) | (gens, mods) <- bags]
        sfPhdrs' = [(phdr, fixBags bags) | (phdr, bags) <- sfPhdrs sfPdata]
        sfInsts' = [(name, fixBags bags) | (name, bags) <- sfInsts sfPdata]
        sfPdata' = sfPdata {sfPhdrs = sfPhdrs', sfInsts = sfInsts'}

-- | Given a function to rename an instrument or preset at a given index, makes some number of copies of each instrument.
--
--   Also copies presets corresponding to each copied instrument (with its corresponding suffix), and reindexes the banks/presets from zero.
sfCopyInstruments :: (Int -> String -> String) -> Int -> SFMod
sfCopyInstruments rename n = sfReindexPresets . overSfPdta go
    where
        go sfPdata = sfPdata {sfPhdrs = sfPhdrs', sfInsts = sfInsts'} where
            insts = sfInsts sfPdata
            numInsts = length insts
            -- replicate instruments with new names
            sfInsts' = concat [[(rename i name, bags) | (name, bags) <- insts] | i <- [0..(n - 1)]]
            -- replicate phdrs with indices of replicated instruments
            fixGen i (InstIndex j) = InstIndex $ fromIntegral (i * numInsts) + j
            fixGen _ gen           = gen
            fixPhdr i phdr = phdr {presetName = rename i $ presetName phdr}
            getNewPhdrs (phdr, bags) = [(fixPhdr i phdr, [(fixGen i <$> gens, mods) | (gens, mods) <- bags]) | i <- [0..(n - 1)]]
            sfPhdrs' = concatMap getNewPhdrs $ sfPhdrs sfPdata

-- *** Filtering

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
        instIndices = instIndicesUsed sfPdata
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
        -- filter instruments and relabel generators
        sfPdata' = sfPdtaFilterInstruments instIndices sfPdata
        sf' = sf {sfPdta = sfPdata'}
        -- determine which presets to keep
        instIdxSet = S.fromList instIndices
        presetIndices = [i | (i, inds) <- zip [0..] (instIndicesForPresets sfPdata), or [j `S.member` instIdxSet | j <- inds]]

-- | Filters out unused instruments.
sfFilterUnusedInstruments :: SFMod
sfFilterUnusedInstruments = overSfPdta sfPdtaFilterUnusedInstruments

-- | Removes unused samples and instruments.
sfClean :: SFMod
sfClean = sfFilterUnusedSamples . sfFilterUnusedInstruments

-- *** Retuning

-- | MIDI key range (bounds are 0 to 127)
type KRange = (Int, Int)
-- | MIDI velocity range (bounds are 0 to 127)
type VRange = (Int, Int)

keyRange :: Generator -> Maybe KRange
keyRange (KeyRange lo hi) = Just (fromIntegral lo, fromIntegral hi)
keyRange _                = Nothing

velRange :: Generator -> Maybe VRange
velRange (VelRange lo hi) = Just (fromIntegral lo, fromIntegral hi)
velRange _                = Nothing

type ZoneModifier = Generators -> [Generators]
type KeyVelZoneModifier = Maybe KRange -> Maybe VRange -> ZoneModifier

-- | Given a function that conditionally modifies a zone based on key and velocity range, applies the modification to a zone.
keyVelModifyZone :: KeyVelZoneModifier -> ZoneModifier
keyVelModifyZone modifier gens = modifier krange vrange gens
    where
        krange = safeHead $ mapMaybe keyRange gens
        vrange = safeHead $ mapMaybe velRange gens

-- | Inserts tuning 'Generator's ('FineTune' or 'CoarseTune') within a list of existing generators.
--
--   NOTE: it is important that the tuning generators come before the 'SampleMode' generator.
insertTuningGens :: Generators -> Generators -> Generators
insertTuningGens tuningGens gens = left ++ tuningGens ++ right
    where
        isSampleMode :: Generator -> Bool
        isSampleMode (SampleMode _) = True
        isSampleMode _              = False
        (left, right) = break isSampleMode gens

-- | Given a number of cents to adjust, returns a list of 'Generator's reflecting the change.
centsToGenerators :: Cents -> Generators
centsToGenerators cts = coarse ++ fine
    where
        (isNeg, absCts) = (cts < 0, round $ abs cts)
        (semis, cts') = divMod absCts 100
        coarse = [CoarseTune (if isNeg then (-semis) else semis) | semis /= 0]
        fine = [FineTune (if isNeg then (-cts') else cts') | cts' /= 0]

-- | Given a pitch adjustment in cents and a zone of 'Generator's, returns a new zone with the adjustment.
retuneGenZone :: Cents -> Generators -> Generators
retuneGenZone cents = insertTuningGens (centsToGenerators cents)

-- | Modifies a zone with a tuning by inserting the appropriate 'Generator's to alter pitch.
tuningModifyZone :: Tuning -> ZoneModifier
tuningModifyZone tuning = keyVelModifyZone modifier
    where
        allCents = centsFromStd tuning
        -- create a distinct retuned zone for each key in the range
        fixGen k (KeyRange _ _) = KeyRange (fromIntegral k) (fromIntegral k)
        fixGen _ gen            = gen
        retuneGens k gens = insertTuningGens (centsToGenerators $ allCents !! k) (fixGen k <$> gens)
        modifier (Just (lo, hi)) _ gens = [retuneGens k gens | k <- [lo..hi]]
        modifier Nothing         _ gens = [gens]

retuneInstBags :: Tuning -> SFBags -> SFBags
retuneInstBags tuning bags = bags'
    where
        modifier = tuningModifyZone tuning
        expandBag (gens, mods) = (, mods) <$> modifier gens
        bags' = concatMap expandBag bags

-- | Retunes all of the instruments in 'SFPdta'.
retuneInstruments :: Tuning -> SFPdta -> SFPdta
retuneInstruments tuning sfPdata = sfPdata'
    where
        go = retuneInstBags tuning
        sfInsts' = [(name, go bags) | (name, bags) <- sfInsts sfPdata]
        sfPdata' = sfPdata {sfInsts = sfInsts'}

-- | Retunes all instruments in 'SFData'.
sfRetuneInstruments :: Tuning -> SFMod
sfRetuneInstruments tuning = overSfPdta $ retuneInstruments tuning

-- | Given 'NamedTuning's and an instrument index, creates a new 'SFData' with one instrument for each retuning of the original instrument.
-- TODO: this will copy samples, which we want to avoid
sfInstrumentRetuned :: [NamedTuning] -> Int -> SFMod
sfInstrumentRetuned namedTunings i sf = mconcat sfs
    where
        sf' = sfReindexPresets $ sfFilterInstruments [i] sf
        rename name = sfRenamePresets (const name) . sfRenameInsts (const name)
        sfs = [rename name $ sfRetuneInstruments tuning sf' | NamedTuning {name, tuning} <- namedTunings]

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
saveSfData path sf = do
    case validateSfData sf of
        Left err -> fail err
        _        -> return ()
    saveSoundFont path $ sfDataToSoundFont sf

-- | Given an IO function modifying an 'SFData', an input path, and an output path, loads a 'SoundFont' from the input file, applies the modification, then saves it to the output file.
modifySfData :: SFModIO -> FilePath -> FilePath -> IO ()
modifySfData f infile outfile = do
    putStrLn $ "Loading " ++ infile
    sf <- loadSfData infile
    sf' <- f sf
    putStrLn $ "Saving " ++ outfile
    saveSfData outfile sf'

-- ** File Operations

-- | Given multiple input .sf2 paths, merges them together into a single SoundFont.
mergeSoundFonts :: [FilePath] -> FilePath -> IO ()
mergeSoundFonts infiles outfile = do
    putStrLn $ "Merging " ++ show (length infiles) ++ " SoundFont(s)."
    let go infile = do
        putStrLn $ "\tLoading " ++ infile
        loadSfData infile
    sfs <- mapM go infiles
    let sf = mconcat sfs
    putStrLn $ "Saving " ++ outfile
    saveSfData outfile sf

-- | Given some named tunings, an input .sf2 file, and an output directory, splits up the SoundFont into separate instruments and creates a new .sf2 file for each instrument containing all of the specified tunings.
retuneSoundFont :: [NamedTuning] -> FilePath -> FilePath -> IO ()
retuneSoundFont pairs infile outdir = do
    putStrLn $ "Loading " ++ infile
    sf <- loadSfData infile
    let instList = sfInsts $ sfPdta sf
    let numInsts = length instList
    let stripDots s = fromMaybe s (T.stripSuffix "." s)
    putStrLn $ "Retuning " ++ show numInsts ++ " instrument(s)..."
    forM_ [0..(numInsts - 1)] $ \i -> do
        let outfile = outdir </> show i ++ " - " ++ (T.unpack $ stripDots $ T.pack $ strip $ fst $ instList !! i) <.> "sf2"
        let sf' = sfInstrumentRetuned pairs i sf
        putStrLn $ "\t" ++ outfile
        saveSfData outfile sf'

-- | Renders text summarizing a SoundFont file.
showSoundFont :: FilePath -> IO String
showSoundFont infile = do
    sf <- loadSfData infile
    return $ P.runPrinter $ P.pretty sf

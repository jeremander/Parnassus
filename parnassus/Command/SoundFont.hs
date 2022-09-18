{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Command.SoundFont where

import Control.Monad (when)
import qualified Data.Attoparsec.Text as A
import Data.Maybe (fromJust, fromMaybe)
import qualified Data.Text as T
import Options.Applicative
import System.Directory (createDirectoryIfMissing)
import Text.Read.Lex (Lexeme(..))

import Misc.Utils (idxOrSpanToIndices, parseIdxOrSpans)
import Music.SoundFont (ModifyHeaderOpts(..), SFData(..), SFModIO, SFPdta(..), mergeSoundFonts, modifySfData, retuneSoundFont, retuneSplitSoundFont, showSoundFont, sfClean, sfFilterPresets, sfModifyHeader)
import Music.Tuning (NamedTuning)
import Command (infileParser, loadNamedTunings, RetuneOpts(..), retuneOptsParser, subcommandOpts)


attoparsecReader :: A.Parser a -> ReadM a
attoparsecReader p = eitherReader (A.parseOnly p . T.pack)

data SFModOpts = SFModOpts {
    inputFile :: FilePath,
    outputFile :: FilePath
}

sfInfile :: Parser FilePath
sfInfile = infileParser "sf2"

sfOutfile :: Parser FilePath
sfOutfile = strOption (long "output-file" <> short 'o' <> metavar "OUTFILE" <> help "(required) output .sf2 file")

sfModOpts :: Parser SFModOpts
sfModOpts = SFModOpts <$> sfInfile <*> sfOutfile

runSfModIO :: SFModOpts -> SFModIO -> IO ()
runSfModIO (SFModOpts {inputFile, outputFile}) mod = modifySfData mod inputFile outputFile

-- ** @clean@

newtype CleanOpts = CleanOpts SFModOpts

cleanOptsParser :: Parser CleanOpts
cleanOptsParser = helper <*> (CleanOpts <$> sfModOpts)

runClean :: CleanOpts -> IO ()
runClean (CleanOpts opts) = do
    runSfModIO opts (return . sfClean)
    putStrLn "Done!"

-- ** @filter-presets@

parseIndices :: A.Parser [Int]
parseIndices = concatMap idxOrSpanToIndices <$> parseIdxOrSpans

data FilterPresetsOpts = FilterPresetsOpts SFModOpts [Int]

filterPresetsOptsParser :: Parser FilterPresetsOpts
filterPresetsOptsParser = helper <*> (FilterPresetsOpts <$>
        sfModOpts
    <*> option (attoparsecReader parseIndices) (long "preset-indices" <> short 'p' <> metavar "PRESETS" <> help "preset indices to keep (given by comma-separated integers or integer ranges [MIN]-[MAX])"))

runFilterPresets :: FilterPresetsOpts -> IO ()
runFilterPresets (FilterPresetsOpts opts indices) = do
    let go sf = do
        let numPresets = length $ sfPhdrs $ sfPdta sf
        let indices' = filter (< numPresets) indices
        when (null indices') (fail $ "must provide at least one index between 0 and " ++ show (numPresets - 1))
        putStrLn $ "Keeping presets with indices: " ++ show indices'
        return $ sfFilterPresets indices' sf
    runSfModIO opts go
    putStrLn "Done!"

-- ** @merge@

data MergeOpts = MergeOpts {
    inputFiles :: [FilePath],
    outputFile :: FilePath
}

mergeOptsParser :: Parser MergeOpts
mergeOptsParser = helper <*> (MergeOpts <$>
    some (argument str (metavar "INFILES..." <> help "(required) one or more .sf2 files"))
    <*> sfOutfile)

runMerge :: MergeOpts -> IO ()
runMerge (MergeOpts {inputFiles, outputFile}) = do
    mergeSoundFonts inputFiles outputFile
    putStrLn "Done!"

-- ** @modify-header@

modifyHeaderOptsParser :: Parser ModifyHeaderOpts
modifyHeaderOptsParser = ModifyHeaderOpts <$>
        optional (strOption (long "bank-name" <> short 'b' <> metavar "BANK_NAME" <> help "name of the SoundFont bank"))
    <*> optional (strOption (long "authors" <> short 'a' <> metavar "AUTHORS" <> help "name of author(s)"))
    <*> optional (strOption (long "copyright" <> metavar "COPYRIGHT" <> help "copyright message"))
    <*> optional (strOption (long "comments" <> short 'c' <> metavar "COMMENTS" <> help "comments message"))

data ModifyHeaderOpts' = ModifyHeaderOpts' SFModOpts ModifyHeaderOpts

modifyHeaderOptsParser' :: Parser ModifyHeaderOpts'
modifyHeaderOptsParser' = helper <*> (ModifyHeaderOpts' <$> sfModOpts <*> modifyHeaderOptsParser)

runModifyHeader :: ModifyHeaderOpts' -> IO ()
runModifyHeader (ModifyHeaderOpts' sfModOpts modHdrOpts) = runSfModIO sfModOpts (sfModifyHeader modHdrOpts)

-- ** @retune@

runRetune' :: ([NamedTuning] -> FilePath -> FilePath -> IO ()) -> RetuneOpts -> IO ()
runRetune' go (RetuneOpts {inputFile, tuningFiles, outputDir}) = do
    namedTunings <- concat <$> mapM loadNamedTunings tuningFiles
    createDirectoryIfMissing False outputDir
    go namedTunings inputFile outputDir
    putStrLn "Done!"

runRetune :: RetuneOpts -> IO ()
runRetune = runRetune' retuneSoundFont

-- ** @retune-split@

runRetuneSplit :: RetuneOpts -> IO ()
runRetuneSplit = runRetune' retuneSplitSoundFont

-- ** @show@

data ShowOpts = ShowOpts FilePath Bool

showOptsParser :: Parser ShowOpts
showOptsParser = helper <*> (ShowOpts <$> sfInfile <*> switch (long "show-presets" <> short 'p' <> help "whether to show table of presets"))

runShow :: ShowOpts -> IO ()
runShow (ShowOpts inputFile showPresets) = putStrLn =<< showSoundFont showPresets inputFile

-- ** @soundfont@ Subcommands

data SFSubcommand =
      Clean CleanOpts
    | FilterPresets FilterPresetsOpts
    | Merge MergeOpts
    | ModifyHeader ModifyHeaderOpts'
    | Retune RetuneOpts
    | RetuneSplit RetuneOpts
    | Show ShowOpts

sfSubcommandParser :: Parser SFSubcommand
sfSubcommandParser = subparser $
       subcommandOpts "clean" (Clean <$> cleanOptsParser) "Remove unused samples and instruments."
    <> subcommandOpts "filter-presets" (FilterPresets <$> filterPresetsOptsParser) "Filter presets by index."
    <> subcommandOpts "merge" (Merge <$> mergeOptsParser) "Merge together multiple SoundFonts into one file."
    <> subcommandOpts "modify-header" (ModifyHeader <$> modifyHeaderOptsParser') "Modify SoundFont header."
    <> subcommandOpts "retune" (Retune <$> retuneOptsParser "sf2") "Retune a SoundFont with one or more tunings."
    <> subcommandOpts "retune-split" (RetuneSplit <$> retuneOptsParser "sf2") "Split SoundFont by instrument and assign one or more tunings to each of them.\nThis will create multiple files, each one named after an instrument."
    <> subcommandOpts "show" (Show <$> showOptsParser) "Print some summary info about a SoundFont."

runSfSubcommand :: SFSubcommand -> IO ()
runSfSubcommand (Clean opts)         = runClean opts
runSfSubcommand (FilterPresets opts) = runFilterPresets opts
runSfSubcommand (Merge opts)         = runMerge opts
runSfSubcommand (ModifyHeader opts)  = runModifyHeader opts
runSfSubcommand (Retune opts)        = runRetune opts
runSfSubcommand (RetuneSplit opts)   = runRetuneSplit opts
runSfSubcommand (Show opts)          = runShow opts

-- ** @soundfont@

newtype SoundFontOpts = SoundFontOpts SFSubcommand

soundFontOptsParser :: Parser SoundFontOpts
soundFontOptsParser = helper <*> (SoundFontOpts <$> sfSubcommandParser)

runSoundFont :: SoundFontOpts -> IO ()
runSoundFont (SoundFontOpts subcmd) = runSfSubcommand subcmd

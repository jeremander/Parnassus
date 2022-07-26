{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}

module SoundFont where

import Control.Monad (when)
import Data.Aeson (decodeFileStrict')
import qualified Data.Attoparsec.Text as A
import Data.Maybe (fromJust)
import qualified Data.Text as T
import Options.Applicative
import System.Directory (createDirectoryIfMissing)
import Text.Read.Lex (Lexeme(..))

import Misc.Utils (idxOrSpanToIndices, parseIdxOrSpans)
import Music.SoundFont (ModifyHeaderOpts(..), SFData(..), SFModIO, SFPdta(..), mergeSoundFonts, modifySfData, retuneSplitSoundFont, showSoundFont, sfClean, sfFilterPresets, sfModifyHeader)


attoparsecReader :: A.Parser a -> ReadM a
attoparsecReader p = eitherReader (A.parseOnly p . T.pack)

data SFModOpts = SFModOpts {
    inputFile :: FilePath,
    outputFile :: FilePath
}

sfInfile :: Parser FilePath
sfInfile = argument str (metavar "INFILE" <> help "(required) input .sf2 file")

sfOutfile :: Parser FilePath
sfOutfile = strOption (long "output-file" <> short 'o' <> metavar "OUTFILE" <> help "(required) output .sf2 file")

sfModOpts :: Parser SFModOpts
sfModOpts = SFModOpts <$> sfInfile <*> sfOutfile

runSfModIO :: SFModOpts -> SFModIO -> IO ()
runSfModIO (SFModOpts {inputFile, outputFile}) mod = modifySfData mod inputFile outputFile

-- ** @clean@

newtype CleanOpts = CleanOpts SFModOpts

cleanOpts :: Parser CleanOpts
cleanOpts = helper <*> (CleanOpts <$> sfModOpts)

runClean :: CleanOpts -> IO ()
runClean (CleanOpts opts) = do
    runSfModIO opts (return . sfClean)
    putStrLn "Done!"

-- ** @filter-presets@

parseIndices :: A.Parser [Int]
parseIndices = concatMap idxOrSpanToIndices <$> parseIdxOrSpans

data FilterPresetsOpts = FilterPresetsOpts SFModOpts [Int]

filterPresetsOpts :: Parser FilterPresetsOpts
filterPresetsOpts = helper <*> (FilterPresetsOpts <$>
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

mergeOpts :: Parser MergeOpts
mergeOpts = helper <*> (MergeOpts <$>
    some (argument str (metavar "INFILES..." <> help "(required) one or more .sf2 files"))
    <*> sfOutfile)

runMerge :: MergeOpts -> IO ()
runMerge (MergeOpts {inputFiles, outputFile}) = do
    mergeSoundFonts inputFiles outputFile
    putStrLn "Done!"

-- ** @modify-header@

modifyHeaderOpts :: Parser ModifyHeaderOpts
modifyHeaderOpts = ModifyHeaderOpts <$>
        optional (strOption (long "bank-name" <> short 'b' <> metavar "BANK_NAME" <> help "name of the SoundFont bank"))
    <*> optional (strOption (long "authors" <> short 'a' <> metavar "AUTHORS" <> help "name of author(s)"))
    <*> optional (strOption (long "copyright" <> metavar "COPYRIGHT" <> help "copyright message"))
    <*> optional (strOption (long "comments" <> short 'c' <> metavar "COMMENTS" <> help "comments message"))

data ModifyHeaderOpts' = ModifyHeaderOpts' SFModOpts ModifyHeaderOpts

modifyHeaderOpts' :: Parser ModifyHeaderOpts'
modifyHeaderOpts' = helper <*> (ModifyHeaderOpts' <$> sfModOpts <*> modifyHeaderOpts)

runModifyHeader :: ModifyHeaderOpts' -> IO ()
runModifyHeader (ModifyHeaderOpts' sfModOpts modHdrOpts) = runSfModIO sfModOpts (sfModifyHeader modHdrOpts)

-- ** @retune-split@

data RetuneSplitOpts = RetuneSplitOpts {
    inputFile :: FilePath,
    tuningFile :: FilePath,
    outputDir :: FilePath
}

retuneSplitOpts :: Parser RetuneSplitOpts
retuneSplitOpts = helper <*> (RetuneSplitOpts <$>
        sfInfile
    <*> argument str (metavar "TUNINGS" <> help "(required) JSON file of tunings containing a list of entries with \"name\" and \"tuning\" fields (the latter is a list of 128 frequencies mapping the MIDI keyboard)")
    <*> strOption (long "output-dir" <> short 'o' <> metavar "OUTDIR" <> value "." <> showDefault <> help "output directory"))

runRetuneSplit :: RetuneSplitOpts -> IO ()
runRetuneSplit (RetuneSplitOpts {inputFile, tuningFile, outputDir}) = do
    namedTunings <- fromJust <$> decodeFileStrict' tuningFile
    createDirectoryIfMissing False outputDir
    retuneSplitSoundFont namedTunings inputFile outputDir
    putStrLn "Done!"

-- ** @show@

data ShowOpts = ShowOpts FilePath Bool

showOpts :: Parser ShowOpts
showOpts = helper <*> (ShowOpts <$> sfInfile <*> switch (long "show-presets" <> short 'p' <> help "whether to show table of presets"))

runShow :: ShowOpts -> IO ()
runShow (ShowOpts inputFile showPresets) = putStrLn =<< showSoundFont showPresets inputFile

-- ** @soundfont@ Subcommands

data SFSubcommand =
      Clean CleanOpts
    | FilterPresets FilterPresetsOpts
    | Merge MergeOpts
    | ModifyHeader ModifyHeaderOpts'
    | RetuneSplit RetuneSplitOpts
    | Show ShowOpts

sfSubcommandOpts :: String -> Parser a -> String -> Mod CommandFields a
sfSubcommandOpts name subcmdParser desc = command name $ info subcmdParser $ progDesc desc

sfSubcommandParser :: Parser SFSubcommand
sfSubcommandParser = subparser $
       sfSubcommandOpts "clean" (Clean <$> cleanOpts) "Remove unused samples and instruments."
    <> sfSubcommandOpts "filter-presets" (FilterPresets <$> filterPresetsOpts) "Filter presets by index."
    <> sfSubcommandOpts "merge" (Merge <$> mergeOpts) "Merge together multiple SoundFonts into one file."
    <> sfSubcommandOpts "modify-header" (ModifyHeader <$> modifyHeaderOpts') "Modify SoundFont header."
    <> sfSubcommandOpts "retune-split" (RetuneSplit <$> retuneSplitOpts) "Split SoundFont by instrument and assign one or more tunings to each of them.\nThis will create multiple files, each one named after an instrument."
    <> sfSubcommandOpts "show" (Show <$> showOpts) "Print some summary info about a SoundFont."

runSfSubcommand :: SFSubcommand -> IO ()
runSfSubcommand (Clean opts)         = runClean opts
runSfSubcommand (FilterPresets opts) = runFilterPresets opts
runSfSubcommand (Merge opts)         = runMerge opts
runSfSubcommand (ModifyHeader opts)  = runModifyHeader opts
runSfSubcommand (RetuneSplit opts)   = runRetuneSplit opts
runSfSubcommand (Show opts)          = runShow opts

-- ** @soundfont@

newtype SoundFontOpts = SoundFontOpts SFSubcommand

soundFontOpts :: Parser SoundFontOpts
soundFontOpts = helper <*> (SoundFontOpts <$> sfSubcommandParser)

runSoundFont :: SoundFontOpts -> IO ()
runSoundFont (SoundFontOpts subcmd) = runSfSubcommand subcmd

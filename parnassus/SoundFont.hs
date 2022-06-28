{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}

module SoundFont where

import Data.Aeson (decodeFileStrict')
import Data.Maybe (fromJust)
import Options.Applicative
import System.Directory (createDirectoryIfMissing)

import Music.SoundFont (SFModIO, mergeSoundFonts, modifySfData, retuneSoundFont, showSoundFont, sfClean)


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
    retuneSoundFont namedTunings inputFile outputDir
    putStrLn "Done!"

-- ** @show@

newtype ShowOpts = ShowOpts FilePath

showOpts :: Parser ShowOpts
showOpts = helper <*> (ShowOpts <$> sfInfile)

runShow :: ShowOpts -> IO ()
runShow (ShowOpts inputFile) = putStrLn =<< showSoundFont inputFile

-- ** @soundfont@ Subcommands

data SFSubcommand =
      Clean CleanOpts
    | Merge MergeOpts
    | RetuneSplit RetuneSplitOpts
    | Show ShowOpts

-- sfSubcommand :: String -> Parser
sfSubcommandOpts name subcmdParser desc = command name $ info subcmdParser $ progDesc desc

sfSubcommandParser :: Parser SFSubcommand
sfSubcommandParser = subparser $
       sfSubcommandOpts "clean" (Clean <$> cleanOpts) "Remove unused samples and instruments."
    <> sfSubcommandOpts "merge" (Merge <$> mergeOpts) "Merge together multiple SoundFonts into one file."
    <> sfSubcommandOpts "retune-split" (RetuneSplit <$> retuneSplitOpts) "Split SoundFont by instrument and assign multiple tunings to each of them.\nThis will create multiple files, each one named after an instrument."
    <> sfSubcommandOpts "show" (Show <$> showOpts) "Print some summary info about a SoundFont."

runSfSubcommand :: SFSubcommand -> IO ()
runSfSubcommand (Clean opts)       = runClean opts
runSfSubcommand (Merge opts)       = runMerge opts
runSfSubcommand (RetuneSplit opts) = runRetuneSplit opts
runSfSubcommand (Show opts)        = runShow opts

-- ** @soundfont@

newtype SoundFontOpts = SoundFontOpts SFSubcommand

soundFontOpts :: Parser SoundFontOpts
soundFontOpts = helper <*> (SoundFontOpts <$> sfSubcommandParser)

runSoundFont :: SoundFontOpts -> IO ()
runSoundFont (SoundFontOpts subcmd) = runSfSubcommand subcmd

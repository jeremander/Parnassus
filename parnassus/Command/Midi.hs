module Command.Midi where

import Control.Monad (forM_)
import Euterpea (Mode(..), Music1, PitchClass(..))
import Options.Applicative
import System.Directory (createDirectoryIfMissing)
import System.FilePath.Posix ((-<.>), (</>), takeFileName)

import Command (loadNamedTunings, RetuneOpts(..), retuneOptsParser, subcommandOpts)
import Music.Midi (keySigToMidiKeySig, MidiMetadata(..), MidiKeySig, MidiTimeSig, timeSigToMidiTimeSig)
import Music.Tuning (approxTuneMusic, NamedTuning(..), tuneMusic, Tuning)
import Music.Types (fromMidiFile, toMidiFile)


-- ** Conversions

type MusicTrans = Music1 -> Music1

-- | Given a function that transforms 'Music1', an input midi path, and an output midi path, loads the midi, transforms it, and saves it to a new file.
transformMidiFile :: MusicTrans -> FilePath -> FilePath -> IO ()
transformMidiFile go infile outfile = do
    putStrLn $ "Loading " ++ infile
    mus <- fromMidiFile infile
    let mus' = go mus
    putStrLn $ "Saving " ++ outfile
    toMidiFile mus' outfile

-- ** @retune-map@

runRetune' :: (Tuning -> Music1 -> Music1) -> RetuneOpts -> IO ()
runRetune' go (RetuneOpts infile tuningFiles outdir) = do
    namedTunings <- concat <$> mapM loadNamedTunings tuningFiles
    createDirectoryIfMissing False outdir
    putStrLn $ "Loading " ++ infile
    mus <- fromMidiFile infile
    let numTunings = length namedTunings
    putStrLn $ "Retuning MIDI with " ++ show numTunings ++ " tuning(s)..."
    forM_ namedTunings $ \(NamedTuning name tuning) -> do
        let outfile = outdir </> takeFileName infile -<.> (name ++ ".mid")
        let mus' = go tuning mus
        putStrLn $ "\t" ++ outfile
        toMidiFile mus' outfile

runRetuneMap :: RetuneOpts -> IO ()
runRetuneMap = runRetune' tuneMusic

-- ** @retune-approx@

runRetuneApprox :: RetuneOpts -> IO ()
runRetuneApprox = runRetune' approxTuneMusic

-- ** @tag@

data TagOpts = TagOpts {
    inputFile :: FilePath,
    outputFile :: FilePath,
    midiMetadata :: MidiMetadata
}

-- data MidiMetadata = MidiMetadata {
--     title :: Maybe String,
--     copyright :: Maybe String,
--     lyrics :: Maybe String,
--     text :: [String],
--     midiTimeSig :: Maybe MidiTimeSig,
--     midiKeySig :: Maybe MidiKeySig
-- } deriving (Eq, Ord, Show)

midiTimeSigParser :: Parser MidiTimeSig
midiTimeSigParser = timeSigToMidiTimeSig <$> option auto (long "timesig" <> metavar "TIMESIG" <> help "time signature, e.g. 3/4")

midiKeySigParser :: Parser MidiKeySig
midiKeySigParser = keySigToMidiKeySig <$> option auto (long "keysig" <> metavar "KEYSIG" <> help "key signature, e.g. G, Am, Cs, Bfm")

midiMetadataParser :: Parser MidiMetadata
midiMetadataParser = MidiMetadata <$>
        optional (strOption (long "title" <> short 't' <> metavar "TITLE" <> help "song title"))
    <*> optional (strOption (long "copyright" <> short 'c' <> metavar "COPYRIGHT" <> help "copyright message"))
    <*> optional (strOption (long "lyrics" <> short 'l' <> metavar "LYRICS" <> help "song lyrics"))
    <*> (many $ strOption (long "text" <> metavar "TEXT" <> help "text/comments"))
    <*> optional midiTimeSigParser
    <*> optional midiKeySigParser

tagOptsParser :: Parser TagOpts
tagOptsParser = helper <*> (TagOpts <$>
        argument str (metavar "INFILE" <> help "(required) input MIDI file")
    <*> argument str (metavar "OUTFILE" <> help "(required) output MIDI file")
    <*> midiMetadataParser)

runTag :: TagOpts -> IO ()
runTag opts = print $ midiMetadata opts

-- ** @midi@ Subcommands

data MidiSubcommand =
      RetuneApprox RetuneOpts
    | RetuneMap RetuneOpts
    | Tag TagOpts

midiSubcommandParser :: Parser MidiSubcommand
midiSubcommandParser = subparser $
           subcommandOpts "retune-approx" (RetuneApprox <$> retuneOptsParser "mid") "Retune a MIDI file with one or more tunings (altering note pitches to match closest pitch in the new tuning)."
        <> subcommandOpts "retune-map" (RetuneMap <$> retuneOptsParser "mid") "Retune a MIDI file with one or more tunings (mapping MIDI notes 0-127 directly)."
        <> subcommandOpts "tag" (Tag <$> tagOptsParser) "Update or add metadata to a MIDI file."

runMidiSubcommand :: MidiSubcommand -> IO ()
runMidiSubcommand (RetuneApprox opts) = runRetuneApprox opts
runMidiSubcommand (RetuneMap opts)    = runRetuneMap opts
runMidiSubcommand (Tag opts)          = runTag opts

-- ** @midi@

newtype MidiOpts = MidiOpts MidiSubcommand

midiOptsParser :: Parser MidiOpts
midiOptsParser = helper <*> (MidiOpts <$> midiSubcommandParser)

runMidi :: MidiOpts -> IO ()
runMidi (MidiOpts subcmd) = runMidiSubcommand subcmd

module Command.Midi where

import Control.Monad (forM_)
import Euterpea (Music1)
import Options.Applicative
import System.Directory (createDirectoryIfMissing)
import System.FilePath.Posix ((-<.>), (</>), takeFileName)

import Command (loadNamedTunings, RetuneOpts(..), retuneOptsParser, subcommandOpts)
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

-- ** @midi@ Subcommands

data MidiSubcommand =
      RetuneApprox RetuneOpts
    | RetuneMap RetuneOpts

midiSubcommandParser :: Parser MidiSubcommand
midiSubcommandParser = subparser $
           subcommandOpts "retune-approx" (RetuneApprox <$> retuneOptsParser "mid") "Retune a MIDI file with one or more tunings (altering note pitches to match closest pitch in the new tuning)."
        <> subcommandOpts "retune-map" (RetuneMap <$> retuneOptsParser "mid") "Retune a MIDI file with one or more tunings (mapping MIDI notes 0-127 directly)."


runMidiSubcommand :: MidiSubcommand -> IO ()
runMidiSubcommand (RetuneApprox opts) = runRetuneApprox opts
runMidiSubcommand (RetuneMap opts)       = runRetuneMap opts

-- ** @midi@

newtype MidiOpts = MidiOpts MidiSubcommand

midiOptsParser :: Parser MidiOpts
midiOptsParser = helper <*> (MidiOpts <$> midiSubcommandParser)

runMidi :: MidiOpts -> IO ()
runMidi (MidiOpts subcmd) = runMidiSubcommand subcmd

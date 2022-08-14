{-# LANGUAGE NamedFieldPuns #-}

module Synth where

import Data.Default (Default(..))
import Data.Maybe (catMaybes, fromJust)
import Options.Applicative
import System.Exit (ExitCode(..))
import System.FilePath.Posix (takeBaseName)
import System.IO.Temp (withTempFile)
import System.Process (callProcess, readProcessWithExitCode)


type Args = [String]

-- | Checks whether some executable is present on the user's path.
checkProgram :: FilePath -> IO Bool
checkProgram prog = do
    (code, _, _) <- readProcessWithExitCode "which" [prog] ""
    return $ code == ExitSuccess

-- | Runs a program and arguments, first checking if the program exists on the user's path.
--   Prints out the command-line arguments before executing it.
runProgram :: FilePath -> Args -> IO ()
runProgram prog args = do
    hasProg <- checkProgram prog
    putStrLn cmd
    if hasProg
        then callProcess prog args
        else fail $ "could not find " ++ prog ++ " executable on PATH"
    return ()
    where cmd = prog ++ " " ++ unwords args

-- ** FluidSynth

runFluidsynthCmd :: Args -> IO ()
runFluidsynthCmd = runProgram "fluidsynth"

data SynthOpts = SynthOpts {
    midiPath :: Maybe FilePath,  -- MIDI file to synthesize
    soundFontPath :: Maybe FilePath  -- SoundFont file
} deriving Show

synthOptsParser :: Bool -> Parser SynthOpts
synthOptsParser midiRequired = helper <*> (SynthOpts <$> parseMidiPath <*> parseSoundFontPath)
    where
        reqMod = if midiRequired then fmap Just else optional
        parseMidiPath = reqMod $ strArgument (metavar "MIDI_FILE" <> help "input MIDI file to play")
        parseSoundFontPath = optional $ strOption (long "sound-font" <> short 's' <> metavar "SOUND_FONT" <> help "SoundFont file")

-- | Converts 'SynthOpts' to @fluidsynth@ command-line arguments.
synthOptsArgs :: SynthOpts -> Args
synthOptsArgs (SynthOpts {midiPath, soundFontPath}) = concat $ catMaybes [soundFontArg, midiArg]
    where
        soundFontArg = pure <$> soundFontPath
        midiArg = pure <$> midiPath

-- ** Playback

data PlayOpts = PlayOpts {
    synthOpts :: SynthOpts,
    portName :: Maybe String,  -- name of MIDI input port
    noShell :: Bool  -- suppress the fluidsynth shell
} deriving Show

playOptsParser :: Parser PlayOpts
playOptsParser = helper <*> (PlayOpts <$> synthOptsParser False <*> parsePortName <*> parseNoShell)
    where
        parsePortName = optional $ strOption (long "port-name" <> short 'p' <> metavar "PORT_NAME" <> help "set the name of the MIDI input port")
        parseNoShell = switch (long "no-shell" <> help "turn off shell mode")

-- | Converts 'PlayOpts' to @fluidsynth@ command-line arguments.
playOptsArgs :: PlayOpts -> Args
playOptsArgs (PlayOpts {synthOpts, portName, noShell}) = concat (catMaybes [portArg, noShellArg]) ++ synthOptsArgs synthOpts
    where
        portArg = ("-p" :) . pure <$> portName
        noShellArg = if noShell then Just ["--no-shell"] else Nothing

runPlay :: PlayOpts -> IO ()
runPlay = runFluidsynthCmd . playOptsArgs

-- ** MIDI to WAV

data MidiWavOpts = MidiWavOpts SynthOpts FilePath

-- | Converts 'WavOpts' to @fluidsynth@ command-line arguments.
midiWavOptsArgs :: MidiWavOpts -> Args
midiWavOptsArgs (MidiWavOpts synthOpts wavPath) = synthOptsArgs synthOpts ++ ["-F", wavPath]

runMidiToWav :: MidiWavOpts -> IO ()
runMidiToWav = runFluidsynthCmd . midiWavOptsArgs

-- ** WAV to MP3

data Mp3Params = Mp3Params {
    sampleFreq :: Int,  -- sample frequency (samples per second)
    audioChannels :: Int,  -- number of audio channels (1 = mono, 2 = stereo)
    bitrate :: Int,  -- bit rate (kilobits per second)
    overwrite :: Bool  -- overwrite output file if it exists
}

instance Default Mp3Params where
    def = Mp3Params {sampleFreq = 44100, audioChannels = 2, bitrate = 192, overwrite = True}

mp3ParamsArgs :: Mp3Params -> Args
mp3ParamsArgs (Mp3Params {sampleFreq, audioChannels, bitrate, overwrite}) = ["-vn", "-ar"] ++ [show sampleFreq] ++ ["-ac"] ++ [show audioChannels] ++ ["-b:a"] ++ [show bitrate ++ "k"] ++ ["-y" | overwrite]

data WavMp3Opts = WavMp3Opts {
    wavPath :: FilePath,
    mp3Path :: FilePath,
    mp3Params :: Mp3Params
}

wavMp3OptsArgs :: WavMp3Opts -> Args
wavMp3OptsArgs (WavMp3Opts {wavPath, mp3Path, mp3Params}) = ["-i", wavPath] ++ mp3ParamsArgs mp3Params ++ [mp3Path]

runWavToMp3 :: WavMp3Opts -> IO ()
runWavToMp3 = runProgram "ffmpeg" . wavMp3OptsArgs

-- ** MIDI to MP3

data MidiMp3Opts = MidiMp3Opts SynthOpts Mp3Params FilePath

runMidiToMp3 :: MidiMp3Opts -> IO ()
runMidiToMp3 (MidiMp3Opts synthOpts mp3Params mp3Path) = withTempFile "." template go
    where
        baseName = takeBaseName $ fromJust $ midiPath synthOpts
        template = baseName ++ ".wav"
        go wavPath _ = do
            let midiWavOpts = MidiWavOpts synthOpts wavPath
            runMidiToWav midiWavOpts
            let wavMp3Opts = WavMp3Opts wavPath mp3Path mp3Params
            runWavToMp3 wavMp3Opts

{-# LANGUAGE NamedFieldPuns #-}

module Synth where

import Data.Maybe (catMaybes)
import Options.Applicative
import System.Exit (ExitCode(..))
import System.Process (callProcess, readProcessWithExitCode)


type Args = [String]

-- | Checks whether the @fluidsynth@ executable is present on the user's path.
checkFluidsynth :: IO Bool
checkFluidsynth = do
    (code, _, _) <- readProcessWithExitCode "which" ["fluidsynth"] ""
    return $ code == ExitSuccess

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

synthOptsArgs :: SynthOpts -> Args
synthOptsArgs (SynthOpts {midiPath, soundFontPath}) = concat $ catMaybes [soundFontArg, midiArg]
    where
        soundFontArg = pure <$> soundFontPath
        midiArg = pure <$> midiPath

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
runPlay playOpts = do
    hasFluidsynth <- checkFluidsynth
    putStrLn cmd
    if hasFluidsynth
        then callProcess "fluidsynth" args
        else fail "could not fluidsynth executable on PATH"
    return ()
    where
        args = playOptsArgs playOpts
        cmd = "fluidsynth " ++ unwords args
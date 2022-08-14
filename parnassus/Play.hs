{-# LANGUAGE NamedFieldPuns #-}

module Play where

import Control.Exception (IOException, try)
import Data.Maybe (catMaybes)
import Options.Applicative
import System.Exit (ExitCode(..))
import System.Process (callProcess, readCreateProcessWithExitCode, readProcessWithExitCode)


data PlayOpts = PlayOpts {
    playPath :: Maybe FilePath,  -- MIDI file to play
    soundFontPath :: Maybe FilePath,  -- SoundFont file
    portName :: Maybe String,  -- name of MIDI input port
    noShell :: Bool  -- suppress the fluidsynth shell
} deriving Show

-- | Checks whether the @fluidsynth@ executable is present on the user's path.
checkFluidsynth :: IO Bool
checkFluidsynth = do
    (code, _, _) <- readProcessWithExitCode "which" ["fluidsynth"] ""
    return $ code == ExitSuccess

runPlay :: PlayOpts -> IO ()
runPlay opts@(PlayOpts {playPath, soundFontPath, portName, noShell}) = do
    hasFluidsynth <- checkFluidsynth
    putStrLn cmd
    if hasFluidsynth
        then callProcess "fluidsynth" args
        else fail "could not fluidsynth executable on PATH"
    return ()
    where
        portArg = ("-p" :) . pure <$> portName
        noShellArg = if noShell then Just ["--no-shell"] else Nothing
        soundFontArg = pure <$> soundFontPath
        playArg = pure <$> playPath
        args = concat $ catMaybes [portArg, noShellArg, soundFontArg, playArg]
        cmd = "fluidsynth " ++ unwords args

playOpts :: Parser PlayOpts
playOpts = helper <*> (PlayOpts <$> parsePlayPath <*> parseSoundFontPath <*> parsePortName <*> parseNoShell)
    where
        parsePlayPath = optional $ strArgument (metavar "MIDI_FILE" <> help "input MIDI file to play")
        parseSoundFontPath = optional $ strOption (long "sound-font" <> short 's' <> metavar "SOUND_FONT" <> help "SoundFont file")
        parsePortName = optional $ strOption (long "port-name" <> short 'p' <> metavar "PORT_NAME" <> help "set the name of the MIDI input port")
        parseNoShell = switch (long "no-shell" <> help "turn off shell mode")

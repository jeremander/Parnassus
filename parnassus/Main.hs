module Main where

import Control.Monad (join)
import Data.List (intercalate)
import Data.Version (Version(versionBranch))
import Options.Applicative
import Data.Semigroup ((<>))

import Command.Convert (ConvertOpts, convertOptsParser, runConvert)
import Command.SoundFont (SoundFontOpts, soundFontOptsParser, runSoundFont)
import Paths_Parnassus (version)
import Synth (PlayOpts, playOptsParser, runPlay)


data Subcommand =
      Convert ConvertOpts
    | Play PlayOpts
    | SoundFont SoundFontOpts
    | Version

subcommandOpts :: Parser Subcommand
subcommandOpts = subparser $
       command "convert" (info (Convert <$> convertOptsParser) (progDesc "Convert between file formats."))
    <> command "play" (info (Play <$> playOptsParser) (progDesc "Play MIDI from an input source or file."))
    <> command "soundfont" (info (SoundFont <$> soundFontOptsParser) (progDesc "Manipulate SoundFont files."))
    <> command "version" (info (pure Version) (progDesc "Show the version number and exit."))
    -- <> ...

versionString :: String
versionString = intercalate "." $ show <$> versionBranch version

runSubcommand :: Subcommand -> IO ()
runSubcommand (Convert opts)   = runConvert opts
runSubcommand (Play opts)      = runPlay opts
runSubcommand (SoundFont opts) = runSoundFont opts
runSubcommand Version          = putStrLn $ "parnassus version " ++ versionString

newtype Options = Options { subcommand :: Subcommand }

options = Options <$> subcommandOpts
optionInfo = info (helper <*> options) $
       header "parnassus"
    <> fullDesc
    <> progDesc "A command-line program for performing various operations on musical files such as MIDI, SoundFont, and LilyPond."

main = execParser optionInfo >>= runSubcommand . subcommand

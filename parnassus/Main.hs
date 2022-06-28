module Main where

import Control.Monad (join)
import Options.Applicative
import Data.Semigroup ((<>))

import Convert (ConvertOpts, convertOpts, runConvert)
import SoundFont (SoundFontOpts, soundFontOpts, runSoundFont)


data Subcommand =
      Convert ConvertOpts
    | SoundFont SoundFontOpts

subcommandOpts :: Parser Subcommand
subcommandOpts = subparser $
       command "convert" (info (Convert <$> convertOpts) (progDesc "Convert between file formats."))
    <> command "soundfont" (info (SoundFont <$> soundFontOpts) (progDesc "Manipulate SoundFont files."))
    -- <> ...

runSubcommand :: Subcommand -> IO ()
runSubcommand (Convert opts) = runConvert opts
runSubcommand (SoundFont opts) = runSoundFont opts

newtype Options = Options { subcommand :: Subcommand }

options = Options <$> subcommandOpts
optionInfo = info (helper <*> options) $
       header "parnassus"
    <> fullDesc
    <> progDesc "A command-line program for performing various operations on musical files such as MIDI, SoundFont, and LilyPond."

main = execParser optionInfo >>= runSubcommand . subcommand

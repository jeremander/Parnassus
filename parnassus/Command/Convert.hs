{-# LANGUAGE NamedFieldPuns #-}

module Command.Convert where

import Control.Monad (unless)
import Data.Char (toLower)
import Data.Default (Default(..))
import Options.Applicative
import System.Directory (doesFileExist)
import System.IO.Error (ioError, userError)
import System.FilePath.Posix (takeExtension)

import Music.Convert
import Synth (MidiMp3Opts(..), MidiWavOpts(..), runMidiToMp3, runMidiToWav, runWavToMp3, SynthOpts(..), WavMp3Opts(..))


data ConvertOpts = ConvertOpts {
    inputFile :: FilePath,
    outputFile :: FilePath,
    soundFont :: Maybe FilePath
}

convertOptsParser :: Parser ConvertOpts
convertOptsParser = helper <*> (ConvertOpts <$>
        argument str (metavar "INFILE" <> help "(required) input file")
    <*> argument str (metavar "OUTFILE" <> help "(required) output file")
    <*> optional (strOption (long "sound-font" <> short 's' <> metavar "SOUNDFONT" <> help "SoundFont (.sf2) file")))

type Converter = ConvertOpts -> IO ()

mkConv :: (FilePath -> FilePath -> IO ()) -> Converter
mkConv func opts = func (inputFile opts) (outputFile opts)

runConv :: ConvertOpts -> Converter -> IO ()
runConv opts conv = do
    putStrLn $ "Converting " ++ inputFile opts ++ " to " ++ outputFile opts ++ "..."
    conv opts
    putStrLn "Done!"

convMidiToWav :: Converter
convMidiToWav (ConvertOpts {inputFile, outputFile, soundFont}) = runMidiToWav midiWavOpts
    where
        synthOpts = SynthOpts (Just inputFile) soundFont
        midiWavOpts = MidiWavOpts synthOpts outputFile

convWavToMp3 :: Converter
convWavToMp3 (ConvertOpts {inputFile, outputFile}) = runWavToMp3 wavMp3Opts
    where
        wavMp3Opts = WavMp3Opts {wavPath = inputFile, mp3Path = outputFile, mp3Params = def}

convMidiToMp3 :: Converter
convMidiToMp3 (ConvertOpts {inputFile, outputFile, soundFont}) = runMidiToMp3 midiMp3Opts
    where
        synthOpts = SynthOpts (Just inputFile) soundFont
        mp3Params = def
        midiMp3Opts = MidiMp3Opts synthOpts mp3Params outputFile

runConvert :: ConvertOpts -> IO ()
runConvert opts@(ConvertOpts {inputFile, outputFile}) = do
    let inputExt = takeExtension inputFile
    let outputExt = takeExtension outputFile
    let inputExt' = toLower <$> inputExt
    let outputExt' = toLower <$> outputExt
    let invalidInputExt = fail $ "invalid input extension " ++ inputExt
    let invalidOutputExt = fail $ "invalid output extension " ++ outputExt
    let runConv' = runConv opts
    inputExists <- doesFileExist inputFile
    unless inputExists $ fail $ "file " ++ show inputFile ++ " does not exist"
    case inputExt' of
        ".ly" -> case outputExt' of
            ".mid" -> runConv' $ mkConv lilypondToMidi
            _      -> invalidOutputExt
        ".mid" -> case outputExt' of
            ".ly"  -> runConv' $ mkConv midiToLilypond
            ".mp3" -> runConv' convMidiToMp3
            ".wav" -> runConv' convMidiToWav
            _      -> invalidOutputExt
        ".wav" -> case outputExt' of
            ".mp3" -> runConv' convWavToMp3
            _      -> invalidOutputExt
        _     -> invalidInputExt
    return ()
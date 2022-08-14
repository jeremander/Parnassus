{-# LANGUAGE NamedFieldPuns #-}

module Command.Convert where

import Data.Char (toLower)
import Options.Applicative
import System.IO.Error (ioError, userError)
import System.FilePath.Posix (takeExtension)

import Music.Convert


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

runConvert :: ConvertOpts -> IO ()
runConvert (ConvertOpts {inputFile, outputFile, soundFont}) = do
    let inputExt = toLower <$> takeExtension inputFile
    let outputExt = toLower <$> takeExtension outputFile
    let runConv conv = do
        putStrLn $ "Converting " ++ inputFile ++ " to " ++ outputFile ++ "..."
        fail "not yet implemented"
        -- conv inputFile outputFile
        putStrLn "Done!"
    case inputExt of
        ".ly" -> case outputExt of
            ".mid" -> runConv lilypondToMidi
            _      -> fail $ "invalid output extension " ++ takeExtension outputFile
        ".mid" -> case outputExt of
            ".ly" -> runConv midiToLilypond
            _     -> fail $ "invalid output extension " ++ takeExtension outputFile
        _     -> fail $ "invalid input extension " ++ takeExtension inputFile
    return ()
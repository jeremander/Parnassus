{-# LANGUAGE OverloadedStrings #-}

module Music.Lilypond.IO (
    lilypondProg,
    lilypondVersion,
    loadLilypond,
    saveLilypond,
    writeLilypond
)
where

import Data.Char (toLower, toUpper)
import Data.List (isSuffixOf)
import System.FilePath.Posix ((<.>), (-<.>), takeDirectory, takeExtension)
import System.Process (callProcess, readProcess)
import Text.Parsec (runParserT)
import Text.Pretty (pretty)

import Misc.Utils (safeTail)
import Music.Lilypond.Parse (defaultLilypondState, includePaths, parseLilypond)
import Music.Lilypond.Score (Lilypond', spliceIncludes, ToLilypond(..))
import Music.Pitch (PrettyPitch(..))


-- * Reading

-- | Parses a Lilypond file into a 'Lilypond' object.
loadLilypond :: FilePath -> IO Lilypond'
loadLilypond infile = do
    s <- readFile infile
    let st = defaultLilypondState {includePaths = takeDirectory infile : includePaths defaultLilypondState}
    res <- runParserT parseLilypond st infile s
    case res of
        Left _   -> ioError $ userError $ "failed to parse " ++ show infile
        Right lp -> do
            -- putStrLn "Splicing include files..."
            let lp' = spliceIncludes lp
            return lp'

-- * Writing

lilypondProg = "lilypond"

-- | Gets the currently installed version of Lilypond.
lilypondVersion :: IO String
lilypondVersion = do
    versionOutput <- readProcess lilypondProg ["--version"] ""
    return $ last $ words $ head $ lines versionOutput

-- | Writes a Lilypond expression to a .ly file.
saveLilypond :: (PrettyPitch a, ToLilypond m a) => m a -> FilePath -> IO ()
saveLilypond mus path = do
    let path' = if (".ly" `isSuffixOf` (toLower <$> path)) then path else path <.> "ly"
    -- TODO: prepend version only if no version element included
    -- version <- lilypondVersion
    -- let content = "\\version " <+> doubleQuotes (string version) <//> pretty (toLilypond mus)
    let content = pretty $ toLilypond mus
    writeFile path' $ show content

-- | Output file format
data FileFormat = LY | PDF | PNG | PS
    deriving (Enum, Eq, Ord, Read, Show)

-- | Engraves an existing Lilypond file as an image file (e.g. PDF).
engraveLilypond :: FileFormat -> FilePath -> IO ()
engraveLilypond fmt infile = case fmt of
    LY -> return ()
    _  -> callProcess lilypondProg ["--" ++ (toLower <$> show fmt), infile]

-- | Saves a Lilypond expression to an LY, PDF, PNG, or PS file.
--   Always creates an LY file as an intermediate step.
--   Uses the file extension to determine what type of file to create.
writeLilypond :: (PrettyPitch a, ToLilypond m a) => m a -> FilePath -> IO ()
writeLilypond mus path = do
    let ext = safeTail $ toUpper <$> takeExtension path
    let fmt = if (ext `elem` (show <$> enumFrom (toEnum 0 :: FileFormat)))
                then read ext
                else error "invalid extension for LilyPond output"
    let lpPath = path -<.> "ly"
    saveLilypond mus lpPath
    engraveLilypond fmt lpPath
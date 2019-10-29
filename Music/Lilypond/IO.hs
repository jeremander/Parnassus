{-# LANGUAGE OverloadedStrings #-}

module Music.Lilypond.IO (
    lilypondProg,
    lilypondVersion,
    writeLilypond
)
where

import Data.Char (toLower, toUpper)
import Data.List (isSuffixOf)
import System.FilePath.Posix ((<.>), (-<.>), takeExtension)
import System.Process (callProcess, readProcess)
import Text.Pretty ((<+>), (<//>), doubleQuotes, pretty, string)

import Music.Lilypond.Score (ToLilypond(..))


safeTail :: [a] -> [a]
safeTail []     = []
safeTail (x:xs) = xs

lilypondProg = "lilypond"

-- | Gets the currently installed version of Lilypond.
lilypondVersion :: IO String
lilypondVersion = do
    versionOutput <- readProcess lilypondProg ["--version"] ""
    return $ last $ words $ head $ lines versionOutput

-- | Writes a Lilypond expression to a .ly file.
writeLy :: (ToLilypond a) => a -> FilePath -> IO ()
writeLy mus path = do
    let path' = if (".ly" `isSuffixOf` (toLower <$> path)) then path else path <.> "ly"
    version <- lilypondVersion
    let content = "\\version " <+> doubleQuotes (string version) <//> pretty (toLilypond mus)
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
writeLilypond :: (ToLilypond a) => a -> FilePath -> IO ()
writeLilypond mus path = do
    let ext = safeTail $ toUpper <$> takeExtension path
    let fmt = if (ext `elem` (show <$> enumFrom (toEnum 0 :: FileFormat)))
                then read ext
                else error "invalid extension for LilyPond output"
    let lpPath = path -<.> "ly"
    writeLy mus lpPath
    engraveLilypond fmt lpPath
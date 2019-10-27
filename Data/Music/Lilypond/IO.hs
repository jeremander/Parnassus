{-# LANGUAGE
    OverloadedStrings
    #-}

module Data.Music.Lilypond.IO (
    lilypondProg,
    lilypondVersion,
    writeLilypond
)
where

import Control.Monad (when)
import Data.Char (toLower)
import Data.List (isSuffixOf)
import System.FilePath.Posix ((<.>), (-<.>), takeExtension)
import System.Process (callProcess, readProcess)
import Text.Pretty ((<+>), (<//>), doubleQuotes, pretty, string)

import Data.Music.Lilypond.Score (ToLilypond(..))

-- | Output file format
data FileFormat = LY | PDF | PNG | PS
    deriving (Eq, Show)

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

-- | Saves a Lilypond expression to an LY, PDF, PNG, or PS file.
--   Always creates an LY file as an intermediate step.
--   Uses the file extension to determine what type of file to create.
writeLilypond :: (ToLilypond a) => a -> FilePath -> IO ()
writeLilypond mus path = do
    let ext = safeTail $ toLower <$> takeExtension path
    let fileType = if (ext `elem` ["ly", "pdf", "png", "ps"])
                        then ext
                        else error "invalid extension for LilyPond output"
    let lpPath = path -<.> "ly"
    writeLy mus lpPath
    when (fileType /= "ly") $ do
        let args = ["--" ++ fileType, path]
        callProcess lilypondProg args
        return ()

-- data EngraveOptions
--     = EngraveOptions {
--         format   :: Format,
--         include  :: FilePath,
--         initFile :: FilePath,
--         logFile  :: FilePath,
--         logLevel :: Int
--     }

-- writeAndEngraveMusic :: FilePath -> EngraveOptions -> Music -> IO ()
-- writeAndEngraveMusic = error "writeAndEngraveMusic: Not implemented"
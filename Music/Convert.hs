module Music.Convert where

import Euterpea (Music1, Note1)

import Music.Lilypond (MusicL, fromLilypond, loadLilypond, saveLilypond)
import Music.Types.MusicT (MusicT(..), ToMidi(..))


lilypondToMidi :: FilePath -> FilePath -> IO ()
lilypondToMidi inputFile outputFile = do
    lp <- loadLilypond inputFile
    let mus = toMusic1 $ fromLilypond lp
    toMidiFile mus outputFile

midiToLilypond :: FilePath -> FilePath -> IO ()
midiToLilypond inputFile outputFile = do
    mus <- fromMidiFile inputFile :: IO Music1
    saveLilypond (fromMusic mus :: MusicL Note1) outputFile
import Control.Monad (join, liftM, mapM_, sequence)
import Data.Ratio ((%))

import Euterpea (Control (..))
import Parnassus.MusicBase (fromMidiFile, MusicT (..), Quantizable (..), TimeSig, toMidiFile)
import Parnassus.Music (MusicU1, withMetronome)

inputPath = "tunes/test/input/"
outputPath = "tunes/test/output/"

-- songs
twinkle = fromMidiFile $ inputPath ++ "twinkle.mid" :: IO MusicU1
birthday = fromMidiFile $ inputPath ++ "birthday8.mid" :: IO MusicU1
bells = fromMidiFile $ inputPath ++ "bells.mid" :: IO MusicU1
zelda = fromMidiFile $ inputPath ++ "zelda16.mid" :: IO MusicU1
bach_invention4 = fromMidiFile $ inputPath ++ "invention.mid" :: IO MusicU1
pachelbel = fromMidiFile $ inputPath ++ "pachelbel.mid" :: IO MusicU1
turkish_march = fromMidiFile $ inputPath ++ "turkish_march.mid" :: IO MusicU1
wedding_march = fromMidiFile $ inputPath ++ "wedding_march.mid" :: IO MusicU1
entertainer = fromMidiFile $ inputPath ++ "entertainer16.mid" :: IO MusicU1
takefive = fromMidiFile $ inputPath ++ "takefive.mid" :: IO MusicU1


type SongData = (String, Rational, TimeSig, MusicU1)

mkSongData :: String -> Rational -> TimeSig -> IO MusicU1 -> IO SongData
mkSongData name tempo timeSig = liftM ((,,,) name tempo timeSig)

songData = [
    mkSongData "twinkle" (9 % 8)  (4, 4)  twinkle,
    mkSongData "birthday" (1 % 1) (3, 4) birthday,
    mkSongData "bells"   (7 % 5)  (6, 8)  bells,
    mkSongData "zelda"   (16 % 9) (12, 8) zelda,
    mkSongData "invention" (2 % 3) (3, 8) bach_invention4,
    mkSongData "pachelbel" (1 % 1) (4, 4) pachelbel,
    mkSongData "turkish_march" (6 % 5) (4, 8) turkish_march,
    mkSongData "wedding_march" (1 % 1) (12, 12) wedding_march,
    mkSongData "entertainer" (8 % 11) (4, 8) entertainer,
    mkSongData "takefive" (7 % 5) (5, 4) takefive
    ]

timeSigMappings = [(n, d) | n <- [2..15], d <- [1, 2]]

-- gives the time-invariant version, then the note-invariant version
changeTime :: SongData -> TimeSig -> (MusicU1, MusicU1)
changeTime (_, tempo, oldTimeSig, mus) newTimeSig = (mus1, mus2)
    where
        mus' = changeTimeSig oldTimeSig newTimeSig mus
        mus1 = modify (Tempo tempo) $ removeTempos $ mus'
        mus2 = modify (Tempo tempo) mus'

makeFile :: SongData -> TimeSig -> IO ()
makeFile sd@(name, _, _, _) ts@(n, d) = do
    let (mus1, mus2) = changeTime sd ts
    let outfile1 = outputPath ++ "note_invariant/" ++ name ++ "_" ++ show n ++ "_" ++ show d ++ ".mid"
    putStrLn outfile1
    toMidiFile (withMetronome ts mus1) outfile1
    let outfile2 = outputPath ++ "duration_invariant/" ++ name ++ "_" ++ show n ++ "_" ++ show d ++ ".mid"
    putStrLn outfile2
    toMidiFile (withMetronome ts mus2) outfile2

main :: IO ()
main = mapM_ id $ (join . liftM f) <$> songData --join $ f <$> sequence songData
    where
        f :: SongData -> IO ()
        f sd@(_, _, (_, d), _) = mapM_ makeFile' timeSigMappings
            where
                makeFile' :: (Int, Rational) -> IO ()
                makeFile' (n', d') = makeFile sd ts
                    where ts = (n', truncate $ (toRational d * d'))
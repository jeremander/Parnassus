{- cabal:
build-depends: aeson, base, filepath, Parnassus
-}

import Data.Aeson (decodeFileStrict')
import Data.Maybe (fromJust)
import System.FilePath.Posix ((</>))

import Music.SoundFont
-- import Music.Tuning


sfPath = "/Users/jerm/Programming/Music/SoundFonts"
gmPath = sfPath </> "gm.sf2"
tuningPath = "default_tunings.json"
outputDir = "soundfonts"


main :: IO ()
main = do
    namedTunings <- fromJust <$> decodeFileStrict' tuningPath
    retuneSoundFont namedTunings gmPath outputDir

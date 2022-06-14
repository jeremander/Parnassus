{- cabal:
build-depends: base, filepath, Parnassus
-}

import System.FilePath.Posix ((<.>), (</>))

import Music.SoundFont (filterSoundFontInstruments, retuneSoundFont)
import Music.Tuning (a440, meantoneTuningPackage)


sfPath = "/Users/jerm/Programming/Music/SoundFonts/GM"
gmPath = sfPath </> "gm.sf2"
outputDir = "."

tuningSystem = meantoneTuningPackage !! 1



main :: IO ()
main = do
    -- retune
    -- let (name, _) = tuningSystem
    -- let outfile = outputDir </> name <.> "sf2"
    -- putStrLn $ "Saving SoundFont to " ++ outfile
    -- retuneSoundFont a440 tuningSystem gmPath outputDir
    -- filter instruments
    let filteredPath = "tmp.sf2"
    filterSoundFontInstruments [30] gmPath filteredPath

{- cabal:
build-depends: base, filepath, Parnassus
-}

import System.FilePath.Posix ((<.>), (</>))

import Music.SoundFont
import Music.Tuning


sfPath = "/Users/jerm/Programming/Music/SoundFonts/GM"
gmPath = sfPath </> "gm.sf2"
outputDir = "."

tuningSystem = meantoneTemperaments !! 1


main :: IO ()
main = do
    -- retune
    -- let (name, _) = tuningSystem
    -- let outfile = outputDir </> name <.> "sf2"
    -- putStrLn $ "Saving SoundFont to " ++ outfile
    -- retuneSoundFont a440 tuningSystem gmPath outputDir
    -- filter instruments
    let outfile = "tmp.retuned.sf2"
    let (name, scale) = tuningSystem
    let tuning = makeTuning scale a440
    -- let mod = return . sfFilterInstruments [7, 15]
    -- let mod = return . sfRetuneInstruments tuning . sfFilterInstruments [0]
    let mod = sfStampWithUserAndTime . sfRename name . sfRetuneAllInstruments tuning . sfStripNames . sfFilterInstruments [7, 15]
    modifySoundFont mod gmPath outfile

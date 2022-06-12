import Codec.SoundFont (importFile, SoundFont)
import System.FilePath.Posix ((<.>), (</>), takeDirectory)

import Music.SoundFont (retuneSoundFont)
import Music.Tuning (a440, meantoneTuningPackage)


-- loads a SoundFont file
loadSoundFont :: FilePath -> IO SoundFont
loadSoundFont path = do
    sf <- importFile path
    return $ case sf of
        Left err  -> error err
        Right sf' -> sf'

sfPath = "/Users/jerm/Programming/Music/SoundFonts/GM"
gmPath = sfPath </> "gm.sf2"
outputDir = "."

tuningSystem = meantoneTuningPackage !! 1


main :: IO ()
main = do
    let (name, _) = tuningSystem
    let outfile = outputDir </> name <.> "sf2"
    putStrLn $ "Saving SoundFont to " ++ outfile
    retuneSoundFont gmPath outputDir a440 tuningSystem

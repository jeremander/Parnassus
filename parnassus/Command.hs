{-# LANGUAGE ScopedTypeVariables #-}

module Command where

import Control.Exception (catch, SomeException)
import Data.Aeson (decodeFileStrict')
import Options.Applicative

import Music.Tuning (NamedTuning)
import Music.Tuning.Tun (loadNamedTuningFromTun)


subcommandOpts :: String -> Parser a -> String -> Mod CommandFields a
subcommandOpts name subcmdParser desc = command name $ info subcmdParser $ progDesc desc

infileParser :: String -> Parser FilePath
infileParser extension = argument str (metavar "INFILE" <> help ("(required) input ." ++ extension ++ " file"))

-- ** Tuning

data RetuneOpts = RetuneOpts {
    inputFile :: FilePath,
    tuningFiles :: [FilePath],
    outputDir :: FilePath
}

retuneOptsParser :: String -> Parser RetuneOpts
retuneOptsParser extension = helper <*> (RetuneOpts <$>
        infileParser extension
    <*> some (strOption (long "tunings" <> short 't' <> metavar "TUNINGS" <> help "(required) one or more tuning files -- a tuning file can be either: 1) a JSON file containing a list of entries with \"name\" and \"tuning\" fields (the latter is a list of 128 frequencies mapping the MIDI keyboard) 2) an AnaMark .tun file"))
    <*> strOption (long "output-dir" <> short 'o' <> metavar "OUTDIR" <> value "." <> showDefault <> help "output directory"))

loadNamedTuningsFromJSON :: FilePath -> IO (Maybe [NamedTuning])
loadNamedTuningsFromJSON path = do  -- parse a list of tunings
    namedTunings <- decodeFileStrict' path
    case namedTunings of
        Just namedTunings' -> return $ Just namedTunings'
        Nothing -> do  -- parse a single tuning
            namedTuning <- decodeFileStrict' path
            return $ case namedTuning of
                Just namedTuning' -> Just [namedTuning']
                Nothing           -> Nothing

loadNamedTunings :: FilePath -> IO [NamedTuning]
loadNamedTunings path = catch loadNamedTuningsFromTun handle
    where
        loadNamedTuningsFromTun = pure <$> loadNamedTuningFromTun path
        handle (err :: SomeException) = do
            namedTunings <- loadNamedTuningsFromJSON path
            case namedTunings of
                Just namedTunings' -> return namedTunings'
                Nothing            -> fail $ "could not load " ++ path ++ " as TUN or JSON"
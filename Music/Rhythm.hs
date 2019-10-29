{-# LANGUAGE MultiParamTypeClasses #-}

module Music.Rhythm (
    Quantizable(..),
    TimeSig,
    getTimeSig
) where

import Codec.Midi (Message(..), Midi, tracks)
import Control.Monad (join)
import Data.Maybe (isJust, listToMaybe)
import Data.Ratio ((%))
import Euterpea (Dur)


type TimeSig = (Int, Int)

-- | Gets the time signature of a Midi.
getTimeSig :: Midi -> Maybe TimeSig
getTimeSig m = join $ listToMaybe $ filter isJust (getSig <$> msgs)
    where
        msgs = snd <$> head (tracks m)  -- time signature should appear in the first track if at all
        getSig msg = case msg of
                        TimeSignature num pow _ _ -> Just (num, (2 :: Int) ^ pow)
                        _                         -> Nothing

class Quantizable m a where
    -- quantizes the music so that every note/rest is a multiple of the given duration
    -- NB: convention will be to ignore tempo modifiers
    quantize :: Dur -> m a -> m a
    -- splits the music into segments of the same length
    split :: Dur -> m a -> [m a]
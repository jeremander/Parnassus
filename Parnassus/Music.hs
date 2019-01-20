{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ParallelListComp #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Parnassus.Music where

import Codec.Midi
import Control.Monad (join)
import qualified Data.List (nub, transpose)
import qualified Data.Map
import Data.Maybe (isJust, listToMaybe)
import Data.Ratio ((%))
import System.IO.Unsafe (unsafePerformIO)

import Euterpea hiding (chord, cut, dur, line, play, scaleDurations, toMusic1, transpose)
import Parnassus.Utils (aggMax, MapPlus (..), quantizeRationals, quantizeTime)
import Parnassus.MusicBase (Pitched, MusicT (..), Quantizable (..), TimeSig, ToMidi (..), (/+/), (/=/), (/*/))
import Parnassus.MusicD (MusicD (..), MusicD1, primD, ToMusicD (..))
import Parnassus.MusicU (mFoldU, MusicU (..), MusicU1, ToMusicU (..))


-- song paths
vgmusicPath :: String = "/Users/jeremander/Programming/Music/Parnassus/tunes/vgmusic/"
xmasPath :: String = "/Users/jeremander/Programming/Music/Parnassus/tunes/xmas/"

-- songs
zelda :: MusicU1 = unsafePerformIO $ fromMidiFile $ vgmusicPath ++ "loz_overworld.mid"
mm3 :: MusicU1 = unsafePerformIO $ fromMidiFile $ vgmusicPath ++ "mm3_password.mid"
gradius :: MusicU1 = unsafePerformIO $ fromMidiFile $ vgmusicPath ++ "gradius_stage4.mid"
bells :: MusicU1 = unsafePerformIO $ fromMidiFile $ xmasPath ++ "ringxms.mid"

twinkle :: MusicU Pitch = fromMusic $ line $ map ($ qn) (section1 ++ section2 ++ section2 ++ section1)
    where
        section1 = [c 4, c 4, g 4, g 4, a 4, a 4, g 4, prim . Rest, f 4, f 4, e 4, e 4, d 4, d 4, c 4, prim . Rest]
        section2 = [g 4, g 4, f 4, f 4, e 4, e 4, d 4, prim . Rest]
twinkleBass :: MusicU Pitch = fromMusic $ line $ (section1 ++ section2 ++ section2 ++ section1)
    where
        section1 = [c 3 hn, e 2 hn, f 2 qn, a 2 qn, c 3 qn, prim $ Rest qn, f 3 hn, c 3 hn, g 3 qn, g 2 qn, c 3 qn, prim $ Rest qn]
        section2 = [e 3 hn, a 3 hn, f 3 en, e 3 en, f 3 en, fs 3 en, g 3 qn, g 2 qn]
twinkle2 = twinkle /=/ twinkleBass

-- Metronome --

-- infinite metronome of a fixed time signature
-- claves for downbeat, high wood block for upbeats
metronome :: (MusicT m Note1) => TimeSig -> m Note1
metronome (n, d) = modify (Instrument Percussion) $ line $ (prim <$> beats)
    where
        flags = [rem i n == 0 | i <- [0..]]
        beats = [Note (1 % toInteger d) (if flag then ((Ds, 5), [Volume 127]) else ((E, 5), [Volume 90])) | flag <- flags]

-- overlays music with a metronome
withMetronome :: (MusicT m Note1) => TimeSig -> m Note1 -> m Note1
withMetronome ts mus = mus /=/ metro'
    where
        metro = (cut (dur mus) (metronome ts))
        -- modify metronome's tempo with the music's global tempo
        (tempo, _) = stripTempo mus
        metro' = modify tempo metro

-- MusicT type conversions

convUtoD :: (MusicD a -> MusicD a -> MusicD a) -> (MusicD a -> MusicD a -> MusicD a) -> MusicU a -> MusicD a
convUtoD mseq mpar m = mFoldU (primD $ durGCD m) (foldr1 mseq) (foldr1 mpar) g m
    where
        g :: Control -> MusicD a -> MusicD a
        g c (MusicD q' ctl m') = MusicD q' (c : ctl) m'

instance (Ord a, Pitched a) => ToMusicU MusicD a where
    toMusicU = fromMusic . toMusic
    fromMusicU m = MusicD q ctl (Data.List.nub <$> m')  -- dedupe identical notes/rests in a chord
        where MusicD q ctl m' = convUtoD (/+/) (/=/) m

instance (Ord a, Pitched a) => ToMusicD MusicU a where
    toMusicD m = MusicD q ctl (Data.List.nub <$> m')  -- dedupe identical notes/rests in a chord
        where MusicD q ctl m' = convUtoD (/+/) (/=/) m
    fromMusicD = fromMusic . toMusic

-- Time Signature --

getTimeSig :: Midi -> Maybe TimeSig
getTimeSig m = join $ listToMaybe $ filter isJust (getSig <$> msgs)
    where
        msgs = snd <$> tracks m !! 0  -- time signature should appear in the first track if at all
        getSig = \msg -> case msg of
                            TimeSignature num pow _ _ -> Just (num, (2 :: Int) ^ pow)
                            otherwise                 -> Nothing

-- Quantizable Instances --

unConjD :: (MusicT m a, ToMusicD m a) => (MusicD a -> MusicD a) -> (m a -> m a)
unConjD f = fromMusicD . f . toMusicD

instance (Ord a, Pitched a) => Quantizable MusicU a where
    quantize :: Rational -> MusicU a -> MusicU a
    quantize q = unConjD $ quantize q
    split :: Rational -> MusicU a -> [MusicU a]
    split d = (fromMusicD <$>) . split d . toMusicD
    changeTimeSig :: TimeSig -> TimeSig -> MusicU a -> MusicU a
    changeTimeSig ts1 ts2 = unConjD $ changeTimeSig ts1 ts2

instance (Ord a, Pitched a) => Quantizable Music a where
    quantize :: Rational -> Music a -> Music a
    quantize q = fromMusicD . quantize q . toMusicD
    split :: Rational -> Music a -> [Music a]
    split d = (fromMusicD <$>) . split d . toMusicD
    changeTimeSig :: TimeSig -> TimeSig -> Music a -> Music a
    changeTimeSig ts1 ts2 = unConjD $ changeTimeSig ts1 ts2

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

import Euterpea hiding (chord, cut, dur, line, play, remove, scaleDurations, toMusic1, transpose)
import Parnassus.Utils (quantizeRational, quantizeRationals, quantizeTime, transposeWithDefault)
import Parnassus.MusicBase (Pitched, MusicT (..), Quantizable (..), TimeSig, ToMidi (..), (/+/), (/=/), (/*/))
import Parnassus.MusicD (isRest, MusicD (..), primD, ToMusicD (..))
import Parnassus.MusicU (mFoldU, MusicU (..), ToMusicU (..))

-- songs

twinkle :: MusicU Pitch = fromMusic $ line $ map ($ qn) (section1 ++ section2 ++ section2 ++ section1)
    where
        section1 = [c 4, c 4, g 4, g 4, a 4, a 4, g 4, prim . Rest, f 4, f 4, e 4, e 4, d 4, d 4, c 4, prim . Rest]
        section2 = [g 4, g 4, f 4, f 4, e 4, e 4, d 4, prim . Rest]
twinkleBass :: MusicU Pitch = fromMusic $ line $ (section1 ++ section2 ++ section2 ++ section1)
    where
        section1 = [c 3 hn, e 2 hn, f 2 qn, a 2 qn, c 3 qn, prim $ Rest qn, f 3 hn, c 3 hn, g 3 qn, g 2 qn, c 3 qn, prim $ Rest qn]
        section2 = [e 3 hn, a 3 hn, f 3 en, e 3 en, f 3 en, fs 3 en, g 3 qn, g 2 qn]
twinkle2 = twinkle /=/ twinkleBass

-- Types --

type MusicU1 = MusicU Note1
type MusicD1 = MusicD Note1

-- Metronome --

-- metronome for a fixed time signature
-- claves for downbeat, high wood block for upbeats
metronome :: (MusicT m Note1) => TimeSig -> Dur -> m Note1
metronome (n, d) r = modify (Instrument Percussion) $ line $ (prim <$> beats)
    where
        numBeats = ceiling (r * fromIntegral d)
        flags = [rem i n == 0 | i <- [0..]]
        beats = take numBeats [Note (1 % toInteger d) (if flag then ((Ds, 5), [Volume 85]) else ((E, 5), [Volume 60])) | flag <- flags]

-- overlays music with a metronome
withMetronome :: (MusicT m Note1) => TimeSig -> m Note1 -> m Note1
withMetronome ts mus = mus /=/ metro'
    where
        (tempo, mus') = stripTempo mus
        d = dur mus'
        metro = metronome ts d
        -- modify metronome's tempo with the music's global tempo
        metro' = distributeTempos $ modify tempo metro

-- MusicT type conversions

convUtoD :: (Ord a, Pitched a) => MusicU a -> MusicD a
convUtoD mus = MusicD q ctl m''
    where
        g :: Control -> MusicD a -> MusicD a
        g c (MusicD q' ctl' x') = MusicD q' (c : ctl') x'
        (MusicD q ctl m') = mFoldU (MusicD 0 [] []) (primD $ durGCD mus) (foldr1 (/+/)) (foldr1 (/=/)) g mus
        m'' = Data.List.nub . filter (not . isRest) <$> m'  -- dedupe identical notes, eliminate rests in a chord

instance (Ord a, Pitched a) => ToMusicU MusicD a where
    toMusicU = fromMusic . toMusic
    fromMusicU = convUtoD

instance (Ord a, Pitched a) => ToMusicD MusicU a where
    toMusicD = convUtoD
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
    -- improve memory efficiency by quantizing parallel sections separately
    quantize :: Rational -> MusicU a -> MusicU a
    --quantize q = unConjD $ quantize q  -- inefficient version
    quantize q mus = case mus of
        Empty            -> Empty
        ParU ms          -> chord $ quantize' <$> ms
        ModifyU ctl mus' -> ModifyU ctl $ quantize q mus'
        otherwise        -> quantize' mus  -- TODO: make more efficient
        where quantize' = (unConjD $ quantize q)
    split :: Rational -> MusicU a -> [MusicU a]
    split d mus = fst <$> takeWhile (\(prev, _) -> not $ isEmpty prev) (tail pairs)
        where pairs = [(Empty, mus)] ++ [bisect d prev | (_, prev) <- pairs]

instance (Ord a, Pitched a) => Quantizable Music a where
    quantize :: Rational -> Music a -> Music a
    quantize q = fromMusicU . quantize q . toMusicU
    split :: Rational -> Music a -> [Music a]
    split d = (fromMusicU <$>) . split d . toMusicU

-- toMusicD $ scaleDurations (4 % 5) $ ((split 1 $ quantize (1 % 8) twinkle) !! 0)
--quantize (1 % 8) $ toMusicD $ scaleDurations (4 % 5) $ ((split 1 $ quantize (1 % 8) twinkle) !! 0)
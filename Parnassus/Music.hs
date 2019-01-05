{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ParallelListComp #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Parnassus.Music where

import Codec.Midi
import Control.Monad (join)
import qualified Data.List (nub, transpose)
import Data.Maybe (isJust, listToMaybe)
import Data.Ratio ((%))
import System.IO.Unsafe (unsafePerformIO)

import Euterpea hiding (chord, cut, dur, line, play, transpose)
import Parnassus.Utils (quantizeRationals, quantizeTime)
import Parnassus.MusicBase (durP, ToMidi (..), MusicT (..), Quantizable (..), Tied, (/+/), (/=/), (/*/))
import Parnassus.MusicD (MusicD (..), MusicD1, primD', ToMusicD (..))
import Parnassus.MusicU (mFoldU, MusicU (..), MusicU1, ToMusicU (..))


-- song paths
vgmusicPath :: String = "/Users/jeremander/Programming/Music/Parnassus/tunes/vgmusic/"
xmasPath :: String = "/Users/jeremander/Programming/Music/Parnassus/tunes/xmas/"

-- songs
zelda :: MusicU1 = unsafePerformIO $ fromMidiFile $ vgmusicPath ++ "loz_overworld.mid"
mm3 :: MusicU1 = unsafePerformIO $ fromMidiFile $ vgmusicPath ++ "mm3_password.mid"
gradius :: MusicU1 = unsafePerformIO $ fromMidiFile $ vgmusicPath ++ "gradius_stage4.mid"
bells :: MusicU1 = unsafePerformIO $ fromMidiFile $ xmasPath ++ "ringxms.mid"

twinkle :: MusicU Pitch = fromMusic $ foldr1 (/+/) $ map ($ (1 % 4)) [c 4, c 4, g 4, g 4, a 4, a 4, g 4, Prim . Rest, f 4, f 4, e 4, e 4, d 4, d 4, c 4]
twinkleBass :: MusicU Pitch = fromMusic $ foldr1 (/+/) $ [c 3 (1 % 2), e 2 (1 % 2), f 2 (1 % 4), a 2 (1 % 4), c 3 (1 % 4), Prim $ Rest (1 % 4), f 3 (1 % 2), c 3 (1 % 2), g 3 (1 % 4), g 2 (1 % 4), c 3 (1 % 4)]
twinkle2 = twinkle /=/ twinkleBass

-- MusicT type conversions

convUtoD :: (MusicD a -> MusicD a -> MusicD a) -> (MusicD a -> MusicD a -> MusicD a) -> MusicU a -> MusicD a
convUtoD mseq mpar m = mFoldU (primD' $ durGCD m) (foldr1 mseq) (foldr1 mpar) g m
    where
        g :: Control -> MusicD a -> MusicD a
        g c (MusicD q' ctl m') = MusicD q' (c : ctl) m'

instance ToMusicU MusicD a where
    toMusicU = fromMusic . toMusic
    fromMusicU = convUtoD (/+/) (/=/)

instance {-# OVERLAPPING #-} ToMusicU MusicD Note1 where
    toMusicU = fromMusic . toMusic
    fromMusicU m = MusicD q ctl (Data.List.nub <$> m')  -- dedupe identical notes/rests in a chord
        where MusicD q ctl m' = convUtoD (/+/) (/=/) m

instance ToMusicD MusicU a where
    toMusicD = convUtoD (/+/) (/=/)
    fromMusicD = fromMusic . toMusic

instance {-# OVERLAPPING #-} ToMusicD MusicU Note1 where
    toMusicD m = MusicD q ctl (Data.List.nub <$> m')  -- dedupe identical notes/rests in a chord
        where MusicD q ctl m' = convUtoD (/+/) (/=/) m
    fromMusicD = fromMusic . toMusic


-- Time Signature --

type TimeSig = (Int, Int)

getTimeSig :: Midi -> Maybe TimeSig
getTimeSig m = join $ listToMaybe $ filter isJust (getSig <$> msgs)
    where
        msgs = snd <$> tracks m !! 0  -- time signature should appear in the first track if at all
        getSig = \msg -> case msg of
                            TimeSignature num pow _ _ -> Just (num, (2 :: Int) ^ pow)
                            otherwise                 -> Nothing

-- Quantizable Instance --

quantizeD :: Eq a => Rational -> MusicD a -> MusicD a
quantizeD q mus@(MusicD q' ctl m)
    | q == q'   = mus
    | otherwise = MusicD q ctl m'
        where
            groups = quantizeTime q' (zip m [0, q..])
            mergeGroup :: [(Rational, Rational, [Tied a], Bool)] -> [Tied a]
            mergeGroup gp =
                where
                    retie :: Tied a -> Bool -> Tied a
                    retie (Untied (_, Note d x)) True = TiedNote d x
                    retie n _ = n
                    -- converts a chord into a list of notes tagged with the duration
                    restructure :: (Rational, Rational, [Tied a], Bool) -> [(Rational, Tied a)]
                    restructure (_, d, notes, flag) = [(d, 
            m' = mergeGroup <$> groups

-- given rational q and a sequence of increasing times delimiting time intervals (including start and end points), returns a list of groups of intervals taking place within each q interval, consisting of (start, duration, flag) where flag is True if the interval is a continuation of an interval from the previous group
-- quantizeTime :: Rational -> [Rational] -> [[(Rational, Rational, Int, Bool)]]

quantizeU :: Eq a => Rational -> MusicU a -> MusicU a
quantizeU q m = case m of
    Empty        -> Empty
    PrimU p      -> prim p
    SeqU ms      -> line [fit qd m' | qd <- qDurs | m' <- ms']
        where
            ms' = rec <$> ms
            qDurs = quantizeRationals q (dur <$> ms')
    ParU ms      -> chord $ (rec <$> ms)
    ModifyU c m' -> ModifyU c (rec m')
    where rec = quantizeU q

instance (Eq a) => Quantizable MusicU a where
    quantize :: Rational -> MusicU a -> MusicU a
    quantize = quantizeU

instance (Eq a) => Quantizable Music a where
    quantize :: Rational -> Music a -> Music a
    quantize q = conj $ (quantizeU q)


-- Measure splitting --

-- type TiedMusic a = Music (Tied a)
-- type TiedMusicU a = MusicU (Tied a)

-- TODO: ties will be eliminated
-- instance ToMusic1 (Tied Note1) where
--     toMusic1 = mMap fst

-- given r and d, returns (r // d, r % d)
-- rationalQuotRem :: Rational -> Rational -> (Int, Rational)
-- rationalQuotRem r d = (q, r - d * fromIntegral q) where q = floor (r / d)

-- given measure duration, initial duration, and total duration, splits the total duration into a list where the head is at most the initial duration, and each subsequent duration is at most the measure duration
-- splitDur :: Rational -> Rational -> Rational -> [Rational]
-- splitDur measureDur _ 0 = []
-- splitDur measureDur 0 totalDur = replicate q measureDur ++ filter (>0) [rem]
--     where (q, rem) = rationalQuotRem totalDur measureDur
-- splitDur measureDur firstDur totalDur
--     | (firstDur >= totalDur) = [totalDur]
--     | otherwise       = [firstDur] ++ splitDur measureDur 0 (totalDur - firstDur)


-- splits MusicU by measure
-- splitMeasuresU :: Eq a => Dur -> Dur -> MusicU a -> [TiedMusicU a]
-- splitMeasuresU measureDur = split
--     where
--         glue :: [MusicU a] -> [MusicU a] -> [MusicU a]
--         glue ms1 ms2 = (init ms1) ++ [last ms1 /+/ head ms2] ++ (tail ms2)
--         split :: Eq a => Dur -> MusicU a -> [TiedMusicU a]
--         split firstDur Empty = []
--         split firstDur (PrimU (Note d p)) = [PrimU (Note r (p, flag)) | r <- splitDur measureDur firstDur d | flag <- tieFlags]
--         split firstDur (PrimU (Rest d)) = [PrimU (Rest r) | r <- splitDur measureDur firstDur d]
--         split firstDur (Seq []) = []
--         split firstDur (Seq [m]) = split firstDur m
--         split firstDur (Seq (m:ms)) = result
--             where
--                 measHead = split firstDur m
--                 remDur = if null measHead then firstDur else (measureDur - dur (last measHead))
--                 measTail = split remDur (mSeq ms)
--                 result
--                     | null measTail = measHead
--                     | otherwise = if (remDur == 0) then (measHead ++ measTail) else (glue measHead measTail)
--         split firstDur (Par ms) = mPar <$> mat
--             where
--                 meas = (split firstDur) <$> ms
--                 mat = transposeWithDefault (PrimU $ Rest measureDur) meas
--         split firstDur (ModifyU c m) = (ModifyU c) <$> (split firstDur m)

-- Changes the time signature of music, measure-wise
-- Assumes music starts at the beginning of a measure
-- NB: LCD of time signatures determine the overall subdivision
-- TODO: resolve ties so they don't rearticulate at beginning of measures
-- changeTimeSig :: Eq a => MusicU a -> TimeSig -> TimeSig -> TiedMusicU a
-- changeTimeSig m (n1, d1) (n2, d2) = mSeq meas'
--     where
--         r1 = (toInteger n1) % (toInteger d1)
--         r2 = (toInteger n2) % (toInteger d2)
--         q = toInteger $ lcm d1 d2
--         meas = splitMeasuresU r1 0 m
--         meas' = map ((quantizeU q) . (scaleDurations (r1 / r2))) meas

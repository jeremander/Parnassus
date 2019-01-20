{-# LANGUAGE FlexibleContexts #-}
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
import qualified Data.Map
import Data.Maybe (isJust, listToMaybe)
import Data.Ratio ((%))
import Data.Tuple.Select
import GHC.Exts (groupWith)
import System.IO.Unsafe (unsafePerformIO)

import Euterpea hiding (chord, cut, dur, line, play, toMusic1, transpose)
import Parnassus.Utils (aggMax, MapPlus (..), quantizeRationals, quantizeTime)
import Parnassus.MusicBase (extractTied, fitTied, isTied, Pitched, ToMidi (..), MusicT (..), Quantizable (..), Tied (..), (/+/), (/=/), (/*/))
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

type TimeSig = (Int, Int)

-- infinite metronome of a fixed time signature
-- claves for downbeat, high wood block for upbeats
metronome :: (MusicT m Note1) => TimeSig -> m Note1
metronome (n, d) = modify (Instrument Percussion) $ line $ (prim <$> beats)
    where
        flags = [rem i n == 0 | i <- [0..]]
        beats = [Note (1 % toInteger d) (if flag then ((Ds, 5), [Volume 127]) else ((E, 5), [Volume 100])) | flag <- flags]

-- overlays music with a metronome
withMetronome :: (MusicT m Note1) => TimeSig -> m Note1 -> m Note1
withMetronome ts mus = mus /=/ (cut (dur mus) (metronome ts))

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

-- Quantizable Instance --

quantizeD :: forall a . (Ord a) => Rational -> MusicD a -> MusicD a
quantizeD q mus@(MusicD q' ctl m)
    | q == q'   = mus
    | otherwise = MusicD q ctl m'
        where
            -- group the chord array by quantization slices
            groups1 :: [[(Rational, Rational, [Tied a], Bool)]]
            groups1 = quantizeTime q (zip (m ++ [[]]) [0, q'..])
            -- distribute time data across a chord
            distrib :: (Rational, Rational, [Tied a], Bool) -> [(Rational, Rational, Tied a, Bool)]
            distrib (t, d, notes, flag) = [(t, d, note, flag) | note <- notes]
            -- undistribute time data for a chord (assuming all time data is the same)
            undistrib :: [(Rational, Rational, Tied a, Bool)] -> (Rational, Rational, [Tied a], Bool)
            undistrib items = (t, d, sel3 <$> items, flag)
                where (t, d, _, flag) = head items
            groups2 :: [[(Rational, Rational, Tied a, Bool)]]
            groups2 = join . map distrib <$> groups1
            -- tie together identical notes
            combine :: (Rational, Rational, Tied a, Bool) -> (Rational, Rational, Tied a, Bool) -> (Rational, Rational, Tied a, Bool)
            combine (t1, d1, p, flag1) (_, d2, _, flag2) = (t1, d1 + d2, p, flag1 && flag2)
            groups3 :: [[(Rational, Rational, Tied a, Bool)]]
            groups3 = (map $ foldr1 combine) . groupWith (extractTied . sel3) <$> groups2
            -- keep only notes that fill up at least half the quantization interval (if untied), or more than half (if tied)
            keepNote :: (Rational, Rational, Tied a, Bool) -> Bool
            keepNote (_, d, p, flag) = if (flag || isTied p) then (d > q / 2) else (d >= q / 2)
            groups4 :: [[(Rational, Rational, Tied a, Bool)]]
            groups4 = filter keepNote <$> groups3
            -- need to fix tie flags
            fix :: [(Rational, Rational, Tied a, Bool)] -> [(Rational, Rational, Tied a, Bool)] -> [(Rational, Rational, Tied a, Bool)]
            fix xs1 xs2 = f <$> xs2
                where
                    pairs = [(extractTied x1, t1 + d1) | (t1, d1, x1, _) <- xs1]
                    f :: (Rational, Rational, Tied a, Bool) -> (Rational, Rational, Tied a, Bool)
                    f (t2, d2, x2, False) = (t2, d2, x2, False)
                    f (t2, d2, x2, flag2) = (t2, d2, x2, flag2 && tiePermitted)
                        where
                            note2 = extractTied x2
                            -- does a previous note tie with this note?
                            tiePermitted = any (\(note1, st1) -> (note1 == note2) && (st1 >= t2)) pairs
            groups5 :: [[(Rational, Rational, [Tied a], Bool)]]
            groups5 = map undistrib <$> gps
                where
                    pairs = zip ([] : groups4) groups4
                    -- fix the ties
                    fixed = snd <$> [(xs1, fix xs1 xs2) | (xs1, xs2) <- pairs]
                    -- undistribute identical time data for efficiency
                    gps = [groupWith (\(t, d, _, flag) -> (t, d, flag)) gp | gp <- fixed]
            mergeGroup :: [(Rational, Rational, [Tied a], Bool)] -> [Tied a]
            mergeGroup gp = [fitTied q x | (x, _) <- pairs]
                where
                    -- a True tie flag converts an Untied note to a Tied one
                    retie :: Tied a -> Bool -> Tied a
                    retie (Untied (_, Note d x)) True = TiedNote d x
                    retie note _ = note
                    -- converts a chord into a list of notes tagged with the duration
                    restructure :: (Rational, Rational, [Tied a], Bool) -> [(Tied a, Rational)]
                    restructure (_, d, notes, flag) = [(retie note flag, d) | note <- notes]
                    -- add up the duration of each note over all sequential segments in the group
                    gatherNotes :: [(Rational, Rational, [Tied a], Bool)] -> [(Tied a, Rational)]
                    gatherNotes gp' = noteGroup
                        where
                            -- helper function; given binary operation on rationals, and two (Tied a, Rational) pairs where the notes are presumed to be the same, combines them appropriately
                            agg :: (Rational -> Rational -> Rational) -> (Tied a, Rational) -> (Tied a, Rational) -> (Tied a, Rational)
                            agg f (Untied (c1, p1), r1) (_, r2) = (Untied (c1, p1), f r1 r2)
                            agg f (_, r1) (Untied (c2, p2), r2) = (Untied (c2, p2), f r1 r2)
                            agg f (TiedNote _ p1, r1) (TiedNote _ _, r2) = (TiedNote r1 p1, f r1 r2)
                            aggPar = agg max  -- parallel aggregation: take max duration
                            aggSeq = agg (+)  -- sequential aggregation: take total duration
                            parGroups = map (foldr1 aggPar) . groupWith (extractTied . fst) . restructure <$> gp'
                            noteGroup = map (foldr1 aggSeq) . groupWith (extractTied . fst) . join $ parGroups
                    pairs = gatherNotes gp
            m' = mergeGroup <$> groups5

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
    quantize q = conj $ quantizeU q


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

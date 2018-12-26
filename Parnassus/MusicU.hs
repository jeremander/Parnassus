{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ParallelListComp #-}

module Parnassus.MusicU where

import qualified Data.List (nub)
import Data.Ratio

import Euterpea hiding (chord, cut, dur, line, scaleDurations)
import Parnassus.Utils
import Parnassus.MusicBase


-- unassociative music data structure (isomorphic to Music)
data MusicU a = Empty | PrimU (Primitive a) | SeqU [MusicU a] | ParU [MusicU a] | ModifyU Control (MusicU a)
    deriving (Show, Eq, Ord)

type MusicU1 = MusicU Note1

primU :: Primitive a -> MusicU a
primU (Rest d)   = if (d <= 0) then Empty else (PrimU $ Rest d)
primU (Note d p) = if (d <= 0) then Empty else (PrimU $ Note d p)

-- strips away outer control, returning the control (if present) and the remaining MusicU
stripControlU :: MusicU a -> (Maybe Control, MusicU a)
stripControlU (ModifyU ctl m) = (Just ctl, m)
stripControlU m = (Nothing, m)

-- smart constructors

-- setup for smart constructors line and chord
-- handles empty and singleton arguments appropriately
-- removes empty elements of input lists
-- un-distributes the same control applied to each element of an input list
-- TODO: strip away controls in ordering-insensitive way
combineU :: Eq a => (MusicU a -> [MusicU a]) -> ([MusicU a] -> MusicU a) -> [MusicU a] -> MusicU a
combineU f con = combine
    where
        op :: Maybe Control -> Maybe Control -> Maybe Control
        op (Just ctl1) (Just ctl2) = if (ctl1 == ctl2) then (Just ctl1) else Nothing
        op _ _ = Nothing
        combine [] = Empty
        combine [x] = case outerCtl of
            Just ctl -> ModifyU ctl m
            Nothing  -> x
            where (outerCtl, m) = stripControlU x
        combine xs = case outerCtl of
            Just ctl -> ModifyU ctl (combine xs')  -- recursively apply combine to the stripped elements
            Nothing  -> con xs
            where
                (outerCtls, xs') = unzip (stripControlU <$> xs)
                outerCtl = foldr1 op outerCtls

-- general fold for MusicU
mFoldU :: (Primitive a -> b) -> ([b] -> b) -> ([b] -> b) -> (Control -> b -> b) -> MusicU a -> b
mFoldU f seqFold parFold g m = case m of
    PrimU p      -> f p
    SeqU ms      -> seqFold (map rec ms)
    ParU ms      -> parFold (map rec ms)
    ModifyU c m' -> g c (rec m')
    where
        rec = mFoldU f seqFold parFold g

instance MusicT MusicU a where
    fromMusic :: Music a -> MusicU a
    fromMusic = mFold primU (/+/) (/=/) ModifyU
    toMusic :: MusicU a -> Music a
    toMusic = mFoldU Prim (foldr1 (:+:)) (foldr1 (:=:)) Modify
    prim :: Primitive a -> MusicU a
    prim = primU
    (/+/) :: MusicU a -> MusicU a -> MusicU a
    (/+/) m1 Empty = m1
    (/+/) Empty m2 = m2
    (/+/) m1 m2 = SeqU (extractSeq m1 ++ extractSeq m2)
        where
            extractSeq :: MusicU a -> [MusicU a]
            extractSeq m = case m of
                SeqU ms    -> ms
                otherwise -> [m]
    (/=/) :: MusicU a -> MusicU a -> MusicU a
    (/=/) m1 Empty = m1
    (/=/) Empty m2 = m2
    (/=/) m1 m2 = ParU (extractPar m1 ++ extractPar m2)
        where
            extractPar :: MusicU a -> [MusicU a]
            extractPar m = case m of
                ParU ms    -> ms
                otherwise -> [m]
    line :: Eq a => [MusicU a] -> MusicU a
    line ms = combine $ filter (not . isEmpty) $ concatMap unLine ms
        where combine = combineU unLine SeqU
    -- TODO: remove rests shorter than duration
    chord :: Eq a => [MusicU a] -> MusicU a
    chord ms = combine $ Data.List.nub $ filter (not . isEmpty) $ concatMap unChord ms
        where combine = combineU unChord ParU
    unLine :: Eq a => MusicU a -> [MusicU a]
    unLine (SeqU ms) = ms
    unLine (ModifyU ctl m) = (ModifyU ctl) <$> (unLine m)
    unLine m = [m]
    unChord :: Eq a => MusicU a -> [MusicU a]
    unChord (ParU ms) = ms
    unChord (ModifyU ctl m) = (ModifyU ctl) <$> (unChord m)
    unChord m = [m]
    dur :: MusicU a -> Dur
    dur Empty = 0
    dur (PrimU (Note d _)) = d
    dur (PrimU (Rest d)) = d
    dur (SeqU ms) = sum (map dur ms)
    dur (ParU ms) = maximum (map dur ms)
    dur (ModifyU (Tempo r) m) = dur m / r
    dur (ModifyU _ m) = dur m
    isEmpty :: MusicU a -> Bool
    isEmpty Empty = True
    isEmpty _     = False
    lcd :: MusicU a -> Integer
    lcd = mFoldU f (foldr lcm 1) (foldr lcm 1) (curry snd)
        where
            f :: Primitive a -> Integer
            f (Rest d) = denominator d
            f (Note d _) = denominator d
    cut :: Eq a => Dur -> MusicU a -> MusicU a
    cut d m | d <= 0            = Empty
    cut _ Empty                 = Empty
    cut d (PrimU (Note oldD p)) = prim $ Note (min oldD d) p
    cut d (PrimU (Rest oldD))   = prim $ Rest (min oldD d)
    cut d (SeqU ms)             = line [cut maxDur m' | (m', maxDur) <- zip ms maxDurs, maxDur > 0]
        where
            cumDurs = scanl (+) 0 (map dur ms)
            maxDurs = [(d - cumDur) | cumDur <- cumDurs]
    cut d (ParU ms)             = chord ((cut d) <$> ms)
    cut d (ModifyU (Tempo r) m) = ModifyU (Tempo r) (cut (d * r) m)
    cut d (ModifyU c m)         = ModifyU c (cut d m)
    pad :: Eq a => Dur -> MusicU a -> MusicU a
    pad d Empty                 = prim $ Rest d
    pad d (PrimU (Note oldD p)) = prim $ Note (max oldD d) p
    pad d (PrimU (Rest oldD))   = prim $ Rest (max oldD d)
    pad d m@(SeqU ms)           = line (ms ++ filter (\_ -> (diff > 0)) [prim $ Rest diff])
        where diff = d - dur m
    pad d (ParU ms)             = chord ((pad d) <$> ms)
    pad d (ModifyU (Tempo r) m) = ModifyU (Tempo r) (pad (d * r) m)
    pad d (ModifyU c m)         = ModifyU c (pad d m)
    stripControls :: MusicU a -> (Controls, MusicU a)
    stripControls = mFoldU f (combine SeqU) (combine ParU) g
        where
        f :: Primitive a -> (Controls, MusicU a)
        f p = ([], PrimU p)
        g :: Control -> (Controls, MusicU a) -> (Controls, MusicU a)
        g ctl (ctls, m) = ([ctl] ++ ctls, m)
        combine :: ([MusicU a] -> MusicU a) -> [(Controls, MusicU a)] -> (Controls, MusicU a)
        combine foldOp pairs = (prefix, foldOp ms')
            where
                (ctls, ms) = unzip pairs
                (prefix, ctls') = unDistribute ctls  -- extract common controls
                ms' = (((foldr (.) id) . (map ModifyU)) <$> ctls') <*> ms  -- reapply unextracted controls
    removeTempos :: MusicU a -> MusicU a
    removeTempos = mFoldU PrimU SeqU ParU g
        where
            g :: Control -> MusicU a -> MusicU a
            g (Tempo d) m = m
            g ctl m = ModifyU ctl m
    distributeTempos :: MusicU a -> MusicU a
    distributeTempos = mFoldU prim SeqU ParU g
        where
            g :: Control -> MusicU a -> MusicU a
            g (Tempo t) m = scaleDurations t m
            g ctl m = ModifyU ctl m
    -- transposes the music by some interval
    transpose :: AbsPitch -> MusicU a -> MusicU a
    transpose i = ModifyU (Transpose i)

instance ToMidi MusicU

-- Quantizable Instance --

quantizeU :: Eq a => Rational -> MusicU a -> MusicU a
quantizeU q m = case m of
    Empty        -> Empty
    PrimU p      -> prim p
    SeqU ms      -> line [fit qd m' | qd <- qDurs | m' <- ms']
        where
            ms' = map rec ms
            qDurs = quantizeRationals q (dur <$> ms')
    ParU ms      -> chord $ map rec ms
    ModifyU c m' -> ModifyU c (rec m')
    where
        quantize' = quantizeRational q
        safeQuantize = \r -> if (r == 0) then 0 else max (quantize' r) q
        rec = quantizeU q

instance (Eq a) => Quantizable MusicU a where
    quantize :: Rational -> MusicU a -> MusicU a
    quantize = quantizeU

instance (Eq a) => Quantizable Music a where
    quantize :: Rational -> Music a -> Music a
    quantize q = conj $ (quantizeU q)

-- MusicU conversion --

class ToMusicU m a where
    toMusicU :: m a -> MusicU a
    fromMusicU :: MusicU a -> m a

instance ToMusicU MusicU a where
    toMusicU = id
    fromMusicU = id

instance ToMusicU Music a where
    toMusicU = fromMusic
    fromMusicU = toMusic
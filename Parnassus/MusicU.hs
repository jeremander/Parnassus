{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ParallelListComp #-}

module Parnassus.MusicU where

import qualified Data.List (nub)
import Data.Ratio
import Data.Tuple.Select

import Euterpea hiding (chord, dur, line, scaleDurations, toMusic1)
import Parnassus.Utils
import Parnassus.MusicBase


-- unassociative music data structure (isomorphic to Music)
data MusicU a = Empty | PrimU (Primitive a) | SeqU [MusicU a] | ParU [MusicU a] | ModifyU Control (MusicU a)
    deriving (Show, Eq, Ord)

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
mFoldU :: b -> (Primitive a -> b) -> ([b] -> b) -> ([b] -> b) -> (Control -> b -> b) -> MusicU a -> b
mFoldU empty f seqFold parFold g m = case m of
    Empty        -> empty
    PrimU p      -> f p
    SeqU ms      -> seqFold (map rec ms)
    ParU ms      -> parFold (map rec ms)
    ModifyU c m' -> g c (rec m')
    where
        rec = mFoldU empty f seqFold parFold g

instance MusicT MusicU a where
    fromMusic :: Music a -> MusicU a
    fromMusic = mFold prim (/+/) (/=/) ModifyU
    toMusic :: MusicU a -> Music a
    toMusic = mFoldU (Prim $ Rest 0) Prim (foldr1 (:+:)) (foldr1 (:=:)) Modify
    prim :: Primitive a -> MusicU a
    prim (Rest d)   = if (d <= 0) then Empty else (PrimU $ Rest d)
    prim (Note d p) = if (d <= 0) then Empty else (PrimU $ Note d p)
    modify :: Control -> MusicU a -> MusicU a
    modify = ModifyU
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
    isEmpty Empty         = True
    isEmpty (ModifyU _ m) = isEmpty m
    isEmpty _             = False
    durGCD :: MusicU a -> Rational
    durGCD = mFoldU 0 durP (foldr rationalGCD 1) (foldr rationalGCD 1) (curry snd)
    bisect :: Eq a => Dur -> MusicU a -> (MusicU a, MusicU a)
    bisect d m | d <= 0            = (Empty, m)
    bisect _ Empty                 = (Empty, Empty)
    bisect d (PrimU (Note oldD p)) = (prim $ Note (min oldD d) p, prim $ Note (oldD - d) p)
    bisect d (PrimU (Rest oldD))   = (prim $ Rest (min oldD d), prim $ Rest (oldD - d))
    bisect d (SeqU ms)             = (line left', line right')
        where
            durs = dur <$> ms
            cumDurs = scanl (+) 0 durs
            items = zip3 (tail cumDurs) durs ms
            (items1, items2) = span (\(cd, _, _) -> cd - d <= 0) items
            emptyRight = null items2
            (cd', d', mid) = if emptyRight then (0, 0, Empty) else (head items2)
            (midLeft, midRight) = bisect (d' + d - cd') mid
            (left, right) = (sel3 <$> items1, sel3 <$> items2)
            left' = if (isEmpty midLeft) then left else (left ++ [midLeft])
            right' = midRight : (if emptyRight then [] else tail right)
    bisect d (ParU ms)             = (chord lefts, chord rights)
        where
            (lefts, rights) = unzip $ (bisect d) <$> ms
    bisect d (ModifyU (Tempo r) m) = (ModifyU (Tempo r) mhead, ModifyU (Tempo r) mtail)
        where (mhead, mtail) = bisect (d * r) m
    bisect d (ModifyU ctl m)        = (ModifyU ctl mhead, ModifyU ctl mtail)
        where (mhead, mtail) = bisect d m
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
    stripControls = mFoldU ([], Empty) f (combine SeqU) (combine ParU) g
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
                ms' = [((foldr (.) id) $ (map ModifyU) $ ctl') $ m | ctl' <- ctls' | m <- ms]
    removeTempos :: MusicU a -> MusicU a
    removeTempos = mFoldU Empty PrimU SeqU ParU g
        where
            g :: Control -> MusicU a -> MusicU a
            g (Tempo d) m = m
            g ctl m = ModifyU ctl m
    distributeTempos :: MusicU a -> MusicU a
    distributeTempos = mFoldU Empty prim SeqU ParU g
        where
            g :: Control -> MusicU a -> MusicU a
            g (Tempo t) m = scaleDurations t m
            g ctl m = ModifyU ctl m
    -- transposes the music by some interval
    transpose :: AbsPitch -> MusicU a -> MusicU a
    transpose i = ModifyU (Transpose i)

instance ToMidi MusicU

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
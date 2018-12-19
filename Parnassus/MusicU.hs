{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ParallelListComp #-}

module Parnassus.MusicU where

import qualified Data.List (nub)
import Data.Ratio

import Euterpea hiding (chord, cut, dur, line, scaleDurations)
import Parnassus.MusicBase


-- unassociative music data structure (isomorphic to Music)
data MusicU a = Empty | PrimU (Primitive a) | SeqU [MusicU a] | ParU [MusicU a] | ModifyU Control (MusicU a)
    deriving (Show, Eq, Ord)

type MusicU1 = MusicU Note1

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
mCombine :: Eq a => (MusicU a -> [MusicU a]) -> ([MusicU a] -> MusicU a) -> [MusicU a] -> MusicU a
mCombine f con = combine
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

instance MusicT MusicU where
    fromMusic :: Music a -> MusicU a
    fromMusic = mFold prim (/+/) (/=/) ModifyU
    toMusic :: MusicU a -> Music a
    toMusic = mFoldU Prim (foldr1 (:+:)) (foldr1 (:=:)) Modify
    prim :: Primitive a -> MusicU a
    prim (Rest d)   = if (d <= 0) then Empty else (PrimU $ Rest d)
    prim (Note d p) = if (d <= 0) then Empty else (PrimU $ Note d p)
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
        where combine = mCombine unLine SeqU
    -- TODO: remove rests shorter than duration
    chord :: Eq a => [MusicU a] -> MusicU a
    chord ms = combine $ Data.List.nub $ filter (not . isEmpty) $ concatMap unChord ms
        where combine = mCombine unChord ParU
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

-- quantizes a sequences of rationals to have denominator d, but attempts to prevent nonzero durations from being truncated to zero
safeQuantizeSeq :: Integer -> [Dur] -> [Dur]
safeQuantizeSeq d rs = newDurs
    where
        -- algorithm that, given a list of integers, attempts to add one to any zeros, with the caveat that they must be subtracted from the largest values
        fixVals :: [Int] -> [Int]
        fixVals xs = fixVals' deficit xs
            where
                deficit = sum $ map fromEnum [x <= 0 | x <- xs]
                fixVals' 0 ys = ys
                fixVals' d ys = if (maximum ys <= 1)
                    then ys -- can't change anything
                    else fixVals' (d - 1) [y + f j | (j, y) <- zip [0..] ys]
                            where
                                imin = argmin ys
                                imax = argmax ys
                                f = \j -> if (j == imin) then 1 else if (j == imax) then -1 else 0
        q = 1 % d
        cumDurs = scanl (+) 0 rs
        qCumDurs = map (quantize d) cumDurs
        qDurs = map (uncurry (-)) (zip (tail qCumDurs) qCumDurs)
        qNumerators = map (floor . (* fromIntegral d)) qDurs
        newDurs = map ((* q) . fromIntegral) (fixVals qNumerators)
        -- TODO: distinguish ACTUAL zeros from rounded zeroes

-- quantizes MusicU so that all durations are multiples of (1 / d)
quantizeU :: Eq a => Integer -> MusicU a -> MusicU a
quantizeU d m = case m of
    Empty        -> Empty
    PrimU p      -> prim p
    SeqU ms      -> line [fit qd m' | qd <- qDurs | m' <- ms']
        where
            ms' = map rec ms
            -- qDurs = safeQuantizeSeq d (dur <$> ms')
            qDurs = quantizeRationals d (dur <$> ms')
    ParU ms      -> chord $ map rec ms
    ModifyU c m' -> ModifyU c (rec m')
    where
        quantize' = quantize d
        minQuant = 1 % d
        safeQuantize = \r -> if (r == 0) then 0 else max (quantize' r) minQuant
        rec = quantizeU d

-- Type class for converting to/from MusicU
class ToMusicU m where
    -- converts to MusicU
    toMusicU :: m a -> MusicU a
    -- converts from MusicU
    fromMusicU :: MusicU a -> m a

instance ToMusicU MusicU where
    toMusicU = id
    fromMusicU = id

instance ToMusicU Music where
    toMusicU = fromMusic
    fromMusicU = toMusic
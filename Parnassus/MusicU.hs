{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ParallelListComp #-}

module Parnassus.MusicU where

import qualified Data.List (nub)
import Data.Ratio

import Euterpea hiding (cut, dur, scaleDurations)
import Parnassus.MusicBase


-- unassociative music data structure (isomorphic to Music)
data MusicU a = Empty | PrimU (Primitive a) | Seq [MusicU a] | Par [MusicU a] | ModifyU Control (MusicU a)
    deriving (Show, Eq, Ord)

infixr 5 /+/, /=/

-- combines together MusicU objects in series
(/+/) :: MusicU a -> MusicU a -> MusicU a
(/+/) m1 Empty = m1
(/+/) Empty m2 = m2
(/+/) m1 m2 = Seq (extractSeq m1 ++ extractSeq m2)
    where
        extractSeq :: MusicU a -> [MusicU a]
        extractSeq m = case m of
            Seq ms    -> ms
            otherwise -> [m]

-- combines together MusicU objects in parallel
(/=/) :: MusicU a -> MusicU a -> MusicU a
(/=/) m1 Empty = m1
(/=/) Empty m2 = m2
(/=/) m1 m2 = Par (extractPar m1 ++ extractPar m2)
    where
        extractPar :: MusicU a -> [MusicU a]
        extractPar m = case m of
            Par ms    -> ms
            otherwise -> [m]

isEmpty :: MusicU a -> Bool
isEmpty Empty = True
isEmpty _     = False

-- splits MusicU into separate chords in sequence
unLineU :: Eq a => MusicU a -> [MusicU a]
unLineU (Seq ms) = ms
unLineU (ModifyU ctl m) = (ModifyU ctl) <$> (unLineU m)
unLineU m = [m]

-- splits MusicU into separate lines in parallel
unChordU :: Eq a => MusicU a -> [MusicU a]
unChordU (Par ms) = ms
unChordU (ModifyU ctl m) = (ModifyU ctl) <$> (unChordU m)
unChordU m = [m]

-- strips away outer control, returning the control (if present) and the remaining MusicU
stripControlU :: MusicU a -> (Maybe Control, MusicU a)
stripControlU (ModifyU ctl m) = (Just ctl, m)
stripControlU m = (Nothing, m)

-- smart constructors

-- smart musical combinator for both Seq and Par
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

mSeq :: Eq a => [MusicU a] -> MusicU a
mSeq ms = combine $ filter (not . isEmpty) $ concatMap unLineU ms
    where combine = mCombine unLineU Seq

-- TODO: remove rests shorter than duration

mPar :: Eq a => [MusicU a] -> MusicU a
mPar ms = combine $ Data.List.nub $ filter (not . isEmpty) $ concatMap unChordU ms
    where combine = mCombine unChordU Par

-- general fold for MusicU
mFoldU :: (Primitive a -> b) -> (b -> b -> b) -> (b -> b -> b) -> (Control -> b -> b) -> MusicU a -> b
mFoldU f (+/) (=/) g m = case m of
    PrimU p      -> f p
    Seq ms       -> foldr1 (+/) (map rec ms)
    Par ms       -> foldr1 (=/) (map rec ms)
    ModifyU c m' -> g c (rec m')
    where
        rec = mFoldU f (+/) (=/) g

instance MusicT MusicU where
    fromMusic :: Music a -> MusicU a
    fromMusic = mFold mkPrimU (/+/) (/=/) ModifyU
        where
            mkPrimU :: Primitive a -> MusicU a
            mkPrimU (Rest 0) = Empty
            mkPrimU p        = PrimU p
    toMusic :: MusicU a -> Music a
    toMusic = mFoldU Prim (:+:) (:=:) Modify
    dur :: MusicU a -> Dur
    dur Empty = 0
    dur (PrimU (Note d _)) = d
    dur (PrimU (Rest d)) = d
    dur (Seq ms) = sum (map dur ms)
    dur (Par ms) = maximum (map dur ms)
    dur (ModifyU (Tempo r) m) = dur m / r
    dur (ModifyU _ m) = dur m
    durLCD :: MusicU a -> Integer
    durLCD = mFoldU f lcm lcm (curry snd)
        where
            f :: Primitive a -> Integer
            f (Rest d) = denominator d
            f (Note d _) = denominator d
    cut :: Eq a => Dur -> MusicU a -> MusicU a
    cut d m | d <= 0            = Empty
    cut _ Empty                 = Empty
    cut d (PrimU (Note oldD p)) = PrimU $ Note (min oldD d) p
    cut d (PrimU (Rest oldD))   = PrimU $ Rest (min oldD d)
    cut d (Seq ms)              = mSeq [cut maxDur m' | (m', maxDur) <- zip ms maxDurs, maxDur > 0]
        where
            cumDurs = scanl (+) 0 (map dur ms)
            maxDurs = [(d - cumDur) | cumDur <- cumDurs]
    cut d (Par ms)              = mPar ((cut d) <$> ms)
    cut d (ModifyU (Tempo r) m) = ModifyU (Tempo r) (cut (d * r) m)
    cut d (ModifyU c m)         = ModifyU c (cut d m)
    pad :: Eq a => Dur -> MusicU a -> MusicU a
    pad d Empty                 = PrimU $ Rest d
    pad d (PrimU (Note oldD p)) = PrimU $ Note (max oldD d) p
    pad d (PrimU (Rest oldD))   = PrimU $ Rest (max oldD d)
    pad d m@(Seq ms)            = mSeq (ms ++ filter (\_ -> (diff > 0)) [PrimU $ Rest diff])
        where diff = d - dur m
    pad d (Par ms)              = mPar ((pad d) <$> ms)
    pad d (ModifyU (Tempo r) m) = ModifyU (Tempo r) (pad (d * r) m)
    pad d (ModifyU c m)         = ModifyU c (pad d m)
    stripTempos :: MusicU a -> MusicU a
    stripTempos = mFoldU PrimU (/+/) (/=/) g
        where
            g :: Control -> MusicU a -> MusicU a
            g (Tempo d) m = m
            g ctl m = ModifyU ctl m
    distributeTempos :: MusicU a -> MusicU a
    distributeTempos = mFoldU PrimU (/+/) (/=/) g
        where
            g :: Control -> MusicU a -> MusicU a
            g (Tempo t) m = scaleDurations t m
            g ctl m = ModifyU ctl m

type MusicU1 = MusicU Note1

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
    PrimU p      -> PrimU p
    Seq ms       -> mSeq [fit qd m' | qd <- qDurs | m' <- ms']
        where
            ms' = map rec ms
            -- qDurs = safeQuantizeSeq d (dur <$> ms')
            qDurs = quantizeRationals d (dur <$> ms')
    Par ms       -> mPar $ map rec ms
    ModifyU c m' -> ModifyU c (rec m')
    where
        quantize' = quantize d
        minQuant = 1 % d
        safeQuantize = \r -> if (r == 0) then 0 else max (quantize' r) minQuant
        rec = quantizeU d
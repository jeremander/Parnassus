{-# LANGUAGE InstanceSigs #-}

module Parnassus.MusicU where

import qualified Data.List (nub)

import Euterpea
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
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ParallelListComp #-}
{-# LANGUAGE StandaloneDeriving #-}

module Parnassus.MusicBase where

import Codec.Midi
import Control.DeepSeq (NFData)
import Data.Either (partitionEithers)
import qualified Data.List (transpose)
import Data.Ratio

import Euterpea hiding (chord, cut, dur, line, play, scaleDurations, transpose)
import qualified Euterpea
import qualified Euterpea.IO.MIDI.FromMidi
import qualified Euterpea.IO.MIDI.ToMidi

-- Types

type Controls = [Control]

deriving instance Ord NoteAttribute

-- Utilities --

composeFuncs :: [(a -> a)] -> a -> a
composeFuncs = foldr (.) id

-- pads a list to a certain length with a default value
padListWithDefault :: Int -> a -> [a] -> [a]
padListWithDefault n def xs = take n (xs ++ repeat def)

-- transposes a list of lists of varying length, padding any short lists with a default value
transposeWithDefault :: a -> [[a]] -> [[a]]
transposeWithDefault def xss = Data.List.transpose mat
    where
        maxlen = maximum (length <$> xss)
        mat = padListWithDefault maxlen def <$> xss

argmax, argmin :: Ord a => [a] -> Int
argmax xs = head [i | (x, i) <- zip xs [0..], x == maximum xs]
argmin xs = head [i | (x, i) <- zip xs [0..], x == minimum xs]

-- quantizes a rational r to the nearest rational with denominator d
quantize :: RealFrac a => Integer -> a -> Rational
quantize d r = round (r * fromIntegral d) % d

-- given denominator d, quantizes a sequences of rationals to have denominator d, with as little perturbation as possible
quantizeRationals :: Integer -> [Rational] -> [Rational]
quantizeRationals d rs = rs'
    where
        q = 1 % d
        qrs = [if (r == 0) then r else max q (quantize d r) | r <- rs]  -- try to round away from 0
        -- qrs = [if (r < q / 2) then r else max q (quantize d r) | r <- rs]  -- try to round away from 0
        totalDiff = (sum rs) - (sum qrs)
        diffs = zipWith (-) rs qrs
        step :: (Rational, [Rational]) -> (Rational, [Rational])
        step (t, xs) = (t + amt, [if (j == i) then x + amt else x | x <- xs | j <- [0..]])
            where
                (imin, imax) = (argmin xs, argmax xs)
                (xmin, xmax) = (xs !! imin, xs !! imax)
                (i, amt) = if (t > 0)  -- have a surplus
                                then (imin, -q)
                                else (imax, q)
        stepSeq = iterate step (totalDiff, diffs)
        pairSeq = zip stepSeq (tail stepSeq)
        dropCondition = \((t1, _), (t2, _)) -> (t1 /= t2) && (abs t1 > (q / 2))
        ((_, finalDiffs), (_, _)) = head $ dropWhile dropCondition pairSeq
        rs' = zipWith (-) rs finalDiffs

-- given a list of lists of elements, strips off the largest common prefix
unDistribute :: Eq a => [[a]] -> ([a], [[a]])
unDistribute [] = ([], [])
unDistribute xs = (prefix, (drop prefixLength) <$> xs)
    where
        longestPrefix _ [] = []
        longestPrefix [] _ = []
        longestPrefix (x:xtail) (y:ytail) = if (x == y) then [x] ++ longestPrefix xtail ytail else []
        prefix = foldr1 longestPrefix xs
        prefixLength = length prefix

-- functions for Music type

unChord' :: Music a -> [Music a]
unChord' (x :=: y) = unChord' x ++ unChord' y
unChord' (Modify ctl x) = map (Modify ctl) (unChord' x)
unChord' x = [x]

unLine' :: Music a -> [Music a]
unLine' (x :+: y) = unLine' x ++ unLine' y
unLine' (Modify ctl x) = map (Modify ctl) (unLine' x)
unLine' x = [x]

pad' :: Dur -> Music a -> Music a
pad' d m
    | diff <= 0 = m
    | otherwise = m :+: (Prim $ Rest diff)
    where diff = d - dur m

lcd' :: Music a -> Integer
lcd' = mFold f lcm lcm (curry snd)
    where
        f :: Primitive a -> Integer
        f (Rest d) = denominator d
        f (Note d _) = denominator d

stripControls' :: Music a -> (Controls, Music a)
stripControls' = mFold f (combine (:+:)) (combine (:=:)) g
    where
        f :: Primitive a -> (Controls, Music a)
        f p = ([], Prim p)
        g :: Control -> (Controls, Music a) -> (Controls, Music a)
        g ctl (ctls, m) = ([ctl] ++ ctls, m)
        combine :: (Music a -> Music a -> Music a) -> (Controls, Music a) -> (Controls, Music a) -> (Controls, Music a)
        combine op (xctl, x) (yctl, y) = (prefix, op x' y')
            where
                (prefix, [xctl', yctl']) = unDistribute [xctl, yctl]
                x' = (foldr (.) id (Modify <$> xctl')) x
                y' = (foldr (.) id (Modify <$> yctl')) y

removeTempos' :: Music a -> Music a
removeTempos' = mFold Prim (:+:) (:=:) g
    where
        g :: Control -> Music a -> Music a
        g (Tempo d) m = m
        g ctl m = Modify ctl m

distributeTempos' :: Music a -> Music a
distributeTempos' = mFold Prim (:+:) (:=:) g
    where
        g :: Control -> Music a -> Music a
        g (Tempo t) m = scaleDurations t m
        g ctl m = Modify ctl m


infixr 5 /+/, /=/

-- Type class for basic music interface
class MusicT m a where
    -- converts to Euterpea's Music type
    toMusic :: m a -> Music a
    -- converts from Euterpea's Music type
    fromMusic :: Music a -> m a
    -- smart constructor out of a Primitive element
    prim :: Primitive a -> m a
    prim = fromMusic . Prim
    -- combines a pair of musical elements in sequence
    (/+/) :: m a -> m a -> m a
    (/+/) m1 m2 = fromMusic $ (:+:) (toMusic m1) (toMusic m2)
    -- combines a pair of musical elements in parallel
    (/=/) :: m a -> m a -> m a
    (/=/) m1 m2 = fromMusic $ (:=:) (toMusic m1) (toMusic m2)
    -- repeats a section of music multiple times
    (/*/) :: m a -> Int -> m a
    (/*/) x n = foldr1 (/+/) (replicate n x)
    -- conjugates an endomorphism on this type to an endomorphism on Music
    conj :: (m a -> m a) -> (Music a -> Music a)
    conj f = toMusic . f . fromMusic
    -- conjugates an endomorphism on Music to an endomorphism on this type
    unConj :: (Music a -> Music a) -> (m a -> m a)
    unConj f = fromMusic . f . toMusic
    -- generalizes conj to functorial input types
    conjF1 :: Functor g => (g (m a) -> m a) -> (g (Music a) -> Music a)
    conjF1 f = toMusic . f . fmap fromMusic
    -- generalizes unConj to functorial input types
    unConjF1 :: Functor g => (g (Music a) -> Music a) -> (g (m a) -> m a)
    unConjF1 f = fromMusic . f . fmap toMusic
    -- generalizes conj to functorial output types
    conjF2 :: Functor g => (m a -> g (m a)) -> (Music a -> g (Music a))
    conjF2 f = fmap toMusic . f . fromMusic
    -- generalizes unConj to functorial output types
    unConjF2 :: Functor g => (Music a -> g (Music a)) -> (m a -> g (m a))
    unConjF2 f = fmap fromMusic . f . toMusic
    -- chains together musical segments in sequence
    line :: Eq a => [m a] -> m a
    line = unConjF1 Euterpea.line
    -- combines musical lines in parallel
    chord :: Eq a => [m a] -> m a
    chord = unConjF1 Euterpea.chord
    -- splits music into time-sequential segments
    unLine :: Eq a => m a -> [m a]
    unLine = unConjF2 unLine'
    -- splits music into separate lines in parallel
    unChord :: Eq a => m a -> [m a]
    unChord = unConjF2 unChord'
    -- play the music (NB: Midi synthesizer like SimpleSynth must be active)
    play :: (NFData a, ToMusic1 a) => m a -> IO ()
    play = Euterpea.play . toMusic
    -- computes the duration of the music
    dur :: m a -> Dur
    dur = Euterpea.dur . toMusic
    -- returns True if the music is empty (has duration 0)
    isEmpty :: m a -> Bool
    isEmpty x = (dur x == 0)
    -- computes the least common denominator of the time intervals occurring in the music, ignoring tempo modifiers
    lcd :: m a -> Integer
    lcd = lcd' . toMusic
    -- scales durations down by a constant
    scaleDurations :: Rational -> m a -> m a
    scaleDurations c = unConj $ Euterpea.scaleDurations c
    -- cuts music to at most the given duration
    cut :: Eq a => Dur -> m a -> m a
    cut d = unConj $ Euterpea.cut d
    -- pads music to at least the given duration
    pad :: Eq a => Dur -> m a -> m a
    pad d = unConj $ pad' d
    -- fits music to equal the given duration, either by padding or by cutting
    fit :: Eq a => Dur -> m a -> m a
    fit d m
        | diff > 0  = pad d m
        | diff < 0  = cut d m
        | otherwise = m
        where diff = d - dur m
    -- strips off outer level controls, returning the controls as a list, and the stripped music
    stripControls :: m a -> (Controls, m a)
    stripControls = unConjF2 stripControls'
    -- eliminates all tempo modifiers
    removeTempos :: m a -> m a
    removeTempos = unConj removeTempos'
    -- applies tempo modifiers to note/rest durations, eliminating the modifiers
    distributeTempos :: m a -> m a
    distributeTempos = unConj distributeTempos'
    -- transposes the music by some interval
    transpose :: AbsPitch -> m a -> m a
    transpose i = unConj $ Euterpea.transpose i

class (MusicT m Note1) => ToMidi m where
    -- constructs from Midi
    fromMidi :: Codec.Midi.Midi -> m Note1
    fromMidi = fromMusic . Euterpea.IO.MIDI.FromMidi.fromMidi
    -- constructs from Midi file
    fromMidiFile :: FilePath -> IO (m Note1)
    fromMidiFile path = Parnassus.MusicBase.fromMidi . head . snd . partitionEithers . pure <$> importFile path
    -- creates a Midi
    toMidi :: m Note1 -> Codec.Midi.Midi
    toMidi = Euterpea.IO.MIDI.ToMidi.toMidi . perform . toMusic
    -- writes Midi to a file
    toMidiFile :: m Note1 -> FilePath -> IO ()
    toMidiFile m path = exportMidiFile path $ Parnassus.MusicBase.toMidi m

-- make Music a trivial instance of MusicT
instance () => MusicT Music a where
    toMusic = id
    fromMusic = id
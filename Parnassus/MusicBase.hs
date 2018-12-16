{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ParallelListComp #-}

module Parnassus.MusicBase where

import Codec.Midi
import Control.DeepSeq (NFData)
import Data.Either (partitionEithers)
import Data.Ratio

import Euterpea hiding (cut, dur, play, scaleDurations)
import qualified Euterpea
import qualified Euterpea.IO.MIDI.FromMidi


-- Utilities --

argmax :: Ord a => [a] -> Int
argmax xs = head [i | (x, i) <- zip xs [0..], x == maximum xs]

argmin :: Ord a => [a] -> Int
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

durLCD' :: Music a -> Integer
durLCD' = mFold f lcm lcm (curry snd)
    where
        f :: Primitive a -> Integer
        f (Rest d) = denominator d
        f (Note d _) = denominator d

stripTempos' :: Music a -> Music a
stripTempos' = mFold Prim (:+:) (:=:) g
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


-- Type class for basic music interface
class MusicT m where
    -- converts to Euterpea's Music type
    toMusic :: m a -> Music a
    -- converts from Euterpea's Music type
    fromMusic :: Music a -> m a
    -- constructs from Midi
    fromMidi :: Codec.Midi.Midi -> m Note1
    fromMidi = fromMusic . Euterpea.IO.MIDI.FromMidi.fromMidi
    -- constructs from Midi file
    fromMidiFile :: FilePath -> IO (m Note1)
    fromMidiFile path = Parnassus.MusicBase.fromMidi . head . snd . partitionEithers . return <$> importFile path
    -- TODO: toMidi
    -- conjugates an endomorphism on this type to an endomorphism on Music
    conj :: (m a -> m a) -> (Music a -> Music a)
    conj f = toMusic . f . fromMusic
    -- conjugates an endomorphism on Music to an endomorphism on this type
    unConj :: (Music a -> Music a) -> (m a -> m a)
    unConj f = fromMusic . f . toMusic
    -- generalizes conj to functorial output types
    conjF :: Functor g => (m a -> g (m a)) -> (Music a -> g (Music a))
    conjF f = fmap toMusic . f . fromMusic
    -- generalizes unConj to functorial output types
    unConjF :: Functor g => (Music a -> g (Music a)) -> (m a -> g (m a))
    unConjF f = fmap fromMusic . f . toMusic
    -- splits Music into time-sequential segments
    unLine :: Eq a => m a -> [m a]
    unLine = unConjF unLine'
    -- splits music into separate lines in parallel
    unChord :: Eq a => m a -> [m a]
    unChord = unConjF unChord'
    -- play the music (NB: Midi synthesizer like SimpleSynth must be active)
    play :: (NFData a, ToMusic1 a) => m a -> IO ()
    play = Euterpea.play . toMusic
    -- computes the duration of the music
    dur :: m a -> Dur
    dur = Euterpea.dur . toMusic
    -- computes the least common denominator of the time intervals occurring in the music, ignoring tempo modifiers
    durLCD :: m a -> Integer
    durLCD = durLCD' . toMusic
    -- scales durations by a constant
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
    -- strips away all tempo modifiers
    stripTempos :: m a -> m a
    stripTempos = unConj stripTempos'
    -- applies tempo modifiers to note/rest durations, eliminating the modifiers
    distributeTempos :: m a -> m a
    distributeTempos = unConj distributeTempos'



-- make Music a trivial instance of MusicT
instance MusicT Music where
    toMusic = id
    fromMusic = id
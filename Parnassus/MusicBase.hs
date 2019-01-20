{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}

module Parnassus.MusicBase where

import Codec.Midi
import Control.DeepSeq (NFData)
import Data.Either (partitionEithers)
import Data.List (partition)
import Data.Ratio

import Euterpea hiding (chord, cut, dur, line, play, scaleDurations, toMusic1, transpose)
import qualified Euterpea
import qualified Euterpea.IO.MIDI.FromMidi
import qualified Euterpea.IO.MIDI.ToMidi
import Parnassus.Utils

-- Types

type Controls = [Control]
type TimeSig = (Int, Int)

class Pitched a where
    getPitch :: a -> Pitch

instance Pitched Pitch where
    getPitch = id

instance Pitched Note1 where
    getPitch = fst

deriving instance Ord NoteAttribute

-- functions for Music type

-- duration of a Primitive
durP :: Primitive a -> Dur
durP p = case p of
    Rest d   -> d
    Note d _ -> d

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
    where diff = d - Euterpea.dur m

gcd' :: Music a -> Rational
gcd' = mFold durP rationalGCD rationalGCD (curry snd)

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
        g (Tempo t) m = Euterpea.scaleDurations t m
        g ctl m = Modify ctl m

infixr 5 /+/, /=/

-- Type class for basic music interface
class MusicT m a where
    -- converts to Euterpea's Music type
    toMusic :: m a -> Music a
    -- converts from Euterpea's Music type
    fromMusic :: Music a -> m a
    -- if possible, convert to Music1
    toMusic1 :: ToMusic1 a => m a -> Music1
    toMusic1 = Euterpea.toMusic1 . toMusic
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
    -- play the music (NB: Midi synthesizer like SimpleSynth must be active)
    play :: (NFData a, ToMusic1 a) => m a -> IO ()
    play = Euterpea.play . toMusic
    -- smart constructor out of a Primitive element
    prim :: Primitive a -> m a
    prim = fromMusic . Prim
    -- modifies the music with a Control
    modify :: Control -> m a -> m a
    modify ctl = unConj $ Modify ctl
    -- combines a pair of musical elements in sequence
    (/+/) :: m a -> m a -> m a
    (/+/) m1 m2 = fromMusic $ (:+:) (toMusic m1) (toMusic m2)
    -- combines a pair of musical elements in parallel
    (/=/) :: m a -> m a -> m a
    (/=/) m1 m2 = fromMusic $ (:=:) (toMusic m1) (toMusic m2)
    -- repeats a section of music multiple times
    (/*/) :: m a -> Int -> m a
    (/*/) x n = foldr1 (/+/) (replicate n x)
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
    -- computes the duration of the music
    dur :: m a -> Dur
    dur = Euterpea.dur . toMusic
    -- returns True if the music is empty (has duration 0)
    isEmpty :: m a -> Bool
    isEmpty x = (dur x == 0)
    -- computes the least common denominator of the time intervals occurring in the music, ignoring tempo modifiers
    durGCD :: m a -> Rational
    durGCD = gcd' . toMusic
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
    -- strips off outer level tempos, aggregating them into one tempo; returns this tempo and the stripped music
    stripTempo :: m a -> (Control, m a)
    stripTempo x = (tempo, ctlMod x')
        where
            (ctls, x') = stripControls x
            isTempo :: Control -> Bool
            isTempo (Tempo _) = True
            isTempo _         = False
            extractTempo :: Control -> Rational
            extractTempo (Tempo t) = t
            extractTempo _         = 1
            (tempos, nonTempos) = partition isTempo ctls
            tempo = Tempo $ foldr (*) 1 (extractTempo <$> tempos)
            ctlMod = composeFuncs (modify <$> nonTempos)
    -- eliminates all tempo modifiers
    removeTempos :: m a -> m a
    removeTempos = unConj removeTempos'
    -- applies tempo modifiers to note/rest durations, eliminating the modifiers
    distributeTempos :: m a -> m a
    distributeTempos = unConj distributeTempos'
    -- transposes the music by some interval
    transpose :: AbsPitch -> m a -> m a
    transpose i = unConj $ Euterpea.transpose i

-- instantiate ToMidi m for ToMusic m Note1
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

class Quantizable m a where
    -- quantizes the music so that every note/rest is a multiple of the given duration
    -- NB: convention will be to ignore tempo modifiers
    quantize :: Dur -> m a -> m a
    -- splits the music into segments of the same length
    split :: Dur -> m a -> [m a]
    -- changes the time signature of the music
    changeTimeSig :: TimeSig -> TimeSig -> m a -> m a

-- class instances for Music

instance MusicT Music a where
    toMusic = id
    fromMusic = id

instance ToMidi Music


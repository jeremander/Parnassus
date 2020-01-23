{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}


module Music.Types.MusicT (
    Controls,
    MusicT(..),
    Quantizable(..),
    ToMidi(..),
    changeTimeSig,
    durP,
    extractTempo,
    metronome,
    withMetronome
) where

import qualified Codec.Midi
import Control.DeepSeq (NFData)
import Data.Default (def)
import Data.Either (partitionEithers)
import Data.List (partition)
import Data.Ratio
import Data.VectorSpace (AdditiveGroup(..))
import qualified Euterpea
import Euterpea (AbsPitch, Control(..), Dur, InstrMap, InstrumentName(..), Music(..), Music1(..), Note1, NoteAttribute(..), Pitch, PitchClass(..), Primitive(..), ToMusic1, absPitch, exportMidiFile, instrument, mFold, note, perform, pitch, rest, tempo, writeWavNorm)
import qualified Euterpea.IO.MIDI.FromMidi
import qualified Euterpea.IO.MIDI.ToMidi

import Misc.Utils (composeFuncs, rationalGCD, unDistribute)
import Music.Pitch (Key, FromPitch(..), ToPitch(..), simplifyMode)
import Music.Rhythm (TimeSig)
import Music.Wave (AudSig, sineInstrMap)


type Controls = [Control]

deriving instance Ord NoteAttribute

-- functions for Music type

-- duration of a Primitive
durP :: Primitive a -> Dur
durP p = case p of
    Rest d   -> d
    Note d _ -> d

-- Gets the tempo from a Tempo control (1 if not a tempo)
extractTempo :: Control -> Rational
extractTempo (Tempo t) = t
extractTempo _         = 1

-- fix Euterpea's chord function to avoid creating empty rests
chord' :: [Music a] -> Music a
chord' (x:[]) = x
chord' xs = Euterpea.chord xs

-- fix Euterpea's line function to avoid creating empty rests
line' :: [Music a] -> Music a
line' (x:[]) = x
line' xs = Euterpea.line xs

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
infixr 7 *^
infixl 7 ^*

-- Type class for basic music interface
class MusicT m a where
    -- converts to Euterpea's Music type
    toMusic :: m a -> Music a
    -- converts from Euterpea's Music type
    fromMusic :: Music a -> m a
    fromMusic = mFold prim (/+/) (/=/) modify
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
    -- empty element (duration 0)
    empty :: m a
    empty = fromMusic $ Prim $ Rest 0
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
    line = unConjF1 line'
    -- combines musical lines in parallel
    chord :: Eq a => [m a] -> m a
    chord = unConjF1 chord'
    -- splits music into time-sequential segments
    unLine :: m a -> [m a]
    unLine = unConjF2 unLine'
    -- splits music into separate lines in parallel
    unChord :: m a -> [m a]
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
    -- scales durations by a constant (pre-multiplication)
    (*^) :: Rational -> m a -> m a
    (*^) 0 x = empty
    (*^) c x = unConj (Euterpea.scaleDurations c) x
    -- scales durations by a constant (post-multiplication)
    (^*) :: m a -> Rational -> m a
    (^*) = flip (*^)
    -- bisects music into two sections
    bisect :: Eq a => Dur -> m a -> (m a, m a)
    -- splits the music into segments of the same length
    -- by default, does this by recursively calling 'bisect'
    split :: Eq a => Rational -> m a -> [m a]
    split d mus = fst <$> takeWhile (\(prev, _) -> not $ isEmpty prev) (tail pairs)
        where pairs = (empty, mus) : [bisect d prev | (_, prev) <- pairs]
    -- cuts music to at most the given duration
    cut :: Eq a => Dur -> m a -> m a
    cut d = fst . bisect d
    -- removes some duration from the start of the music
    remove :: Eq a => Dur -> m a -> m a
    remove d = snd . bisect d
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
    fromMidiFile path = fromMidi . head . snd . partitionEithers . pure <$> Codec.Midi.importFile path
    -- creates a Midi
    toMidi :: m Note1 -> Codec.Midi.Midi
    toMidi = Euterpea.IO.MIDI.ToMidi.toMidi . perform . toMusic
    -- writes Midi to a file
    toMidiFile :: m Note1 -> FilePath -> IO ()
    toMidiFile m path = exportMidiFile path $ toMidi m

instance {-# OVERLAPPABLE #-} (FromPitch a, MusicT m a) => FromPitch (m a) where
    fromPitch :: Pitch -> m a
    fromPitch p = prim $ Note (1 % 4) (fromPitch p)

-- class instances for Music

instance MusicT Music a where
    toMusic = id
    fromMusic = id
    bisect :: Eq a => Dur -> Music a -> (Music a, Music a)
    bisect d m | d <= 0             = (rest 0, m)
    bisect d m@(Prim (Note oldD p)) = if (d < oldD) then (note d p, note (oldD - d) p) else (m, rest 0)
    bisect d m@(Prim (Rest oldD))   = if (d < oldD) then (rest d, rest (oldD - d)) else (m, rest 0)
    bisect d (m1 :+: m2)
        | d == d1 = (m1, m2)
        | d < d1  = (m1head, m1tail :+: m2)
        | d > d1  = (m1 :+: m2head, m2tail)
        where
            d1 = dur m1
            (m1head, m1tail) = bisect d m1
            (m2head, m2tail) = bisect (d - d1) m2
    bisect d (m1 :=: m2)            = (m1head :=: m2head, m1tail :=: m2tail)
        where
            (m1head, m1tail) = bisect d m1
            (m2head, m2tail) = bisect d m2
    bisect d (Modify (Tempo r) m)   = (tempo r mhead, tempo r mtail)
        where (mhead, mtail) = bisect (d * r) m
    bisect d (Modify ctl m)         = (Modify ctl mhead, Modify ctl mtail)
        where (mhead, mtail) = bisect d m

instance ToMidi Music

-- * Metronome

-- | Metronome for a fixed time signature.
-- Uses claves for downbeat, high wood block for upbeats.
metronome :: (MusicT m Note1) => TimeSig -> Dur -> m Note1
metronome (n, d) r = modify (Instrument Percussion) $ line $ (prim <$> beats)
    where
        numBeats = ceiling (r * fromIntegral d)
        flags = [rem i n == 0 | i <- [0..]]
        beats = take numBeats [Note (1 % toInteger d) (if flag then ((Ds, 5), [Volume 85]) else ((E, 5), [Volume 60])) | flag <- flags]

-- | Overlays music with a metronome.
withMetronome :: (MusicT m Note1) => TimeSig -> m Note1 -> m Note1
withMetronome ts mus = mus /=/ metro'
    where
        (tempo, mus') = stripTempo mus
        d = dur mus'
        metro = metronome ts d
        -- modify metronome's tempo with the music's global tempo
        metro' = distributeTempos $ modify tempo metro


-- * Quantization

class Quantizable m a where
    -- quantizes the music so that every note/rest is a multiple of the given duration
    -- NB: convention will be to ignore tempo modifiers
    quantize :: Dur -> m a -> m a

-- | Changes time signature of the music.
changeTimeSig :: (MusicT m a, Quantizable m a, Eq a) => TimeSig -> TimeSig -> m a -> m a
changeTimeSig (n1, d1) (n2, d2) mus = modify ctl $ line measures'
    where
        r1 = (toInteger n1) % (toInteger d1)
        r2 = (toInteger n2) % (toInteger d2)
        -- ensure quantization is appropriate for measure splitting
        q = durGCD mus
        q' = foldr1 rationalGCD [q, r1, r2]
        mus' = quantize q' mus
        measures = split r1 mus'  -- split the measures
        measures' = quantize q' . ((r2 / r1) *^) <$> measures
        -- measures' = quantize q' . scaleDurations (r1 / r2) <$> measures
        -- rescale tempo externally so that the music's total duration is invariant
        -- (strip this off to make note durations invariant)
        ctl = Tempo (r2 / r1)


-- * WAV conversion

musicToWav :: (MusicT m a, ToMusic1 a) => FilePath -> InstrMap AudSig -> m a -> IO ()
musicToWav path instrMap music = writeWavNorm path instrMap music1
    where
        (instrName, _) = head instrMap
        music1 = instrument instrName $ toMusic1 music

-- saves music to a wav file via the sine instrument
musicToSineWav :: FilePath -> Music1 -> IO ()
musicToSineWav path music = writeWavNorm path sineInstrMap music
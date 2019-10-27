{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}

module Parnassus.Music.MusicBase where

import Codec.Midi hiding (Key)
import Control.DeepSeq (NFData)
import Data.Either (partitionEithers)
import Data.List (partition)
import qualified Data.Map
import Data.Maybe (fromJust)
import Data.Ratio

import qualified Data.Music.Lilypond as LP
import Euterpea hiding (chord, cut, dur, fromMidi, line, play, remove, scaleDurations, toMidi, toMusic1, transpose)
import qualified Euterpea
import qualified Euterpea.IO.MIDI.FromMidi
import qualified Euterpea.IO.MIDI.ToMidi
import Parnassus.Utils

-- Types

type Controls = [Control]
type TimeSig = (Int, Int)
type Key = (PitchClass, Mode)

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

-- Lilypond conversion --

-- converts Euterpea PitchClass to Lilypond PitchName (with Accidental)
noteMapping :: Data.Map.Map PitchClass (LP.PitchName, LP.Accidental)
noteMapping = Data.Map.fromList [(Cff, (LP.C, -2)), (Cf, (LP.C, -1)), (C, (LP.C, 0)), (Dff, (LP.D, -2)), (Cs, (LP.C, 1)), (Df, (LP.D, -1)), (Css, (LP.C, 2)), (D, (LP.D, 0)), (Eff, (LP.E, -2)), (Ds, (LP.D, 1)), (Ef, (LP.E, -1)), (Fff, (LP.F, -2)), (Dss, (LP.D, 2)), (E, (LP.E, 0)), (Ff, (LP.F, -1)), (Es, (LP.E, 1)), (F, (LP.F, 0)), (Gff, (LP.G, -2)), (Ess, (LP.E, 2)), (Fs, (LP.F, 1)), (Gf, (LP.G, -1)), (Fss, (LP.F, 2)), (G, (LP.G, 0)), (Aff, (LP.A, -2)), (Gs, (LP.G, 1)), (Af, (LP.A, -1)), (Gss, (LP.G, 2)), (A, (LP.A, 0)), (Bff, (LP.B, -2)), (As, (LP.A, 1)), (Bf, (LP.B, -1)), (Ass, (LP.A, 2)), (B, (LP.B, 0)), (Bs, (LP.B, 1)), (Bss, (LP.B, 2))]

-- converts a proper mode to the corresponding major or minor key signature
modeToMajMin :: Key -> LP.Music
modeToMajMin (pc, mode) = LP.Key (LP.Pitch (pc'', acc, 0)) mode'
    where
        (pc', mode') = case mode of
            Major      -> (pc, LP.Major)
            Minor      -> (pc, LP.Minor)
            Ionian     -> (pc, LP.Major)
            Dorian     -> (fst $ pitch (absPitch (pc, 4) - 5), LP.Minor)
            Phrygian   -> (fst $ pitch (absPitch (pc, 4) - 7), LP.Minor)
            Lydian     -> (fst $ pitch (absPitch (pc, 4) - 5), LP.Minor)
            Mixolydian -> (fst $ pitch (absPitch (pc, 4) - 7), LP.Major)
            Aeolian    -> (pc, LP.Minor)
            Locrian    -> (fst $ pitch (absPitch (pc, 4) + 1), LP.Major)
            _          -> (pc, LP.Major)
        (pc'', acc) = fromJust $ Data.Map.lookup pc' noteMapping


-- TODO: consolidate, make toPDF for LilyPond
-- Lilypond doesn't allow more than two dots, so we must represent some notes as tied
splitRational :: Rational -> [Rational]
splitRational r = if (n == 0)
                    then []
                    else if (diff == 0) || (diff * 2 == n')
                            then [r]
                            else (n'' % d) : (splitRational $ diff % d)
    where
        (n, d) = (numerator r, denominator r)
        n' = 2 ^ (truncate $ logBase 2 (fromInteger n))
        nPlusHalf = n' + n' `quot` 2
        n'' = if nPlusHalf <= n then nPlusHalf else n'
        diff = n - n''

toLilypond' :: (Pitched a) => Music a -> LP.Music
toLilypond' = mFold f combineSeq combinePar g
    where
        splitNote :: (Rational -> LP.Music) -> Rational -> LP.Music
        splitNote func r = LP.Sequential $ func <$> splitRational r
        f :: (Pitched a) => Primitive a -> LP.Music
        f (Note d x) = LP.Sequential [LP.Note (LP.NotePitch (LP.Pitch (p', acc, oct + 1)) Nothing) (Just (LP.Duration d')) (if i == (length ds) then [] else [LP.Tie]) | (i, d') <- zip [1..] ds]
            where
                (p, oct) = getPitch x
                (p', acc) = fromJust $ Data.Map.lookup p noteMapping
                ds = splitRational d
        f (Rest d) = LP.Sequential [LP.Rest (Just (LP.Duration d')) [] | d' <- splitRational d]
        combineSeq :: LP.Music -> LP.Music -> LP.Music
        combineSeq m1 m2 = LP.sequential m1 m2
        combinePar :: LP.Music -> LP.Music -> LP.Music
        combinePar m1 m2 = LP.simultaneous m1 m2
        g :: Control -> LP.Music -> LP.Music
        g (Tempo r) m         = LP.Sequential [t, m]
            where
                bpm = round $ 120 * r
                t = LP.Tempo Nothing (Just (LP.Duration (1 % 4), bpm))
        g (Transpose d) m     = LP.Transpose (LP.Pitch (LP.C, 0, 4)) (LP.Pitch (p', acc, oct)) m
            where
                (p, oct) = pitch (absPitch (C, 4) + d)
                (p', acc) = fromJust $ Data.Map.lookup p noteMapping
        g (Instrument inst) m = LP.Sequential [LP.Set "Staff.instrumentName" (LP.toValue $ show inst), m]
        g (KeySig p mode) m   = LP.Sequential [key, m]
            where key = modeToMajMin (p, mode)
        g _ m                 = m  -- TODO: PhraseAttribute

infixr 5 /+/, /=/

-- Type class for basic music interface
class MusicT m a where
    -- converts to Euterpea's Music type
    toMusic :: m a -> Music a
    -- converts from Euterpea's Music type
    fromMusic :: Music a -> m a
    -- converts music to a Lilypond object
    toLilypond :: (Pitched a) => m a -> String -> TimeSig -> LP.Lilypond
    toLilypond mus title (n, d) = LP.setHeader hdr $ LP.toLilypond mus'
        where
            mus' = LP.Sequential [LP.Time (toInteger n) (toInteger d), toLilypond' $ toMusic mus]
            hdr = LP.emptyHeader {LP.title = Just $ LP.toValue title}
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
    line = unConjF1 line'
    -- combines musical lines in parallel
    chord :: Eq a => [m a] -> m a
    chord = unConjF1 chord'
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
    -- bisects music into two sections
    bisect :: Eq a => Dur -> m a -> (m a, m a)
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
    fromMidiFile path = fromMidi . head . snd . partitionEithers . pure <$> importFile path
    -- creates a Midi
    toMidi :: m Note1 -> Codec.Midi.Midi
    toMidi = Euterpea.IO.MIDI.ToMidi.toMidi . perform . toMusic
    -- writes Midi to a file
    toMidiFile :: m Note1 -> FilePath -> IO ()
    toMidiFile m path = exportMidiFile path $ toMidi m

class Quantizable m a where
    -- quantizes the music so that every note/rest is a multiple of the given duration
    -- NB: convention will be to ignore tempo modifiers
    quantize :: Dur -> m a -> m a
    -- splits the music into segments of the same length
    split :: Dur -> m a -> [m a]
    -- changes the time signature of the music
    changeTimeSig :: (MusicT m a, Eq a) => TimeSig -> TimeSig -> m a -> m a
    changeTimeSig (n1, d1) (n2, d2) mus = modify ctl $ line measures'
        where
            r1 = (toInteger n1) % (toInteger d1)
            r2 = (toInteger n2) % (toInteger d2)
            -- ensure quantization is appropriate for measure splitting
            q = durGCD mus
            q' = foldr1 rationalGCD [q, r1, r2]
            mus' = quantize q' mus
            measures = split r1 mus'  -- split the measures
            measures' = quantize q' . scaleDurations (r1 / r2) <$> measures
            -- rescale tempo externally so that the music's total duration is invariant
            -- (strip this off to make note durations invariant)
            ctl = Tempo (r2 / r1)


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

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ParallelListComp #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Parnassus.Music where

import Codec.Midi
import Control.DeepSeq (NFData)
import Control.Monad (join)
import Data.Counter (Counter, singleton, union)
import Data.Either
import qualified Data.List (nub, transpose)
import Data.Maybe (isJust, listToMaybe)
import Data.Ratio
import Data.Set (empty, insert, member)
import System.IO.Unsafe
import Euterpea
import Euterpea.IO.MIDI.FromMidi
import Euterpea.IO.MIDI.FromMidi2 (fromMidi2)


-- Midi --

-- loads a midi file, unsafely exiting the IO monad
unsafeLoadMidi :: FilePath -> Midi
unsafeLoadMidi = head . snd . partitionEithers . (:[]) . unsafePerformIO . importFile

-- song paths
vgmusicPath :: String = "/Users/jeremander/Programming/Music/Parnassus/tunes/vgmusic/"
xmasPath :: String = "/Users/jeremander/Programming/Music/Parnassus/tunes/xmas/"

-- songs
zelda = fromMidi $ unsafeLoadMidi $ vgmusicPath ++ "loz_overworld.mid"
zelda2 = fromMidi2 $ unsafeLoadMidi $ vgmusicPath ++ "loz_overworld.mid"
mm3 = fromMidi $ unsafeLoadMidi $ vgmusicPath ++ "mm3_password.mid"
gradius = fromMidi $ unsafeLoadMidi $ vgmusicPath ++ "gradius_stage4.mid"
bells = fromMidi $ unsafeLoadMidi $ xmasPath ++ "ringxms.mid"


-- Utilities --

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

argmax :: Ord a => [a] -> Int
argmax xs = head [i | (x, i) <- zip xs [0..], x == maximum xs]

argmin :: Ord a => [a] -> Int
argmin xs = head [i | (x, i) <- zip xs [0..], x == minimum xs]

-- transposes a list of lists of varying length, padding any short lists with a default value
transposeWithDefault :: a -> [[a]] -> [[a]]
transposeWithDefault def xss = Data.List.transpose mat
    where
        maxlen = maximum (length <$> xss)
        mat = [xs ++ (replicate (maxlen - length xs) def) | xs <- xss] 

-- Time Signature --

type TimeSig = (Int, Int)

getTimeSig :: Midi -> Maybe TimeSig
getTimeSig m = join $ listToMaybe $ filter isJust (getSig <$> msgs)
    where
        msgs = snd <$> tracks m !! 0  -- time signature should appear in the first track if at all
        getSig = \msg -> case msg of
                            TimeSignature num pow _ _ -> Just (num, (2 :: Int) ^ pow)
                            otherwise                 -> Nothing

-- MusicU --

-- unassociative music data structure (isomorphic to Music)
data MusicU a = PrimU (Primitive a) | Seq [MusicU a] | Par [MusicU a] | ModifyU Control (MusicU a)
    deriving (Show, Eq, Ord)

infixr 5 /+/, /=/

-- combines together MusicU objects in series
(/+/) :: MusicU a -> MusicU a -> MusicU a
(/+/) m1 m2 = Seq (extractSeq m1 ++ extractSeq m2)
    where
        extractSeq :: MusicU a -> [MusicU a]
        extractSeq m = case m of
            Seq ms    -> ms
            otherwise -> [m]

-- combines together MusicU objects in parallel
(/=/) :: MusicU a -> MusicU a -> MusicU a
(/=/) m1 m2 = Par (extractPar m1 ++ extractPar m2)
    where
        extractPar :: MusicU a -> [MusicU a]
        extractPar m = case m of
            Par ms    -> ms
            otherwise -> [m]

-- general fold for MusicU
mFoldU :: (Primitive a -> b) -> (b -> b -> b) -> (b -> b -> b) -> (Control -> b -> b) -> MusicU a -> b
mFoldU f (+:) (=:) g m = case m of
    PrimU p      -> f p
    Seq ms       -> foldr1 (+:) (map rec ms)
    Par ms       -> foldr1 (=:) (map rec ms)
    ModifyU c m' -> g c (rec m')
    where
        rec = mFoldU f (+:) (=:) g

-- converts Music to MusicU
mUnassoc :: Music a -> MusicU a
mUnassoc = mFold PrimU (/+/) (/=/) ModifyU

-- converts MusicU to Music
mAssoc :: MusicU a -> Music a
mAssoc = mFoldU Prim (:+:) (:=:) Modify

-- conjugates an endomorphism on Music by mUnassoc/mAssoc, resulting in an endomorphism on MusicU
conj :: (Music a -> Music a) -> (MusicU a -> MusicU a)
conj f = mUnassoc . f . mAssoc

-- conjugates an endomorphism on MusicU by mAssoc/mUnassoc, resulting in an endomorphism on Music
conjU :: (MusicU a -> MusicU a) -> (Music a -> Music a)
conjU f = mAssoc . f . mUnassoc

-- plays MusicU
playU :: (NFData a, ToMusic1 a) => MusicU a -> IO ()
playU = play . mAssoc

-- duration function
durU :: MusicU a -> Dur
durU (PrimU (Note d _)) = d
durU (PrimU (Rest d)) = d
durU (Seq ms) = sum (map durU ms)
durU (Par ms) = maximum (map durU ms)
durU (ModifyU (Tempo r) m) = durU m / r
durU (ModifyU _ m) = durU m

-- splits MusicU into separate lines in parallel
unChordU :: MusicU a -> [MusicU a]
unChordU (Par ms) = ms
unChordU (ModifyU ctl m) = (ModifyU ctl) <$> (unChordU m)
unChordU m = [m]

-- splits MusicU into separate chords in sequence
unLineU :: MusicU a -> [MusicU a]
unLineU (Seq ms) = ms
unLineU (ModifyU ctl m) = map (ModifyU ctl) (unLineU m)
unLineU m = [m]

-- strips away all modifiers from MusicU
unModifyU :: MusicU a -> MusicU a
unModifyU (ModifyU _ m) = m
unModifyU m = m

-- strips away all tempo modifiers from MusicU
unTempoU :: MusicU a -> MusicU a
unTempoU = mFoldU PrimU (/+/) (/=/) g
    where
        g :: Control -> MusicU a -> MusicU a
        g (Tempo d) m = m
        g ctl m = ModifyU ctl m

-- scales durations by a constant
scaleDurationsU :: Rational -> MusicU a -> MusicU a
scaleDurationsU c = conj $ scaleDurations c

-- applies tempo modifiers to note/rest durations, eliminating the modifiers
distributeTempoU :: MusicU a -> MusicU a
distributeTempoU = mFoldU PrimU (/+/) (/=/) g
    where
        g :: Control -> MusicU a -> MusicU a
        g (Tempo t) m = scaleDurationsU t m
        g ctl m = ModifyU ctl m

-- cuts MusicU to at most the given duration
cutU :: Dur -> MusicU a -> MusicU a
cutU d m | d <= 0            = PrimU $ Rest 0
cutU d (PrimU (Note oldD p)) = PrimU $ Note (min oldD d) p
cutU d (PrimU (Rest oldD))   = PrimU $ Rest (min oldD d)
cutU d (Seq ms)              = Seq [cutU maxDur m' | (m', maxDur) <- zip ms maxDurs, maxDur > 0]
    where
        cumDurs = scanl (+) 0 (map durU ms)
        maxDurs = [(d - cumDur) | cumDur <- cumDurs]
cutU d (Par ms)              = Par (map (cutU d) ms)
cutU d (ModifyU (Tempo r) m) = ModifyU (Tempo r) (cutU (d * r) m)
cutU d (ModifyU c m)         = ModifyU c (cutU d m)

-- pads MusicU to at least the given duration
padU :: Dur -> MusicU a -> MusicU a
padU d (PrimU (Note oldD p)) = PrimU $ Note (max oldD d) p
padU d (PrimU (Rest oldD))   = PrimU $ Rest (max oldD d)
padU d m@(Seq ms)              = Seq (ms ++ filter (\_ -> (diff > 0)) [PrimU $ Rest diff])
    where diff = d - durU m
padU d (Par ms)              = Par (map (padU d) ms)
padU d (ModifyU (Tempo r) m) = ModifyU (Tempo r) (padU (d * r) m)
padU d (ModifyU c m)         = ModifyU c (padU d m)

-- fits MusicU to equal the given duration, either by padding or by cutting
fitU :: Dur -> MusicU a -> MusicU a
fitU d m
    | diff > 0  = padU d m
    | diff < 0  = cutU d m
    | otherwise = m
    where diff = d - durU m

-- computes the least common denominator of the time intervals occurring in the music
-- ignores tempo changes
durLCD :: MusicU a -> Integer
durLCD = mFoldU f lcm lcm (curry snd)
    where
        f :: Primitive a -> Integer
        f (Rest d) = denominator d
        f (Note d _) = denominator d

-- returns the LCD of the music durations, along with a counter of the denominators occurring
countDurations :: MusicU a -> (Integer, Counter Integer Int)
countDurations m = (lcd, mFoldU f union union (curry snd) m)
    where
        lcd = durLCD m
        f :: Primitive a -> Counter Integer Int
        f (Rest d) = singleton $ denominator d
        f (Note d _) = singleton $ denominator d

-- quantizes MusicU so that all durations are multiples of (1 / d)
mQuantizeU :: Integer -> MusicU a -> MusicU a
mQuantizeU d m = case m of
    PrimU p      -> PrimU p
    Seq ms       -> Seq [fitU qd m' | qd <- qDurs | m' <- ms']
        where
            ms' = map rec ms
            -- qDurs = safeQuantizeSeq d (map durU ms')
            qDurs = quantizeRationals d (map durU ms')
            -- cumDurs = scanl (+) 0 (map durU ms')  -- cumulative durations
            -- qCumDurs = map safeQuantize cumDurs   -- quantized cumulative durations
            -- qDurs = map (uncurry (-)) (zip (tail qCumDurs) qCumDurs) -- quantized durations
    Par ms       -> Par $ map rec ms
    ModifyU c m' -> ModifyU c (rec m')
    where
        quantize' = quantize d
        minQuant = 1 % d
        safeQuantize = \r -> if (r == 0) then 0 else max (quantize' r) minQuant
        rec = mQuantizeU d



-- strips off all tempo modifications
-- unTempo :: Music a -> Music a
-- unTempo = mFold Prim (:+:) (:=:) g
--     where
--         g :: Control -> Music a -> Music a
--         g (Tempo d) m = m
--         g ctl m = Modify ctl m

-- distributes tempo changes across primitive notes/rests to eliminate the modifier
-- distributeTempo :: Music a -> Music a
-- distributeTempo = mFold Prim (:+:) (:=:) g
--     where
--         g :: Control -> Music a -> Music a
--         g (Tempo t) m = scaleDurations t m
--         g ctl m = Modify ctl m

-- splits Music into separate lines in parallel
-- unChord :: Music a -> [Music a]
-- unChord (x :=: y) = unChord x ++ unChord y
-- unChord (Modify ctl x) = map (Modify ctl) (unChord x)
-- unChord x = [x]

-- splits Music into separate chords in sequence
-- unLine :: Music a -> [Music a]
-- unLine (x :+: y) = unLine x ++ unLine y
-- unLine (Modify ctl x) = map (Modify ctl) (unLine x)
-- unLine x = [x]

-- computes the least common denominator of the time intervals occurring in the music
-- ignores tempo changes
-- mLCD :: Music a -> Integer
-- mLCD = mFold f lcm lcm (curry snd)
--     where
--         f :: Primitive a -> Integer
--         f (Rest d) = denominator d
--         f (Note d _) = denominator d

-- returns the LCD of the music durations, along with a counter of the denominators occurring
-- countDurations :: Music a -> (Integer, Counter Integer Int)
-- countDurations m = (lcd, mFold f union union (curry snd) m)
--     where
--         lcd = mLCD m
--         f :: Primitive a -> Counter Integer Int
--         f (Rest d) = singleton $ denominator d
--         f (Note d _) = singleton $ denominator d

-- pads Music to at least the given duration
-- pad :: Dur -> Music a -> Music a
-- pad d m
--     | diff <= 0 = m
--     | otherwise = m :+: (Prim $ Rest diff)
--     where diff = d - dur m

-- given denominator d, quantizes music so that each segment's duration has this denominator
-- ignores tempo changes
-- mQuantize :: Integer -> Music a -> Music a
-- mQuantize d = padQuantize . mFold Prim qplus (:=:) Modify
--     where 
--         padQuantize :: Music a -> Music a
--         padQuantize m = pad (quantize d (dur m)) m
--         qplus :: Music a -> Music a -> Music a
--         qplus m1 m2 = m1' :+: m2
--             where
--                 d1 = dur m1
--                 d1' = quantize d d1
--                 m1' = if (d1 - d1' >= 0) then (cut d1' m1) else (pad d1' m1)

-- mQuantize' :: Integer -> Music a -> Music a
-- mQuantize' d = conjU (mQuantizeU d)

type Tied a = (a, Bool)
type TiedMusic a = Music (Tied a)
type TiedMusicU a = MusicU (Tied a)

-- TODO: ties will be eliminated
instance ToMusic1 (Tied Note1) where
    toMusic1 = mMap fst

-- given r and d, returns (r // d, r % d)
rationalQuotRem :: Rational -> Rational -> (Int, Rational)
rationalQuotRem r d = (q, r - d * fromIntegral q) where q = floor (r / d)

-- given measure duration, initial duration, and total duration, splits the total duration into a list where the head is at most the initial duration, and each subsequent duration is at most the measure duration
splitDur :: Rational -> Rational -> Rational -> [Rational]
splitDur measureDur _ 0 = []
splitDur measureDur 0 totalDur = replicate q measureDur ++ filter (>0) [rem]
    where (q, rem) = rationalQuotRem totalDur measureDur
splitDur measureDur firstDur totalDur
    | (firstDur >= totalDur) = [totalDur]
    | otherwise       = [firstDur] ++ splitDur measureDur 0 (totalDur - firstDur)

tieFlags :: [Bool]
tieFlags = [False] ++ repeat True

-- zips two lists, padding the shorter of the two with a default value
-- zipWithDefault :: a -> b -> [a] -> [b] -> [(a, b)]
-- zipWithDefault da db la lb = take len $ zip la' lb'
--     where
--         len = max (length la) (length lb)
--         la' = la ++ (repeat da)
--         lb' = lb ++ (repeat db)

-- combines MusicU in parallel, but only if they are not equal
foldParallelU :: Eq a => [MusicU a] -> MusicU a
foldParallelU ms = foldr1 (/=/) (Data.List.nub ms)

-- splits MusicU by measure
splitMeasuresU :: Eq a => Dur -> Dur -> MusicU a -> [TiedMusicU a]
splitMeasuresU measureDur = split
    where
        glue :: [MusicU a] -> [MusicU a] -> [MusicU a]
        glue ms1 ms2 = (init ms1) ++ [last ms1 /+/ head ms2] ++ (tail ms2)
        split :: Eq a => Dur -> MusicU a -> [TiedMusicU a]
        split firstDur (PrimU (Note d p)) = [PrimU (Note r (p, flag)) | r <- splitDur measureDur firstDur d | flag <- tieFlags]
        split firstDur (PrimU (Rest d)) = [PrimU (Rest r) | r <- splitDur measureDur firstDur d]
        split firstDur (Seq []) = []
        split firstDur (Seq [m]) = split firstDur m
        split firstDur (Seq (m:ms)) = result
            where
                measHead = split firstDur m
                remDur = if null measHead then firstDur else (measureDur - durU (last measHead))
                measTail = split remDur (Seq ms)
                result
                    | null measTail = measHead
                    | otherwise = if (remDur == 0) then (measHead ++ measTail) else (glue measHead measTail)
        split firstDur (Par ms) = (Par . Data.List.nub) <$> mat
            where
                meas = (split firstDur) <$> ms
                mat = transposeWithDefault (PrimU $ Rest measureDur) meas
        split firstDur (ModifyU c m) = (ModifyU c) <$> (split firstDur m)
        -- TODO: issue with tempo!
        -- make MusicU a monoid with an empty element
        -- add runtime constructors seq and par that will unassociate and eliminate empty elements

-- splits Music by measure
splitMeasures :: Eq a => Dur -> Dur -> Music a -> [TiedMusic a]
splitMeasures measureDur firstDur m = mAssoc <$> (splitMeasuresU measureDur firstDur (mUnassoc m))
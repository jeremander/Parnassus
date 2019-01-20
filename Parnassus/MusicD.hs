{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Parnassus.MusicD where

import Control.Monad (join)
import Data.Foldable (foldl')
import Data.Function (on)
import qualified Data.List (nub, partition, sort, sortBy, transpose)
import Data.Ratio
import Data.Sort (sortOn)
import Data.Tuple.Select
import GHC.Exts (groupWith)

import Euterpea hiding (line, scaleDurations)
import qualified Euterpea
import Parnassus.Utils
import Parnassus.MusicBase 


data Tied a = Untied (Controls, Primitive a) | TiedNote Dur a
    deriving (Eq, Ord, Show)

extractTied :: Tied a -> Maybe a
extractTied (Untied (_, Note _ p)) = Just p
extractTied (TiedNote _ p) = Just p
extractTied _ = Nothing

-- returns True if the note is tied
isTied :: Tied a -> Bool
isTied (Untied _)     = False
isTied (TiedNote _ _) = True

-- fits a Tied into the given duration
fitTied :: Dur -> Tied a -> Tied a
fitTied d (Untied (ctl, Note _ p)) = Untied (ctl, Note d p)
fitTied d (Untied (ctl, Rest _))   = Untied (ctl, Rest d)
fitTied d (TiedNote _ p)           = TiedNote d p

type ArrD a = [[Tied a]]

-- "dense" music data structure
-- represents music as a sequence of chords with fixed duration q (which should be the smallest subdivision of the music
-- can support a list of global controls to be applied to the whole music
data MusicD a = MusicD Dur Controls (ArrD a)
    deriving (Eq, Ord, Show)

type MusicD1 = MusicD Note1

-- gets the shape of the (padded) chord array of MusicD
shape :: MusicD a -> (Int, Int)
shape (MusicD _ _ arr) = (length arr, maximum (length <$> arr))

-- creates MusicD from a Primitive element with the given subdivision q
primD :: Dur -> Primitive a -> MusicD a
primD q p = MusicD q [] m
    where m = case p of
            Rest d   -> replicate (floor $ d / q) [Untied ([], Rest q)]
            Note d p -> if (d < q)
                            then [[]]
                            else [[Untied ([], Note q p)]] ++ replicate ((floor $ d / q) - 1) [TiedNote q p]

-- pads a sequence of chords (lists of Tied a) of duration q so that the total duration is d
-- pads with single rests of duration q
padArr :: Dur -> Dur -> ArrD a -> ArrD a
padArr q d xs = padListWithDefault (floor $ d / q) [Untied ([], Rest q)] xs

-- gets the tempo from a Tempo Control, or 1 if the Control is not a Tempo
getTempo :: Control -> Rational
getTempo (Tempo t) = t
getTempo _         = 1

-- returns True if the Control is a Tempo
isTempo :: Control -> Bool
isTempo (Tempo _) = True
isTempo _         = False

-- given a sequence of possibly tied notes, combines all tied notes into their previous note; also combines rests
-- this is permissive in that it does not check that tied note values match
resolveTies :: [Tied a] -> [(Controls, Primitive a)]
resolveTies = reverse . (foldl' combine [])
    where
        combine [] (TiedNote d p)                      = [([], Note d p)]
        combine ((ctl1, Rest d1):ps) (Untied (ctl2, Rest d2))
            | ctl1 == ctl2                             = (ctl1, Rest (d1 + d2)) : ps
            | otherwise                                = (ctl2, Rest d2) : (ctl1, Rest d1) : ps
        combine x@((_, Rest _):_) y@(TiedNote _ _)     = error "cannot have tied note after rest"
        combine ((ctl1, Note d1 p):ps) (TiedNote d2 _) = (ctl1, Note (d1 + d2) p) : ps
        combine ps               (Untied (ctl2, p))    = (ctl2, p) : ps

-- applies controls note-wise to each Untied note/rest in the array
distributeControls :: Controls -> ArrD a -> ArrD a
distributeControls ctls = map (map f)
    where
        f :: Tied a -> Tied a
        f (Untied (ctls', p)) = Untied (ctls ++ ctls', p)
        f t                   = t

instance ToMidi MusicD

-- MusicD conversion --

class ToMusicD m a where
    -- converts to MusicD
    toMusicD :: m a -> MusicD a
    -- converts from MusicD
    fromMusicD :: MusicD a -> m a

instance ToMusicD MusicD a where
    toMusicD = id
    fromMusicD = id

instance (Ord a, Pitched a) => ToMusicD Music a where
    toMusicD = fromMusic
    fromMusicD = toMusic

-- given a distance function f and two equal-sized lists, returns a matching (list of pairs) that greedily minimize f
-- preserves the order of the first list
greedyMatching :: (Ord a, Ord b, Ord s) => (a -> b -> s) -> [a] -> [b] -> [(a, b)]
greedyMatching f xs ys
    | n1 /= n2  = error "xs and ys must be the same length"
    | n1 == 0   = []
    | otherwise = reverse chosen
    where
        n1 = length xs
        n2 = length ys
        items = Data.List.sort [(f x y, (i, x), (j, y)) | (i, x) <- zip [0..] xs, (j, y) <- zip [0..] ys]
        pred i j = \(_, (i', _), (j', _)) -> (i' /= i) && (j' /= j)
        (s, (i, x), (j, y)) = head items
        itemSeq = [([(s, (i, x), (j, y))], filter (pred i j) items)] ++ [((s', (i', x'), (j', y')) : chosen, filter (pred i' j') remaining) | (chosen, (s', (i', x'), (j', y')) : remaining) <- itemSeq]
        sortKey (_, (i, _), (_, _)) = -i
        select (_, (_, x), (_, y)) = (x, y)
        chosen = ((map select . sortOn sortKey . fst) <$> itemSeq) !! (n1 - 1)

-- given a list of equal-sized sublists, reorders the sublists so that each adjacent sublist matches greedily according to the distance function; transposes at the end so each sublist is now a sequence of matched elements spanning all the original sublists
greedyMatchSeq :: (Ord a, Ord s) => (a -> a -> s) -> [[a]] -> [[a]]
greedyMatchSeq f []  = []
greedyMatchSeq f xss = sortedSeqs
    where
        reorder f []            = []
        reorder f (x0:[])       = [x0]
        reorder f (x0:x1:xtail) = x0' : xtail'
            where
                (x0', x1') = unzip $ greedyMatching f x0 x1
                xtail' = reorder f (x1':xtail)  -- recursively reorder
        xss' = reorder f xss
        -- sort by first element of each sublist
        sortedSeqs = sortOn head (Data.List.transpose xss')

-- note distance is a pair, to allow for priority tiering as well as pitch distance
type NoteDistance = (Int, Int)

tiedNoteDistance :: (Pitched a) => Tied a -> Tied a -> NoteDistance
tiedNoteDistance n1 n2 = case (n1, n2) of
    ((Untied (_, Rest _)), (Untied (_, Rest _)))   -> (2, 0)
    ((Untied (_, Rest _)), (Untied (_, Note _ _))) -> (3, 0)
    ((Untied (_, Rest _)), (TiedNote _ _))         -> (maxBound, maxBound)
    ((Untied (_, Note _ _)), (Untied (_, Rest _))) -> (3, 0)
    ((Untied (_, Note _ p1)), (Untied (_, Note _ p2))) -> (1, abs (absPitch (getPitch p1) - absPitch (getPitch p2)))
    ((Untied (_, Note _ p1)), (TiedNote _ p2)) -> if (getPitch p1 == getPitch p2) then (0, 1) else (maxBound, maxBound)
    ((TiedNote _ _), (Untied (_, Rest _)))               -> (3, 0)
    ((TiedNote _ p1), (Untied (_, Note _ p2))) -> (1, abs (absPitch (getPitch p1) - absPitch (getPitch p2)))
    ((TiedNote _ p1), (TiedNote _ p2))         -> if (getPitch p1 == getPitch p2) then (0, 0) else (maxBound, maxBound)

instance (Ord a, Pitched a) => MusicT MusicD a where
    toMusic :: MusicD a -> Music a
    toMusic (MusicD q ctl m) = ctlMod $ Euterpea.chord lines'
        where
            ctlMod = composeFuncs (Modify <$> ctl)  -- compose the global controls into one modifier
            maxlen = maximum (length <$> m)
            m' = padListWithDefault maxlen (Untied ([], Rest q)) <$> m
            lines = resolveTies <$> greedyMatchSeq tiedNoteDistance m'  -- simplify the lines by agglomerating rests & tied notes
            f = \(ctl', p) -> composeFuncs (Modify <$> ctl') $ Prim p
            lines' = [Euterpea.line $ f <$> ln | ln <- lines]
    fromMusic :: Music a -> MusicD a
    fromMusic m = MusicD q ctl (Data.List.nub <$> m')  -- dedupe identical notes/rests in a chord
        where
            MusicD q ctl m' = Parnassus.MusicBase.fromMusic m
    prim :: Primitive a -> MusicD a
    prim p = primD (durP p) p
    modify :: Control -> MusicD a -> MusicD a
    modify c (MusicD q ctl m) = MusicD q (c:ctl) m
    (/+/) :: MusicD a -> MusicD a -> MusicD a
    (/+/) (MusicD q1 ctl1 m1) (MusicD q2 ctl2 m2)
        | q1 == q2  = MusicD q1 prefix (m1' ++ m2')
        | otherwise = error "MusicD quantization levels must match"
        where  -- distribute controls that are not in common prefix
            (prefix, [ctl1', ctl2']) = unDistribute [ctl1, ctl2]
            m1' = distributeControls ctl1' m1
            m2' = distributeControls ctl2' m2
    (/=/) :: MusicD a -> MusicD a -> MusicD a
    (/=/) (MusicD q1 ctl1 m1) (MusicD q2 ctl2 m2)
        | q1 == q2  = let d = q1 * fromIntegral (max (length m1) (length m2))
                    in MusicD q1 prefix (zipWith (++) (padArr q1 d m1') (padArr q2 d m2'))
        | otherwise = error "MusicD quantization level must match"
        where  -- distribute controls that are not in common prefix
            (prefix, [ctl1', ctl2']) = unDistribute [ctl1, ctl2]
            m1' = distributeControls ctl1' m1
            m2' = distributeControls ctl2' m2
    line :: Eq a => [MusicD a] -> MusicD a
    line = foldr1 (/+/)
    chord :: Eq a => [MusicD a] -> MusicD a
    chord ms = MusicD q ctl (Data.List.nub <$> ms')
        where (MusicD q ctl ms') = foldr1 (/=/) ms
    unLine :: Eq a => MusicD a -> [MusicD a]
    unLine (MusicD q ctl m) = [MusicD q ctl [seg] | seg <- m]
    unChord :: Eq a => MusicD a -> [MusicD a]
    unChord (MusicD q ctl m) = [MusicD q ctl (pure <$> ln) | ln <- lines]
        where lines = transposeWithDefault (Untied ([], Rest q)) m
    -- NB: ignores tempo variations at the note level
    dur :: MusicD a -> Dur
    dur (MusicD q ctl m) = (q / tempoFactor) * (fromIntegral $ length m)
        where tempoFactor = foldr (*) 1 (getTempo <$> ctl)
    durGCD :: MusicD a -> Rational
    durGCD (MusicD q _ _) = q
    scaleDurations :: Rational -> MusicD a -> MusicD a
    scaleDurations c (MusicD q ctl m) = MusicD (q / c) ctl m
    cut :: Eq a => Dur -> MusicD a -> MusicD a
    cut d (MusicD q ctl m) = MusicD q ctl (take (floor (d / q)) m)
    pad :: Dur -> MusicD a -> MusicD a
    pad d (MusicD q ctl m) = MusicD q ctl (padArr q d m)
    stripControls :: MusicD a -> (Controls, MusicD a)
    stripControls (MusicD q ctl m) = (ctl, MusicD q [] m)
    removeTempos :: MusicD a -> MusicD a
    removeTempos (MusicD q ctl m) = MusicD q (filter (not . isTempo) ctl) m
    distributeTempos :: MusicD a -> MusicD a
    distributeTempos (MusicD q ctl m) = MusicD (q / scale) ctl' m
        where
            (tempos, ctl') = Data.List.partition isTempo ctl
            scale = foldr (*) 1 (getTempo <$> tempos)
    transpose :: AbsPitch -> MusicD a -> MusicD a
    transpose i (MusicD q ctl m) = MusicD q ((Transpose i) : ctl) m

-- Quantizable Instance --

instance (Ord a, Pitched a) => Quantizable MusicD a where
    quantize :: Rational -> MusicD a -> MusicD a
    quantize q mus@(MusicD q' ctl m)
        | q == q' = mus
        | q `divides` q' = MusicD q ctl (concatMap chopChord m)  -- chop each chord, then concatenate together in sequence
        | otherwise = MusicD q ctl (mergeGroup <$> groups5)
            where
                -- simple case of refinement
                n = truncate (q' / q)
                chopNote :: Tied a -> [Tied a]
                chopNote (Untied (ctl, Note d x)) = [Untied (ctl, Note q x)] ++ replicate (n - 1) (TiedNote q x)
                chopNote (Untied (ctl, Rest d))   = [Untied (ctl, Rest q)] ++ replicate (n - 1) (Untied (ctl, Rest q))
                chopNote (TiedNote d x)           = replicate n (TiedNote q x)
                chopChord :: [Tied a] -> [[Tied a]]
                chopChord = Data.List.transpose . map chopNote
                -- "otherwise" case: need to process
                -- group the chord array by quantization slices
                groups1 :: [[(Rational, Rational, [Tied a], Bool)]]
                groups1 = quantizeTime q (zip (m ++ [[]]) [0, q'..])
                -- distribute time data across a chord
                distrib :: (Rational, Rational, [Tied a], Bool) -> [(Rational, Rational, Tied a, Bool)]
                distrib (t, d, notes, flag) = [(t, d, note, flag) | note <- notes]
                -- undistribute time data for a chord (assuming all time data is the same)
                undistrib :: [(Rational, Rational, Tied a, Bool)] -> (Rational, Rational, [Tied a], Bool)
                undistrib items = (t, d, sel3 <$> items, flag)
                    where (t, d, _, flag) = head items
                groups2 :: [[(Rational, Rational, Tied a, Bool)]]
                groups2 = join . map distrib <$> groups1
                -- tie together identical notes
                combine :: (Rational, Rational, Tied a, Bool) -> (Rational, Rational, Tied a, Bool) -> (Rational, Rational, Tied a, Bool)
                combine (t1, d1, p, flag1) (_, d2, _, flag2) = (t1, d1 + d2, p, flag1 && flag2)
                groups3 :: [[(Rational, Rational, Tied a, Bool)]]
                groups3 = (map $ foldr1 combine) . groupWith (extractTied . sel3) <$> groups2
                -- keep only notes that fill up at least half the quantization interval (if untied), or more than half (if tied)
                keepNote :: (Rational, Rational, Tied a, Bool) -> Bool
                keepNote (_, d, p, flag) = if (flag || isTied p) then (d > q / 2) else (d >= q / 2)
                groups4 :: [[(Rational, Rational, Tied a, Bool)]]
                groups4 = filter keepNote <$> groups3
                -- need to fix tie flags
                fix :: [(Rational, Rational, Tied a, Bool)] -> [(Rational, Rational, Tied a, Bool)] -> [(Rational, Rational, Tied a, Bool)]
                fix xs1 xs2 = f <$> xs2
                    where
                        pairs = [(extractTied x1, t1 + d1) | (t1, d1, x1, _) <- xs1]
                        f :: (Rational, Rational, Tied a, Bool) -> (Rational, Rational, Tied a, Bool)
                        f (t2, d2, x2, False) = (t2, d2, x2, False)
                        f (t2, d2, x2, flag2) = (t2, d2, x2, flag2 && tiePermitted)
                            where
                                note2 = extractTied x2
                                -- does a previous note tie with this note?
                                tiePermitted = any (\(note1, st1) -> (note1 == note2) && (st1 >= t2)) pairs
                groups5 :: [[(Rational, Rational, [Tied a], Bool)]]
                groups5 = map undistrib <$> gps
                    where
                        pairs = zip ([] : groups4) groups4
                        -- fix the ties
                        fixed = snd <$> [(xs1, fix xs1 xs2) | (xs1, xs2) <- pairs]
                        -- undistribute identical time data for efficiency
                        gps = [groupWith (\(t, d, _, flag) -> (t, d, flag)) gp | gp <- fixed]
                mergeGroup :: [(Rational, Rational, [Tied a], Bool)] -> [Tied a]
                mergeGroup gp = [fitTied q x | (x, _) <- pairs]
                    where
                        -- a True tie flag converts an Untied note to a Tied one
                        retie :: Tied a -> Bool -> Tied a
                        retie (Untied (_, Note d x)) True = TiedNote d x
                        retie note _ = note
                        -- converts a chord into a list of notes tagged with the duration
                        restructure :: (Rational, Rational, [Tied a], Bool) -> [(Tied a, Rational)]
                        restructure (_, d, notes, flag) = [(retie note flag, d) | note <- notes]
                        -- add up the duration of each note over all sequential segments in the group
                        gatherNotes :: [(Rational, Rational, [Tied a], Bool)] -> [(Tied a, Rational)]
                        gatherNotes gp' = noteGroup
                            where
                                -- helper function; given binary operation on rationals, and two (Tied a, Rational) pairs where the notes are presumed to be the same, combines them appropriately
                                agg :: (Rational -> Rational -> Rational) -> (Tied a, Rational) -> (Tied a, Rational) -> (Tied a, Rational)
                                agg f (Untied (c1, p1), r1) (_, r2) = (Untied (c1, p1), f r1 r2)
                                agg f (_, r1) (Untied (c2, p2), r2) = (Untied (c2, p2), f r1 r2)
                                agg f (TiedNote _ p1, r1) (TiedNote _ _, r2) = (TiedNote r1 p1, f r1 r2)
                                aggPar = agg max  -- parallel aggregation: take max duration
                                aggSeq = agg (+)  -- sequential aggregation: take total duration
                                parGroups = map (foldr1 aggPar) . groupWith (extractTied . fst) . restructure <$> gp'
                                noteGroup = map (foldr1 aggSeq) . groupWith (extractTied . fst) . join $ parGroups
                        pairs = gatherNotes gp
    split :: Rational -> MusicD a -> [MusicD a]
    split d mus@(MusicD q ctl m)
        | q `divides` d = [MusicD q ctl group | group <- chunkListWithDefault (truncate (d / q)) [Untied ([], Rest q)] m]
        | otherwise = split d (quantize (rationalGCD q d) mus)
    changeTimeSig :: TimeSig -> TimeSig -> MusicD a -> MusicD a
    changeTimeSig (n1, d1) (n2, d2) m = line m'
        where
            r1 = (toInteger n1) % (toInteger d1)
            r2 = (toInteger n2) % (toInteger d2)
            scale = r1 / r2
            measures = split r1 m
            q = rationalGCD (1 % toInteger d1) (durGCD m)
            m' = quantize q . scaleDurations (r1 / r2) <$> measures
            --MusicD _ ctl arr = line m'
                -- TODO: fix twinkleBass (8/4) time

---         r1 = (toInteger n1) % (toInteger d1)
---         r2 = (toInteger n2) % (toInteger d2)
---         q = toInteger $ lcm d1 d2
---         meas = splitMeasuresU r1 0 m
---         meas' = map ((quantizeU q) . (scaleDurations (r1 / r2))) meas
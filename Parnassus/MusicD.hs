{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Parnassus.MusicD where

import Data.Foldable (foldl')
import Data.Function (on)
import qualified Data.List (nub, partition, sort, sortBy, transpose)
import Data.Ratio
import Data.Sort (sortOn)

import Euterpea
import Parnassus.Utils
import Parnassus.MusicBase 


type ArrD a = [[Tied a]]

-- "dense" music data structure
-- represents music as a sequence of chords with fixed duration q (which should be the smallest subdivision of the music
-- can support a list of global controls to be applied to the whole music
data MusicD a = MusicD Dur Controls (ArrD a)
    deriving (Eq, Ord, Show)

-- gets the quantization level of MusicD
quantumD :: MusicD a -> Dur
quantumD (MusicD q _ _) = q

-- gets the controls of MusicD
controlD :: MusicD a -> Controls
controlD (MusicD _ ctl _) = ctl

-- gets the note array of MusicD
arrD :: MusicD a -> ArrD a
arrD (MusicD _ _ arr) = arr

-- gets the shape of the (padded) chord array of MusicD
shapeD :: MusicD a -> (Int, Int)
shapeD (MusicD _ _ arr) = (length arr, maximum (length <$> arr))

type MusicD1 = MusicD Note1

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

-- combine two MusicD in sequence
seqD :: MusicD a -> MusicD a -> MusicD a
seqD (MusicD q1 ctl1 m1) (MusicD q2 ctl2 m2)
    | q1 == q2  = MusicD q1 prefix (m1' ++ m2')
    | otherwise = error "MusicD quantization levels must match"
    where  -- distribute controls that are not in common prefix
        (prefix, [ctl1', ctl2']) = unDistribute [ctl1, ctl2]
        m1' = distributeControls ctl1' m1
        m2' = distributeControls ctl2' m2

-- combine two MusicD in parallel
parD :: MusicD a -> MusicD a -> MusicD a
parD (MusicD q1 ctl1 m1) (MusicD q2 ctl2 m2)
    | q1 == q2  = let d = q1 * fromIntegral (max (length m1) (length m2))
                  in MusicD q1 prefix (zipWith (++) (padArr q1 d m1') (padArr q2 d m2'))
    | otherwise = error "MusicD quantization level must match"
    where  -- distribute controls that are not in common prefix
        (prefix, [ctl1', ctl2']) = unDistribute [ctl1, ctl2]
        m1' = distributeControls ctl1' m1
        m2' = distributeControls ctl2' m2


instance {-# OVERLAPPABLE #-} ToMusic MusicD a where
    -- in absence of pitch information, just zip together the notes top-to-bottom, padding with rests as needed
    toMusic :: MusicD a -> Music a
    toMusic (MusicD q ctl m) = ctlMod $ Euterpea.chord lines'
        where
            ctlMod = composeFuncs (Modify <$> ctl)  -- compose the global controls into one modifier
            m' = transposeWithDefault (Untied ([], Rest q)) m  -- pad chords so they are all the same size, then transpose into lines
            lines = resolveTies <$> m'  -- simplify the lines by agglomerating rests & tied notes
            f = \(ctl', p) -> composeFuncs (Modify <$> ctl') $ Prim p
            lines' = [Euterpea.line $ f <$> ln | ln <- lines]
    -- TODO: use smallest subdivision, which may in fact be bigger than 1 / LCD
    fromMusic :: Music a -> MusicD a
    fromMusic m = mFold (primD q) (/+/) (/=/) g m
        where
            qinv = lcd' m
            q = 1 / fromIntegral qinv
            g :: Control -> MusicD a -> MusicD a
            g c (MusicD q' ctl m') = MusicD q' (c : ctl) m'


instance MusicT MusicD a where
    prim :: Primitive a -> MusicD a
    prim p = primD q p
        where q = case p of
                    Rest d   -> d
                    Note d _ -> d
    (/+/) :: MusicD a -> MusicD a -> MusicD a
    (/+/) = seqD
    (/=/) :: MusicD a -> MusicD a -> MusicD a
    (/=/) = parD
    line :: Eq a => [MusicD a] -> MusicD a
    line = foldr1 (/+/)
    chord :: Eq a => [MusicD a] -> MusicD a
    chord ms = MusicD q ctl (Data.List.nub <$> ms')
        where
            (MusicD q ctl ms') = foldr1 (/=/) ms
    unLine :: Eq a => MusicD a -> [MusicD a]
    unLine (MusicD q ctl m) = [MusicD q ctl [seg] | seg <- m]
    unChord :: Eq a => MusicD a -> [MusicD a]
    unChord (MusicD q ctl m) = [MusicD q ctl (pure <$> ln) | ln <- lines]
        where
            lines = transposeWithDefault (Untied ([], Rest q)) m
    dur :: MusicD a -> Dur
    dur (MusicD q _ m) = q * (fromIntegral $ length m)
    lcd :: MusicD a -> Integer
    lcd (MusicD q _ _) = denominator q
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
            getTempo :: Control -> Rational
            getTempo (Tempo t) = t
            getTempo _         = 1
            scale = foldr (*) 1 (getTempo <$> tempos)
    transpose :: AbsPitch -> MusicD a -> MusicD a
    transpose i (MusicD q ctl m) = MusicD q ((Transpose i) : ctl) m

instance ToMidi MusicD

-- subdivides MusicD by some factor n
refineD :: Int -> MusicD a -> MusicD a
refineD n (MusicD q ctl arr) = MusicD (q `divInt` n) ctl arr'
    where
        chopNote :: Tied a -> [Tied a]
        chopNote (Untied (ctl, Note d x)) = [Untied (ctl, Note (d `divInt` n) x)] ++ replicate (n - 1) (TiedNote (d `divInt` n) x)
        chopNote (Untied (ctl, Rest d))   = [Untied (ctl, Rest (d `divInt` n))] ++ replicate (n - 1) (Untied (ctl, Rest (d `divInt` n)))
        chopNote (TiedNote d x)           = replicate n (TiedNote (d `divInt` n) x)
        chopChord :: [Tied a] -> [[Tied a]]
        chopChord = Data.List.transpose . map chopNote
        arr' = concatMap chopChord arr  -- chop each chord, then concatenate together in sequence

instance (Eq a) => Quantizable MusicD a where
    quantize :: Rational -> MusicD a -> MusicD a
    quantize q m@(MusicD q' ctl arr)
        | q <= 0 = error errMsg
        | q `divides` q' = refineD n1 m
        | otherwise = error errMsg
        where
            errMsg = "invalid quantization: (" ++ (show q') ++ ") to (" ++ (show q) ++ ")"
            n1 = truncate (q' / q)

-- MusicD conversion --

class ToMusicD m a where
    -- converts to MusicD
    toMusicD :: m a -> MusicD a
    -- converts from MusicD
    fromMusicD :: MusicD a -> m a

instance ToMusicD MusicD a where
    toMusicD = id
    fromMusicD = id

instance ToMusicD Music a where
    toMusicD = fromMusic
    fromMusicD = toMusic

-- override toMusic for MusicD1 to optimally match notes in sequential chords

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

tiedNoteDistance :: Tied Note1 -> Tied Note1 -> NoteDistance
tiedNoteDistance n1 n2 = case (n1, n2) of
    ((Untied (_, Rest _)), (Untied (_, Rest _)))   -> (2, 0)
    ((Untied (_, Rest _)), (Untied (_, Note _ _))) -> (3, 0)
    ((Untied (_, Rest _)), (TiedNote _ _))         -> (maxBound, maxBound)
    ((Untied (_, Note _ _)), (Untied (_, Rest _))) -> (3, 0)
    ((Untied (_, Note _ (p1, _))), (Untied (_, Note _ (p2, _)))) -> (1, abs (absPitch p1 - absPitch p2))
    ((Untied (_, Note _ (p1, _))), (TiedNote _ (p2, _))) -> if (p1 == p2) then (0, 1) else (maxBound, maxBound)
    ((TiedNote _ _), (Untied (_, Rest _)))               -> (3, 0)
    ((TiedNote _ (p1, _)), (Untied (_, Note _ (p2, _)))) -> (1, abs (absPitch p1 - absPitch p2))
    ((TiedNote _ (p1, _)), (TiedNote _ (p2, _)))         -> if (p1 == p2) then (0, 0) else (maxBound, maxBound)

instance {-# OVERLAPPING #-} ToMusic MusicD Note1 where
    toMusic :: MusicD Note1 -> Music Note1
    toMusic (MusicD q ctl m) = ctlMod $ Euterpea.chord lines'
        where
            ctlMod = composeFuncs (Modify <$> ctl)  -- compose the global controls into one modifier
            maxlen = maximum (length <$> m)
            m' = padListWithDefault maxlen (Untied ([], Rest q)) <$> m
            lines = resolveTies <$> greedyMatchSeq tiedNoteDistance m'  -- simplify the lines by agglomerating rests & tied notes
            f = \(ctl', p) -> composeFuncs (Modify <$> ctl') $ Prim p
            lines' = [Euterpea.line $ f <$> ln | ln <- lines]
    fromMusic :: Music Note1 -> MusicD Note1
    fromMusic m = MusicD q ctl (Data.List.nub <$> m')  -- dedupe identical notes/rests in a chord
        where
            MusicD q ctl m' = Parnassus.MusicBase.fromMusic m

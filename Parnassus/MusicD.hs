{-# LANGUAGE InstanceSigs #-}

module Parnassus.MusicD where

import Data.Foldable (foldl')
import Data.Function (on)
import qualified Data.List (nub, partition, sort, sortBy, transpose)
import Data.Ratio

import Euterpea
import Parnassus.MusicBase hiding (pad')

data Tied a = Untied (Primitive a) | TiedNote Dur a
    deriving (Eq, Ord, Show)

-- TODO: make it lossless so that Tied a = Untied (Controls, Primitive a) | TiedNote Dur a

-- "dense" music data structure
-- represents music as a sequence of chords with fixed duration q (which should be the smallest subdivision of the music
-- can support a list of global controls to be applied to the whole music
data MusicD a = MusicD Dur Controls [[Tied a]]
    deriving (Eq, Ord, Show)

type MusicD1 = MusicD Note1

-- creates MusicD from a Primitive element with the given subdivision q
primD :: Dur -> Primitive a -> MusicD a
primD q p = MusicD q [] m
    where m = case p of
            Rest d   -> replicate (floor $ d / q) [Untied $ Rest q]
            Note d p -> if (d < q)
                            then [[]]
                            else [[Untied $ Note q p]] ++ replicate ((floor $ d / q) - 1) [TiedNote q p]

-- pads a sequence of chords (lists of Tied a) of duration q so that the total duration is d
-- pads with single rests of duration q
pad' :: Dur -> Dur -> [[Tied a]] -> [[Tied a]]
pad' q d xs = padListWithDefault (floor $ d / q) [Untied $ Rest q] xs

-- returns True if the Control is a Tempo
isTempo :: Control -> Bool
isTempo (Tempo _) = True
isTempo _         = False

-- given a sequence of possibly tied notes, combines all tied notes into their previous note; also combines rests
-- this is permissive in that it does not check that tied note values match
resolveTies :: [Tied a] -> [Primitive a]
resolveTies = reverse . (foldl' combine [])
    where
        combine [] (TiedNote d p) = [Note d p]
        combine ((Rest d1):ps)   (Untied (Rest d2)) = (Rest (d1 + d2)) : ps
        combine ((Rest _):_)     (TiedNote _ _)     = error "cannot have tied note after rest"
        combine ((Note d1 p):ps) (TiedNote d2 _)    = (Note (d1 + d2) p) : ps
        combine ps               (Untied p)         =  p : ps


instance MusicT MusicD where
    -- in absence of pitch information, just zip together the notes top-to-bottom, padding with rests as needed
    toMusic :: MusicD a -> Music a
    toMusic (MusicD q ctl m) = ctlMod $ Euterpea.chord lines'
        where
            ctlMod = foldr (.) id (Modify <$> ctl)  -- compose the controls into one modifier
            m' = transposeWithDefault (Untied (Rest q)) m  -- pad chords so they are all the same size, then transpose into lines
            lines = resolveTies <$> m'  -- simplify the lines by agglomerating rests & tied notes
            lines' = [Euterpea.line $ Prim <$> ln | ln <- lines]
    -- TODO: use smallest subdivision, which may in fact be bigger than 1 / LCD
    fromMusic :: Music a -> MusicD a
    fromMusic m = mFold (primD q) (/+/) (/=/) (curry snd) m
        where
            qinv = lcd m
            q = 1 / fromIntegral qinv
    prim :: Primitive a -> MusicD a
    prim p = primD q p
        where q = case p of
                    Rest d   -> d
                    Note d _ -> d
    (/+/) :: MusicD a -> MusicD a -> MusicD a
    (/+/) (MusicD q1 ctl1 m1) (MusicD q2 ctl2 m2)
        | q1 == q2  = MusicD q1 prefix (m1 ++ m2)
        | otherwise = error "MusicD quantization levels must match"
        where (prefix, _) = unDistribute [ctl1, ctl2]
    (/=/) :: MusicD a -> MusicD a -> MusicD a
    (/=/) (MusicD q1 ctl1 m1) (MusicD q2 ctl2 m2)
        | q1 == q2  = let d = q1 * fromIntegral (max (length m1) (length m2))
                      in MusicD q1 prefix (zipWith (++) (pad' q1 d m1) (pad' q2 d m2))
        | otherwise = error "MusicD quantization level must match"
        where (prefix, _) = unDistribute [ctl1, ctl2]
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
            padChord :: Int -> [Tied a] -> [Tied a]
            padChord n xs = xs ++ replicate (n - length xs) (Untied $ Rest q)
            lines = Data.List.transpose $ padChord (maximum (length <$> m)) <$> m
    dur :: MusicD a -> Dur
    dur (MusicD q _ m) = q * (fromIntegral $ length m)
    lcd :: MusicD a -> Integer
    lcd (MusicD q _ _) = denominator q
    scaleDurations :: Rational -> MusicD a -> MusicD a
    scaleDurations c (MusicD q ctl m) = MusicD (q / c) ctl m
    cut :: Eq a => Dur -> MusicD a -> MusicD a
    cut d (MusicD q ctl m) = MusicD q ctl (take (floor (d / q)) m)
    pad :: Dur -> MusicD a -> MusicD a
    pad d (MusicD q ctl m) = MusicD q ctl (pad' q d m)
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

-- Type class for converting to/from MusicU
class ToMusicD m where
    -- converts to MusicD
    toMusicD :: m a -> MusicD a
    -- converts from MusicD
    fromMusicD :: MusicD a -> m a

instance ToMusicD MusicD where
    toMusicD = id
    fromMusicD = id

instance ToMusicD Music where
    toMusicD = Parnassus.MusicBase.fromMusic
    fromMusicD = Parnassus.MusicBase.toMusic

-- override toMusic for MusicD1 to optimally match notes in sequential chords

-- given a distance function f and two equal-sized lists, returns a matching (list of pairs) that greedily minimize f
greedyMatching :: (Ord a, Ord b, Ord s) => (a -> b -> s) -> [a] -> [b] -> [(a, b)]
greedyMatching f xs ys
    | n1 /= n2  = error "xs and ys must be the same length"
    | n1 == 0   = []
    | otherwise = reverse chosen
    where
        n1 = length xs
        n2 = length ys
        items = Data.List.sort [(f x y, x, y) | x <- xs, y <- ys]
        pred x y = \(_, x', y') -> (x' /= x) && (y' /= y)
        (s, x, y) = head items
        itemSeq = [([(s, x, y)], filter (pred x y) items)] ++ [((s', x', y') : chosen, filter (pred x' y') remaining) | (chosen, (s', x', y') : remaining) <- itemSeq]
        select (_, x, y) = (x, y)
        chosen = ((map select . fst) <$> itemSeq) !! (n1 - 1)

-- given a list of equal-sized sublists, reorders the sublists so that each adjacent sublist matches greedily according to the distance function; transposes at the end so each sublist is now a sequence of matched elements spanning all the original sublists
greedyMatchSeq :: (Ord a, Ord s) => (a -> a -> s) -> [[a]] -> [[a]]
greedyMatchSeq f []  = []
greedyMatchSeq f xss = sortedSeqs
    where
        reorder f []            = []
        reorder f [xs]          = [xs]
        reorder f (x0:x1:xtail) = x0' : xtail'
            where
                (x0', x1') = unzip $ greedyMatching f x0 x1
                xtail' = reorder f (x1':xtail')  -- recursively reorder
        xss' = reorder f xss
        -- sort by first element of each sublist
        sortedSeqs = Data.List.sortBy (compare `on` head) (Data.List.transpose xss')

-- note distance is a tuple (!same note/rest type, abs note distance, !second note is tied)
type NoteDistance = (Bool, Int, Bool)
tiedNoteDistance :: Tied Note1 -> Tied Note1 -> NoteDistance
tiedNoteDistance (Untied (Rest _)) (Untied (Rest _)) = (False, 0, True)
tiedNoteDistance (Untied (Rest _)) _ = (True, 0, False)
tiedNoteDistance _ (Untied (Rest _)) = (True, 0, False)
tiedNoteDistance (Untied (Note _ (p1, _))) (Untied (Note _ (p2, _))) = (False, abs (absPitch p1 - absPitch p2), True)
tiedNoteDistance (Untied (Note _ (p1, _))) (TiedNote _ (p2, _)) = (False, abs (absPitch p1 - absPitch p2), False)
tiedNoteDistance (TiedNote _ (p1, _)) (Untied (Note _ (p2, _))) = (False, abs (absPitch p1 - absPitch p2), True)
tiedNoteDistance (TiedNote _ (p1, _)) (TiedNote _ (p2, _)) = (False, abs (absPitch p1 - absPitch p2), False)


-- specialization of MusicD for MusicD1

toMusic :: MusicD1 -> Music1
toMusic (MusicD q ctl m) = ctlMod $ Euterpea.chord lines'
    where
        ctlMod = foldr (.) id (Modify <$> ctl)  -- compose the controls into one modifier
        m' = transposeWithDefault (Untied (Rest q)) m  -- pad chords so they are all the same size, then transpose into lines
        lines = resolveTies <$> greedyMatchSeq tiedNoteDistance m'  -- simplify the lines by agglomerating rests & tied notes
        lines' = [Euterpea.line $ Prim <$> ln | ln <- lines]

fromMusic :: Music1 -> MusicD1
fromMusic m = MusicD q ctl (Data.List.nub <$> m')  -- dedupe identical notes/rests in a chord
    where
        MusicD q ctl m' = Parnassus.MusicBase.fromMusic m

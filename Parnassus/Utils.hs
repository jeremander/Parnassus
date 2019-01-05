{-# LANGUAGE ParallelListComp #-}

module Parnassus.Utils where

import qualified Data.List (transpose)
import qualified Data.Set
import Data.Ratio
import Data.Tuple.Select
import GHC.Exts (groupWith)


-- MISC --

-- merges two sorted lists, but terminates when the first list is exhausted
-- f is the sort key
merge :: Ord b => (a -> b) -> [a] -> [a] -> [a]
merge f [] ys = []
merge f xs [] = xs
merge f (x:xs) (y:ys) = if f x <= f y
                            then x : merge f xs (y:ys)
                            else y : merge f (x:xs) ys

-- composes a sequence of functions
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

-- given [A1, A2, ..., An], let A be the intersection of these sets; returns (A, [A1 \ A, A2 \ A, ..., An \ A])
unDistribute :: Ord a => [[a]] -> ([a], [[a]])
unDistribute [] = ([], [])
unDistribute xss = (Data.Set.toList xint, (Data.Set.toList . (`Data.Set.difference` xint)) <$> xsets)
    where
        xsets = Data.Set.fromList <$> xss
        xint = foldr1 Data.Set.intersection xsets

-- computes GCD of two rationals
rationalGCD :: Rational -> Rational -> Rational
rationalGCD x y = (gcd a c) % (lcm b d)
    where
        (a, b) = (numerator x, denominator x)
        (c, d) = (numerator y, denominator y)


-- QUANTIZATION --

divInt :: RealFrac a => a -> Int -> a
divInt x y = x / (fromIntegral y)

-- returns True if a divides b
divides :: Rational -> Rational -> Bool
divides a b = fromIntegral (truncate q) == q
    where q = b / a

-- quantizes a rational r to the nearest multiple of q
quantizeRational ::  Rational -> Rational -> Rational
quantizeRational q r = fromIntegral (round (r / q)) * q

-- given rational q, quantizes a sequences of rationals to be multiples of q, with as little perturbation as possible
quantizeRationals :: Rational -> [Rational] -> [Rational]
quantizeRationals q rs = rs'
    where
        qrs = [if (r == 0) then r else max q (quantizeRational q r) | r <- rs]  -- try to round away from 0
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

-- like groupWith, but we assume the key values are in monotonically nondecreasing order
-- this allows us to group lazily
lazyGroupWith :: Ord b => (a -> b) -> [a] -> [[a]]
lazyGroupWith f [] = []
lazyGroupWith f (x:xs) = (x : gp) : lazyGroupWith f rest
    where
        key = f x
        pred = (/=) key . f
        (gp, rest) = break pred xs

-- given rational q and a sequence of (x_i, t_i) pairs, where the x_i are values and the t_i are increasing times delimiting time intervals (including start and end points), returns a list of groups of intervals taking place within each q interval, consisting of (start, duration, x, flag), where x denotes the original value, and flag is True if the interval is a continuation of an interval from the previous group

quantizeTime :: Rational -> [(a, Rational)] -> [[(Rational, Rational, a, Bool)]]
quantizeTime q [] = []
quantizeTime q pairs = lazyGroupWith (truncate . (/q) . sel1) items'
    where
        (x0, t0) = head pairs
        triples = merge sel1 [(t, Just x, True) | (x, t) <- pairs] [(t, Nothing, False) | t <- [t0, (t0 + q)..]]
        itemPairs = filter (\((t1, _, _), (t2, _, _)) -> t1 /= t2) $ zip triples (tail triples ++ [(t0 - 1, Nothing, False)])
        items = init [(t1, t2 - t1, x1, not flag1) | ((t1, x1, flag1), (t2, _, _)) <- itemPairs]
        f :: a -> Maybe a -> a
        f x Nothing  = x
        f x (Just y) = y
        values = scanl f x0 $ map sel3 items
        items' = [(t, d, x, flag) | (t, d, _, flag) <- items | x <- tail values]

test = [0, 1 % 3, 2 % 3, 1, 5 % 4, 3 % 2, 5 % 2, 10 % 3, 4, 21 % 5, 22 % 5, 23 % 5, 24 % 5, 5, 21 % 4, 25 % 4, 33 % 4, 10]

test1 = zip [0..] test
test2 = zip "abcdefghijklmnopqrstuvwxyz" test
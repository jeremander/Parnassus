{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ParallelListComp #-}
{-# LANGUAGE ScopedTypeVariables #-}


module Misc.Utils where

import qualified Data.Array as A
import qualified Data.List (transpose)
import Data.List.Split (chunksOf)
import Data.Maybe (fromMaybe)
import qualified Data.Set as S
import Data.Ratio
import qualified Data.Text as T
import Data.Tuple.Select


-- * Math

-- | Coercion to 'Double'.
class ToDouble a where
    toDouble :: a -> Double
instance ToDouble Double where
    toDouble = id
instance ToDouble Rational where
    toDouble = fromRational

-- | Computes GCD of two rationals.
rationalGCD :: Rational -> Rational -> Rational
rationalGCD x y = (gcd a c) % (lcm b d)
    where
        (a, b) = (numerator x, denominator x)
        (c, d) = (numerator y, denominator y)

argmax, argmin :: Ord a => [a] -> Int
argmax xs = head [i | (x, i) <- zip xs [0..], x == maximum xs]
argmin xs = head [i | (x, i) <- zip xs [0..], x == minimum xs]

-- | Cumulative sums.
cumsum :: (Num a) => [a] -> [a]
cumsum = scanl (+) 0

log2 :: Floating a => a -> a
log2 = logBase 2

pow2 :: Floating a => a -> a
pow2 x = 2 ** x

-- | Division with rule that 0 / 0 = 0.
safeDiv :: (Eq a, Fractional a) => a -> a -> a
safeDiv 0 0 = 0
safeDiv x y = x / y

-- * Arrays

-- | Constructs an array from a list of elements, using standard 0-up indexing.
mkArray :: (A.Ix i, Integral i) => [e] -> A.Array i e
mkArray xs = A.listArray (0, fromIntegral $ length xs - 1) xs

-- | Given a filter function and an integer-indexed array, returns a new reindexed filtered array.
filterArray :: (A.Ix i, Integral i) => (e -> Bool) -> A.Array i e -> A.Array i e
filterArray f arr = mkArray $ filter f $ A.elems arr

-- | Given an array and a list of integer indices, returns the elements at the corresponding indices.
atIndices ::  (A.Ix i, Integral i) => A.Array i e -> [Int] -> [e]
atIndices xs indices = [xs A.! fromIntegral i | i <- indices]

-- * Miscellaneous

notImpl :: String -> a
notImpl a = error $ "Not implemented: " ++ a

-- | Enumerates a 'Bounded' 'Enum' type.
enumerate :: forall a . (Bounded a, Enum a) => [a]
enumerate = toEnum <$> [0..(fromEnum (maxBound::a))]

-- | Strips leading and trailing whitespace from a string.
strip :: String -> String
strip = T.unpack . T.strip . T.pack

-- | Takes successive pairs of a list.
pairwise :: [a] -> [(a, a)]
pairwise xs = zip xs (tail xs)

-- | Produces all ordered pairs \((x_i, x_j)\) in a list, where \(i < j\).
orderedPairs :: [a] -> [(a, a)]
orderedPairs xs = [(xi, xj) | (i, xi) <- zip [0..] xs, (j, xj) <- zip [1..] (tail xs), i < j]

-- | Given a predicate and a list of elements, computes the list of indices whose elements satisfy the predicate.
indicesWhere :: (a -> Bool) -> [a] -> [Int]
indicesWhere pred xs = [i | (i, x) <- zip [0..] xs, pred x]

-- | Given a predicate and a list of elements, returns a mapping \(i \rightarrow j\), where \(i\) is the original index and \(j\) is the filtered index. This mapping will only include filtered elements.
filteredIndexMap :: (a -> Bool) -> [a] -> [(Int, Int)]
filteredIndexMap pred xs = zip indices [0..(length indices - 1)]
    where indices = indicesWhere pred xs

-- | Given filtered, returns a mapping from original indices to filtered indices.
inverseIndexMap :: [Int] -> [(Int, Int)]
inverseIndexMap indices = zip indices [0..]

-- | Rotates a list left by some number.
rotateL :: Int -> [a] -> [a]
rotateL n xs = drop n' xs ++ take n' xs
    where n' = n `mod` (length xs)

-- | Safe head function, returning Nothing if the list is empty.
safeHead :: [a] -> Maybe a
safeHead []    = Nothing
safeHead (x:_) = Just x

-- | Safe tail function, returning empty list if the list is empty.
safeTail :: [a] -> [a]
safeTail []     = []
safeTail (_:xs) = xs

-- | Extracts a 'Just' value, emitting a specified error if it is 'Nothing'
justOrError :: Maybe a -> String -> a
justOrError mx err = fromMaybe (error err) mx

-- | Given n, a list of sorted indices in [0, n), and a list of items, selects items from the list at the corresponding indices.
--   Does no bounds checking.
selector :: Int -> [Int] -> [a] -> [a]
selector n indices xs = fst <$> filter snd (zip xs indexIndicators)
    where
        indexSet = S.fromList indices
        indexIndicators = [S.member i indexSet | i <- [0..(n - 1)]]

-- | Merges two sorted lists, but terminates when the second list is exhausted.
--
--   The first argument @f@ is the sort key.
merge :: Ord b => (a -> b) -> [a] -> [a] -> [a]
merge _ [] ys = ys
merge _ _ []  = []
merge f (x:xs) (y:ys) = if f x <= f y
                            then x : merge f xs (y:ys)
                            else y : merge f (x:xs) ys

-- | Composes a sequence of functions.
composeFuncs :: [(a -> a)] -> a -> a
composeFuncs = foldr (.) id

-- | "Commutes" a function in a commutative diagram by a pre-composed function and post-composed function.
commute :: (a -> b) -> (c -> d) -> (b -> c) -> (a -> d)
commute f g h = g . h . f

type Conj a b = (b -> b) -> (a -> a)

-- | "Conjugates" a function by some other function and its inverse.
conj :: (a -> b) -> (b -> a) -> Conj a b
conj = commute

-- | Pads a list to a certain length with a default value.
padListWithDefault :: Int -> a -> [a] -> [a]
padListWithDefault n def xs = take n (xs ++ repeat def)

-- | Chunks a list into length-n pieces; the last chunk may be too short, so pads it with a default value.
chunkListWithDefault :: Int -> a -> [a] -> [[a]]
chunkListWithDefault n def xs = padListWithDefault n def <$> chunksOf n xs

-- | Transposes a list of lists of varying length, padding any short lists with a default value.
transposeWithDefault :: a -> [[a]] -> [[a]]
transposeWithDefault def xss = Data.List.transpose mat
    where
        maxlen = maximum (length <$> xss)
        mat = padListWithDefault maxlen def <$> xss

-- | Given \([A_1, A_2, ..., A_n]\), let \(A\) be the intersection of these sets. Returns \((A, [A_1 \backslash A, A_2 \backslash A, ..., A_n \backslash A])\).
unDistribute :: Ord a => [[a]] -> ([a], [[a]])
unDistribute [] = ([], [])
unDistribute xss = (S.toList xint, (S.toList . (`S.difference` xint)) <$> xsets)
    where
        xsets = S.fromList <$> xss
        xint = foldr1 S.intersection xsets

-- | Computes \(n\)-grams from a list of items.
ngrams :: Int -> [a] -> [[a]]
ngrams n xs
    | (n <= length xs) = take n xs : ngrams n (drop 1 xs)
    | otherwise = []

-- * Quantization

divInt :: RealFrac a => a -> Int -> a
divInt x y = x / (fromIntegral y)

-- | Returns True if \(a\) divides \(b\).
divides :: Rational -> Rational -> Bool
divides a b = fromIntegral (truncate q) == q
    where q = b / a

-- | Quantizes a rational \(r\) to the nearest multiple of \(q\).
quantizeRational ::  Rational -> Rational -> Rational
quantizeRational q r = fromIntegral (round (r / q)) * q

-- | Given rational \(q\), quantizes a sequences of rationals to be multiples of \(q\), with as little perturbation as possible
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
                (i, amt) = if (t > 0)  -- have a surplus
                                then (imin, -q)
                                else (imax, q)
        stepSeq = iterate step (totalDiff, diffs)
        pairSeq = zip stepSeq (tail stepSeq)
        dropCondition ((t1, _), (t2, _)) = (t1 /= t2) && (abs t1 > (q / 2))
        ((_, finalDiffs), (_, _)) = head $ dropWhile dropCondition pairSeq
        rs' = zipWith (-) rs finalDiffs

-- | Like 'Data.List.groupWith', but we assume the key values are in monotonically nondecreasing order.
--   This allows us to group lazily.
lazyGroupWith :: Ord b => (a -> b) -> [a] -> [[a]]
lazyGroupWith _ []     = []
lazyGroupWith f (x:xs) = (x : gp) : lazyGroupWith f rest
    where
        key = f x
        pred = (/=) key . f
        (gp, rest) = break pred xs

-- | Given rational \(q\) and a sequence of \((x_i, t_i)\) pairs, where the \(x_i\) are values and the \(t_i\) are increasing times delimiting time intervals (including start and end points), returns a list of groups of intervals taking place within each \(q\) interval, consisting of @(start, duration, x, flag)@, where @x@ denotes the original value, and @flag@ is @True@ if the interval is a continuation of an interval from the previous group.
quantizeTime :: Rational -> [(a, Rational)] -> [[(Rational, Rational, a, Bool)]]
quantizeTime _ []   = []
quantizeTime q pairs = lazyGroupWith (truncate . (/q) . sel1) items'
    where
        (x0, t0) = head pairs
        pairs' = merge sel1 [(t, Nothing) | t <- [t0, (t0 + q)..]] [(t, Just x) | (x, t) <- pairs]
        pairPairs = filter (\((t1, _), (t2, _)) -> t1 /= t2) $ zip ((t0 - 1, Nothing) : init pairs') pairs'
        items = tail [(t1, t2 - t1, x1, null x1) | ((t1, x1), (t2, _)) <- pairPairs]
        f :: a -> Maybe a -> a
        f x Nothing  = x
        f _ (Just y) = y
        values = scanl f x0 $ map sel3 items
        items' = [(t, d, x, flag) | (t, d, _, flag) <- items | x <- tail values]
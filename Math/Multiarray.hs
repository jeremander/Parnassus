{-# LANGUAGE NamedFieldPuns #-}

module Math.Multiarray where

import Data.List (scanl', zip4)
import qualified Data.Vector.Storable as V


data MultiArray a = MArray
    { sizes :: [Int],
      cumProds :: [Int],
      entries :: V.Vector a
    }
    deriving (Eq, Show)

-- constructor
multiarray :: (V.Storable a) => [Int] -> [a] -> MultiArray a
multiarray sizes entries = arr
    where
        cumProds = scanl' (*) 1 (reverse sizes)
        cumProds' = reverse $ init cumProds
        numExpectedEntries = if (length cumProds == 1) then 0 else last cumProds
        entryVec = V.fromList entries
        numEntries = V.length entryVec
        arr
            | numExpectedEntries == numEntries = MArray {sizes = sizes, cumProds = cumProds', entries = entryVec}
            | otherwise                        = error (show numExpectedEntries ++ " entries expected, " ++ show numEntries ++ " given.")

-- converts a multidimensional index to the corresponding 1D index in the entry vector
multiIndexToIndex :: MultiArray a -> [Int] -> Int
multiIndexToIndex (MArray {sizes, cumProds}) indices = sum indices'
    where indices' = [if (0 <= i) && (i < size) then cumProd * i else error ("index " ++ show i ++ " out of bounds for dimension " ++ show j) | (j, size, cumProd, i) <- zip4 [0..] sizes cumProds indices]

-- converts a 1D index to a multidimensional index
indexToMultiIndex :: MultiArray a -> Int -> [Int]
indexToMultiIndex (MArray {sizes, cumProds}) i
    | (0 <= i) && (i < size) = f i cumProds
    | otherwise              = error $ "index " ++ show i ++ " out of bounds"
        where
            size = product sizes
            f :: Int -> [Int] -> [Int]
            f i [cp]  = [i]
            f i (cp:cps) = i `quot` cp : f (i `rem` cp) cps

-- gets the entry at a multi-index
infixr 5 ^!^
(^!^) :: (V.Storable a) => MultiArray a -> [Int] -> a
(^!^) arr@(MArray {entries}) indices = entries V.! (multiIndexToIndex arr indices)

-- slices in multiple dimensions
-- the index consists of a list of Maybe [Int], where an actual list gives the slice indices, while Nothing means to include the whole axis
(^:^) :: (V.Storable a) => MultiArray a -> [Maybe [Int]] -> MultiArray a
(^:^) m []           = m
(^:^) m [Nothing] = m
(^:^) (MArray {sizes = [size], entries}) [Just indices] = MArray {sizes = [length indices], cumProds = [1], entries = V.fromList [entries V.! i | i <- indices]}
(^:^) (MArray {sizes = szs, cumProds = cps, entries = ents}) (idx:idxs) = slc
    where
        size = head szs
        (size', idx') = case idx of
            Just indices -> (length indices, indices)
            Nothing      -> (size, [0..(size - 1)])
        rowSize = head cps
        rows = [V.slice (i * rowSize) rowSize ents | i <- idx']
        arrs = [MArray {sizes = tail szs, cumProds = tail cps, entries = row} | row <- rows]
        slices = [arr ^:^ idxs | arr <- arrs]
        slice0 = head slices
        cp = (head $ sizes slice0) * (head $ cumProds slice0)
        slc = MArray {sizes = size' : sizes slice0, cumProds = cp : cumProds slice0, entries = V.concat (entries <$> slices)}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}

module Parnassus.Dist where

import Control.Exception.Base (assert)
import Data.Counter (count)
import Data.List (nub, scanl', sort, transpose, zip4, (\\))
import qualified Data.Map as M
import Data.Maybe (fromJust)
import Data.Set as S hiding (filter, (\\))
import qualified Data.Vector as VB
import qualified Data.Vector.Unboxed as V
import System.Random
import Test.QuickCheck

import Parnassus.Utils (merge, prod)

-- TYPES --

type Name = String
type Prob = Double

-- UTILITIES --

-- division with rule that 0 / 0 = 0
safeDiv :: Double -> Double -> Double
safeDiv 0 0 = 0
safeDiv x y = x / y

-- given an element e and a sorted vector v, returns the index i for which v[i] <= e < v[i + 1]
bisect :: (Ord a, V.Unbox a) => a -> V.Vector a -> Int
bisect e v = f 0 (V.length v)
    where
        f :: Int -> Int -> Int
        f imin imax
            | imin + 1 >= imax = imin
            | otherwise = f imin' imax'
                where
                    imid = imin + (quot (imax - imin) 2)
                    (imin', imax') = if (v V.! imid > e) then (imin, imid) else (imid, imax)

-- computes the Cartesian product of several lists
cartesianProduct :: [[a]] -> [[a]]
cartesianProduct [] = [[]]
cartesianProduct (x:xs) = concat [[y : item | item <- prev] | y <- x]
    where prev = cartesianProduct xs

-- MULTIARRAY --

data MultiArray a = MArray
    { sizes :: [Int],
      cumProds :: [Int],
      entries :: V.Vector a
    }
    deriving (Eq, Show)

-- constructor
multiarray :: (V.Unbox a) => [Int] -> [a] -> MultiArray a
multiarray sizes entries = arr
    where
        cumProds = scanl' (*) 1 (reverse sizes)
        cumProds' = reverse $ init $ cumProds
        numExpectedEntries = last cumProds
        entryVec = V.fromList entries
        numEntries = V.length entryVec
        arr
            | numExpectedEntries == numEntries = MArray {sizes = sizes, cumProds = cumProds, entries = entryVec}
            | otherwise                        = error (show numExpectedEntries ++ " entries expected, " ++ show numEntries ++ " given.")

-- converts a multidimensional index to the corresponding 1D index in the entry vector
multiIndexToIndex :: MultiArray a -> [Int] -> Int
multiIndexToIndex (MArray {sizes, cumProds}) indices = sum indices'
    where indices' = [if (0 <= i) && (i < size) then cumProd * i else error ("index " ++ show i ++ " out of bounds for dimension " ++ show j) | (j, size, cumProd, i) <- zip4 [0..] sizes cumProds indices]

-- gets the entry at a multi-index
infixr 5 ^!^
(^!^) :: (V.Unbox a) => MultiArray a -> [Int] -> a
(^!^) arr@(MArray {entries}) indices = entries V.! (multiIndexToIndex arr indices)

-- slices in multiple dimensions
-- the index consists of a list of Maybe [Int], where an actual list gives the slice indices, while Nothing means to include the whole axis
(^:^) :: (V.Unbox a) => MultiArray a -> [Maybe [Int]] -> MultiArray a
(^:^) m []           = m
(^:^) m (Nothing:[]) = m
(^:^) (MArray {sizes = [size], entries}) ((Just indices):[]) = MArray {sizes = [length indices], cumProds = [1], entries = V.fromList [entries V.! i | i <- indices]}
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

-- DISCRETE DISTRIBUTION --

data DiscreteDist a = Discrete
    { var :: Name,
      vals :: VB.Vector a,
      valIdx :: M.Map a Int,
      probs :: V.Vector Prob,
      cdf :: V.Vector Prob
    }
    deriving (Eq)

instance (Show a) => Show (DiscreteDist a) where
    show :: DiscreteDist a -> String
    show (Discrete {var, vals, probs}) = "discreteDist " ++ show var ++ " " ++ show (VB.toList vals) ++ " " ++ show (V.toList probs)

-- constructors --

-- main constructor for DiscreteDist
discreteDist :: (Ord a) => Name -> [a] -> [Prob] -> DiscreteDist a
discreteDist var vals probs
    | length probs /= numVals = error "mismatch between number of vals and probs"
    | V.minimum probVec < 0.0 = error "minimum prob must be >= 0"
    | otherwise               = Discrete {var = var, vals = VB.fromList vals', valIdx = valIdx, probs = pmf, cdf = cdf}
        where
            vals' = nub vals
            numVals = length vals'
            probVec = V.fromList probs
            probSum = V.sum probVec
            valIdx = M.fromList $ zip vals' [0..]
            pmf = V.map (flip safeDiv $ probSum) probVec
            cdf = V.scanl' (+) 0.0 pmf

-- creates DiscreteDist from data (optionally include a smoothing constant)
trainDiscrete :: (Ord a) => Name -> Maybe [a] -> Int -> [a] -> DiscreteDist a
trainDiscrete var vals smooth xs = discreteDist var vals' counts
    where
        freqs = count xs
        observedVals = sort $ M.keys freqs
        vals' = case vals of
            Just vs -> sort $ nub vs
            Nothing -> observedVals
        counts = [fromIntegral $ M.findWithDefault 0 val freqs + smooth | val <- vals']

-- accessors --

-- gets the probability of a particular outcome
getProb :: (Ord a, Show a) => DiscreteDist a -> a -> Double
getProb (Discrete {valIdx, probs}) val = case idx of
    Just i  -> probs V.! i
    Nothing -> error $ "invalid value: " ++ show val
    where idx = M.lookup val valIdx

-- gets the probability of an event (a set of particular outcomes)
getProbEvent :: (Ord a, Show a) => DiscreteDist a -> [a] -> Double
getProbEvent d vals = sum $ [getProb d val | val <- nub vals]

-- simulation --

-- computes random samples from the discrete distribution
samples :: DiscreteDist a -> IO [a]
samples (Discrete {vals, cdf}) = do
    gen <- newStdGen
    return [vals VB.! (bisect r cdf) | r <- randoms gen]

-- computes a single random sample from the discrete distribution
sample :: DiscreteDist a -> IO a
sample d = head <$> samples d

-- JOINT DISCRETE DISTRIBUTION --

class JointDiscreteDistribution d a where
    -- gets the list of variable names associated with the joint distribution
    getVars :: d a -> [String]
    -- trains a JointDiscreteDistribution from data
    trainJointDiscrete :: [Name] -> Maybe [[a]] -> Int -> [[a]] -> d a
    -- gets the probability of a particular outcome
    getJointProb :: d a -> [a] -> Double
    -- gets the probability of an event (a set of particular outcomes)
    -- the event is given as a list of events for each variable, and the joint event is the Cartesian product of them all
    getJointProbEvent :: d a -> [[a]] -> Double
    getJointProbEvent d vals = sum $ [getJointProb d val | val <- cartesianProduct vals]
    -- marginal distribution over the given variables
    marginalizeOver :: d a -> [String] -> d a
    marginalizeOver dist vars = marginalizeOut dist (getVars dist \\ vars)
    -- marginalizes out the given variables (given in any order)
    marginalizeOut :: d a -> [String] -> d a
    marginalizeOut dist vars = marginalizeOver dist (getVars dist \\ vars)


-- JointDiscreteDist: full joint probability table --

data JointDiscreteDist a = JointDiscrete
    { numVars :: Int,
      jVars :: [Name],
      jVals :: [VB.Vector a],
      jValIdx :: [M.Map a Int],
      jProbs :: MultiArray Prob,
      jCdf :: V.Vector Prob
    }
    deriving (Eq)

instance (Show a) => Show (JointDiscreteDist a) where
    show :: JointDiscreteDist a -> String
    show (JointDiscrete {jVars, jVals, jProbs}) = "jointDiscreteDist " ++ show jVars ++ " " ++ show jVals ++ " " ++ show jProbs

jointDiscreteDist :: (Ord a) => [Name] -> [[a]] -> MultiArray Prob -> JointDiscreteDist a
jointDiscreteDist vars vals probs
    | numVars /= (length $ nub vars)  = error "duplicate var names not allowed"
    | V.length probVec /= numCells    = error "mismatch between number of vals and probs"
    | V.minimum probVec < 0.0         = error "minimum prob must be >= 0"
    | otherwise                       = JointDiscrete {numVars = numVars, jVars = vars, jVals = jVals, jValIdx = jValIdx, jProbs = jProbs, jCdf = cdf}
        where
            numVars = length vars
            vals' = nub <$> vals
            numVals = length <$> vals'
            numCells = prod numVals
            jVals = VB.fromList <$> vals'
            jValIdx = [M.fromList $ zip vs [0..] | vs <- vals']
            probVec = entries probs
            probSum = V.sum probVec
            pmf = V.map (flip safeDiv $ probSum) probVec
            jProbs = MArray {sizes = sizes probs, cumProds = cumProds probs, entries = pmf}
            cdf = V.scanl' (+) 0.0 pmf

instance (Ord a, Show a) => JointDiscreteDistribution JointDiscreteDist a where
    getVars :: JointDiscreteDist a -> [String]
    getVars = jVars
    trainJointDiscrete :: [Name] -> Maybe [[a]] -> Int -> [[a]] -> JointDiscreteDist a
    trainJointDiscrete vars vals smooth xs = jointDiscreteDist vars vals' countArr
        where
            numVars = length vars
            lengths = length <$> xs
            freqs
                | all (== numVars) lengths = count xs
                | otherwise                = error $ "all data entries must have length " ++ show numVars
            observedVals = (sort . nub) <$> (transpose $ M.keys freqs)
            vals' = case vals of
                Just vs -> vs'
                    where vs' | (length vs == numVars) = (sort . nub) <$> vs
                              | otherwise              = error $ show numVars ++ " value lists required, " ++ show (length vs) ++ " given"
                Nothing -> observedVals
            valProduct = cartesianProduct vals'
            counts = [fromIntegral $ M.findWithDefault 0 val freqs + smooth | val <- valProduct]
            countArr = multiarray (length <$> vals') counts
    -- gets the probability of a particular outcome
    getJointProb :: JointDiscreteDist a -> [a] -> Double
    getJointProb (JointDiscrete {numVars, jValIdx, jProbs}) vals = jProbs ^!^ multiIdx
        where
            getIdx valIdx val = case idx of
                Just i  -> i
                Nothing -> error $ "invalid value: " ++ show val
                where idx = M.lookup val valIdx
            numVals = length vals
            multiIdx
                | (numVals == numVars) = zipWith getIdx jValIdx vals
                | otherwise            = error $ show numVars ++ " values required, " ++ show numVals ++ " given"
    marginalizeOver :: JointDiscreteDist a -> [String] -> JointDiscreteDist a
    marginalizeOver dist vars = dist'
        where
            n = numVars dist
            origVars = jVars dist
            indicesByVar = M.fromList $ zip origVars [0..]
            indices = sort [fromJust $ M.lookup var indicesByVar | var <- nub vars]
            indexSet = S.fromList indices
            indexIndicators = [S.member i indexSet | i <- [0..(n - 1)]]
            selector xs = fst <$> filter snd (zip xs indexIndicators)
            probs = jProbs dist
            sizes1 = sizes probs
            size1 = V.length $ entries probs
            sizes2 = selector sizes1
            cumProds2 = reverse $ init $ scanl' (*) 1 (reverse sizes2)
            size2 = prod sizes2
            multiIndices1 = cartesianProduct [[0..(sz - 1)] | sz <- sizes1]
            multiIndices2 = selector <$> multiIndices1
            midxToIdx = \midx -> sum [cumProd * i | (cumProd, i) <- zip cumProds2 midx]
            updateInds = midxToIdx <$> multiIndices2
            updatePairs = zip updateInds (V.toList $ entries probs)
            probVec = V.replicate size2 0.0
            probs' = MArray {sizes = sizes2, cumProds = cumProds2, entries = V.accum (+) probVec updatePairs}
            dist' = jointDiscreteDist (selector origVars) (VB.toList <$> (selector $ jVals dist)) probs'


-- IndependentDiscreteDist: joint probability factorized as several independent discrete distributions --

data IndependentDiscreteDist a = IndependentDiscrete Int [DiscreteDist a]
    deriving (Eq, Show)

independentDiscreteDist :: [DiscreteDist a] -> IndependentDiscreteDist a
independentDiscreteDist dists
    | numVars /= (length $ nub $ var <$> dists) = error "duplicate var names not allowed"
    | otherwise                                 = IndependentDiscrete numVars dists
        where numVars = length dists

instance (Ord a, Show a) => JointDiscreteDistribution IndependentDiscreteDist a where
    getVars :: IndependentDiscreteDist a -> [String]
    getVars (IndependentDiscrete _ dists) = var <$> dists
    trainJointDiscrete :: [Name] -> Maybe [[a]] -> Int -> [[a]] -> IndependentDiscreteDist a
    trainJointDiscrete vars vals smooth xs = independentDiscreteDist dists
        where
            numVars = length vars
            lengths = length <$> xs
            xs'
                | all (== numVars) lengths = transpose xs
                | otherwise                = error $ "all data entries must have length " ++ show numVars
            vals' = case vals of
                Just vs -> vs'
                    where vs' | (length vs == numVars) = Just <$> vs
                              | otherwise              = error $ show numVars ++ " value lists required, " ++ show (length vs) ++ " given"
                Nothing -> replicate numVars Nothing
            dists = [trainDiscrete var vs smooth col | (var, vs, col) <- zip3 vars vals' xs']
    getJointProb :: IndependentDiscreteDist a -> [a] -> Double
    getJointProb (IndependentDiscrete _ dists) vals = prod $ zipWith getProb dists vals
    getJointProbEvent :: IndependentDiscreteDist a -> [[a]] -> Double
    getJointProbEvent (IndependentDiscrete _ dists) vals = prod $ zipWith getProbEvent dists vals
    

-- conditional

-- TESTS --

testArr = multiarray [2, 3, 4] [(0::Int)..23]
testDist = trainDiscrete "example" Nothing 0 ['a', 'c', 'b', 'b', 'c', 'c', 'c', 'c', 'd', 'b']
testJointDist = (trainJointDiscrete ["var1", "var2", "var3"] Nothing 0 ["aA0", "aB1", "cB0", "dC0", "dA0", "bB0", "cC1", "aA0", "bB0", "aA0", "aC1", "dC1", "cA0", "cB1", "bB0", "aB1", "dA0", "dC1", "aA0", "aA1"]) :: JointDiscreteDist Char
testIndepDist = (trainJointDiscrete ["var1", "var2", "var3"] Nothing 0 ["aA0", "aB1", "cB0", "dC0", "dA0", "bB0", "cC1", "aA0", "bB0", "aA0", "aC1", "dC1", "cA0", "cB1", "bB0", "aB1", "dA0", "dC1", "aA0", "aA1"]) :: IndependentDiscreteDist Char

test :: IO ()
test = do
    let qc = quickCheck
    let approx = \x y -> abs (x - y) < 1e-12
    qc $ multiIndexToIndex testArr [1, 1, 1] == 17
    qc $ testArr ^!^ [1, 1, 3] == 19
    qc $ testArr ^:^ [Nothing, Nothing, Nothing] == testArr
    qc $ testArr ^:^ [Just [0], Nothing, Just [0, 2]] == MArray {sizes = [1,3,2], cumProds = [6,2,1], entries = V.fromList [0,2,4,6,8,10]}
    qc $ getProb testDist 'd' `approx` 0.1
    qc $ getProbEvent testDist "abc" `approx` 0.9
    qc $ getProbEvent testDist "" `approx` 0.0
    qc $ getJointProb testJointDist "aA0" `approx` 0.2
    qc $ getJointProbEvent testJointDist ["ab", "BC", ""] `approx` 0.0
    qc $ getJointProbEvent testJointDist ["ab", "BC", "0"] `approx` 0.15
    qc $ getJointProbEvent testJointDist ["abcd", "ABC", "01"] `approx` 1.0
    qc $ getJointProb testIndepDist "aA0" `approx` 0.096
    qc $ getJointProbEvent testIndepDist ["ab", "BC", ""] `approx` 0.0
    qc $ getJointProbEvent testIndepDist ["ab", "BC", "0"] `approx` 0.198
    qc $ getJointProbEvent testIndepDist ["abcd", "ABC", "01"] `approx` 1.0


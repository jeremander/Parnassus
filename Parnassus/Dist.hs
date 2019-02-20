{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}


module Parnassus.Dist where

import Control.Monad.Random (getRandoms, interleave, Rand)
import Data.Counter (count)
import Data.List (elemIndex, nub, scanl', sort, transpose, zip4, (\\))
import qualified Data.Map as M
import Data.Maybe (fromJust)
import qualified Data.Set as S
import qualified Data.Vector as VB
import qualified Data.Vector.Storable as V
import GHC.Exts (groupWith)
import qualified Numeric.Log as L
import System.Random
import Test.QuickCheck hiding (sample)

import Parnassus.Utils (merge, prod, selector)

import Debug.Trace

-- TYPES --

type Prob = Double
type IndexMap a = M.Map a Int

-- UTILITIES --

-- \log \sum_i \exp{x_i}
logSumExp :: [Double] -> Double
logSumExp = L.ln . L.sum . map L.Exp 

-- gets the index of an element in a vector
vecElemIndex :: (Eq a) => a -> VB.Vector a -> Maybe Int
vecElemIndex x v = elemIndex x (VB.toList v)

-- division with rule that 0 / 0 = 0
safeDiv :: Double -> Double -> Double
safeDiv 0 0 = 0
safeDiv x y = x / y

-- normalizes a vector of nonnegative numbers so that they sum to 1
normalizeVec :: V.Vector Double -> V.Vector Prob
normalizeVec v = V.map (flip safeDiv $ V.sum v) v

-- given an element e and a sorted vector v, returns the index i for which v[i] <= e < v[i + 1]
bisect :: (Ord a, V.Storable a) => a -> V.Vector a -> Int
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
multiarray :: (V.Storable a) => [Int] -> [a] -> MultiArray a
multiarray sizes entries = arr
    where
        cumProds = scanl' (*) 1 (reverse sizes)
        cumProds' = reverse $ init $ cumProds
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
            size = prod sizes
            f :: Int -> [Int] -> [Int]
            f i (cp:[])  = [i]
            f i (cp:cps) = i `quot` cp : f (i `rem` cp) cps

-- gets the entry at a multi-index
infixr 5 ^!^
(^!^) :: (V.Storable a) => MultiArray a -> [Int] -> a
(^!^) arr@(MArray {entries}) indices = entries V.! (multiIndexToIndex arr indices)

-- slices in multiple dimensions
-- the index consists of a list of Maybe [Int], where an actual list gives the slice indices, while Nothing means to include the whole axis
(^:^) :: (V.Storable a) => MultiArray a -> [Maybe [Int]] -> MultiArray a
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

data DiscreteDist v a = Discrete
    { var :: v,
      vals :: VB.Vector a,
      probs :: V.Vector Prob,
      cdf :: V.Vector Prob
    }
    deriving (Eq)

instance (Show v, Show a) => Show (DiscreteDist v a) where
    show :: DiscreteDist v a -> String
    show (Discrete {var, vals, probs}) = "discreteDist " ++ show var ++ " " ++ show (VB.toList vals) ++ " " ++ show (V.toList probs)

instance Functor (DiscreteDist v) where
    fmap :: (a -> b) -> DiscreteDist v a -> DiscreteDist v b
    fmap f (Discrete {var, vals, probs, cdf}) = Discrete {var = var, vals = VB.map f vals, probs = probs, cdf = cdf}

-- constructors --

-- main constructor for DiscreteDist
discreteDist :: (Ord a) => v -> [a] -> [Prob] -> DiscreteDist v a
discreteDist var vals probs
    | length probs /= length vals = error "mismatch between number of vals and probs"
    | V.minimum probVec < 0.0     = error "minimum prob must be >= 0"
    | otherwise                   = Discrete {var = var, vals = VB.fromList vals', probs = pmf, cdf = cdf}
        where
            -- aggregate probs for identical values
            groups = groupWith fst (zip vals probs)
            vals' = (fst . head) <$> groups
            probs' = (sum . map snd) <$> groups
            probVec = V.fromList probs'
            pmf = normalizeVec probVec
            cdf = V.scanl' (+) 0.0 pmf

-- normalizes the probability vector in a DiscreteDist
normalizeDist :: (Ord a) => DiscreteDist v a -> DiscreteDist v a
normalizeDist (Discrete {var, vals, probs}) = discreteDist var (VB.toList vals) (V.toList probs)

-- creates DiscreteDist from data (optionally include a smoothing constant)
trainDiscrete :: (Ord a) => v -> Maybe [a] -> Double -> [a] -> DiscreteDist v a
trainDiscrete var vals smooth xs = discreteDist var vals' counts
    where
        freqs = count xs
        observedVals = sort $ M.keys freqs
        vals' = case vals of
            Just vs -> sort $ nub vs
            Nothing -> observedVals
        counts = [(fromIntegral $ M.findWithDefault 0 val freqs) + smooth | val <- vals']

-- accessors --

-- converts a list of values to their corresponding indices
valsToIndices :: (Ord a, Show a) => DiscreteDist v a -> [a] -> [Int]
valsToIndices (Discrete {vals}) = map f
    where 
        valIdx = M.fromList $ zip (VB.toList vals) ([0..]::[Int])
        f val = case (M.lookup val valIdx) of
            Just i -> i
            Nothing -> error $ "invalid value " ++ show val

-- gets the probability of a particular outcome
getProb :: (Ord a, Show a) => DiscreteDist v a -> a -> Prob
getProb (Discrete {vals, probs}) val = case i of
    Just i' -> probs V.! i'
    Nothing -> error $ "invalid value " ++ show val
    where i = vecElemIndex val vals

-- gets the probability of an event (a set of particular outcomes)
getProbEvent :: (Ord a, Show a) => DiscreteDist v a -> [a] -> Prob
getProbEvent dist@(Discrete {probs}) vals = sum [probs V.! i | i <- valsToIndices dist (nub vals)]

-- gets the log-probability of a particular outcome
getLogProb :: (Ord a, Show a) => DiscreteDist v a -> a -> Double
getLogProb dist val = log $ getProb dist val

-- gets the log-probability of an event (a set of particular outcomes)
getLogProbEvent :: (Ord a, Show a) => DiscreteDist v a -> [a] -> Double
getLogProbEvent dist@(Discrete {probs}) vals = log $ getProbEvent dist vals

-- -- simulation --

class Simulable s a b where
    -- computes a single random sample from a distribution
    sample :: (RandomGen g) => s a -> Rand g b
    sample = (head <$>) . samples
    -- computes random samples from a distribution
    samples :: (RandomGen g) => s a -> Rand g [b]
    samples = sequence . repeat . interleave . sample


instance (b ~ a) => Simulable (DiscreteDist v) a b where
    samples :: (RandomGen g) => DiscreteDist v a -> Rand g [a]
    samples (Discrete {vals, cdf}) = do
        rs <- getRandoms
        return [vals VB.! (bisect r cdf) | r <- rs]


-- -- JOINT DISCRETE DISTRIBUTION --

class (Eq v, Ord v) => JointDiscreteDistribution d v a where
    -- gets the list of variables associated with the joint distribution
    getVars :: d v a -> [v]
    -- gets the probability of a particular outcome
    getJointProb :: d v a -> [a] -> Prob
    getJointProb dist val = exp $ getJointLogProb dist val
    -- gets the probability of an event (a set of particular outcomes)
    -- the event is given as a list of events for each variable, and the joint event is the Cartesian product of them all
    getJointProbEvent :: d v a -> [[a]] -> Prob
    getJointProbEvent dist vals = sum $ getJointProb dist <$> cartesianProduct vals
    -- gets the log-probability of a particular outcome
    getJointLogProb :: d v a -> [a] -> Double
    getJointLogProb dist val = log $ getJointProb dist val
    -- gets the log-probability of an event
    getJointLogProbEvent :: d v a -> [[a]] -> Double
    getJointLogProbEvent dist vals = logSumExp $ getJointLogProb dist <$> cartesianProduct vals
    -- marginal distribution over the given indices
    marginalizeIndices :: d v a -> [Int] -> d v a
    -- marginal distribution over the given variables
    marginalizeOver :: d v a -> [v] -> d v a
    marginalizeOver dist vars = marginalizeIndices dist indices
        where
            indicesByVar = M.fromList $ zip (getVars dist) [0..]
            indices = sort [fromJust $ M.lookup var indicesByVar | var <- nub vars]            
    -- marginalizes out the given variables (given in any order)
    marginalizeOut :: d v a -> [v] -> d v a
    marginalizeOut dist vars = marginalizeOver dist (getVars dist \\ vars)
    -- gets a list of all the marginal distributions for each variable
    marginals :: d v a -> [DiscreteDist v a]
    -- gets conditional distribution from a list of values (Nothing denotes full slice)
    conditionOn :: d v a -> [Maybe [a]] -> d v a
    -- "forgets" joint structure and returns a DiscreteDist
    forgetJoint :: d v a -> DiscreteDist [v] [a]

-- convert joint distribution into a 1D distribution on lists, then sample
instance {-# OVERLAPPABLE #-} (JointDiscreteDistribution d v a, b ~ [a]) => Simulable (d v) a b where
    samples :: (RandomGen g) => d v a -> Rand g [[a]]
    samples = samples . forgetJoint


-- JointDiscreteDist: full joint probability table --

data JointDiscreteDist v a = JointDiscrete
    { numVars :: Int,
      jVars :: [v],
      jVals :: [VB.Vector a],
      jValIdx :: [IndexMap a],
      jProbs :: MultiArray Prob,
      jCdf :: V.Vector Prob
    }
    deriving (Eq)

instance (Show v, Show a) => Show (JointDiscreteDist v a) where
    show :: JointDiscreteDist v a -> String
    show (JointDiscrete {jVars, jVals, jProbs}) = "jointDiscreteDist " ++ show jVars ++ " " ++ show jVals ++ " " ++ show jProbs

jointDiscrete :: (Eq v, Ord a, Show a) => [v] -> [[a]] -> MultiArray Prob -> JointDiscreteDist v a
jointDiscrete vars vals probs
    | numVars /= (length $ nub vars)  = error "duplicate var names not allowed"
    | probMin < 0.0                   = error "minimum prob must be >= 0"
    | otherwise                       = JointDiscrete {numVars = numVars, jVars = vars, jVals = jVals, jValIdx = jValIdx, jProbs = jProbs, jCdf = cdf}
        where
            numVars = length vars
            vals' = nub <$> vals
            numVals = length <$> vals'
            numCells = prod numVals
            jVals = VB.fromList <$> vals'
            jValIdx = [M.fromList $ zip vs [0..] | vs <- vals']
            probVec = entries probs
            probLen = V.length probVec
            probMin
                | probLen /= numCells = error "mismatch between number of vals and probs"
                | probLen == 0        = 0.0
                | otherwise           = V.minimum probVec
            probSum = V.sum probVec
            pmf = V.map (flip safeDiv $ probSum) probVec
            jProbs = MArray {sizes = sizes probs, cumProds = cumProds probs, entries = pmf}
            cdf = V.scanl' (+) 0.0 pmf

trainJointDiscrete :: (Eq v, Eq a, Ord a, Show a) => [v] -> Maybe [[a]] -> Int -> [[a]] -> JointDiscreteDist v a
trainJointDiscrete vars vals smooth xs = jointDiscrete vars vals' countArr
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

instance (Eq v, Ord v, Ord a, Show a) => JointDiscreteDistribution JointDiscreteDist v a where
    getVars :: JointDiscreteDist v a -> [v]
    getVars = jVars
    -- gets the probability of a particular outcome
    getJointProb :: JointDiscreteDist v a -> [a] -> Prob
    getJointProb (JointDiscrete {numVars, jValIdx, jProbs}) vals = jProbs ^!^ multiIdx
        where
            numVals = length vals
            multiIdx
                | (numVals == numVars) = zipWith (M.!) jValIdx vals
                | otherwise            = error $ show numVars ++ " values required, " ++ show numVals ++ " given"
    marginalizeIndices :: JointDiscreteDist v a -> [Int] -> JointDiscreteDist v a
    marginalizeIndices (JointDiscrete {numVars, jVars, jVals, jProbs}) indices = dist
        where
            sel :: [b] -> [b]
            sel = selector numVars indices
            sizes1 = sizes jProbs
            probs = entries jProbs
            size1 = V.length probs
            sizes2 = sel sizes1
            cumProds2 = reverse $ init $ scanl' (*) 1 (reverse sizes2)
            size2 = prod sizes2
            multiIndices1 = cartesianProduct [[0..(sz - 1)] | sz <- sizes1]
            multiIndices2 = sel <$> multiIndices1
            midxToIdx = \midx -> sum [cumProd * i | (cumProd, i) <- zip cumProds2 midx]
            updateInds = midxToIdx <$> multiIndices2
            updatePairs = zip updateInds (V.toList probs)
            probVec = V.replicate size2 0.0
            probs' = MArray {sizes = sizes2, cumProds = cumProds2, entries = V.accum (+) probVec updatePairs}
            dist = jointDiscrete (sel jVars) (VB.toList <$> (sel jVals)) probs'
    marginals :: JointDiscreteDist v a -> [DiscreteDist v a]
    marginals dist = zipWith3 discreteDist (jVars dist) (VB.toList <$> jVals dist) pss
        where pss = (V.toList . entries . jProbs) <$> [marginalizeIndices dist [i] | i <- [0..(numVars dist) - 1]]
    conditionOn :: JointDiscreteDist v a -> [Maybe [a]] -> JointDiscreteDist v a
    conditionOn (JointDiscrete {numVars, jVars, jVals, jValIdx, jProbs}) valss = dist'
        where
            getSliceDim valIdx vals = (fmap (valIdx M.!)) <$> vals
            numVals = length valss
            slice
                | (numVals == numVars) = zipWith getSliceDim jValIdx valss
                | otherwise            = error $ show numVars ++ " value lists required, " ++ show numVals ++ " given"
            probs = jProbs ^:^ slice
            getVals valIdx vals indices = case indices of
                Just inds -> (vals VB.!) <$> inds
                Nothing   -> VB.toList vals
            valss' = zipWith3 getVals jValIdx jVals slice
            dist = jointDiscrete jVars valss' probs
            -- marginalize out variables with a single value
            margVars = [var | (var, vals) <- zip jVars valss, (length <$> vals) == Just 1]
            dist' = marginalizeOut dist margVars
    forgetJoint :: JointDiscreteDist v a -> DiscreteDist [v] [a]
    forgetJoint (JointDiscrete {jVars, jVals, jProbs}) = discreteDist jVars vals probs
        where
            vals = cartesianProduct $ VB.toList <$> jVals
            probs = V.toList $ entries jProbs


-- IndependentDiscreteDist: joint probability factorized as several independent discrete distributions --

data IndependentDiscreteDist v a = IndependentDiscrete [DiscreteDist v a]
    deriving (Eq, Show)

instance Monoid (IndependentDiscreteDist v a) where
    mempty :: IndependentDiscreteDist v a
    mempty = IndependentDiscrete []
    mappend :: IndependentDiscreteDist v a -> IndependentDiscreteDist v a -> IndependentDiscreteDist v a
    mappend (IndependentDiscrete dists1) (IndependentDiscrete dists2) = IndependentDiscrete (dists1 ++ dists2)
    mconcat :: [IndependentDiscreteDist v a] -> IndependentDiscreteDist v a
    mconcat = IndependentDiscrete . concat . map extract
        where extract (IndependentDiscrete dists) = dists

independentDiscreteDist :: (Eq v) => [DiscreteDist v a] -> IndependentDiscreteDist v a
independentDiscreteDist dists
    | numVars /= (length $ nub $ var <$> dists) = error "duplicate var names not allowed"
    | otherwise                                 = IndependentDiscrete dists
        where numVars = length dists

trainJointIndependent :: (Eq v, Ord a) => [v] -> Maybe [[a]] -> Double -> [[a]] -> IndependentDiscreteDist v a
trainJointIndependent vars vals smooth xs = independentDiscreteDist dists
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

instance (Eq v, Ord v, Ord a, Show a) => JointDiscreteDistribution IndependentDiscreteDist v a where
    getVars :: IndependentDiscreteDist v a -> [v]
    getVars (IndependentDiscrete dists) = var <$> dists
    getJointProb :: IndependentDiscreteDist v a -> [a] -> Prob
    getJointProb (IndependentDiscrete dists) vals = prod $ zipWith getProb dists vals
    getJointProbEvent :: IndependentDiscreteDist v a -> [[a]] -> Prob
    getJointProbEvent (IndependentDiscrete dists) valss = prod $ zipWith getProbEvent dists valss
    getJointLogProb :: IndependentDiscreteDist v a -> [a] -> Double
    getJointLogProb (IndependentDiscrete dists) vals = sum $ zipWith getLogProb dists vals
    getJointLogProbEvent :: IndependentDiscreteDist v a -> [[a]] -> Double
    getJointLogProbEvent (IndependentDiscrete dists) valss = sum $ zipWith getLogProbEvent dists valss
    marginalizeIndices :: IndependentDiscreteDist v a -> [Int] -> IndependentDiscreteDist v a
    marginalizeIndices (IndependentDiscrete dists) indices = independentDiscreteDist $ selector numVars indices dists
        where numVars = length dists
    marginals :: IndependentDiscreteDist v a -> [DiscreteDist v a]
    marginals (IndependentDiscrete dists) = dists
    conditionOn :: IndependentDiscreteDist v a -> [Maybe [a]] -> IndependentDiscreteDist v a
    conditionOn (IndependentDiscrete dists) valss = independentDiscreteDist dists'
        where
            getCondDist dist@(Discrete {var, vals, probs}) vals' = case vals' of
                Just vs -> discreteDist var vs (((probs V.!) . fromJust . (flip vecElemIndex vals)) <$> vs)
                Nothing -> dist
            dists' = filter (\dist -> (VB.length $ vals dist) > 1) (zipWith getCondDist dists valss)
    forgetJoint :: IndependentDiscreteDist v a -> DiscreteDist [v] [a]
    forgetJoint (IndependentDiscrete dists) = discreteDist vars vals' probs'
        where
            vars = var <$> dists
            vals' = cartesianProduct $ (VB.toList . vals) <$> dists
            probs' = prod <$> (cartesianProduct $ (V.toList . probs) <$> dists)


-- TESTS --

testArr = multiarray [2, 3, 4] [(0::Int)..23]
testDist = trainDiscrete "example" Nothing 0 ['a', 'c', 'b', 'b', 'c', 'c', 'c', 'c', 'd', 'b']
testJointDist = (trainJointDiscrete ["var1", "var2", "var3"] Nothing 0 ["aA0", "aB1", "cB0", "dC0", "dA0", "bB0", "cC1", "aA0", "bB0", "aA0", "aC1", "dC1", "cA0", "cB1", "bB0", "aB1", "dA0", "dC1", "aA0", "aA1"]) :: JointDiscreteDist String Char
testIndepDist = (trainJointIndependent ["var1", "var2", "var3"] Nothing 0 ["aA0", "aB1", "cB0", "dC0", "dA0", "bB0", "cC1", "aA0", "bB0", "aA0", "aC1", "dC1", "cA0", "cB1", "bB0", "aB1", "dA0", "dC1", "aA0", "aA1"]) :: IndependentDiscreteDist String Char

test :: IO ()
test = do
    let tol = 1e-12
    let qc = quickCheck
    let approx = \x y -> abs (x - y) < tol
    let approxVec = \x y -> V.maximum (V.map abs (V.zipWith (-) x y)) < tol
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
    qc $ (V.maximum $ V.map abs $ V.zipWith (-) (probs $ marginals testJointDist !! 0) (probs $ marginals testIndepDist !! 0)) < tol
    qc $ (testJointDist `conditionOn` [Just "a", Nothing, Nothing]) == (trainJointDiscrete ["var2", "var3"] Nothing 0 ["A0", "B1", "A0", "A0", "C1", "B1", "A0", "A1"])
    qc $ (testIndepDist `conditionOn` [Just "a", Nothing, Nothing]) == (testIndepDist `marginalizeOut` ["var1"])
    qc $ approxVec (entries $ jProbs testJointDist) (probs $ forgetJoint testJointDist)

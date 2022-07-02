{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeFamilies #-}


module Math.Dist where

import Control.Monad.Random (getRandoms, interleave, Rand)
import Data.Counter (count)
import Data.List ((\\), elemIndex, nub, scanl', sort, transpose)
import qualified Data.Map as M
import Data.Maybe (fromJust)
import qualified Data.Vector as VB
import qualified Data.Vector.Storable as V
import GHC.Exts (groupWith)
import qualified Numeric.Log as L
import System.Random (RandomGen)

import Misc.Utils (justOrError, safeDiv, selector)
import Math.Multiarray ((^!^), (^:^), MultiArray(..), multiarray)


-- * Types

type Prob = Double
type IndexMap a = M.Map a Int

-- * Utilities

-- \log \sum_i \exp{x_i}
logSumExp :: [Double] -> Double
logSumExp = L.ln . L.sum . map L.Exp

-- | Gets the index of an element in a vector.
vecElemIndex :: (Eq a) => a -> VB.Vector a -> Maybe Int
vecElemIndex x v = elemIndex x (VB.toList v)

-- | Normalizes a vector of nonnegative numbers so that they sum to 1.
normalizeVec :: V.Vector Double -> V.Vector Prob
normalizeVec v = V.map (flip safeDiv $ V.sum v) v

-- | Given an element e and a sorted vector v, returns the index i for which v[i] <= e < v[i + 1].
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

-- | Computes the Cartesian product of several lists.
cartesianProduct :: [[a]] -> [[a]]
cartesianProduct [] = [[]]
cartesianProduct (x:xs) = concat [[y : item | item <- prev] | y <- x]
    where prev = cartesianProduct xs


-- * Discrete Distribution

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

-- ** Constructors

-- | Main constructor for 'DiscreteDist'.
discreteDist :: (Ord a) => v -> [a] -> [Prob] -> DiscreteDist v a
discreteDist var vals probs
    | null probs                  = error "cannot have empty distribution"
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

-- | Normalizes the probability vector in a 'DiscreteDist'.
normalizeDist :: (Ord a) => DiscreteDist v a -> DiscreteDist v a
normalizeDist (Discrete {var, vals, probs}) = discreteDist var (VB.toList vals) (V.toList probs)

-- | Creates 'DiscreteDist' from data (optionally include a smoothing constant).
trainDiscrete :: (Ord a) => v -> Maybe [a] -> Double -> [a] -> DiscreteDist v a
trainDiscrete var vals smooth xs = discreteDist var vals' counts
    where
        freqs = count xs
        observedVals = sort $ M.keys freqs
        vals' = maybe observedVals nub vals
        counts = [(fromIntegral $ M.findWithDefault 0 val freqs) + smooth | val <- vals']

uniformDiscreteDist :: (Ord a) => v -> [a] -> DiscreteDist v a
uniformDiscreteDist var vals = discreteDist var vals' (replicate n p)
    where
        vals' = nub vals
        n = length vals'
        p = 1 / fromIntegral n

-- ** Accessors

-- | Converts a list of values to their corresponding indices.
valsToIndices :: (Ord a, Show a) => DiscreteDist v a -> [a] -> [Int]
valsToIndices (Discrete {vals}) = map f
    where
        valIdx = M.fromList $ zip (VB.toList vals) ([0..]::[Int])
        f val = justOrError (M.lookup val valIdx) ("invalid value " ++ show val)

-- | Gets the probability of a particular outcome.
getProb :: (Ord a, Show a) => DiscreteDist v a -> a -> Prob
getProb (Discrete {vals, probs}) val = case i of
    Just i' -> probs V.! i'
    --Nothing -> error $ "invalid value " ++ show val
    Nothing -> 0.0  -- silently return 0 probability
    where i = vecElemIndex val vals

-- | Gets the probability of an event (a set of particular outcomes).
getProbEvent :: (Ord a, Show a) => DiscreteDist v a -> [a] -> Prob
getProbEvent dist@(Discrete {probs}) vals = sum [probs V.! i | i <- valsToIndices dist (nub vals)]

-- | Gets the log-probability of a particular outcome.
getLogProb :: (Ord a, Show a) => DiscreteDist v a -> a -> Double
getLogProb dist val = log $ getProb dist val

-- | Gets the log-probability of an event (a set of particular outcomes).
getLogProbEvent :: (Ord a, Show a) => DiscreteDist v a -> [a] -> Double
getLogProbEvent dist vals = log $ getProbEvent dist vals

-- * Simulation

class Simulable s a b where
    -- | Computes a single random sample from a distribution.
    sample :: (RandomGen g) => s a -> Rand g b
    sample = (head <$>) . samples
    -- | Computes random samples from a distribution.
    samples :: (RandomGen g) => s a -> Rand g [b]
    samples = sequence . repeat . interleave . sample

instance (b ~ a) => Simulable (DiscreteDist v) a b where
    samples :: (RandomGen g) => DiscreteDist v a -> Rand g [a]
    samples (Discrete {vals, cdf}) = do
        rs <- getRandoms
        return [vals VB.! (bisect r cdf) | r <- rs]

-- | Samples a 'DiscreteDist' without replacement (producing a permutation of the elements).
samplesWithoutReplacement :: (RandomGen g, Eq a) => DiscreteDist v a -> Rand g [a]
samplesWithoutReplacement dist@(Discrete {var, vals, probs})
    | totalProb == 0 = return []
    | otherwise = do
        val <- sample dist
        let (vals', probs') = unzip $ filter ((/= val) . fst) (zip (VB.toList vals) (V.toList probs))
        let positiveSum = sum probs' > 0.0
        rest <- case (vals', positiveSum) of
            ((_:_), True) -> samplesWithoutReplacement dist'
                where
                    vals'' = VB.fromList vals'
                    probs'' = normalizeVec $ V.fromList probs'
                    cdf'' = V.scanl' (+) 0.0 probs''
                    dist' = Discrete {var = var, vals = vals'', probs = probs'', cdf = cdf''}
            _             -> return []
        return (val : rest)
        where totalProb = V.sum probs


-- * Joint Discrete Distribution

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

-- | Convert joint distribution into a 1D distribution on lists, then sample.
instance {-# OVERLAPPABLE #-} (JointDiscreteDistribution d v a, b ~ [a]) => Simulable (d v) a b where
    samples :: (RandomGen g) => d v a -> Rand g [[a]]
    samples = samples . forgetJoint


-- ** 'JointDiscreteDist': full joint probability table

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
            numCells = product numVals
            jVals = VB.fromList <$> vals'
            jValIdx = [M.fromList $ zip vs [0..] | vs <- vals']
            probVec = entries probs
            probLen = V.length probVec
            probMin
                | probLen /= numCells = error "mismatch between number of vals and probs"
                | probLen == 0        = 0.0
                | otherwise           = V.minimum probVec
            probSum = V.sum probVec
            pmf = V.map (`safeDiv` probSum) probVec
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
                where vs' | (length vs == numVars) = nub <$> vs
                          | otherwise              = error $ show numVars ++ " value lists required, " ++ show (length vs) ++ " given"
            Nothing -> observedVals
        valProduct = cartesianProduct vals'
        counts = [fromIntegral $ M.findWithDefault 0 val freqs + smooth | val <- valProduct]
        countArr = multiarray (length <$> vals') counts

instance (Eq v, Ord v, Ord a, Show a) => JointDiscreteDistribution JointDiscreteDist v a where
    getVars :: JointDiscreteDist v a -> [v]
    getVars = jVars
    -- | Gets the probability of a particular outcome.
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
            sizes2 = sel sizes1
            cumProds2 = tail $ reverse $ scanl' (*) 1 (reverse sizes2)
            size2 = product sizes2
            multiIndices1 = cartesianProduct [[0..(sz - 1)] | sz <- sizes1]
            multiIndices2 = sel <$> multiIndices1
            midxToIdx midx = sum [cumProd * i | (cumProd, i) <- zip cumProds2 midx]
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
            getVals _ vals indices = case indices of
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


-- ** 'IndependentDiscreteDist': joint probability factorized as several independent discrete distributions

newtype IndependentDiscreteDist v a = IndependentDiscrete [DiscreteDist v a]
    deriving (Eq, Show)

instance Semigroup (IndependentDiscreteDist v a) where
    (IndependentDiscrete dists1) <> (IndependentDiscrete dists2) = IndependentDiscrete (dists1 ++ dists2)

instance Monoid (IndependentDiscreteDist v a) where
    mempty = IndependentDiscrete []
    mconcat = IndependentDiscrete . concatMap extract
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
    getJointProb (IndependentDiscrete dists) vals = product $ zipWith getProb dists vals
    getJointProbEvent :: IndependentDiscreteDist v a -> [[a]] -> Prob
    getJointProbEvent (IndependentDiscrete dists) valss = product $ zipWith getProbEvent dists valss
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
                Just vs -> discreteDist var vs (((probs V.!) . fromJust . (`vecElemIndex` vals)) <$> vs)
                Nothing -> dist
            dists' = filter (\dist -> (VB.length $ vals dist) > 1) (zipWith getCondDist dists valss)
    forgetJoint :: IndependentDiscreteDist v a -> DiscreteDist [v] [a]
    forgetJoint (IndependentDiscrete dists) = discreteDist vars vals' probs'
        where
            vars = var <$> dists
            vals' = cartesianProduct $ (VB.toList . vals) <$> dists
            probs' = product <$> (cartesianProduct $ (V.toList . probs) <$> dists)

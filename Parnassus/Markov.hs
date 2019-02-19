{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeFamilies #-}


module Parnassus.Markov where

import Control.Monad.Loops (unfoldrM)
import Data.Counter (count)
import Data.List (nub, sort)
import qualified Data.Map as M
import Data.Maybe (fromJust, isJust)
import qualified Data.Vector as VB
import qualified Data.Vector.Storable as V
import qualified Numeric.LinearAlgebra as LA
import System.Random (newStdGen, randoms)

import Parnassus.Dist

data MarkovModel v a = Markov { pi0 :: DiscreteDist v a, trans :: LA.Matrix Prob }
    deriving (Eq, Show)

-- construct a MarkovModel from an initial distribution and a transition matrix (entry i, j is probability of transition from i to j, so that the matrix is row-stochastic)
markov :: DiscreteDist v a -> [V.Vector Prob] -> MarkovModel v a
markov dist rows
    | length rows /= n               = error $ "transition matrix must have " ++ show n ++ " rows"
    | any (/= n) $ V.length <$> rows = error $ "all rows of transition matrix must have length " ++ show n
    | otherwise                      = Markov {pi0 = dist, trans = mat}
    where
        n = length $ vals dist
        mat
            | (minimum $ V.sum <$> rows) <= 0.0 = error "row sums must be positive"
            | otherwise                       = LA.fromRows $ normalizeVec <$> rows


instance (Enum v, Num v, Ord v, Ord a, Show a) => JointDiscreteDistribution MarkovModel v a where
    -- vars are always [0, 1, 2, ...]
    getVars :: MarkovModel v a -> [v]
    getVars _ = [0..]
    getJointProbEvent :: MarkovModel v a -> [[a]] -> Prob
    getJointProbEvent dist valss = exp $ getJointLogProbEvent dist valss
    getJointLogProb :: MarkovModel v a -> [a] -> Double
    getJointLogProb _ []                       = 0.0
    getJointLogProb (Markov {pi0, trans}) vals = sum logProbs
        where
            indices = valsToIndices pi0 vals
            transitionIndices = zip indices (tail indices)
            logProbs = log <$> ((probs pi0 V.! (head indices)) : (LA.atIndex trans <$> transitionIndices))
    -- marginal distribution is no longer a simple Markov chain
    marginalizeIndices :: MarkovModel v a -> [Int] -> MarkovModel v a
    marginalizeIndices = undefined
    -- infinite list of marginal distributions
    marginals :: MarkovModel v a -> [DiscreteDist v a]
    marginals (Markov {pi0, trans}) = dists
        where
            pis = iterate (LA.<# trans) (probs pi0)
            vals' = vals pi0
            pmfs = normalizeVec <$> pis
            cdfs = V.scanl' (+) 0.0 <$> pmfs
            dists = [Discrete {var = i, vals = vals', probs = pmf', cdf = cdf'} | (i, pmf', cdf') <- zip3 [0..] pmfs cdfs]
    -- conditional distribution is no longer a simple Markov chain
    conditionOn :: MarkovModel v a -> [Maybe [a]] -> MarkovModel v a
    conditionOn = undefined
    -- distribution is infinite so can't be represented as a DiscreteDist
    forgetJoint :: MarkovModel v a -> DiscreteDist [v] [a]
    forgetJoint = undefined

-- trains a Markov model from data
-- smoothing constant is how much to add to each entry of transition matrix, and to each entry of initial vector
trainMarkovModel :: (Ord a, Show a) => Maybe [a] -> Double -> [[a]] -> MarkovModel Integer a
trainMarkovModel vals smooth chains = markov pi0 (LA.toRows trans)
    where
        allVals = case vals of
            Just vs -> vs
            Nothing -> sort $ nub $ concat chains
        chains' = filter (not . null) chains
        pi0 = trainDiscrete 0 (Just allVals) smooth (head <$> chains)
        indices = valsToIndices pi0 <$> chains
        transitionPairs = concat [zip inds (tail inds) | inds <- indices]
        n = length allVals
        addSmooth = \(key, val) -> (key, val + smooth)
        trans = LA.assoc (n, n) smooth (addSmooth <$> (M.toList $ count transitionPairs))

-- type for values in a Markov chain
data Token a = Token a | Stop
    deriving (Eq, Ord, Show)

-- converts elements to Justs and appends a final Nothing element
-- this Nothing value can be used as a terminating value for Markov chain generation
terminate :: [a] -> [Token a]
terminate = (++ [Stop]) . map Token

-- samples a Markov model (infinitely)
markovSample :: MarkovModel v a -> IO [a]
markovSample (Markov {pi0, trans}) = do
    gen <- newStdGen
    let rs = randoms gen
    let indices = [bisect (head rs) cdf0] ++ [bisect r (transCdfs VB.! i) | (i, r) <- zip indices (tail rs)]
    return $ (vs VB.!) <$> indices
    where
        vs = vals pi0
        cdf0 = cdf pi0
        transCdfs = VB.fromList $ (V.scanl' (+) 0.0) <$> (LA.toRows trans)

instance (b ~ [a]) => Simulable (MarkovModel v) a b where
    sample :: MarkovModel v a -> IO [a]
    sample = markovSample
    -- FIXIT: this will hang!!  Migrate to MonadRandom?
    samples :: MarkovModel v a -> IO [[a]]
    --samples = sequence . repeat . sample
    samples = unfoldrM f
        where
            f dist = (\samp -> Just (samp, dist)) <$> sample dist

instance {-# OVERLAPPING #-} (b ~ [a]) => Simulable (MarkovModel v) (Token a) b where
    -- override behavior so the chain terminates upon reaching Stop
    sample = fmap (map fromToken . takeWhile (not . isStop)) . markovSample
        where
            isStop Stop = True
            isStop _    = False
            fromToken Stop      = error "Stop"
            fromToken (Token t) = t
    --samples = sequence . repeat . sample
    samples = unfoldrM f
        where
            f dist = (\samp -> Just (samp, dist)) <$> sample dist


testMarkov = trainMarkovModel Nothing 0.01 ([terminate "the quick brown fox jumps over the lazy dog"])
samps = sample testMarkov
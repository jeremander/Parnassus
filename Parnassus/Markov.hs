{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeFamilies #-}


module Parnassus.Markov where

import Control.Monad.Random (getRandoms, Rand, RandomGen)
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
        smooth' = smooth / fromIntegral n  -- total smoothing in each row equals the smooth parameter
        addSmooth = \(key, val) -> (key, val + smooth')
        trans = LA.assoc (n, n) smooth (addSmooth <$> (M.toList $ count transitionPairs))

-- samples a Markov model (infinitely)
markovSample :: (RandomGen g) => MarkovModel v a -> Rand g [a]
markovSample (Markov {pi0, trans}) = do
    rs <- getRandoms
    let indices = [bisect (head rs) cdf0] ++ [bisect r (transCdfs VB.! i) | (i, r) <- zip indices (tail rs)]
    return $ (vs VB.!) <$> indices
    where
        vs = vals pi0
        cdf0 = cdf pi0
        transCdfs = VB.fromList $ (V.scanl' (+) 0.0) <$> (LA.toRows trans)

instance (b ~ [a]) => Simulable (MarkovModel v) a b where
    sample :: (RandomGen g) => MarkovModel v a -> Rand g [a]
    sample = markovSample

-- TERMINATING MARKOV CHAINS --

-- type for values in a Markov chain
data Token a = Start | Token a | Stop
    deriving (Eq, Ord, Show)

isStart :: Token a -> Bool
isStart Start = True
isStart _     = False

isToken :: Token a -> Bool
isToken (Token _) = True
isToken _         = False

fromToken :: Token a -> a
fromToken (Token t) = t
fromToken _         = error "invalid token"

-- converts elements to Tokens and appends a final Stop element
-- this Nothing value can be used as a terminating value for Markov chain generation
terminate :: [a] -> [Token a]
terminate = (++ [Stop]) . map Token

trainTerminatingMarkovModel :: (Ord a, Show a) => Maybe [a] -> Double -> [[a]] -> MarkovModel Integer (Token a)
trainTerminatingMarkovModel vals smooth chains = trainMarkovModel (map Token <$> vals) smooth (terminate <$> chains)

instance {-# OVERLAPPING #-} (b ~ [a]) => Simulable (MarkovModel v) (Token a) b where
    -- override behavior so sampling skips over Start and terminates upon reaching Stop
    sample = fmap (map fromToken . takeWhile isToken . dropWhile isStart) . markovSample

-- POLYGRAPHIC MODELS --

newtype Polygraph a = Poly { getPoly :: [a] }
    deriving (Eq, Ord, Show)

-- newtype NgramModel a = NgramModel { getMarkov :: MarkovModel Integer (Polygraph (Token a)) }
--     deriving (Eq, Show)

-- computes n-grams from a list of items
ngrams :: Int -> [a] -> [[a]]
ngrams n xs 
    | (n <= length xs) = take n xs : ngrams n (drop 1 xs)
    | otherwise = []


-- stores n, initial distribution on alphabet, and transition probability matrices for each Markovity level
data NgramModel a = Ngrams Int (DiscreteDist a) [LA.Matrix Prob]
    deriving (Eq, Show)


-- construct a MarkovModel from an initial distribution and a transition matrix (entry i, j is probability of transition from i to j, so that the matrix is row-stochastic)
markov :: DiscreteDist v a -> [V.Vector Prob] -> MarkovModel v a
-- markov dist rows
--     | length rows /= n               = error $ "transition matrix must have " ++ show n ++ " rows"
--     | any (/= n) $ V.length <$> rows = error $ "all rows of transition matrix must have length " ++ show n
--     | otherwise                      = Markov {pi0 = dist, trans = mat}
--     where
--         n = length $ vals dist
--         mat
--             | (minimum $ V.sum <$> rows) <= 0.0 = error "row sums must be positive"
--             | otherwise                       = LA.fromRows $ normalizeVec <$> rows

-- trainNgramModel :: (Ord a, Show a) => Int -> Maybe [a] -> Double -> [[a]] -> NgramModel a
-- trainNgramModel n alphabet smooth chains = NgramModel mkov
--     where
--         alphabet' = case alphabet of
--             Just alph -> alph
--             Nothing   -> sort $ nub $ concat chains
--         alphabetTokens = [Start, Stop] ++ (Token <$> alphabet')
--         allNgrams = Poly <$> cartesianProduct (replicate n alphabetTokens)
--         fixChain :: [a] -> [Token a]
--         fixChain chain = (replicate (n - 1) Start) ++ (Token <$> chain) ++ [Stop]
--         observedNgrams = (map Poly . ngrams n . fixChain) <$> chains
--         mkov = trainMarkovModel (Just allNgrams) smooth observedNgrams

-- instance (b ~ [a]) => Simulable NgramModel a b where
--     sample (NgramModel markov) = do
--         samp <- sample markov
--         let toks = takeWhile isToken $ dropWhile isStart $ head . getPoly <$> samp
--         return $ fromToken <$> toks



testMarkov = trainTerminatingMarkovModel Nothing 0.01 (["the quick brown fox jumps over the lazy dog", "i like pizza", "this is english", "beef curd stew", "to be or not to be that is the question", "fourscore and seven years ago", "give me fuel give me fire give me that which i desire"])
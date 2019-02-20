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

rowStochasticMatrix :: [V.Vector Double] -> LA.Matrix Prob
rowStochasticMatrix rows = LA.fromRows $ normalizeVec <$> rows

-- construct a MarkovModel from an initial distribution and a transition matrix (entry i, j is probability of transition from i to j, so that the matrix is row-stochastic)
markov :: DiscreteDist v a -> [V.Vector Prob] -> MarkovModel v a
markov dist rows
    | length rows /= n               = error $ "transition matrix must have " ++ show n ++ " rows"
    | any (/= n) $ V.length <$> rows = error $ "all rows of transition matrix must have length " ++ show n
    | otherwise                      = Markov {pi0 = dist, trans = mat}
    where
        n = length $ vals dist
        mat = rowStochasticMatrix rows

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
    conditionOn = undefined
    -- distribution is infinite so can't be represented as a DiscreteDist
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
data Token a = Token a | Stop
    deriving (Eq, Ord, Show)

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
    -- override behavior so sampling terminates upon reaching Stop
    sample = fmap (map fromToken . takeWhile isToken) . markovSample

-- N-GRAM MODELS --

-- computes n-grams from a list of items
ngrams :: Int -> [a] -> [[a]]
ngrams n xs 
    | (n <= length xs) = take n xs : ngrams n (drop 1 xs)
    | otherwise = []

-- stores n, mapping from alphabet to indices, and transition probability matrices for each Markovity level
data NgramModel v a = Ngrams Int (IndexMap (Token a)) [MultiArray Prob]
    deriving (Eq, Show)

instance (Enum v, Num v, Ord v, Ord a, Show a) => JointDiscreteDistribution NgramModel v a where
    getVars :: NgramModel v a -> [v]
    getVars _ = [0..]
    getJointProbEvent :: NgramModel v a -> [[a]] -> Prob
    getJointProbEvent dist valss = exp $ getJointLogProbEvent dist valss
    getJointLogProb :: NgramModel v a -> [a] -> Double
    getJointLogProb (Ngrams n tokIdx mats) vals = sum logprobs
        where
            toks = terminate vals
            indices = (tokIdx M.!) <$> toks
            idxGrams = [take i indices | i <- [1..(n - 1)]] ++ ngrams n indices
            matSeq = init mats ++ repeat (last mats)
            logprobs = log <$> zipWith (^!^) matSeq idxGrams
    marginalizeIndices = undefined
    marginals = undefined
    conditionOn = undefined
    forgetJoint = undefined

trainNgramModel :: (Ord a, Show a) => Int -> Maybe [a] -> Double -> [[a]] -> NgramModel Integer a
trainNgramModel n alphabet smooth chains = Ngrams n tokIdx transArrs
    where
        alphabet' = case alphabet of
            Just alph -> alph
            Nothing   -> sort $ nub $ concat chains
        alphabetTokens = [Stop] ++ (Token <$> alphabet')
        m = length alphabetTokens
        tokIdx = M.fromList $ zip alphabetTokens [0..]  -- mapping from alphabet values to indices
        stoppedChains = terminate <$> chains
        idxChains = map (tokIdx M.!) <$> stoppedChains
        getTransitions :: Int -> MultiArray Prob
        getTransitions k = multiarray (replicate k m) (concat rows)
            where
                pairs = concat $ (map (splitAt (k - 1)) . ngrams k) <$> idxChains
                cumProds = (m ^) <$> [(k - 2)..0]
                transitionPairs = [(sum $ zipWith (*) cumProds fromInds, head toInds) | (fromInds, toInds) <- pairs]
                addSmooth = \(key, val) -> (key, val + smooth)
                trans = LA.assoc (m ^ (k - 1), m) smooth (addSmooth <$> (M.toList $ count transitionPairs))
                rows = V.toList . normalizeVec <$> LA.toRows trans
        transArrs = getTransitions <$> [1..n]





-- instance (b ~ [a]) => Simulable (NgramModel v) a b where
--     sample (Ngrams n tokIdx mats) = do
        -- samp <- sample markov
        -- let toks = takeWhile isToken $ dropWhile isStart $ head . getPoly <$> samp
        -- return $ fromToken <$> toks

    --         getJointLogProb :: NgramModel v a -> [a] -> Double
    -- getJointLogProb (Ngrams n tokIdx mats) vals = sum logprobs


testText = ["the quick brown fox jumps over the lazy dog", "i like pizza", "this is english", "beef curd stew", "to be or not to be that is the question", "fourscore and seven years ago", "give me fuel give me fire give me that which i desire"]
testMarkov = trainTerminatingMarkovModel Nothing 0.01 testText
test1gram = trainNgramModel 1 Nothing 0 ["abccc"]
test2gram = trainNgramModel 2 Nothing 0 ["abccc"]
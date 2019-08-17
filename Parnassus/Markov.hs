{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeFamilies #-}


module Parnassus.Markov where

import Control.Monad.Random (getRandoms, Rand, RandomGen)
import Data.Counter (count)
import Data.List (nub, sort)
import Data.List.Split (splitOn)
import qualified Data.Map as M
import Data.Maybe (fromJust, isJust)
import qualified Data.Set as S
import qualified Data.Vector as VB
import qualified Data.Vector.Storable as V
import qualified Numeric.LinearAlgebra as LA

import Parnassus.Utils (ngrams)
import Parnassus.Dist

import System.IO.Unsafe

-- MARKOV MODEL --

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
            pis = normalizeVec <$> iterate (LA.<# trans) (probs pi0)
            vals' = vals pi0
            cdfs = V.scanl' (+) 0.0 <$> pis
            dists = [Discrete {var = i, vals = vals', probs = pmf, cdf = cdf'} | (i, pmf, cdf') <- zip3 [0..] pis cdfs]
    -- conditional distribution is no longer a simple Markov chain
    conditionOn = undefined
    -- distribution is infinite so can't be represented as a DiscreteDist
    forgetJoint = undefined

-- computes conditional distribution on a single variable given the nearest preceding and succeeding variable (if existent)
-- input is t and a pair ((k, val), (l, val')), and we compute P(X_t | X_{t - k} = val, X_{t + l} = val')
-- each subpair can also be Nothing, indicating the absence of such a condition
markovConditionOn :: (Integral v, Ord a) => MarkovModel v a -> v -> (Maybe (Int, a), Maybe (Int, a)) -> DiscreteDist v a
markovConditionOn (Markov {pi0, trans}) t pair = Discrete {var = t, vals = vals', probs = pmf', cdf = cdf'}
    where
        stepRow :: V.Vector Prob -> Int -> V.Vector Prob
        stepRow vec k = (iterate (LA.<# trans) vec) !! k
        stepCol :: V.Vector Prob -> Int -> V.Vector Prob
        stepCol vec k = (iterate (trans LA.#>) vec) !! k
        pis = iterate (LA.<# trans) (probs pi0)
        vals' = vals pi0
        n = VB.length vals'
        indVec :: Int -> V.Vector Prob
        indVec i = V.generate n (\j -> if (j == i) then 1.0 else 0.0)
        vecMul = V.zipWith (*)
        t' = (fromInteger $ toInteger t) :: Int
        valIdx = M.fromList $ zip (VB.toList (vals pi0)) [0..]
        val2Index = (valIdx M.!)
        pmf = case pair of
            (Nothing, Nothing)              -> pis !! t'
            (Just (k, val), Nothing)        -> stepRow (indVec $ val2Index val) k
            (Nothing, Just (l, val'))       -> stepCol (indVec $ val2Index val') l `vecMul` (pis !! t')
            (Just (k, val), Just (l, val')) -> stepCol (indVec $ val2Index val') l `vecMul` stepRow (indVec $ val2Index val) k
        pmf' = normalizeVec pmf
        cdf' = V.scanl' (+) 0.0 pmf'

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

-- given a distribution on integers steps, creates a MarkovModel representing the random walk on the i.i.d. steps
-- must be bounded within a finite range
boundedIntegerRandomWalk :: DiscreteDist v Int -> (Int, Int) -> MarkovModel Integer Int
boundedIntegerRandomWalk dist (low, high) = markov pi0 rows
    where
        n = high - low + 1
        pi0 = trainDiscrete 0 (Just [low..high]) 0.0 [0]
        rows = [V.generate n (\j -> getProb dist (j + low - i)) | i <- [low..high]]

-- gets backward transition probabilities P(X_n = i | X_{n+1} = j)
backwardTransitions :: MarkovModel v a -> [V.Vector Prob]
backwardTransitions (Markov {trans}) = normalizeVec <$> (LA.toRows $ LA.tr trans)

-- TERMINATING MARKOV MODEL --

-- type for values in a terminating Markov chain
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
trainTerminatingMarkovModel vals smooth chains = trainMarkovModel tokens smooth (terminate <$> chains)
    where tokens = (Stop :) . map Token <$> vals

instance {-# OVERLAPPING #-} (b ~ [a]) => Simulable (MarkovModel v) (Token a) b where
    -- override behavior so sampling terminates upon reaching Stop
    sample = fmap (map fromToken . takeWhile isToken) . markovSample

-- N-GRAM MODELS --

-- stores n, alphabet, mapping from alphabet to indices, and transition probability matrices for each Markovity level
data NgramModel v a = Ngrams Int (VB.Vector a) (IndexMap a) [MultiArray Prob]
    deriving (Eq, Show)

instance (Enum v, Num v, Ord v, Ord a, Show a) => JointDiscreteDistribution NgramModel v a where
    getVars :: NgramModel v a -> [v]
    getVars _ = [0..]
    getJointProbEvent :: NgramModel v a -> [[a]] -> Prob
    getJointProbEvent dist valss = exp $ getJointLogProbEvent dist valss
    getJointLogProb :: NgramModel v a -> [a] -> Double
    getJointLogProb (Ngrams n _ valIdx mats) vals = sum logprobs
        where
            indices = (valIdx M.!) <$> vals
            idxGrams = [take i indices | i <- [1..(n - 1)]] ++ ngrams n indices
            matSeq = init mats ++ repeat (last mats)
            logprobs = log <$> zipWith (^!^) matSeq idxGrams
    marginalizeIndices = undefined
    marginals = undefined
    conditionOn = undefined
    forgetJoint = undefined

trainNgramModel :: (Ord a, Show a) => Int -> Maybe [a] -> Double -> [[a]] -> NgramModel Integer a
trainNgramModel n alphabet smooth chains = Ngrams n (VB.fromList alphabet') valIdx transArrs
    where
        alphabet' = case alphabet of
            Just alph -> alph
            Nothing   -> sort $ nub $ concat chains
        m = length alphabet'
        valIdx = M.fromList $ zip alphabet' [0..]  -- mapping from alphabet values to indices
        idxChains = map (valIdx M.!) <$> chains
        getTransitions :: Int -> MultiArray Prob
        getTransitions k = multiarray (replicate k m) (concat rows)
            where
                pairs = concat $ (map (splitAt (k - 1)) . ngrams k) <$> idxChains
                cumProds = (m ^) <$> reverse [0..(k - 2)]
                transitionPairs = [(sum $ zipWith (*) cumProds fromInds, head toInds) | (fromInds, toInds) <- pairs]
                addSmooth = \(key, val) -> (key, val + smooth)
                trans = LA.assoc (m ^ (k - 1), m) smooth (addSmooth <$> (M.toList $ count transitionPairs))
                rows = V.toList . normalizeVec <$> LA.toRows trans
        transArrs = getTransitions <$> [1..n]

-- samples an n-gram model (infinitely)
ngramSample :: (RandomGen g) => NgramModel v a -> Rand g [a]
ngramSample (Ngrams n alphabet _ transArrs) = do
    rs <- getRandoms
    let indices = [[]] ++ [rotateInds arr prev r | (prev, arr, r) <- zip3 indices arrs rs]
    return $ [alphabet VB.! (last inds) | inds <- tail indices]
    where
        getTail :: [Int] -> [Int]
        getTail inds = if (length inds < n) then inds else tail inds
        rotateInds :: MultiArray Prob -> [Int] -> Double -> [Int]
        rotateInds arr inds r = indTail ++ [nextInd]
            where
                indTail = getTail inds
                row = entries $ arr ^:^ (((Just . pure) <$> indTail) ++ [Nothing])
                cdf = V.scanl' (+) 0.0 row
                nextInd = bisect r cdf
        arrs = transArrs ++ (repeat $ last transArrs)

instance (b ~ [a]) => Simulable (NgramModel v) a b where
    sample :: (RandomGen g) => NgramModel v a -> Rand g [a]
    sample = ngramSample

-- condSlice :: (Ord a) => NgramModel v a -> [a] -> [(Int, a, Prob)]
-- condSlice (Ngrams n vals valIdx transArrs) vals' = zip3 [0..] (VB.toList vals) (V.toList pdf)
--     where
--         lastN = reverse $ take (n - 1) $ reverse vals'
--         inds = (valIdx M.!) <$> lastN
--         arr = transArrs !! length inds
--         pdf = entries $ arr ^:^ (((Just . pure) <$> inds) ++ [Nothing])

-- TERMINATING N-GRAM MODELS --

trainTerminatingNgramModel :: (Ord a, Show a) => Int -> Maybe [a] -> Double -> [[a]] -> NgramModel Integer (Token a)
trainTerminatingNgramModel n alphabet smooth chains = trainNgramModel n toks smooth (terminate <$> chains)
    where toks = (Stop :) . map Token <$> alphabet

instance {-# OVERLAPPING #-} (b ~ [a]) => Simulable (NgramModel v) (Token a) b where
    -- override behavior so sampling terminates upon reaching Stop
    sample = fmap (map fromToken . takeWhile isToken) . ngramSample

-- TEXT N-GRAM MODELS --

data CharNgramParams = CharNgramParams { 
      charAlphabet :: Maybe [Char],   -- alphabet to use
      charSmooth :: Double,           -- smoothing constant
      charLineSep :: String,          -- line separator string
      charTransform :: Char -> Char,  -- function to transform each character
      charFilter :: Bool              -- if True, filter out missing characters
}

defaultCharNgramParams = CharNgramParams { charAlphabet = Nothing, charSmooth = 1e-8, charLineSep = "\n", charTransform = id, charFilter = True }

trainCharNgramModel :: Int -> CharNgramParams -> String -> NgramModel Integer (Token Char)
trainCharNgramModel n params text = trainTerminatingNgramModel n (charAlphabet params) (charSmooth params) lines''
    where
        lines = splitOn (charLineSep params) text
        lines' = map (charTransform params) <$> lines
        lines'' = case (charAlphabet params) of
            Just alphabet -> if (charFilter params)
                                then filter (`S.member` alphabetSet) <$> lines'
                                else lines'
                where alphabetSet = S.fromList alphabet
            Nothing       -> lines'


-- TESTS --

testMarkovSimple = trainTerminatingMarkovModel Nothing 0 ["cababacabbbcabccbacccccccccc"]

testText = ["the quick brown fox jumps over the lazy dog", "i like pizza", "this is english", "beef curd stew", "to be or not to be that is the question", "fourscore and seven years ago", "give me fuel give me fire give me that which i desire"]
testMarkov = trainTerminatingMarkovModel Nothing 0.01 testText
test1gram = trainTerminatingNgramModel 1 Nothing 0 ["abccc"]
test2gram = trainTerminatingNgramModel 2 Nothing 0 ["abcccabcababbbabacbaccbaa"]

alicePath = "test/alice.txt"
alice = unsafePerformIO $ readFile alicePath
alice1gram = trainCharNgramModel 1 defaultCharNgramParams alice
alice2gram = trainCharNgramModel 2 defaultCharNgramParams alice
alice3gram = trainCharNgramModel 3 defaultCharNgramParams alice
alice4gram = trainCharNgramModel 4 defaultCharNgramParams alice
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Math.Optimize where

import Control.Monad.Identity (forM, runIdentity)
import Data.Default (Default(..))
import Data.Maybe (fromJust)
import qualified Data.Vector as V
import Debug.Trace (trace)

import Misc.Utils (safeDiv)
import Math.Search (dfsM, FinalStatePredicate)


-- LINEAR ALGEBRA --

-- TODO: migrate to VectorSpace package

-- point in Euclidean space
type Point = V.Vector Double
-- vector in Euclidean space
type Vec = V.Vector Double

-- normalizes a vector to become a unit vector
unitVec :: Vec -> Vec
unitVec v = (`safeDiv` (norm v)) <$> v

(^+^) :: (Real a) => V.Vector a -> V.Vector a -> V.Vector a
(^+^) = V.zipWith (+)

(^-^) :: (Real a) => V.Vector a -> V.Vector a -> V.Vector a
(^-^) = V.zipWith (-)

-- product of scalar with vector
(*^) :: (Real a) => a -> V.Vector a -> V.Vector a
(*^) c v = (* c) <$> v
infixl 5 *^

-- dot product of two vectors
(^.^) :: (Real a) => V.Vector a -> V.Vector a -> a
(^.^) u v = V.sum $ V.zipWith (*) u v

-- standard basis vector v of length n, where v_i = 1 and v_j = 0 for j != i
elemBasis :: (Real a) => Int -> Int -> V.Vector a
elemBasis n i = V.generate n (\j -> if (j == i) then 1 else 0)


-- METRIC SPACE --

class MetricSpace s where
    norm :: s -> Double
    distance :: s -> s -> Double

newtype Real' a = Real' a

instance (Real a) => MetricSpace (Real' a) where
    norm (Real' x) = realToFrac $ abs x
    distance (Real' x) (Real' y) = norm $ Real' $ x - y

instance (Real a) => MetricSpace (V.Vector a) where
    norm x = sqrt $ V.sum $ (** 2) . realToFrac <$> x
    distance x y = norm $ x ^-^ y


-- OPTIMIZATION --

data OptParams = OptParams {
    maxIter :: Int,   -- max number of iterations
    xtol :: Double,   -- absolute error in x
    ftol :: Double,   -- absolute error in f
    verbosity :: Int  -- verbosity level
}

instance Default OptParams where
    def = OptParams {maxIter = 100, xtol = 1e-8, ftol = 1e-8, verbosity = 2}

-- function to optimize
type ScalarFunc s = s -> Double
type ScalarFuncM m s = s -> m Double
-- state and its function value
type OptState s = (Double, s)
-- (current iteration, previous state, current state)
type OptStep s = (Int, OptState s, OptState s)

isFinalStep :: (MetricSpace s, Show s) => OptParams -> FinalStatePredicate (OptStep s)
isFinalStep (OptParams {maxIter, xtol, ftol, verbosity}) (i, (f0, x0), (f1, x1)) = result
    where
        xerr = distance x1 x0
        df = f1 - f0
        ferr = abs df
        msg = "Iter " ++ show i ++ (if (verbosity > 1) then ("\n\tx =    " ++ show x1) else "") ++ "\n\tf =    " ++ show f1 ++ "\n\tdf =   " ++ show df ++ "\n\txerr = " ++ show xerr
        shouldTerminate = (i >= 0) && ((i >= maxIter) || (xerr <= xtol) || (df > -ftol))
        msg' = if shouldTerminate then (msg ++ "\nTerminating after " ++ show (i + 1) ++ " iteration(s).") else msg
        result = if ((verbosity > 0) && (i >= 0)) then trace msg' shouldTerminate else shouldTerminate

-- given iteration and point, returns next point
type PointStep = (Int, Point) -> Point
type PointStepM m = (Int, Point) -> m Point
-- computes the gradient at a point
type GradientFunc = Point -> Point
type GradientFuncM m = Point -> m Point

-- given a fixed step size and a gradient function, returns a new function that takes a point to its next step
fixedGradientDescentStep :: (Monad m) => Double -> GradientFuncM m -> PointStepM m
fixedGradientDescentStep size grad (_, x) = do
    g <- grad x
    return $ x ^-^ (size *^ g)

type ArmijoParams = (Double, Double, Double)  -- alpha_0, eta, tau

defaultArmijoParams = (32.0, 0.5, 0.5)

-- performs a backtracking Armijo linesearch to obtain a good step size
armijoStep :: (Monad m) => ArmijoParams -> ScalarFuncM m Point -> GradientFuncM m -> Vec -> Point -> m Point
armijoStep (alpha0, eta, tau) func grad p x = do
    f <- func x
    g <- grad x
    let p' = unitVec p
    let thresh = eta * (p' ^.^ g)
    let alphas = alpha0 : [tau * alpha | alpha <- alphas]
    let steps = [x ^+^ (alpha *^ p') | alpha <- alphas]
    fvals <- sequence [func step | step <- steps]
    let (_, (_, step, _):_) = break (\(alpha, step, fval) -> fval <= f + alpha * thresh) (zip3 alphas steps fvals)
    return step

-- performs a backtracking Armijo linesearch to obtain a good gradient step
armijoGradientDescentStep :: (Monad m) => ArmijoParams -> ScalarFuncM m Point -> GradientFuncM m -> PointStepM m
armijoGradientDescentStep params func grad (_, x) = do
    p <- (-1 *^) <$> grad x
    armijoStep params func grad p x

-- monadic minimization function (allows for nondeterministic function/neighborhood evaluation
minimizeM :: (Monad m) => OptParams -> ScalarFuncM m Point -> PointStepM m -> Point -> m (Double, Point)
minimizeM params func step x0 = do
    f0 <- func x0
    let pair0 = (f0, x0)
    let state0 = (-1, pair0, pair0)
    -- get the second-to-last state since loss may have increased on the last step
    (_, _, pointF) <- (!! 1) . fromJust <$> dfsM neighborGen termination state0
    --(_, _, pointF) <- head . fromJust <$> dfsM neighborGen termination state0
    return pointF
    where
        termination :: FinalStatePredicate (OptStep Point)
        termination = isFinalStep params
        neighborGen (i, _, (f1, x1)) = do
            x2 <- step (i + 1, x1)  -- ensure the first iteration is iteration 0
            f2 <- func x2
            return [(i + 1, (f1, x1), (f2, x2))]

-- deterministic minimization function
minimize :: OptParams -> ScalarFunc Point -> PointStep -> Point -> (Double, Point)
minimize params func step x0 = runIdentity $ minimizeM params (return . func) (return . step) x0

-- backtracking Armijo linesearch with gradient descent direction
armijoGradientDescent :: OptParams -> ArmijoParams -> ScalarFunc Point -> GradientFunc -> Point -> (Double, Point)
armijoGradientDescent optParams armijoParams func grad = minimize optParams func (runIdentity . armijoGradientDescentStep armijoParams (return . func) (return . grad))


-- GRADIENT APPROXIMATION --

type GainSeqFunc = Int -> Double  -- defines sequence of gains (step size coefficients)
type DiffSeqFunc = Int -> Double  -- defines sequence of step sizes for gradient approximation

type FinDiffFuncs = (GainSeqFunc, DiffSeqFunc)
-- canonical params for defining gain and diff sequences (see Spall)
data FinDiffParams = FinDiffParams { a :: Double, b :: Double, alpha :: Double, c :: Double, gamma :: Double }
    deriving (Eq, Show)

mkFinDiffFuncs :: FinDiffParams -> FinDiffFuncs
mkFinDiffFuncs (FinDiffParams {a, b, alpha, c, gamma}) = (ak, ck)
    where
        ak k = a / ((fromIntegral k + 1 + b) ** alpha)
        ck k = c / ((fromIntegral k + 1) ** gamma)

defaultFinDiffParams = FinDiffParams {a = 0.5, b = 50.0, alpha = 0.602, c = 0.1, gamma = 0.101}
defaultFinDiffFuncs = mkFinDiffFuncs defaultFinDiffParams

-- uses finite difference method to approximate the gradient
finiteDifferenceGradient :: (Monad m) => DiffSeqFunc -> ScalarFuncM m Point -> Int -> GradientFuncM m
finiteDifferenceGradient diffFunc func k x = do
    let n = V.length x
    let ck = diffFunc k
    let perturbations = [ck *^ elemBasis n i | i <- [0..(n - 1)]]
    res <- forM perturbations $ \p -> do
        fPos <- func (x ^+^ p)
        fNeg <- func (x ^-^ p)
        return $ (fPos - fNeg) / (2.0 * ck)
    return $ V.fromList res

fdsaGradientDescent :: OptParams -> FinDiffFuncs -> ScalarFunc Point -> Point -> (Double, Point)
fdsaGradientDescent optParams (gainFunc, diffFunc) func = minimize optParams func pointStep
    where
        grad i x = runIdentity $ finiteDifferenceGradient diffFunc (return . func) i x
        pointStep (i, x) = x ^-^ (gainFunc i *^ grad i x)


-- TEST --

testFunc :: ScalarFunc Point
testFunc = (** 2) . norm

testGradient :: GradientFunc
testGradient = fmap (2.0 *)

testStep :: PointStep
testStep = runIdentity . fixedGradientDescentStep 0.1 (return . testGradient)

testPoint = V.fromList [1.0,2,3,4,5]

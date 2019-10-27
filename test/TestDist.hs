module TestDist where

import Test.QuickCheck hiding (sample)
import qualified Data.Vector.Storable as V

import Parnassus.Math.Dist


testArr = multiarray [2, 3, 4] [(0::Int)..23]
testSingleDist = trainDiscrete "example" Nothing 0 ['a', 'c', 'b', 'b', 'c', 'c', 'c', 'c', 'd', 'b']
testJointDist = (trainJointDiscrete ["var1", "var2", "var3"] Nothing 0 ["aA0", "aB1", "cB0", "dC0", "dA0", "bB0", "cC1", "aA0", "bB0", "aA0", "aC1", "dC1", "cA0", "cB1", "bB0", "aB1", "dA0", "dC1", "aA0", "aA1"]) :: JointDiscreteDist String Char
testIndepDist = (trainJointIndependent ["var1", "var2", "var3"] Nothing 0 ["aA0", "aB1", "cB0", "dC0", "dA0", "bB0", "cC1", "aA0", "bB0", "aA0", "aC1", "dC1", "cA0", "cB1", "bB0", "aB1", "dA0", "dC1", "aA0", "aA1"]) :: IndependentDiscreteDist String Char

testDist :: IO ()
testDist = do
    let tol = 1e-12
    let qc = quickCheck
    let approx x y = abs (x - y) < tol
    let approxVec x y = V.maximum (V.map abs (V.zipWith (-) x y)) < tol
    qc $ multiIndexToIndex testArr [1, 1, 1] == 17
    qc $ testArr ^!^ [1, 1, 3] == 19
    qc $ testArr ^:^ [Nothing, Nothing, Nothing] == testArr
    qc $ testArr ^:^ [Just [0], Nothing, Just [0, 2]] == MArray {sizes = [1,3,2], cumProds = [6,2,1], entries = V.fromList [0,2,4,6,8,10]}
    qc $ getProb testSingleDist 'd' `approx` 0.1
    qc $ getProbEvent testSingleDist "abc" `approx` 0.9
    qc $ getProbEvent testSingleDist "" `approx` 0.0
    qc $ getJointProb testJointDist "aA0" `approx` 0.2
    qc $ getJointProbEvent testJointDist ["ab", "BC", ""] `approx` 0.0
    qc $ getJointProbEvent testJointDist ["ab", "BC", "0"] `approx` 0.15
    qc $ getJointProbEvent testJointDist ["abcd", "ABC", "01"] `approx` 1.0
    qc $ getJointProb testIndepDist "aA0" `approx` 0.096
    qc $ getJointProbEvent testIndepDist ["ab", "BC", ""] `approx` 0.0
    qc $ getJointProbEvent testIndepDist ["ab", "BC", "0"] `approx` 0.198
    qc $ getJointProbEvent testIndepDist ["abcd", "ABC", "01"] `approx` 1.0
    qc $ (V.maximum $ V.map abs $ V.zipWith (-) (probs $ head $ marginals testJointDist) (probs $ head $ marginals testIndepDist)) < tol
    qc $ (testJointDist `conditionOn` [Just "a", Nothing, Nothing]) == (trainJointDiscrete ["var2", "var3"] Nothing 0 ["A0", "B1", "A0", "A0", "C1", "B1", "A0", "A1"])
    qc $ (testIndepDist `conditionOn` [Just "a", Nothing, Nothing]) == (testIndepDist `marginalizeOut` ["var1"])
    qc $ approxVec (entries $ jProbs testJointDist) (probs $ forgetJoint testJointDist)

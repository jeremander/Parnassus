module Test.Dist where

import Test.Tasty
import Test.Tasty.HUnit
import qualified Data.Vector.Storable as V

import Math.Multiarray
import Math.Dist


testArr = multiarray [2, 3, 4] [(0::Int)..23]
testSingleDist = trainDiscrete "example" Nothing 0 ['a', 'c', 'b', 'b', 'c', 'c', 'c', 'c', 'd', 'b']
testJointDist = (trainJointDiscrete ["var1", "var2", "var3"] Nothing 0 ["aA0", "aB1", "cB0", "dC0", "dA0", "bB0", "cC1", "aA0", "bB0", "aA0", "aC1", "dC1", "cA0", "cB1", "bB0", "aB1", "dA0", "dC1", "aA0", "aA1"]) :: JointDiscreteDist String Char
testIndepDist = (trainJointIndependent ["var1", "var2", "var3"] Nothing 0 ["aA0", "aB1", "cB0", "dC0", "dA0", "bB0", "cC1", "aA0", "bB0", "aA0", "aC1", "dC1", "cA0", "cB1", "bB0", "aB1", "dA0", "dC1", "aA0", "aA1"]) :: IndependentDiscreteDist String Char

tol = 1e-12
approxZero x = abs x < tol
approx x y = approxZero $ x - y
assertClose x y = assertBool (show x ++ " /~= " ++ show y) $ approx x y
approxVec x y = V.maximum (V.map abs (V.zipWith (-) x y)) < tol
assertCloseVec x y = assertBool (show x ++ " /~= " ++ show y) $ approxVec x y

multiIndexTests = testCase "multi-index" $ do
    multiIndexToIndex testArr [1, 1, 1] @?= 17
    testArr ^!^ [1, 1, 3] @?= 19
    testArr ^:^ [Nothing, Nothing, Nothing] @?= testArr
    testArr ^:^ [Just [0], Nothing, Just [0, 2]] @?= MArray {sizes = [1,3,2], cumProds = [6,2,1], entries = V.fromList [0,2,4,6,8,10]}

discreteTests = testCase "discrete dist" $ do
    assertClose (getProb testSingleDist 'd') 0.1
    assertClose (getProbEvent testSingleDist "abc") 0.9
    assertClose (getProbEvent testSingleDist "") 0.0

jointDiscreteTests = testCase "joint discrete dist" $ do
    assertClose (getJointProb testJointDist "aA0") 0.2
    assertClose (getJointProbEvent testJointDist ["ab", "BC", ""]) 0.0
    assertClose (getJointProbEvent testJointDist ["ab", "BC", "0"]) 0.15
    assertClose (getJointProbEvent testJointDist ["abcd", "ABC", "01"]) 1.0
    assertClose (getJointProb testIndepDist "aA0") 0.096
    assertClose (getJointProbEvent testIndepDist ["ab", "BC", ""]) 0.0
    assertClose (getJointProbEvent testIndepDist ["ab", "BC", "0"]) 0.198
    assertClose (getJointProbEvent testIndepDist ["abcd", "ABC", "01"]) 1.0
    assertClose (V.maximum $ V.map abs $ V.zipWith (-) (probs $ head $ marginals testJointDist) (probs $ head $ marginals testIndepDist)) 0.0
    testJointDist `conditionOn` [Just "a", Nothing, Nothing] @?= trainJointDiscrete ["var2", "var3"] Nothing 0 ["A0", "B1", "A0", "A0", "C1", "B1", "A0", "A1"]
    testIndepDist `conditionOn` [Just "a", Nothing, Nothing] @?= testIndepDist `marginalizeOut` ["var1"]
    assertCloseVec (entries $ jProbs testJointDist) (probs $ forgetJoint testJointDist)

distTests = testGroup "probability distribution tests" [multiIndexTests, discreteTests, jointDiscreteTests]

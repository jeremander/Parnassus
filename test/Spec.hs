module Main where

import Test.Tasty

import Test.Dist (distTests)
-- import TestLilypond (testLilypond)
import Test.Markov (markovTests)
import Test.Tun (tunTests)

tests :: TestTree
tests = testGroup "Tests" [distTests, markovTests, tunTests]

main :: IO ()
main = defaultMain tests

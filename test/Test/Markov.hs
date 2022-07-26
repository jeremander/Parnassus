module Test.Markov where

import Test.Tasty
import Test.Tasty.HUnit

import Data.Default (Default(..))
import Math.Markov (CharNgramParams(..), trainCharNgramModel, trainTerminatingMarkovModel, trainTerminatingNgramModel)


testMarkovSimple = trainTerminatingMarkovModel Nothing 0 ["cababacabbbcabccbacccccccccc"]

testText = ["the quick brown fox jumps over the lazy dog", "i like pizza", "this is english", "beef curd stew", "to be or not to be that is the question", "fourscore and seven years ago", "give me fuel give me fire give me that which i desire"]
testMarkov' = trainTerminatingMarkovModel Nothing 0.01 testText
test1gram = trainTerminatingNgramModel 1 Nothing 0 ["abccc"]
test2gram = trainTerminatingNgramModel 2 Nothing 0 ["abcccabcababbbabacbaccbaa"]

alicePath = "test/alice.txt"
aliceCorpus = readFile alicePath

ngramTest :: Int -> TestTree
ngramTest n = testCase (show n ++ "-gram") $ do
    corpus <- aliceCorpus
    let model = trainCharNgramModel n def corpus
    return $ seq model ()

ngramTests = testGroup "n-gram tests" $ ngramTest <$> [1..4]

markovTests = testGroup "Markov chain tests" [ngramTests]

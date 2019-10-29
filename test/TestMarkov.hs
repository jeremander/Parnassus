module TestMarkov where

import Data.Default (Default(..))
import Math.Markov (CharNgramParams(..), trainCharNgramModel, trainTerminatingMarkovModel, trainTerminatingNgramModel)


testMarkovSimple = trainTerminatingMarkovModel Nothing 0 ["cababacabbbcabccbacccccccccc"]

testText = ["the quick brown fox jumps over the lazy dog", "i like pizza", "this is english", "beef curd stew", "to be or not to be that is the question", "fourscore and seven years ago", "give me fuel give me fire give me that which i desire"]
testMarkov' = trainTerminatingMarkovModel Nothing 0.01 testText
test1gram = trainTerminatingNgramModel 1 Nothing 0 ["abccc"]
test2gram = trainTerminatingNgramModel 2 Nothing 0 ["abcccabcababbbabacbaccbaa"]

alicePath = "test/alice.txt"

testMarkov :: IO ()
testMarkov = do
    alice <- readFile alicePath
    return $ trainCharNgramModel 1 def alice
    return $ trainCharNgramModel 2 def alice
    return $ trainCharNgramModel 3 def alice
    return $ trainCharNgramModel 4 def alice
    putStrLn "Markov test complete"
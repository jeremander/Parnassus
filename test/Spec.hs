
import TestDist (testDist)
import TestLilypond (testLilypond)
import TestMarkov (testMarkov)

main :: IO ()
main = do
    testDist
    testMarkov
    testLilypond
{-# LANGUAGE OverloadedStrings #-}

module Test.Tun where

import qualified Data.Attoparsec.Text as A
import Data.Either (isRight)
import Data.Text (pack)
import Test.Tasty
import Test.Tasty.HUnit
import qualified Text.Pretty as P

import Music.Tuning (Tuning(..))
import Music.Tuning.Tun


-- check that pretty is the left inverse of parse
prettyParseInverse parser s = do
    let s' =  P.runPrinter . P.pretty <$> parseFull parser (pack s)
    s' @?= Right s

-- check that parse is the left inverse of pretty (after applying parse once)
parsePrettyInverse parser s = do
    let val = parseFull parser $ pack s
    case val of
        Left err -> assertFailure err
        _        -> pure ()
    let s' = P.runPrinter . P.pretty <$> val
    let val' = parseFull parser . pack =<< s'
    val' @?= val

prettyParseInverseKeyVal = prettyParseInverse parseKeyVal
prettyParseInverseTun = prettyParseInverse parseTun
parsePrettyInverseKeyVal = parsePrettyInverse parseKeyVal
parsePrettyInverseTun = parsePrettyInverse parseTun

prettyParseInverseTests = testCase "pretty . parse = id" $ do
    prettyParseInverseKeyVal "keyA = 123"
    prettyParseInverseKeyVal "keyB = -123"
    prettyParseInverseKeyVal "keyA = 123.45"
    prettyParseInverseKeyVal "keyB = -123.45"
    prettyParseInverseKeyVal "keyA = \"string\""
    prettyParseInverseKeyVal "keyA = \"\\n line break, \\t tabulator and hex ch\\0x41racter\""
    prettyParseInverseTun "[Section1]\nnote 1 = 100\nnote 2 = 200\nnote 3 = 300\n; ... and so on ..."
    prettyParseInverseTun "[Section1]"
    prettyParseInverseTun "[Section1]\n; Content of Section1\n[Section2]\n; Content of Section2\n[Section1]\n; ERROR, because Section1 is repeated\n[Section1]\n; Content of Section1"

parsePrettyInverseTests = testCase "parse . pretty . parse = parse" $ do
    parsePrettyInverseKeyVal "keyA = 1.23e2"
    parsePrettyInverseKeyVal "keyB = -1.23e-2"

testFilePath = "test/data/19EDO_v1.tun"

fileTest = testCase "test TUN file" $ do
    testFileStr <- readFile testFilePath
    parsePrettyInverseTun testFileStr
    -- test successful interpretation
    tunData <- loadTun testFilePath
    let (Right (Tuning tuning)) = tuningFromTunData tunData
    -- in 19-EDO, an octave should occur every 19 notes
    (tuning !! 19) / (head tuning) @?= 2.0
    -- this tuning was calibrated so that A4 is 420 Hz
    abs ((tuning !! 69) - 420.0) <= 1e-8 @? "note 69 should be 420 Hz"
    return ()

tunTests = testGroup "TUN format tests" [prettyParseInverseTests, parsePrettyInverseTests, fileTest]

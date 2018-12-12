{-# LANGUAGE ParallelListComp #-}

module Parnassus.Primes where

import Data.Counter (Counter, count)
import Data.List (unfoldr)
import Data.Map (toList)
import Data.Maybe (listToMaybe)

import Data.WAVE
import Parnassus.Wave

-- Primes --

-- computes prime factors of an integer, given a list of prime factors in order
pfactors :: [Integer] -> Integer -> [Integer]
pfactors prs n = unfoldr (\(ds, n) -> listToMaybe
    [(x, (dropWhile (< x) ds, div n x)) | x <- takeWhile ((<= n) . (^ 2)) ds ++
                                                [n | n > 1], mod n x == 0]) (prs, n)

-- infinite list of primes                                                
primes :: [Integer]
primes = 2 : 3 : [x | x <- [5, 7..], head (pfactors (tail primes) x) == x]
 
-- prime factors of an integer (with multiplicity)
primeFactors :: Integer -> [Integer]
primeFactors n 
    | n > 1 = go n primes
    | n < 0 = primeFactors (-n)
    | otherwise = [1]
        where
            go n ps@(p:t)
                | p * p > n  = [n]
                | r == 0     =  p : go q ps
                | otherwise  =      go n t
                        where
                        (q, r) = quotRem n p


-- computes a map from prime factors to exponents, for a given integer
primeFactorCounts :: Integer -> Counter Integer Int
primeFactorCounts = count . primeFactors


-- Collatz --

collatz :: Integer -> Integer -> [Integer]
collatz a x0 = seq
    where
        seq = [x0] ++ [if prev `mod` 2 == 0 then (prev `quot` 2) else (a * prev + 1) | prev <- seq]

-- given base frequency, note duration (in seconds), and an integer, generates a signal representing the prime factorization of that integer
intToSig :: Double -> Double -> Integer -> Sig
intToSig base d n = signalChord notes
    where
        (primes, exponents) = unzip $ toList $ primeFactorCounts n
        freqs = ((* base) . fromIntegral) <$> primes
        tones = (envTone d 0.25) <$> freqs
        notes = [[fromIntegral vol * samp | samp <- tn] | vol <- exponents | tn <- tones]

-- normalizes a sequence of floats to be in the range [0, 1]
normalize :: (Fractional a, Ord a) => [a] -> [a]
normalize xs = map (/ xAbsMax) xs
    where xAbsMax = maximum $ map abs xs

-- given base frequency, note duration (in seconds), and an integer sequence, generates a signal corresponding to the sequence
intSeqToSig :: Double -> Double -> [Integer] -> Sig
intSeqToSig base d = normalize . concat . map (intToSig base d)

-- default WAV file header
defaultWaveHeader :: WAVEHeader
defaultWaveHeader = WAVEHeader { waveNumChannels = 1, waveFrameRate = audRate, waveBitsPerSample = 32, waveFrames = Nothing }

-- saves a (normalized) sequence of samples to a .wav file
sigToWav :: FilePath -> Sig -> IO ()
sigToWav path sig = putWAVEFile path wave
    where
        wave = WAVE { waveHeader = defaultWaveHeader {waveFrames = Just $ length sig},
                      waveSamples = map (return . doubleToSample) sig }
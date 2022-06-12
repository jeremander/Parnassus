{-# LANGUAGE ParallelListComp #-}

module Music.Primes where

import Data.Counter (Counter, count)
import Data.List (unfoldr)
import qualified Data.Map as M
import Data.Maybe (listToMaybe)
import Data.WAVE (WAVE(..), WAVEHeader(..), doubleToSample, putWAVEFile)

import Music.Wave (audRate, envTone, signalChord, Sig)


-- * Primes

-- | Computes prime factors of an integer, given a list of prime factors in order.
pfactors :: [Integer] -> Integer -> [Integer]
pfactors prs n = unfoldr (\(ds, n) -> listToMaybe
    [(x, (dropWhile (< x) ds, div n x)) | x <- takeWhile ((<= n) . (^ 2)) ds ++
                                                [n | n > 1], mod n x == 0]) (prs, n)

-- | Infinite list of primes.
primes :: [Integer]
primes = 2 : 3 : [x | x <- [5, 7..], head (pfactors (tail primes) x) == x]

-- | Prime factors of an integer (with multiplicity).
primeFactors :: Integer -> [Integer]
primeFactors n
    | n >= 1    = go n primes
    | otherwise = primeFactors (-n)
        where
            go n ps@(p:t)
                | p * p > n  = [n]
                | r == 0     =  p : go q ps
                | otherwise  =      go n t
                        where (q, r) = quotRem n p
            go _ [] = [1]

-- | Computes a map from prime factors to exponents, for a given integer.
primeFactorCounts :: Integer -> Counter Integer Int
primeFactorCounts = count . primeFactors


-- * Collatz

collatz :: Integer -> Integer -> [Integer]
collatz a x0 = seq
    where
        seq = x0 : [if even prev then (prev `quot` 2) else (a * prev + 1) | prev <- seq]

-- | Given base frequency, note duration (in seconds), and an integer, generates a signal representing the prime factorization of that integer.
intToSig :: Double -> Double -> Integer -> Sig
intToSig base d n = signalChord notes
    where
        (primes, exponents) = unzip $ M.toList $ primeFactorCounts n
        freqs = ((* base) . fromIntegral) <$> primes
        tones = (envTone d 0.25) <$> freqs
        notes = [[fromIntegral vol * samp | samp <- tn] | vol <- exponents | tn <- tones]

-- | Normalizes a sequence of floats to be in the range [-1, 1].
normalize :: (Fractional a, Ord a) => [a] -> [a]
normalize xs = map (/ xAbsMax) xs
    where xAbsMax = maximum $ map abs xs

-- | Given base frequency, note duration (in seconds), and an integer sequence, generates a signal corresponding to the sequence.
intSeqToSig :: Double -> Double -> [Integer] -> Sig
intSeqToSig base d = normalize . concatMap (intToSig base d)

-- | Default WAV file header.
defaultWaveHeader :: WAVEHeader
defaultWaveHeader = WAVEHeader { waveNumChannels = 1, waveFrameRate = audRate, waveBitsPerSample = 32, waveFrames = Nothing }

-- | Saves a (normalized) sequence of samples to a .wav file.
sigToWav :: FilePath -> Sig -> IO ()
sigToWav path sig = putWAVEFile path wave
    where
        wave = WAVE { waveHeader = defaultWaveHeader {waveFrames = Just $ length sig},
                      waveSamples = map (return . doubleToSample) sig }
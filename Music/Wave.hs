{-# LANGUAGE Arrows #-}

module Music.Wave where

import Data.Array.Unboxed as UA
import qualified Data.List (transpose)
import Data.WAVE (getWAVEFile, sampleToDouble, waveSamples)
import Euterpea (AudSF, Instr, InstrMap, InstrumentName(..), Table(..), apToHz, envASR, osc, outA, tableSinesN)
import System.IO.Unsafe (unsafePerformIO)


-- | Signals with 44100 Hz sample rate.
type AudSig = AudSF () Double

-- | Audio signal represented as a sequence of sample values.
type Sig = [Double]

-- | Default CD sample rate.
audRate :: Int
audRate = 44100

-- ** Euterpea to WAV

-- | Removes clicking that results from sudden phase shift.
asrEnvelope :: Double -> AudSig
asrEnvelope dur = envASR (0.1 * dur) (1.0 * dur) (0.1 * dur)

-- | Converts a wave table to an instrument for playing to a wav file.
waveTableToInstr :: Table -> Instr AudSig
waveTableToInstr tab = instr
    where
        instr dur ap vol _ =
            let env = asrEnvelope $ fromRational dur
                freq = apToHz ap
                v = fromIntegral vol / 127   -- normalize volume as amplitude factor
            in proc _ -> do
                aud <- osc tab 0 -< freq  -- sample with appropriate frequency
                amp <- env -< ()          -- envelope magnitude
                outA -< amp * v * aud

-- | Loads a single-channel normalized wave cycle into a 'Table'.
loadCycle :: FilePath -> Table
loadCycle path = tab
    where
        waveData = unsafePerformIO $ getWAVEFile path
        samples = (sampleToDouble . head <$> waveSamples waveData) ++ [head samples]
        numSamples = length samples
        tab = Table numSamples (UA.listArray (0, numSamples) samples) True

-- | Constructs an 'InstrMap' from a name and 'Table'.
instrMap :: String -> Table -> InstrMap AudSig
instrMap name tab = [(CustomInstrument name, waveTableToInstr tab)]

-- | Simple sine wave instrument.
sineInstrMap :: InstrMap AudSig
sineInstrMap = instrMap "Sine" (tableSinesN 4096 [1])

loadWaveInstrMap :: String -> FilePath -> InstrMap AudSig
loadWaveInstrMap name path = instrMap name (loadCycle path)


-- ** Direct signal approach

linspace :: Double -> Double -> Int -> [Double]
linspace start stop n = [start, (start + delta)..stop]
    where delta = (stop - start) / (fromIntegral n - 1)

-- | A sine wave tone of the given duration and frequency.
tone :: Double -> Double -> Sig
tone d freq = [sin (c * t) | t <- ts]
    where
        c = 2 * pi * freq
        ts = linspace 0 d (round $ fromIntegral audRate * d)

-- | Creates a trapezoidal amplitude envelope for the given duration.
--   @frac@ is the fraction of the time spent ramping (should be in [0, 1]).
trapEnvelope :: Double -> Double -> Sig
trapEnvelope d frac = seg1 ++ (replicate (n - 2 * k) 1.0) ++ seg2
    where
        n = round $ (fromIntegral audRate) * d
        k = round $ frac * (fromIntegral n) / 2
        seg1 = linspace 0 1 k
        seg2 = linspace 1 0 k

-- | A tone modulated by an amplitude envelope.
envTone :: Double -> Double -> Double -> Sig
envTone d frac freq = zipWith (*) (trapEnvelope d frac) (tone d freq)

-- | Adds multiple signals into one.
signalChord :: [Sig] -> Sig
signalChord sigs = map sum (Data.List.transpose sigs)
{-# LANGUAGE Arrows #-}

module Parnassus.Wave where

import qualified Data.List (transpose)
import Euterpea


-- signals with 44100 Hz sample rate
type AudSig a = AudSF () a

-- audio signal represented as a sequence of sample values
type Sig = [Double]

-- default CD sample rate
audRate :: Int
audRate = 44100


-- Euterpea to wav --

-- removes clicking that results from sudden phase shift
asrEnvelope :: Double -> AudSig Double
asrEnvelope dur = envASR (0.1 * dur) (1.0 * dur) (0.1 * dur)

-- named sine instrument
sineInstrName :: InstrumentName
sineInstrName = CustomInstrument "Sine"

-- instrument for playing Music to a .wav file
sineInstr :: Instr (AudSig Double)
sineInstr dur ap vol _ =
    let env = asrEnvelope $ fromRational dur
        freq = apToHz ap
        v = fromIntegral vol / 127   -- normalize volume as amplitude factor
    in proc () -> do
        aud <- (oscFixed freq) -< ()  -- a sine wave
        amp <- env -< ()              -- envelope magnitude
        outA -< amp * v * aud

-- maps sine instrument name to the instrument itself
sineInstrMap :: InstrMap (AudSig Double)
sineInstrMap = [(sineInstrName, sineInstr)]

-- saves music to a wav file via the sine instrument
musicToWav :: FilePath -> Music1 -> IO ()
musicToWav path music = outFile path d sf
    where 
        music' = instrument sineInstrName music
        (d, sf) = renderSF music' sineInstrMap


-- Direct signal approach --

linspace :: Double -> Double -> Int -> [Double]
linspace start stop n = [start, (start + delta)..stop]
    where delta = (stop - start) / (fromIntegral n - 1)

-- a sine wave tone of the given duration and frequency
tone :: Double -> Double -> Sig
tone d freq = [sin (c * t) | t <- ts]
    where
        c = 2 * pi * freq
        ts = linspace 0 d (round $ fromIntegral audRate * d)

-- creates a trapezoidal amplitude envelope for the given duration
-- frac is the fraction of the time spent ramping (should be in [0, 1])
trapEnvelope :: Double -> Double -> Sig
trapEnvelope d frac = seg1 ++ (replicate (n - 2 * k) 1.0) ++ seg2
    where
        n = round $ (fromIntegral audRate) * d
        k = round $ frac * (fromIntegral n) / 2
        seg1 = linspace 0 1 k
        seg2 = linspace 1 0 k

-- a tone modulated by an amplitude envelope
envTone :: Double -> Double -> Double -> Sig
envTone d frac freq = zipWith (*) (trapEnvelope d frac) (tone d freq)

-- adds multiple signals into one
signalChord :: [Sig] -> Sig
signalChord sigs = map sum (Data.List.transpose sigs)
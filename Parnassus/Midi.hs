{-# LANGUAGE NamedFieldPuns #-}

module Parnassus.Midi (quantizeMidi) where

import Codec.Midi
-- import Data.List
-- import Euterpea
import Euterpea.IO.MIDI.FromMidi (SimpleMsg)

-- given an integer quantization q, quantizes all ticks to be multiples of q
quantizeTrackTicks :: Int -> Track Ticks -> Track Ticks
quantizeTrackTicks q track = [(quantize t, msg) | (t, msg) <- track]
    where
        quantize :: Ticks -> Ticks
        quantize t = round (fromIntegral t / fromIntegral q) * q

-- given a duration in beats, quantizes midi messages so that time intervals are always some whole number of this duration
quantizeMidi :: Rational -> Midi -> Midi
quantizeMidi r Midi{fileType, timeDiv, tracks} = Midi {fileType, timeDiv, tracks = tracks'}
    where
        tpb = case timeDiv of
            TicksPerBeat tpb' -> tpb'
            TicksPerSecond _ _ -> error "TimeDiv must be in ticks per beat"
        q = round (r * fromIntegral tpb)
        tracks' = map (quantizeTrackTicks q) tracks

simplifyTrack :: Int -> [(Ticks, Message)] -> [SimpleMsg]
simplifyTrack icur [] = []
simplifyTrack icur ((t, m) : ts) = case m of
    (NoteOn c p v) -> SE (fromIntegral t, p, v, icur, On) : simplifyTrack icur ts
    (NoteOff c p v) -> SE (fromIntegral t, p, v, icur, Off) : simplifyTrack icur ts
    (ProgramChange c p) -> simplifyTrack (if c == 9 then (-1) else p) ts 
    (TempoChange x) -> T (fromIntegral t, fromIntegral x) : simplifyTrack icur ts
    otherwise -> simplifyTrack icur ts 




-- > midiToEvents :: Midi -> [[SimpleMsg]]
-- > midiToEvents m = 
-- >     let ts = map (simplifyTrack 0) $ map (addTrackTicks 0) (tracks m) 
-- >     in  distributeTempos $ map (map (applyTD $ timeDiv m)) ts where 
-- >   simplifyTrack :: Int -> [(Ticks, Message)] -> [SimpleMsg]
-- >   simplifyTrack icur [] = []
-- >   simplifyTrack icur ((t,m):ts) = 
-- >     case m of (NoteOn c p v) -> 
-- >                   SE (fromIntegral t, p, v, icur, On) : simplifyTrack icur ts
-- >               (NoteOff c p v) -> 
-- >                   SE (fromIntegral t, p, v, icur, Off) : simplifyTrack icur ts
-- >               (ProgramChange c p) -> simplifyTrack (if c==9 then (-1) else p) ts 
-- >               (TempoChange x) -> T (fromIntegral t, fromIntegral x) : simplifyTrack icur ts
-- >               _ -> simplifyTrack icur ts 

-- > fromMidi :: Midi -> Music1
-- > fromMidi m = 
-- >     let seList = midiToEvents m
-- >         iNums = filter (>0) $ map getInstrument seList
-- >         upm = makeUPM $ map toEnum iNums
-- >     in  mMap (\(p,v) -> (p, [Volume v])) $ eventsToMusic seList


-- > fromMidi2 :: Midi -> Music (Pitch, Volume)
-- > fromMidi2 = restructure . fromMidi
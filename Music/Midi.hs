{-# LANGUAGE NamedFieldPuns #-}

module Music.Midi where

import Codec.Midi (Message(..), Midi(..), Ticks, Track, tracks)
import Data.Default (Default(..))
import Data.List (foldl', sortOn)
import Data.Maybe (isNothing)
import Euterpea (Mode(..))

import Misc.Utils (safeHead)
import Music.Pitch (getSharpFlatCount, Key, simplifyMode)
import Music.Rhythm (TimeSig(..))


data MidiTimeSig = MidiTimeSig {
    numerator :: Int,
    logDenominator :: Int,
    clocksPerClick :: Int,
    notated32ndNotesPerBeat :: Int
} deriving (Eq, Ord, Show)

midiTimeSigToTimeSig :: MidiTimeSig -> TimeSig
midiTimeSigToTimeSig (MidiTimeSig num den _ _) = TimeSig (num, 2 ^ den)

timeSigToMidiTimeSig :: TimeSig -> MidiTimeSig
timeSigToMidiTimeSig (TimeSig (num, den)) = MidiTimeSig num den' 24 8
    where den' = floor $ logBase 2 (fromIntegral den :: Float)

data MidiKeySig = MidiKeySig {
    sharpFlat :: Int,   -- -7 for 7 flats, -1 for 1 flat, etc, 0 for key of c, 1 for 1 sharp, etc.
    isMinor :: Bool
} deriving (Eq, Ord, Show)

midiKeySigToKeySig :: MidiKeySig -> Key
midiKeySigToKeySig (MidiKeySig sharpFlat isMinor) = (pc, mode)
    where
        mode = if isMinor then Minor else Major
        -- brute force invert function
        pitchClasses = sortOn (length . show) $ enumFrom minBound
        pc = head [pc' | pc' <- pitchClasses, getSharpFlatCount (pc', mode) == sharpFlat]

keySigToMidiKeySig :: Key -> MidiKeySig
keySigToMidiKeySig key = MidiKeySig sharpFlat isMinor
    where
        (pc, mode) = simplifyMode key
        sharpFlat = getSharpFlatCount (pc, mode)
        isMinor = mode == Minor

-- | Global metadata fields for MIDI files.
data MidiMetadata = MidiMetadata {
    title :: Maybe String,
    copyright :: Maybe String,
    lyrics :: Maybe String,
    text :: [String],
    midiTimeSig :: Maybe MidiTimeSig,
    midiKeySig :: Maybe MidiKeySig
} deriving (Eq, Ord, Show)

instance Default MidiMetadata where
    def = MidiMetadata {title = Nothing, copyright = Nothing, lyrics = Nothing, text = [], midiTimeSig = Nothing, midiKeySig = Nothing}

-- | Returns True if the given 'MidiMetadata' is empty (has no content).
midiMetadataIsEmpty :: MidiMetadata -> Bool
midiMetadataIsEmpty metadata = metadata == def

-- | Gets the first track in a 'Midi', if there is one.
getFirstMidiTrack :: Midi -> Maybe (Track Ticks)
getFirstMidiTrack = safeHead . tracks

-- | Gets the global metadata from a 'Midi'.
getMidiMetadata :: Midi -> Maybe MidiMetadata
getMidiMetadata mid = do
    track <- getFirstMidiTrack mid
    let msgs = snd <$> track
    return $ foldl' go def msgs
    where
        go md (Text txt) = md {text = text md ++ [txt]}
        go md (Copyright cpy) = if isNothing (copyright md) then md {copyright = Just cpy} else md
        go md (TrackName name) = if isNothing (title md) then md {title = Just name} else md
        go md (Lyrics lyr) = if isNothing (lyrics md) then md {lyrics = Just lyr} else md
        go md (TimeSignature num den clocks perBeat) = if isNothing (midiTimeSig md) then md {midiTimeSig = Just $ MidiTimeSig num den clocks perBeat} else md
        go md (KeySignature sharpFlat isMinor) = if isNothing (midiKeySig md) then md {midiKeySig = Just $ MidiKeySig sharpFlat (isMinor /= 0)} else md
        go md _ = md

midiMetadataToTrack :: MidiMetadata -> Track Ticks
midiMetadataToTrack (MidiMetadata {title, copyright, lyrics, text, midiTimeSig, midiKeySig}) = titlePairs ++ copyrightPairs ++ lyricsPairs ++ textPairs ++ timeSigPairs ++ keySigPairs
    where
        toPairs x = [(0, x)]
        titlePairs = maybe [] (toPairs . TrackName) title
        copyrightPairs = maybe [] (toPairs . Copyright) copyright
        lyricsPairs = maybe [] (toPairs . Lyrics) lyrics
        textPairs = [(0, Text txt) | txt <- text]
        timeSigPairs = maybe [] (\(MidiTimeSig num den clocks perBeat) -> toPairs $ TimeSignature num den clocks perBeat) midiTimeSig
        keySigPairs = maybe [] (\(MidiKeySig sharpFlat isMinor) -> toPairs $ KeySignature sharpFlat (fromEnum isMinor)) midiKeySig

removeFirstMidiTrack :: Midi -> Midi
removeFirstMidiTrack (Midi {fileType, timeDiv, tracks}) = Midi fileType timeDiv (tail tracks)

prependMidiTrack :: Track Ticks -> Midi -> Midi
prependMidiTrack track (Midi {fileType, timeDiv, tracks}) = Midi fileType timeDiv (track : tracks)

stripMidiMetadata :: Midi -> Midi
stripMidiMetadata (Midi {fileType, timeDiv, tracks}) = Midi fileType timeDiv tracks'
    where
        filterMsg (Text _)           = False
        filterMsg (Copyright _)      = False
        filterMsg (TrackName _)      = False
        filterMsg (Lyrics _)         = False
        filterMsg (TimeSignature {}) = False
        filterMsg (KeySignature {})  = False
        filterMsg _                  = True
        go (_, msg) = filterMsg msg
        firstTrack = filter go $ head tracks
        tracks' = if null tracks then [] else firstTrack : tail tracks

-- | Updates the global metadata in a 'Midi'.
updateMidiMetadata :: MidiMetadata -> Midi -> Midi
updateMidiMetadata md mid = go midStripped
    where
        midStripped = stripMidiMetadata mid
        go = if midiMetadataIsEmpty md then id else prependMidiTrack (midiMetadataToTrack md)

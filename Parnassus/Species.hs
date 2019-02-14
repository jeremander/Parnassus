-- Species Counterpoint --

{-# LANGUAGE InstanceSigs #-}

module Parnassus.Species where

import Control.Exception.Base (assert)
import Data.Char (isDigit)
import Data.List.Split (splitOn)
import Data.Range.Range (Range (SpanRange), inRange)
import Data.Ratio ((%))

import Euterpea (AbsPitch, Control (..), InstrumentName (..), Mode (..), Pitch, PitchClass (..), absPitch, pitch)

import Parnassus.MusicBase (Key, MusicT (..), Pitched (getPitch), (/=/))
import Parnassus.MusicD (MusicD (MusicD), Tied (..), extractTied, isTied)


type Interval = Int

data Voice = Bass | Tenor | Alto | Soprano
    deriving (Eq, Ord, Show)

-- vocal range for each voice part
voiceRange :: Voice -> Range AbsPitch
voiceRange Bass    = SpanRange (absPitch (E, 2)) (absPitch (E, 4))
voiceRange Tenor   = SpanRange (absPitch (C, 3)) (absPitch (A, 4))
voiceRange Alto    = SpanRange (absPitch (F, 3)) (absPitch (F, 5))
voiceRange Soprano = SpanRange (absPitch (C, 4)) (absPitch (A, 5))

-- returns True if the voice can sing the note
voiceCanSing :: Voice -> Pitch -> Bool
voiceCanSing voice p = inRange (voiceRange voice) (absPitch p)

-- only church modes are acceptable for species counterpoint
convertMode :: Mode -> Mode
convertMode Major = Ionian
convertMode Minor = Aeolian
convertMode Locrian = error "Locrian is an invalid mode for species counterpoint"
convertMode (CustomMode _) = error "CustomMode is an invalid mode for species counterpoint"
convertMode mode = mode

-- returns True if two pitch classes are equivalent
equivPitchClass :: PitchClass -> PitchClass -> Bool
equivPitchClass pc1 pc2 = (absPitch (pc1, 4)) == (absPitch (pc2, 4))

-- transposes a pitch class by some number of semitones
shiftPitch :: PitchClass -> Int -> PitchClass
shiftPitch pc shift = fst $ pitch $ absPitch (pc, 4) + shift

-- gets the pitch class scale for a given key
scaleForKey :: Key -> [PitchClass]
scaleForKey (pc, mode) = take 7 [shiftPitch pc' j | j <- drop i (cycle majorScale)]
    where
        majorScale = [0, 2, 4, 5, 7, 9, 11]
        i = case mode of
            Dorian -> 1
            Phrygian -> 2
            Lydian -> 3
            Mixolydian -> 4
            Aeolian -> 5
            Minor -> 5
            Locrian -> 6
            otherwise -> 0
        pc' = shiftPitch pc (-(majorScale !! i))        
        
-- parses a note from a string: starts with the pitch, followed by an octave, followed by ~ if it is tied; a rest is simply a *
parseNote :: String -> Tied Pitch
parseNote "*" = Rest
parseNote s   = tp
    where
        (tie, s') = span (== '~') s
        (pc, s'') = span (not . isDigit) s'
        (oct, _) = span isDigit s''
        tp = case tie of
            ""        -> Untied [] (read pc, read oct)
            "~"       -> Tied (read pc, read oct)
            otherwise -> error "parse error"

parseLine :: String -> [Tied Pitch]
parseLine s = parseNote <$> filter (not . null) (splitOn " " s)

class Species a where
    toMusicD :: a -> MusicD Pitch

-- First Species --

type VoiceLine = (Voice, [Tied Pitch])

data FirstSpecies = FirstSpecies { key :: Key, cantusFirmus :: VoiceLine, counterpoint :: VoiceLine}
    deriving (Show)

-- convenience constructor from strings
firstSpecies :: Key -> (Voice, String) -> (Voice, String) -> FirstSpecies
firstSpecies (pc, mode) (cfVoice, cfStr) (cptVoice, cptStr) = spec
    where
        key' = (pc, convertMode mode)
        validateNote :: Voice -> Tied Pitch -> Bool
        validateNote voice (Untied _ p) = voiceCanSing voice p
        validateNote voice (Tied p)     = voiceCanSing voice p
        validateNote _ Rest             = False
        cf = parseLine cfStr
        cpt = parseLine cptStr
        cadencePitch = fst <$> extractTied ((dropWhile isTied (reverse cf)) !! 1)
        scale = scaleForKey key'
        isValid = (all (validateNote cfVoice) cf) && 
                  (all (validateNote cptVoice) cpt) && 
                  (length cf == length cpt) &&
                  ((fst <$> extractTied (head cf)) == Just pc) &&
                  ((fst <$> extractTied (last cf)) == Just pc) &&
                  ((equivPitchClass (scale !! 1) <$> cadencePitch) == Just True) -- penultimate note of c.f. must be the second note of the scale
        spec = case isValid of
            True  -> FirstSpecies {key = key', cantusFirmus = (cfVoice, cf), counterpoint = (cptVoice, cpt)}
            False -> error "invalid input"

instance Species FirstSpecies where
    toMusicD :: FirstSpecies -> MusicD Pitch
    toMusicD FirstSpecies {cantusFirmus = (_, cf), counterpoint = (_, cpt)} = modify (Tempo 3) $ (MusicD 1 [Instrument VoiceOohs] (pure <$> cf)) /=/ (MusicD 1 [Instrument VoiceOohs] (pure <$> cpt))

-- gets index of the cadence measure
cadenceMeasure :: FirstSpecies -> Int
cadenceMeasure (FirstSpecies {cantusFirmus = (_, cf)}) = fst $ (dropWhile (isTied . snd) (zip [0..] (reverse cf))) !! 1


-- generateFirstSpecies :: Key -> VoiceLine -> Voice -> FirstSpecies
--     if ((int(cf_track[spec.cadence_measure() - 1][0][2][0]) % 12) != (int(MGNote(spec.mode.scale[1])) % 12)):



-- Fux --

fig5 = firstSpecies (D, Dorian) (Alto, "D4 F4 E4 D4 G4 F4 A4 G4 F4 E4 D4 ~D4") (Soprano, "A4 A4 G4 A4 B4 C5 C5 B4 D5 Cs5 D5 ~D5")
fig6Bad = firstSpecies (D, Dorian) (Alto, "D4 F4 E4 D4 G4 F4 A4 G4 F4 E4 D4 ~D4") (Tenor, "G3 D4 A3 F3 E3 D3 F3 C4 D4 Cs4 D4 ~D4")
fig6Good = firstSpecies (D, Dorian) (Alto, "D4 F4 E4 D4 G4 F4 A4 G4 F4 E4 D4 ~D4") (Tenor, "D3 D3 A3 F3 E3 D3 F3 C4 D4 Cs4 D4 ~D4")
fig11 = firstSpecies (E, Phrygian) (Alto, "E4 C4 D4 C4 A3 A4 G4 E4 F4 E4 ~E4") (Soprano, "B4 C5 F4 G4 A4 C5 B4 E5 D5 E5 ~E5")
fig12Bad = firstSpecies (E, Phrygian) (Alto, "E4 C4 D4 C4 A3 A4 G4 E4 F4 E4 ~E4") (Tenor, "E3 A3 D3 E3 F3 F3 B3 C4 D4 E4 ~E4")
fig12Good = firstSpecies (E, Phrygian) (Alto, "E4 C4 D4 C4 A3 A4 G4 E4 F4 E4 ~E4") (Tenor, "E3 A3 D3 E3 F3 F3 C4 C4 D4 E4 ~E4")
fig13 = firstSpecies (F, Lydian) (Tenor, "F3 G3 A3 F3 D3 E3 F3 C4 A3 F3 G3 F3 ~F3") (Alto, "F4 E4 C4 F4 F4 G4 A4 G4 C4 F4 E4 F4 ~F4")
fig14 = firstSpecies (F, Lydian) (Tenor, "F3 G3 A3 F3 D3 E3 F3 C4 A3 F3 G3 F3 ~F3") (Bass, "F3 E3 F3 A3 Bf3 G3 A3 E3 F3 D3 E3 F3 ~F3")
fig15Bad = firstSpecies (G, Mixolydian) (Alto, "G3 C4 B3 G3 C4 E4 D4 G4 E4 C4 D4 B3 A3 G3 ~G3") (Soprano, "G4 E4 D4 G4 G4 G4 A4 B4 G4 E5 D5 G4 Fs4 G4 ~G4")
fig15Good = firstSpecies (G, Mixolydian) (Alto, "G3 C4 B3 G3 C4 E4 D4 G4 E4 C4 D4 B3 A3 G3 ~G3") (Soprano, "G4 E4 D4 G4 G4 G4 A4 B4 G4 C5 A4 G4 Fs4 G4 ~G4")
fig21 = firstSpecies (G, Mixolydian) (Alto, "G3 C4 B3 G3 C4 E4 D4 G4 E4 C4 D4 B3 A3 G3 ~G3") (Tenor, "G3 A3 G3 E3 E3 C3 G3 B3 C4 A3 Fs3 G3 Fs3 G3 ~G3")
fig22 = firstSpecies (A, Aeolian) (Alto, "A3 C4 B3 D4 C4 E4 F4 E4 D4 C4 B3 A3 ~A3") (Soprano, "A4 E4 G4 F4 E4 C5 A4 B4 B4 A4 Gs4 A4 ~A4")
fig23 = firstSpecies (A, Aeolian) (Alto, "A3 C4 B3 D4 C4 E4 F4 E4 D4 C4 B3 A3 ~A3") (Tenor, "A3 A3 G3 F3 E3 E3 D3 C3 G3 A3 Gs3 A3 ~A3")

fuxFirstSpecies = [fig5, fig6Good, fig11, fig12Good, fig13, fig14, fig15Good, fig21, fig22, fig23]
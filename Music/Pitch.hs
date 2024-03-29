
{-# LANGUAGE FlexibleContexts,
             FlexibleInstances,
             OverloadedStrings,
             StandaloneDeriving,
             TupleSections,
             UndecidableInstances
#-}

module Music.Pitch where

import Data.Char (toLower)
import Data.Default (Default(..))
import qualified Data.Map as M
import qualified Music.Pitch.Literal.Pitch
import Music.Pitch.Literal.Pitch hiding (IsPitch(..))
import Text.Pretty hiding (Mode)

import qualified Euterpea as E
import Euterpea (Mode(..), Note1, PitchClass(..), Pitch, absPitch, pitch)


-- | Converts 'PitchClass' to a diatonic PitchClass and a number of accidentals (from -2 to 2).
noteMapping :: PitchClass -> (PitchClass, Accidental)
noteMapping pc = (pc', acc)
    where
        (c:cs) = show pc
        pc' = read [c]
        acc = case cs of
            "ff" -> -2
            "f"  -> -1
            ""   -> 0
            "s"  -> 1
            "ss" -> 2
            _    -> error $ "invalid PitchClass \"" ++ show pc ++ "\""

-- * Pitches

class ToPitch a where
    toPitch :: a -> Pitch

class FromPitch a where
    fromPitch :: Pitch -> a

-- | Given a function modifying a 'Pitch', conjugates that function by conversion to/from 'Pitch'.
modPitch :: (FromPitch a, ToPitch a) => (Pitch -> Pitch) -> a -> a
modPitch f = fromPitch . f . toPitch

-- | Transposes a pitch.
trans :: (FromPitch a, ToPitch a) => Int -> a -> a
trans = modPitch . E.trans

instance ToPitch Pitch where toPitch = id
instance FromPitch Pitch where fromPitch = id
instance ToPitch Note1 where toPitch = fst
instance FromPitch Note1 where fromPitch = (, [])

instance ToPitch PitchL where
    toPitch pc = pitch $ fromIntegral (Music.Pitch.Literal.Pitch.fromPitch pc :: Integer) + 48

-- | Ordinal position on the diatonic staff, C = 0, D = 1, ..., B = 6.
--   Accidentals are not allowed.
diatonicPosition :: PitchClass -> Int
diatonicPosition pc = case pc of
    C -> 0
    D -> 1
    E -> 2
    F -> 3
    G -> 4
    A -> 5
    B -> 6
    _ -> error "invalid pitch"

-- | Diatonic staff distance between two pitches.
staffDistance :: (ToPitch a) => a -> a -> Int
staffDistance p1 p2 = n2 - n1
    where
        ((pc1, oct1), (pc2, oct2)) = (toPitch p1, toPitch p2)
        ((dpc1, _), (dpc2, _)) = (noteMapping pc1, noteMapping pc2)
        n1 = 7 * oct1 + diatonicPosition dpc1
        n2 = 7 * oct2 + diatonicPosition dpc2

fromPitch' :: Pitch -> PitchL
fromPitch' (pc, oct) = PitchL (diatonicPosition pc', Just $ fromIntegral acc, oct)
    where (pc', acc) = noteMapping pc

instance FromPitch PitchL where
    fromPitch = fromPitch'

instance {-# OVERLAPPING #-} Music.Pitch.Literal.Pitch.IsPitch Pitch where
    fromPitch = toPitch

instance {-# OVERLAPPABLE #-} (FromPitch a) => Music.Pitch.Literal.Pitch.IsPitch a where
    fromPitch = fromPitch . toPitch

-- | For double flat -2, flat -1, natural 0, sharp 1 and double sharp 2.
type Accidental = Int

-- | Number of octaves raised (positive) or flattened (negative).
type Octave = Int

-- * Languages

data Language = Nederlands | Catalan | Deutsch | English | Espanol | Francais | Italiano | Norsk | Portugues | Suomi | Svenska | Vlaams
    deriving (Bounded, Enum, Eq, Ord, Show)

instance Default Language where
    def = Nederlands

instance Pretty Language where
    pretty Francais = "français"
    pretty lang = string $ toLower <$> show lang

stdNoteNames :: M.Map PitchClass String
stdNoteNames = M.fromList [(pc, toLower <$> show pc) | pc <- [C, D, E, F, G, A, B]]

solfegeNoteNames :: M.Map PitchClass String
solfegeNoteNames = M.fromList [(C, "do"), (D, "re"), (E, "mi"), (F, "fa"), (G, "sol"), (A, "la"), (B, "si")]

germanNoteNames :: M.Map PitchClass String
germanNoteNames = M.fromList $ [(pc, toLower <$> show pc) | pc <- [C, D, E, F, G, A]] ++ [(B, "h")]

-- | Maps from language to (diatonic note name map, (flat names, sharp names)).
languageNoteMap :: M.Map Language (M.Map PitchClass String, ([String], [String]))
languageNoteMap = M.fromList [
    (Nederlands, (stdNoteNames, (["es"], ["is"]))),
    (Catalan, (solfegeNoteNames, (["b"], ["d", "s"]))),
    (Deutsch, (germanNoteNames, (["es"], ["is"]))),
    (English, (stdNoteNames, (["f", "flat"], ["s", "sharp"]))),
    (Espanol, (solfegeNoteNames, (["b"], ["s"]))),
    (Francais, (solfegeNoteNames, (["b"], ["d"]))),
    (Italiano, (solfegeNoteNames, (["b"], ["d"]))),
    (Norsk, (germanNoteNames, (["es", "ess"], ["is", "iss"]))),
    (Portugues, (solfegeNoteNames, (["b"], ["s"]))),
    (Suomi, (germanNoteNames, (["es"], ["is"]))),
    (Svenska, (germanNoteNames, (["ess"], ["iss"]))),
    (Vlaams, (solfegeNoteNames, (["b"], ["k"])))
    ]

noteName' :: Language -> PitchClass -> String
noteName' lang pc = nameMap M.! base ++ concat (replicate n accSym)
    where
        (nameMap, (flat:_, sharp:_)) = languageNoteMap M.! lang
        (base, acc) = noteMapping pc
        accSym = if (acc >= 0) then sharp else flat
        n = abs acc

germanNoteName' :: Language -> PitchClass -> String
germanNoteName' _ Eff = "eses"
germanNoteName' _ Ef = "es"
germanNoteName' _ Aff = "ases"
germanNoteName' _ Af = "as"
germanNoteName' _ Bf = "b"
germanNoteName' lang pc = noteName' lang pc

-- | Language-specific rendering of note names.
noteName :: Language -> PitchClass -> String
noteName Norsk Bff = "bes"
noteName Svenska Eff = "essess"
noteName Svenska Ef = "ess"
noteName Svenska Aff = "assess"
noteName Svenska Af = "ass"
noteName Svenska Bf = "b"
noteName lang pc = if (lang `elem` [Deutsch, Norsk, Suomi, Svenska]) then germanNoteName' lang pc else noteName' lang pc

class PrettyPitch a where
    prettyPitch :: Language -> a -> Printer
    prettyPitchList :: Language -> [a] -> Printer
    prettyPitchList lang = brackets . sepBy (char ',') . map (prettyPitch lang)

instance PrettyPitch a => PrettyPitch (Maybe a) where
    prettyPitch lang = maybe empty (prettyPitch lang)

instance PrettyPitch PitchClass where
    prettyPitch lang = string . noteName lang

instance {-# OVERLAPS #-} PrettyPitch a => Pretty a where
    pretty = prettyPitch def
    prettyList = prettyPitchList def

instance PrettyPitch Pitch where
    prettyPitch lang (pc, oct) = prettyPitch lang pc <> string octStr
        where
            n = oct - 3
            octStr  | n < 0     = concat $ replicate (negate n) ","
                    | n > 0     = concat $ replicate n "'"
                    | otherwise = ""

instance {-# OVERLAPPABLE #-} (ToPitch a) => PrettyPitch a where
    prettyPitch lang = prettyPitch lang . toPitch


-- * Modes and Keys

type Key = (PitchClass, Mode)

-- | Converts a proper mode to the corresponding major or minor key signature.
simplifyMode :: Key -> Key
simplifyMode (pc, mode) = case mode of
    Major      -> (pc, Major)
    Minor      -> (pc, Minor)
    Ionian     -> (pc, Major)
    Dorian     -> (fst $ pitch (absPitch (pc, 4) - 5), Minor)
    Phrygian   -> (fst $ pitch (absPitch (pc, 4) - 7), Minor)
    Lydian     -> (fst $ pitch (absPitch (pc, 4) - 5), Minor)
    Mixolydian -> (fst $ pitch (absPitch (pc, 4) - 7), Major)
    Aeolian    -> (pc, Minor)
    Locrian    -> (fst $ pitch (absPitch (pc, 4) + 1), Major)
    _          -> (pc, Major)

instance Pretty Mode where
    pretty Major = "\\major"
    pretty Minor = "\\minor"
    pretty mode  = error $ "invalid mode \"" ++ show mode ++ "\""

deriving instance Read Mode

-- | Gets the number of sharps or flats in a key.
--   In general, this is an integer from -7 (7 flats) to 7 (7 sharps), but it can also have more than 7 if it is a "theoretical key" (in which case, double accidentals would occur).
getSharpFlatCount :: Key -> Int
getSharpFlatCount key = case simplifyMode key of
    (Cff, Major) -> -14
    (Aff, Minor) -> -14
    (Gff, Major) -> -13
    (Eff, Minor) -> -13
    (Dff, Major) -> -12
    (Bff, Minor) -> -12
    (Aff, Major) -> -11
    (Ff,  Minor) -> -11
    (Eff, Major) -> -10
    (Cf,  Minor) -> -10
    (Bff, Major) -> -9
    (Gf,  Minor) -> -9
    (Ff,  Major) -> -8
    (Df,  Minor) -> -8
    (Cf,  Major) -> -7
    (Af,  Minor) -> -7
    (Gf,  Major) -> -6
    (Ef,  Minor) -> -6
    (Fff, Minor) -> -6
    (Df,  Major) -> -5
    (Bf,  Minor) -> -5
    (Cff, Minor) -> -5
    (Af,  Major) -> -4
    (F,   Minor) -> -4
    (Gff, Minor) -> -4
    (Ef,  Major) -> -3
    (Fff, Major) -> -3
    (C,   Minor) -> -3
    (Dff, Minor) -> -3
    (Bf,  Major) -> -2
    (G,   Minor) -> -2
    (F,   Major) -> -1
    (D,   Minor) -> -1
    (C,   Major) -> 0
    (A,   Minor) -> 0
    (G,   Major) -> 1
    (E,   Minor) -> 1
    (D,   Major) -> 2
    (B,   Minor) -> 2
    (Gss, Major) -> 3
    (A,   Major) -> 3
    (Ess, Minor) -> 3
    (Fs,  Minor) -> 3
    (Dss, Major) -> 4
    (E,   Major) -> 4
    (Cs,  Minor) -> 4
    (Bss, Minor) -> 4
    (Ass, Major) -> 5
    (B,   Major) -> 5
    (Gs,  Minor) -> 5
    (Ess, Major) -> 6
    (Fs,  Major) -> 6
    (Ds,  Minor) -> 6
    (Bss, Major) -> 7
    (Cs,  Major) -> 7
    (As,  Minor) -> 7
    (Gs,  Major) -> 8
    (Es,  Minor) -> 8
    (Ds,  Major) -> 9
    (Bs,  Minor) -> 9
    (As,  Major) -> 10
    (Fss, Minor) -> 10
    (Es,  Major) -> 11
    (Css, Minor) -> 11
    (Bs,  Major) -> 12
    (Gss, Minor) -> 12
    (Fss, Major) -> 13
    (Dss, Minor) -> 13
    (Css, Major) -> 14
    (Ass, Minor) -> 14
    _            -> error "invalid key"

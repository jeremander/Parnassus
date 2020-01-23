
{-# LANGUAGE FlexibleContexts,
             FlexibleInstances,
             OverloadedStrings,
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


-- | Converts PitchClass to a diatonic PitchClass and a number of accidentals (from -2 to 2)
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

trans :: (FromPitch a, ToPitch a) => Int -> a -> a
trans n = fromPitch . E.trans n . toPitch

instance ToPitch Pitch where toPitch = id
instance FromPitch Pitch where fromPitch = id
instance ToPitch Note1 where toPitch = fst
instance FromPitch Note1 where fromPitch = (, [])

instance ToPitch PitchL where
    toPitch pc = pitch $ fromIntegral (Music.Pitch.Literal.Pitch.fromPitch pc :: Integer) + 48

fromPitch' :: Pitch -> PitchL
fromPitch' (pc, oct) = PitchL (pc'', Just $ fromIntegral acc, oct)
    where
        (pc', acc) = noteMapping pc
        pc'' = case pc' of
            C -> 0
            D -> 1
            E -> 2
            F -> 3
            G -> 4
            A -> 5
            B -> 6
            _ -> error "invalid pitch"

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

-- maps from language to (diatonic note name map, (flat names, sharp names))
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
germanNoteName' lang Eff = "eses"
germanNoteName' lang Ef = "es"
germanNoteName' lang Aff = "ases"
germanNoteName' lang Af = "as"
germanNoteName' lang Bf = "b"
germanNoteName' lang pc = noteName' lang pc

-- language-specific rendering of note names
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
            octStr  | n < 0  = concat $ replicate (negate n) ","
                    | n == 0 = ""
                    | n > 0   = concat $ replicate n "'"


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
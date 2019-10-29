
{-# LANGUAGE FlexibleContexts,
             FlexibleInstances,
             OverloadedStrings,
             TupleSections,
             UndecidableInstances
#-}

module Music.Pitch where

import Data.Char (toLower)
import qualified Music.Pitch.Literal.Pitch
import Music.Pitch.Literal.Pitch hiding (IsPitch(..))
import Text.Pretty hiding (Mode)

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
type Octaves = Int

data OctaveCheck = OctaveCheck
    deriving (Eq, Show)

instance Pretty PitchClass where
    pretty pc = string (toLower <$> show pc') <> string acc'
        where
            (pc', acc) = noteMapping pc
            acc' = case acc of
                -2 -> "eses"
                -1 -> "es"
                0  -> ""
                1  -> "is"
                2  -> "isis"
                _  -> error "invalid PitchClass \"" ++ show pc ++ "\""

instance {-# OVERLAPPING #-} Pretty Pitch where
    pretty (pc, oct) = pretty pc <> string octStr
        where
            n = oct - 3
            octStr  | n < 0  = concat $ replicate (negate n) ","
                    | n == 0 = ""
                    | n >0   = concat $ replicate n "'"


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
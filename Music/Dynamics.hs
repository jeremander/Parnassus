module Music.Dynamics where

import Data.Char (toLower)
import Text.Pretty (Pretty(..), string)
import qualified Data.Char as Char


data DynamicFixed = PPPPP | PPPP | PPP | PP | P | MP | MF | F | FF | FFF | FFFF | FFFFF | SF | SFF | SP | SPP | SFZ | RFZ
    deriving (Bounded, Enum, Eq, Ord, Read, Show)

instance Pretty DynamicFixed where
    pretty = string . ("\\" ++) . fmap toLower . show

data DynamicMotion = Crescendo | Decrescendo | EndDynamicMotion
    deriving (Bounded, Enum, Eq, Ord, Read, Show)

instance Pretty DynamicMotion where
    pretty Crescendo = string "\\<"
    pretty Decrescendo = string "\\>"
    pretty EndDynamicMotion = string "\\!"

data Dynamics = DynFixed DynamicFixed | DynMotion DynamicMotion
    deriving (Eq, Ord, Show)

instance Pretty Dynamics where
    pretty (DynFixed d)  = pretty d
    pretty (DynMotion d) = pretty d

{-# LANGUAGE OverloadedStrings #-}

module Music.Lilypond.Dynamics (
        Dynamics(..),
  ) where

import Data.Char (toLower)
import Text.Pretty (Pretty(..), string)
import qualified Data.Char as Char


data Dynamics = PPPPP | PPPP | PPP | P | MP | MF | F | FF | FFF | SF | SFF | SP | SPP | SFZ | RFZ
    deriving (Eq, Ord, Show, Enum, Bounded)

instance Pretty Dynamics where
    pretty = string . ("\\" ++) . fmap toLower . show
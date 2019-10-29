{-# LANGUAGE
    OverloadedStrings
    #-}

module Music.Lilypond.Score (
    Score(..),
    Header(..),
    BookPart(..),
    Book(..),
    Lilypond(..),
    HasHeader(..),
    ToLilypond(..)
) where

import Data.Default (Default(..))
import Text.Pretty (Pretty(..), Printer, (<+>), (<//>), nest, string, vcat)

import Music.Lilypond.Music (Music(..))
import Music.Lilypond.Value (Value(..))



mkSection :: String -> Printer -> Printer
mkSection name p = string (name ++ "{") <//> nest 4 p <//> string "}"

-- | A Score is a compound musical expression.
newtype Score = Score Music
    deriving (Eq, Show)

instance Pretty Score where
    pretty (Score m) = mkSection "\\score" $ pretty m

-- | LilyPond score header with various information about the score.
-- TODO: enable markup text in the fields
data Header = Header {
    dedication :: Maybe Value,
    title :: Maybe Value,
    subtitle :: Maybe Value,
    subsubtitle :: Maybe Value,
    instrument :: Maybe Value,
    poet :: Maybe Value,
    composer :: Maybe Value,
    meter :: Maybe Value,
    arranger :: Maybe Value,
    tagline :: Maybe Value,
    copyright :: Maybe Value
} deriving (Eq, Show)

mkHdrField :: String -> Maybe Value -> Printer
mkHdrField name val = pretty $ (string name <+> "+" <+>) . pretty <$> val

instance Pretty Header where
    pretty hdr = mkSection "\\header" $ vcat [
                    mkHdrField "dedication" $ dedication hdr,
                    mkHdrField "title" $ title hdr,
                    mkHdrField "subtitle" $ subtitle hdr,
                    mkHdrField "subsubtitle" $ subsubtitle hdr,
                    mkHdrField "instrument" $ instrument hdr,
                    mkHdrField "poet" $ poet hdr,
                    mkHdrField "composer" $ composer hdr,
                    mkHdrField "meter" $ meter hdr,
                    mkHdrField "arranger" $ arranger hdr,
                    mkHdrField "tagline" $ tagline hdr,
                    mkHdrField "copyright" $ copyright hdr
                ]

instance Default Header where
    def = Header Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

-- | A BookPart consists of an optional header and one or more Scores.
data BookPart = BookPart (Maybe Header) [Score]
    deriving (Eq, Show)

instance Pretty BookPart where
    pretty (BookPart hdr scores) = mkSection "\\bookpart" $ pretty hdr <//> vcat (pretty <$> scores)

-- | A Book consists of an optional header and one or more BookParts.
data Book = Book (Maybe Header) [BookPart]
    deriving (Eq, Show)

instance Pretty Book where
    pretty (Book hdr bookParts) = mkSection "\\book" $ pretty hdr <//> vcat (pretty' <$> bookParts)
        where
            pretty' (BookPart Nothing [score]) = pretty score
            pretty' bookPart                   = pretty bookPart

-- | Lilypond object is the top-level element of a LilyPond file.
data Lilypond = Lilypond (Maybe Header) [Book]
    deriving (Eq, Show)

instance Pretty Lilypond where
    pretty (Lilypond hdr books) = pretty hdr <//> vcat (pretty <$> books)

class HasHeader a where
    setHeader :: Header -> a -> a

instance HasHeader Lilypond where
    setHeader hdr (Lilypond _ books) = Lilypond (Just hdr) books

instance HasHeader Book where
    setHeader hdr (Book _ bookParts) = Book (Just hdr) bookParts

instance HasHeader BookPart where
    setHeader hdr (BookPart _ scores) = BookPart (Just hdr) scores

class ToLilypond a where
    toLilypond :: a -> Lilypond

instance ToLilypond Lilypond where
    toLilypond = id

instance ToLilypond Book where
    toLilypond book = Lilypond Nothing [book]

instance ToLilypond BookPart where
    toLilypond bookPart = toLilypond $ Book Nothing [bookPart]

instance ToLilypond Score where
    toLilypond score = toLilypond $ BookPart Nothing [score]

instance ToLilypond Music where
    toLilypond mus = toLilypond $ Score mus
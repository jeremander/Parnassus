{-# LANGUAGE
    OverloadedStrings,
    TemplateHaskell
    #-}

module Music.Lilypond.Score (
    Score(..),
    Header(..),
    BookPart(..),
    Book(..),
    TopLevel(..),
    Lilypond(..),
    HasHeader(..),
    ToLilypond(..)
) where

import Control.Lens ((^.), makeLenses)
import Data.Default (Default(..))
import Data.List (intersperse)
import Text.Pretty (Pretty(..), Printer, (<+>), (<//>), nest, string, vcat)

import Music.Lilypond.Music (Assignment(..), Literal(..), MusicL(..))
import Music.Pitch (Language(..))


mkSection :: String -> Printer -> Printer
mkSection name p = string (name ++ "{") <//> nest 4 p <//> string "}"

-- | A Score is a compound musical expression.
newtype Score = Score MusicL
    deriving (Eq, Show)

instance Pretty Score where
    pretty (Score m) = mkSection "\\score" $ pretty m

-- | LilyPond score header with various information about the score.
-- TODO: enable markup text in the fields
data Header = Header {
    _dedication :: Maybe Literal,
    _title :: Maybe Literal,
    _subtitle :: Maybe Literal,
    _subsubtitle :: Maybe Literal,
    _instrument :: Maybe Literal,
    _poet :: Maybe Literal,
    _composer :: Maybe Literal,
    _meter :: Maybe Literal,
    _arranger :: Maybe Literal,
    _tagline :: Maybe Literal,
    _copyright :: Maybe Literal
} deriving (Eq, Show)

makeLenses ''Header

mkHdrField :: String -> Maybe Literal -> Printer
mkHdrField name lit = pretty $ (string name <+> "=" <+>) . pretty <$> lit

instance Pretty Header where
    pretty hdr = mkSection "\\header" $ vcat [
                    mkHdrField "dedication" $ hdr^.dedication,
                    mkHdrField "title" $ hdr^.title,
                    mkHdrField "subtitle" $ hdr^.subtitle,
                    mkHdrField "subsubtitle" $ hdr^.subsubtitle,
                    mkHdrField "instrument" $ hdr^.instrument,
                    mkHdrField "poet" $ hdr^.poet,
                    mkHdrField "composer" $ hdr^.composer,
                    mkHdrField "meter" $ hdr^.meter,
                    mkHdrField "arranger" $ hdr^.arranger,
                    mkHdrField "tagline" $ hdr^.tagline,
                    mkHdrField "copyright" $ hdr^.copyright
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

data TopLevel = Version String
              | Lang Language
              | Include FilePath Lilypond
              | AssignmentTop Assignment
              | HeaderTop Header
              | BookTop Book
    deriving (Eq, Show)

instance Pretty TopLevel where
    pretty (Version v)       = "\\version" <+> (string $ show v)
    pretty (Lang lang)       = "\\language" <+> pretty lang
    pretty (Include p _ )    = "\\include" <+> (string $ show p)
    pretty (AssignmentTop a) = pretty a
    pretty (HeaderTop h)     = pretty h
    pretty (BookTop b)       = pretty b

-- | Lilypond object is the root element of a LilyPond file.
newtype Lilypond = Lilypond [TopLevel]
    deriving (Eq, Show)

instance Pretty Lilypond where
    pretty (Lilypond elts) = vcat $ intersperse (string "") $ pretty <$> elts

class HasHeader a where
    setHeader :: Header -> a -> a

instance HasHeader Book where
    setHeader hdr (Book _ bookParts) = Book (Just hdr) bookParts

instance HasHeader BookPart where
    setHeader hdr (BookPart _ scores) = BookPart (Just hdr) scores

class ToLilypond a where
    toLilypond :: a -> Lilypond

instance ToLilypond Lilypond where
    toLilypond = id

instance ToLilypond Book where
    toLilypond book = Lilypond [BookTop book]

instance ToLilypond BookPart where
    toLilypond bookPart = toLilypond $ Book Nothing [bookPart]

instance ToLilypond Score where
    toLilypond score = toLilypond $ BookPart Nothing [score]

instance ToLilypond MusicL where
    toLilypond mus = toLilypond $ Score mus
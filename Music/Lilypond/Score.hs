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
import Data.Maybe (fromMaybe)
import Data.Foldable (foldl')
import Data.List (intersperse)
import Text.Pretty (Pretty(..), Printer, (<+>), (<//>), nest, string, vcat)

import Music.Lilypond.Music (Assignment(..), Literal(..), MusicL(..))
import Music.Pitch (Language(..), PrettyPitch(..))


mkSection :: String -> Printer -> Printer
mkSection name p = string (name ++ "{") <//> nest 4 p <//> string "}"

-- | A Score is a compound musical expression.
newtype Score = Score MusicL
    deriving (Eq, Show)

instance PrettyPitch Score where
    prettyPitch lang (Score m) = mkSection "\\score" $ prettyPitch lang m

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

instance PrettyPitch BookPart where
    prettyPitch lang (BookPart hdr scores) = mkSection "\\bookpart" $ pretty hdr <//> vcat (prettyPitch lang <$> scores)

-- | A Book consists of an optional header and one or more BookParts.
data Book = Book (Maybe Header) [BookPart]
    deriving (Eq, Show)

instance PrettyPitch Book where
    prettyPitch lang (Book hdr bookParts) = mkSection "\\book" $ pretty hdr <//> vcat (pretty' <$> bookParts)
        where
            pretty' (BookPart Nothing [score]) = prettyPitch lang score
            pretty' bookPart                   = prettyPitch lang bookPart

data TopLevel = Version String
              | Lang Language
              | Include FilePath Lilypond
              | AssignmentTop Assignment
              | HeaderTop Header
              | BookTop Book
    deriving (Eq, Show)

instance PrettyPitch TopLevel where
    prettyPitch _ (Version v)          = "\\version" <+> (string $ show v)
    prettyPitch _ (Lang lang)          = "\\language" <+> pretty lang
    prettyPitch _ (Include p _ )       = "\\include" <+> (string $ show p)
    prettyPitch lang (AssignmentTop a) = prettyPitch lang a
    prettyPitch _ (HeaderTop h)        = pretty h
    prettyPitch lang (BookTop b)       = prettyPitch lang b

-- | Lilypond object is the root element of a LilyPond file.
newtype Lilypond = Lilypond [TopLevel]
    deriving (Eq, Show)

-- | Attempts to detect the language of a Lilypond file
detectLanguage :: Lilypond -> Maybe Language
detectLanguage (Lilypond elts) = foldl' combine Nothing (detectLang' <$> elts)
    where
        detectLang' elt = case elt of
            Lang lang    -> Just lang
            Include _ lp -> detectLanguage lp  -- recursively call on included file
            _            -> Nothing
        combine Nothing Nothing = Nothing
        combine Nothing lang'   = lang'
        combine lang Nothing    = lang
        combine lang lang'      = error "Multiple languages detected in Lilypond file."

instance Pretty Lilypond where
    pretty lp@(Lilypond elts) = vcat $ intersperse (string "") (prettyPitch lang <$> elts)
        where lang = fromMaybe def (detectLanguage lp)

class HasHeader a where
    setHeader :: Header -> a -> a

instance HasHeader BookPart where
    setHeader hdr (BookPart _ scores) = BookPart (Just hdr) scores

instance HasHeader Book where
    setHeader hdr (Book _ bookParts) = Book (Just hdr) bookParts

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
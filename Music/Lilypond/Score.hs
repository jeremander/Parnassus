{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module Music.Lilypond.Score where

import Data.Default (Default(..))
import Data.Maybe (fromMaybe)
import Data.Foldable (foldl')
import Data.List (intersperse)
import Text.Pretty (Pretty(..), Printer, (<+>), (<//>), nest, string, vcat)

import Music.Lilypond.Literal
import Music.Lilypond.MusicL
import Music.Pitch


mkSection :: String -> Printer -> Printer
mkSection name p = string (name ++ "{") <//> nest 4 p <//> string "}"

-- | LilyPond score header with various information about the score.
newtype Header = Header [LitAssignment]
    deriving (Eq, Show)

instance Pretty Header where
    pretty (Header assignments) = mkSection "\\header" $ vcat (pretty <$> assignments)

instance Default Header where
    def = Header []

data LayoutItem a = LayoutAssignment LitAssignment | LayoutVar Variable | LayoutContext (Context a)
    deriving (Eq, Show)

instance (PrettyPitch a) => PrettyPitch (LayoutItem a) where
    prettyPitch _ (LayoutAssignment assignment) = pretty assignment
    prettyPitch _ (LayoutVar v)                 = pretty v
    prettyPitch lang (LayoutContext context)    = prettyPitch lang context

newtype Layout a = Layout [LayoutItem a]
    deriving (Eq, Show)

instance (PrettyPitch a) => PrettyPitch (Layout a) where
    prettyPitch lang (Layout items) = mkSection "\\layout" $ vcat (prettyPitch lang <$> items)

data MidiItem a = MidiTempo Tempo | MidiContext (Context a)
    deriving (Eq, Show)

instance (PrettyPitch a) => PrettyPitch (MidiItem a) where
    prettyPitch _    (MidiTempo tempo) = pretty tempo
    prettyPitch lang (MidiContext ctx) = prettyPitch lang ctx

newtype Midi a = Midi [MidiItem a]
    deriving (Eq, Show)

instance (PrettyPitch a) => PrettyPitch (Midi a) where
    prettyPitch lang (Midi items) = mkSection "\\midi" $ vcat (prettyPitch lang <$> items)

data ScoreItem a = ScoreMusic (MusicL a) | ScoreHeader Header | ScoreLayout (Layout a) | ScoreMidi (Midi a)
    deriving (Eq, Show)

instance (PrettyPitch a) => PrettyPitch (ScoreItem a) where
    prettyPitch lang (ScoreMusic mus)     = prettyPitch lang mus
    prettyPitch _    (ScoreHeader hdr)    = pretty hdr
    prettyPitch lang (ScoreLayout layout) = prettyPitch lang layout
    prettyPitch lang (ScoreMidi midi)     = prettyPitch lang midi

newtype Score a = Score [ScoreItem a]
    deriving (Eq, Show)

instance (PrettyPitch a) => PrettyPitch (Score a) where
    prettyPitch lang (Score items) = mkSection "\\score" $ vcat (prettyPitch lang <$> items)

-- | A BookPart consists of an optional header and one or more Scores.
data BookPart a = BookPart (Maybe Header) [Score a]
    deriving (Eq, Show)

instance (PrettyPitch a) => PrettyPitch (BookPart a) where
    prettyPitch lang (BookPart hdr scores) = mkSection "\\bookpart" $ pretty hdr <//> vcat (prettyPitch lang <$> scores)

-- | A Book consists of an optional header and one or more BookParts.
data Book a = Book (Maybe Header) [BookPart a]
    deriving (Eq, Show)

instance (PrettyPitch a) => PrettyPitch (Book a) where
    prettyPitch lang (Book hdr bookParts) = mkSection "\\book" $ pretty hdr <//> vcat (pretty' <$> bookParts)
        where
            pretty' (BookPart Nothing [score]) = prettyPitch lang score
            pretty' bookPart                   = prettyPitch lang bookPart

data TopLevel a = Version String
              | Lang Language
              | Include FilePath (Lilypond a)
              | AssignmentTop (Assignment a)
              | HeaderTop Header
              | BookTop (Book a)
    deriving (Eq, Show)

instance (PrettyPitch a) => PrettyPitch (TopLevel a) where
    prettyPitch _ (Version v)          = "\\version" <+> (string $ show v)
    prettyPitch _ (Lang lang)          = "\\language" <+> ("\"" <> pretty lang <> "\"")
    prettyPitch _ (Include p _ )       = "\\include" <+> (string $ show p)
    prettyPitch lang (AssignmentTop a) = prettyPitch lang a
    prettyPitch _ (HeaderTop h)        = pretty h
    prettyPitch lang (BookTop b)       = prettyPitch lang b

-- | Lilypond object is the root element of a LilyPond file.
newtype Lilypond a = Lilypond [TopLevel a]
    deriving (Eq, Show)

-- | Attempts to detect the language of a Lilypond file
detectLanguage :: Lilypond a -> Maybe Language
detectLanguage (Lilypond elts) = foldl' combine Nothing (detectLang' <$> elts)
    where
        detectLang' elt = case elt of
            Lang lang    -> Just lang
            Include _ lp -> detectLanguage lp  -- recursively call on included file
            _            -> Nothing
        combine lang Nothing    = lang
        combine _ lang'         = lang'  -- override with more recent language

instance (PrettyPitch a) => Pretty (Lilypond a) where
    pretty lp@(Lilypond elts) = vcat $ intersperse (string "") (prettyPitch lang <$> elts)
        where lang = fromMaybe def (detectLanguage lp)

class HasHeader a where
    setHeader :: Header -> a -> a

instance HasHeader (BookPart a) where
    setHeader hdr (BookPart _ scores) = BookPart (Just hdr) scores

instance HasHeader (Book a) where
    setHeader hdr (Book _ bookParts) = Book (Just hdr) bookParts

instance HasHeader (Lilypond a) where
    setHeader hdr (Lilypond topLevels) = Lilypond (HeaderTop hdr : topLevels)

class ToLilypond m a where
    toLilypond :: m a -> Lilypond a

instance ToLilypond Lilypond a where
    toLilypond = id

instance ToLilypond Book a where
    toLilypond book = Lilypond [BookTop book]

instance ToLilypond BookPart a where
    toLilypond bookPart = toLilypond $ Book Nothing [bookPart]

instance ToLilypond Score a where
    toLilypond score = toLilypond $ BookPart Nothing [score]

instance ToLilypond MusicL a where
    toLilypond mus = toLilypond $ Score [ScoreMusic mus]

-- | Eliminates all \includes by substituting the corresponding Lilypond code.
spliceIncludes :: Lilypond a -> Lilypond a
spliceIncludes (Lilypond tops) = Lilypond (go tops)
    where
        go []         = []
        go (x:xs) = case x of
            Include _ (Lilypond tops') -> go tops' ++ go xs
            _                          -> x : go xs


-- convenience aliases

type MidiItem' = MidiItem NotePitch
type Midi' = Midi NotePitch
type ScoreItem' = ScoreItem NotePitch
type Score' = Score NotePitch
type BookPart' = BookPart NotePitch
type Book' = Book NotePitch
type TopLevel' = TopLevel NotePitch
type Lilypond' = Lilypond NotePitch
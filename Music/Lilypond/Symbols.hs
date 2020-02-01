{-# LANGUAGE OverloadedStrings #-}

module Music.Lilypond.Symbols where

import Data.Char (toLower)
import Data.Default (Default(..))
import Text.Pretty ((<+>), Pretty(..), hcat, string)

import Music.Dynamics (Dynamics)
import Music.Lilypond.Literal (Markup(..))
import Music.Rhythm (Duration(..))


data BarLine = BarCheck | BarLine String
    deriving (Eq, Ord, Show)

instance Pretty BarLine where
    pretty BarCheck    = "|"
    pretty (BarLine s) = "\\bar" <+> (string $ show s)

data Beam = BeamOn | BeamOff
    deriving (Eq, Ord, Show)

instance Pretty Beam where
    pretty BeamOn  = "["
    pretty BeamOff = "]"

data Slur = SlurOn | SlurOff | PhraseSlurOn | PhraseSlurOff
    deriving (Eq, Ord, Show)

instance Pretty Slur where
    pretty SlurOn = "("
    pretty SlurOff = ")"
    pretty PhraseSlurOn  = "\\("
    pretty PhraseSlurOff = "\\)"

data Direction = Below | Default | Above
    deriving (Eq, Ord, Show)

instance Default Direction where
    def = Default

instance Pretty Direction where
    pretty Below   = "_"
    pretty Default = "-"
    pretty Above   = "^"

-- | Articulations. These include ornaments.
data Articulation
    = Accent
    | Marcato
    | Staccatissimo
    | Espressivo
    | Staccato
    | Tenuto
    | Portato
    | Upbow
    | Downbow
    | Flageolet
    | Thumb
    | LeftHeel
    | RightHeel
    | LeftToe
    | RightToe
    | Open
    | Stopped
    | Turn
    | ReverseTurn
    | Trill
    | Prall
    | Mordent
    | PrallPrall
    | PrallMordent
    | UpPrall
    | DownPrall
    | UpMordent
    | DownMordent
    | PrallDown
    | PrallUp
    | LinePrall
    | SignumCongruentiae
    | ShortFermata
    | Fermata
    | LongFermata
    | VeryLongFermata
    | Segno
    | Coda
    | VarCoda
    deriving (Bounded, Enum, Eq, Ord, Read, Show)

instance Pretty Articulation where
    pretty Accent             = ">"
    pretty Marcato            = "^"
    pretty Staccatissimo      = "!"
    pretty Espressivo         = "\\espressivo"
    pretty Staccato           = "."
    pretty Tenuto             = "-"
    pretty Portato            = "_"
    pretty Upbow              = "\\upbow"
    pretty Downbow            = "\\downbow"
    pretty Flageolet          = "\\flageolet"
    pretty Thumb              = "\\thumb"
    pretty LeftHeel           = "\\leftheel"
    pretty RightHeel          = "\\rightheel"
    pretty LeftToe            = "\\lefttoe"
    pretty RightToe           = "\\righttoe"
    pretty Open               = "\\open"
    pretty Stopped            = "+"
    pretty Turn               = "\\turn"
    pretty ReverseTurn        = "\\reverseturn"
    pretty Trill              = "\\trill"
    pretty Prall              = "\\prall"
    pretty Mordent            = "\\mordent"
    pretty PrallPrall         = "\\prallprall"
    pretty PrallMordent       = "\\prallmordent"
    pretty UpPrall            = "\\upprall"
    pretty DownPrall          = "\\downprall"
    pretty UpMordent          = "\\upmordent"
    pretty DownMordent        = "\\downmordent"
    pretty PrallDown          = "\\pralldown"
    pretty PrallUp            = "\\prallup"
    pretty LinePrall          = "\\lineprall"
    pretty SignumCongruentiae = "\\signumCongruentiae"
    pretty ShortFermata       = "\\shortfermata"
    pretty Fermata            = "\\fermata"
    pretty LongFermata        = "\\longfermata"
    pretty VeryLongFermata    = "\\verylongfermata"
    pretty Segno              = "\\segno"
    pretty Coda               = "\\coda"
    pretty VarCoda            = "\\varcoda"
    prettyList                = hcat . fmap pretty

-- | Something 'expressive' appended to a note.
data Expressive
    = Articulation Direction Articulation
    | Dynamics Direction Dynamics
    | Tie
    | Glissando
    | Beam Beam
    | Slur Slur
    | Text Direction String    -- ^quoted string markup
    | Markup Direction Markup  -- ^\markup
    deriving (Eq, Ord, Show)

instance Pretty Expressive where
    pretty (Articulation d a)   = pretty d <> pretty a
    pretty (Dynamics d a)       = pretty d <> pretty a
    pretty Tie                  = "~"
    pretty Glissando            = "\\glissando"
    pretty (Beam b)             = pretty b
    pretty (Slur s)             = pretty s
    pretty (Text d s)           = pretty d <> (string . show) s -- add quotes
    pretty (Markup d m)         = pretty d <> ("\\markup" <+> pretty m)
    prettyList                  = hcat . fmap pretty

data StdClef = Treble
             | French
             | GG
             | TenorG
             | Soprano
             | Mezzosoprano
             | Alto
             | Tenor
             | Baritone
             | AltovarC
             | TenorvarC
             | BaritonevarC
             | Varbaritone
             | Bass
             | Subbass
             | Percussion
             | Varpercussion
             | Tab
             | Moderntab
    deriving (Bounded, Enum, Eq, Ord, Read, Show)

instance Pretty StdClef where
    pretty = string . fmap toLower . show

data Clef = StdClef StdClef | CustomClef String
    deriving (Eq, Ord, Show)

instance Pretty Clef where
    pretty (StdClef std)  = pretty std
    pretty (CustomClef s) = string $ show s

data Staff =  Staff
            | DrumStaff
            | RhythmicStaff
            | TabStaff
            | GregorianTranscriptStaff
            | StaffGroup
            | ChoirStaff
            | GrandStaff
            | PianoStaff
    deriving (Bounded, Enum, Eq, Ord, Read, Show)

instance Pretty Staff where
    pretty = string . show
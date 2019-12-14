{-# LANGUAGE
    OverloadedStrings,
    TypeFamilies
#-}

module Music.Lilypond.Music where

import Control.Arrow ((***), (<<<), second)
import Data.Char (isAlpha, isUpper, toLower, toUpper)
import Data.Default (Default(..))
import Data.List.Split (splitOn)
import Data.Ratio ((%), denominator, numerator)
import Data.VectorSpace ((*^), AdditiveGroup(..), Scalar, VectorSpace)
import Text.Pretty (Pretty(..), Printer, (<+>), (<//>), char, empty, hcat, hsep, int, nest, sep, sepByS, string, vcat)

import Euterpea (Mode(..), Pitch)
import Music.Dynamics (DynamicMotion, Dynamics)
import Music.Lilypond.Literal
import Music.Pitch (FromPitch(..), Language(..), PrettyPitch(..), ToPitch(..), Key)
import Music.Rhythm (TimeSig)


-- Utilities

notImpl :: String -> a
notImpl a = error $ "Not implemented: " ++ a

-- capitalizes the first letter of a string
cap :: String -> String
cap cs = [if (i == 0) then toUpper c else c | (i, c) <- zip [0..] cs]


-- Types

-- * Musical Notation

data BarLine = BarCheck | BarLine String
    deriving (Eq, Show)

instance Pretty BarLine where
    pretty BarCheck    = "|"
    pretty (BarLine s) = "\\bar" <+> (string $ show s)

data Beam = BeamOn | BeamOff
    deriving (Eq, Show)

instance Pretty Beam where
    pretty BeamOn  = "["
    pretty BeamOff = "]"

data Slur = SlurOn | SlurOff | PhraseSlurOn | PhraseSlurOff
    deriving (Eq, Show)

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
    deriving (Bounded, Enum, Eq, Read, Show)

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
    deriving (Eq, Show)

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

data OctaveCheck = OctaveCheck
    deriving (Eq, Show)

data NotePitch = NotePitch Pitch (Maybe OctaveCheck) | DrumNotePitch
    deriving (Eq, Show)

instance PrettyPitch NotePitch where
    prettyPitch lang (NotePitch p Nothing) = prettyPitch lang p
    prettyPitch lang (NotePitch p _)       = string $ p' ++ "=" ++ oct
        where (p', oct) = span isAlpha $ show $ prettyPitch lang p
    -- TODO: implement
    prettyPitch _ DrumNotePitch            = notImpl "Non-standard pitch"
    prettyPitchList lang                   = hsep . fmap (prettyPitch lang)

instance ToPitch NotePitch where
    toPitch (NotePitch p _) = p
    toPitch _               = notImpl "Non-standard pitch"

instance FromPitch NotePitch where
    fromPitch = (`NotePitch` Nothing)

-- | Notated time in fractions, in @[2^^i | i <- [-10..3]]@, along with a number of dots and a multiplier.
data Duration = Duration Rational Int Int
    deriving (Eq, Ord, Show)

instance Real Duration where
    toRational (Duration r nd m) = r * ((3 / 2) ^^ nd) * (fromIntegral m)

instance Fractional Duration where
    fromRational r          = Duration r 0 1
    recip (Duration r _ _ ) = Duration (1 / r) 0 1

instance Num Duration where
    (+) d1 d2 = fromRational (toRational d1 + toRational d2)
    (-) d1 d2 = fromRational (toRational d1 - toRational d2)
    (*) d1 d2 = fromRational (toRational d1 * toRational d2)
    abs d = fromRational $ abs $ toRational d
    signum d = fromRational $ signum $ toRational d
    fromInteger = fromRational . fromIntegral

intLog2 :: (Integral a, Integral b) => a -> b
intLog2 = truncate . logBase 2 . fromIntegral

type DottedDuration = (Rational, Int)

-- | Separate a rational duration into a sequence of undotted durations with some number of dots
separateDots :: Rational -> [DottedDuration]
separateDots r
    | r == 0       = []
    | r >= 16      = (8, 0) : separateDots (r - 8)
    | diff == 0    = [(r, 0)]
    | n' == d' - 1 = [(b, intLog2 d')]
    | otherwise    = (b, 0) : separateDots diff
    where
        (n, d) = (numerator r, denominator r)
        b = (2 ^ (intLog2 $ fromInteger n)) % d
        diff = r - b
        q = diff / b
        (n', d') = (numerator q, denominator q)

showDottedDuration :: DottedDuration -> String
showDottedDuration (r, nd) = go r ++ (concat $ replicate nd ".")
    where
        go 8 = "\\maxima"
        go 4 = "\\longa"
        go 2 = "\\breve"
        go r' = show $ denominator r'

instance Pretty Duration where
    pretty (Duration r nd m) = case (separateDots r) of
        [(r', nd')] -> string (showDottedDuration (r', nd' + nd)) <> (if (m == 1) then "" else (string $ "*" ++ show m))
        _           -> error "invalid duration"

data RestType
    = StdRest  -- ^ standard rest
    | FullRest -- ^ multi-measure rest
    | Skip     -- ^ invisible rest
    deriving (Eq, Ord, Show)

instance Pretty RestType where
    pretty StdRest  = "r"
    pretty FullRest = "R"
    pretty Skip     = "s"

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
    deriving (Eq, Show)

instance Pretty Clef where
    pretty (StdClef std)  = pretty std
    pretty (CustomClef s) = string $ show s

data Tempo = Tempo (Maybe String) (Maybe (Duration, Integer))
    deriving (Eq, Show)

instance Pretty Tempo where
    pretty (Tempo Nothing Nothing)          = mempty
    pretty (Tempo (Just t) Nothing)         = "\\tempo" <+> pretty t
    pretty (Tempo Nothing (Just (d, bpm)))  = "\\tempo" <+> pretty d <+> "=" <+> pretty bpm
    pretty (Tempo (Just t) (Just (d, bpm))) = "\\tempo" <+> pretty t <+> pretty d <+> "=" <+> pretty bpm

data Staff =  Staff
            | DrumStaff
            | RhythmicStaff
            | TabStaff
            | GregorianTranscriptStaff
            | StaffGroup
            | ChoirStaff
            | GrandStaff
            | PianoStaff
    deriving (Bounded, Enum, Eq, Read, Show)

instance Pretty Staff where
    pretty = string . show

data Assignment = Assignment String MusicL  -- foo = {a b c}
            | SymbAssignment String Literal MusicL  -- foo #'bar = baz
            | PropAssignment Tweak  -- \override, etc.
            | Set Assignment -- \set ...
            | Once Assignment  -- \once ...
    deriving (Eq, Show)

instance PrettyPitch Assignment where
    prettyPitch lang (Assignment s e) = string s <+> "=" <+> prettyPitch lang e
    prettyPitch lang (SymbAssignment s l e) = string s <+> pretty l <+> "=" <+> prettyPitch lang e
    prettyPitch _    (PropAssignment t) = pretty t
    prettyPitch lang (Set a) = "\\set" <+> prettyPitch lang a
    prettyPitch lang (Once a) = "\\once" <+> prettyPitch lang a

newtype WithBlock = WithBlock Assignment
    deriving (Eq, Show)

instance PrettyPitch WithBlock where
    prettyPitch lang (WithBlock a) = "\\with {" <+> (prettyPitch lang a) <+> "}"

data NewItem = Voice | NewStaff Staff | Lyrics | NewItem String
    deriving (Eq, Show)

instance Pretty NewItem where
    pretty Voice            = "Voice"
    pretty (NewStaff staff) = pretty staff
    pretty Lyrics           = "Lyrics \\lyricsto"
    pretty (NewItem s)      = string s

data Context = Context (Maybe (String, (Maybe String))) MusicL
    deriving (Eq, Show)

instance PrettyPitch Context where
    prettyPitch lang (Context pref x) = "\\context" <+>
        (case pref of
            Nothing          -> string ""
            Just (typ, name) -> string typ <+> pretty name)
        <//> prettyPitch lang x

newtype Variable = Variable String
    deriving (Eq, Show)

instance Pretty Variable where
    pretty (Variable s) = "\\" <> string s

-- | A Lilypond music expression.
data MusicL
    = Note NotePitch (Maybe Duration) [Expressive]     -- ^ Single note.
    | Rest RestType (Maybe Duration) [Expressive]      -- ^ Single rest.
    | Chord [NotePitch] (Maybe Duration) [Expressive]  -- ^ Single chord.
    | Bar BarLine                                      -- ^ Bar line.
    | Clef Clef                                        -- ^ Clef.
    | Key Key                                          -- ^ Key signature.
    | Time TimeSig                                     -- ^ Time signature.
    | Tmp Tempo                                        -- ^ Tempo mark.
    | Sequential [MusicL]                              -- ^ Sequential composition.
    | Simultaneous Bool [MusicL]                       -- ^ Parallel composition (split voices?).
    | PartCombine MusicL MusicL                        -- ^ Combines two musical expressions into single staff
    | Repeat Bool Int MusicL (Maybe [MusicL])          -- ^ Repetition (unfold?, times, music, alternatives).
    | Tremolo Int MusicL                               -- ^ Tremolo (multiplier).
    | Times Rational MusicL                            -- ^ Stretch music (multiplier).
    | Tuplet Rational MusicL                           -- ^ Tuplet.
    | TupletSpan (Maybe Duration)                      -- \tupletSpan (duration)
    | Transpose Pitch Pitch MusicL                     -- ^ Transpose music (from to).
    | Relative (Maybe Pitch) MusicL                    -- ^ Use relative octave (octave).
    | Lit Literal                                      -- ^ Single literal.
    | Assign Assignment                                -- ^ Single assignment.
    | New NewItem (Maybe String) (Maybe WithBlock) MusicL  -- ^ \new (Voice/Staff/Lyrics, etc.)
    | Ctx Context                                      -- ^ Context expression.
    | Var Variable                                     -- ^ Variable occurrence.
    deriving (Eq, Show)

infixl <=>
a <=> b = sep [a,b]

fracPrinter :: Rational -> Printer
fracPrinter r = pretty (numerator r) <> "/" <> pretty (denominator r)

instance PrettyPitch MusicL where
    prettyPitch lang (Note p d exps) = prettyPitch lang p <> pretty d <> prettyList exps
    prettyPitch _  (Rest t d exps) = pretty t <> pretty d <> prettyList exps
    prettyPitch lang (Chord ns d exps) = (char '<' <+> nest 2 (sepByS "" $ prettyPitch lang <$> ns) <+> char '>') <> prettyList exps
    prettyPitch _ (Bar b) = pretty b
    prettyPitch _ (Clef c) = "\\clef" <+> pretty c
    prettyPitch lang (Key (p, m)) = "\\key" <+> prettyPitch lang p <+> pretty m
    prettyPitch _ (Time (m, n)) = "\\time" <+> (pretty m <> "/" <> pretty n)
    prettyPitch _ (Tmp tempo) = pretty tempo
    prettyPitch lang (Sequential xs) = "{" <+> (hsep . fmap (prettyPitch lang)) xs <+> "}"
    prettyPitch lang (Simultaneous b xs) = "<<" <//> nest 2 ((sepFunc . fmap (prettyPitch lang)) xs) <//> ">>"
        where sepFunc = if b then sepByS " \\\\" else vcat
    prettyPitch lang (PartCombine x1 x2) = "\\partcombine" <+> prettyPitch lang x1 <+> prettyPitch lang x2
    prettyPitch lang (Repeat unfold times x alts) = "\\repeat" <=> unf unfold <=> int times <=> prettyPitch lang x <=> alt alts
        where
            unf p = if p then "unfold" else "volta"
            alt Nothing      = empty
            alt (Just exps)  = "\\alternative" <> prettyPitchList lang exps
    prettyPitch lang (Tremolo n x) = "\\repeat tremolo" <+> pretty n <=> prettyPitch lang x
    prettyPitch lang (Times r x) = "\\times" <+> fracPrinter r <+> prettyPitch lang x
    prettyPitch lang (Tuplet r x) = "\\tuplet" <+> fracPrinter r <=> prettyPitch lang x
    prettyPitch _ (TupletSpan d) = "\\tupletSpan" <+> maybe "\\default" pretty d
    prettyPitch lang (Transpose from to x) = "\\transpose" <+> prettyPitch lang from <=> prettyPitch lang to <=> prettyPitch lang x
    prettyPitch lang (Relative p x) = "\\relative" <+> prettyPitch lang p <+> prettyPitch lang x
    prettyPitch _ (Lit lit) = pretty lit
    prettyPitch lang (Assign as) = prettyPitch lang as
    prettyPitch lang (New item name block x) = "\\new" <+> pretty item <+> maybe "" (\s -> maybeEq item <+> pretty s) name <+> pretty block <//> prettyPitch lang x
        where
            maybeEq Lyrics = ""
            maybeEq _      = "="
    prettyPitch lang (Ctx context) = prettyPitch lang context
    prettyPitch _ (Var v) = pretty v
    prettyPitchList lang = hsep . fmap (prettyPitch lang)

instance FromPitch MusicL where
    fromPitch p = Note (NotePitch p Nothing) (Just (1 / 4)) []

instance AdditiveGroup MusicL where
    zeroV   = Rest StdRest (Just $ 1 / 4) []
    a ^+^ b = Sequential [a, b]
    negateV = error "No Music.Lilypond.Music.negateV"

instance VectorSpace MusicL where
    type Scalar MusicL = Duration
    a *^ (Note n (Just d) p)   = Note n (Just $ a * d) p
    a *^ (Rest typ (Just d) p) = Rest typ (Just $ a * d) p
    a *^ (Chord ns (Just d) p) = Chord ns (Just $ a * d) p
    a *^ x                     = x

sequential :: MusicL -> MusicL -> MusicL
Sequential as `sequential` Sequential bs = Sequential (as <> bs)
Sequential as `sequential` b             = Sequential (as <> [b])
a `sequential` Sequential bs             = Sequential ([a] <> bs)
a `sequential` b                         = Sequential ([a,b])

simultaneous :: MusicL -> MusicL -> MusicL
Simultaneous s as `simultaneous` Simultaneous t bs = Simultaneous True (as <> bs)
Simultaneous s as `simultaneous` b                 = Simultaneous s (as <> [b])
a `simultaneous` Simultaneous t bs                 = Simultaneous t ([a] <> bs)
a `simultaneous` b                                 = Simultaneous True ([a,b])
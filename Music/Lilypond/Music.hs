{-# LANGUAGE
     GeneralizedNewtypeDeriving,
     OverloadedStrings,
     TypeFamilies
     #-}

module Music.Lilypond.Music where

import Control.Arrow ((***), (<<<), second)
import Data.Char (isAlpha, toLower)
import Data.Default (Default(..))
import Data.Ratio ((%), denominator, numerator)
import Data.String (IsString(..))
import Data.VectorSpace ((*^), AdditiveGroup(..), Scalar, VectorSpace)
import Text.Pretty (Pretty(..), Printer, (<+>), (<//>), char, empty, hcat, hsep, int, nest, sep, sepByS, string, vcat)

import Euterpea (Mode(..), Pitch)
import Music.Dynamics (DynamicMotion, Dynamics)
import Music.Pitch (FromPitch(..), ToPitch(..), Key)
import Music.Rhythm (TimeSig)


-- Utilities

notImpl :: String -> a
notImpl a = error $ "Not implemented: " ++ a

-- Types

-- * Identifier

newtype Identifier = Identifier { getIdentifier :: String } -- \foo
    deriving (Eq, Show)

instance Pretty Identifier where
    pretty (Identifier s) = char '\\' <> string s

-- * Markup

data Markup
    = MarkupText String
    | MarkupList [Markup]
    | Bold Markup
    | Box Markup
    | Caps Markup
    | DynamicsFont Markup
    | FingeringFont Markup
    | Fontsize Double Markup
    | Huge Markup
    | Italic Markup
    | Large Markup
    | Larger Markup
    | Magnify Double Markup
    | Medium Markup
    | Roman Markup
    | Sans Markup
    | Sub Markup
    | Super Markup
    | TextFont Markup
    | Tiny Markup
    | TypewriterFont Markup
    | Upright Markup
    deriving (Eq, Show)

class HasMarkup a where
    markup :: a -> Markup

instance HasMarkup Markup where
    markup = id

instance HasMarkup a => HasMarkup [a] where
    markup = MarkupList . fmap markup

instance IsString Markup where
    fromString = MarkupText

instance Pretty Markup where
    pretty (MarkupText s)       = (string . show) s
    pretty (MarkupList as)      = "{" <+> hsep (fmap pretty as) <+> "}"
    pretty (Bold a)             = "\\bold" <+> pretty a
    pretty (Box a)              = "\\box" <+> pretty a
    pretty (Caps a)             = "\\caps" <+> pretty a
    pretty (DynamicsFont a)     = "\\dynamics" <+> pretty a
    pretty (FingeringFont a)    = "\\fingering" <+> pretty a
    pretty (Fontsize n a)       = "\\fontsize" <+> ("#" <> pretty n) <+> pretty a
    pretty (Huge a)             = "\\huge" <+> pretty a
    pretty (Italic a)           = "\\italic" <+> pretty a
    pretty (Large a)            = "\\large" <+> pretty a
    pretty (Larger a)           = "\\larger" <+> pretty a
    pretty (Magnify n a)        = "\\magnify" <+> ("#" <> pretty n) <+> pretty a
    pretty (Medium a)           = "\\medium" <+> pretty a
    pretty (Roman a)            = "\\roman" <+> pretty a
    pretty (Sans a)             = "\\sans" <+> pretty a
    pretty (Sub a)              = "\\sub" <+> pretty a
    pretty (Super a)            = "\\super" <+> pretty a
    pretty (TextFont a)         = "\\text" <+> pretty a
    pretty (Tiny a)             = "\\tiny" <+> pretty a
    pretty (TypewriterFont a)   = "\\typewriter" <+> pretty a
    pretty (Upright a)          = "\\upright" <+> pretty a

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

instance Pretty NotePitch where
    pretty (NotePitch p Nothing) = pretty p
    pretty (NotePitch p _)       = string $ p' ++ "=" ++ oct
        where (p', oct) = span isAlpha $ show $ pretty p
    -- TODO: implement
    pretty DrumNotePitch         = notImpl "Non-standard pitch"
    prettyList                   = hsep . fmap pretty

instance ToPitch NotePitch where
    toPitch (NotePitch p _) = p
    toPitch _               = notImpl "Non-standard pitch"

instance FromPitch NotePitch where
    fromPitch = (`NotePitch` Nothing)

-- | Notated time in fractions, in @[2^^i | i <- [-10..3]]@.
newtype Duration = Duration Rational
    deriving (Eq, Ord, Num, Enum, Fractional, Real, RealFrac, Show)

-- base duration and number of dots
type DottedDuration = (Duration, Int)

intLog2 :: (Integral a, Integral b) => a -> b
intLog2 = truncate . logBase 2 . fromIntegral

-- | Separate a duration into a sequence of undotted durations with some number of dots
separateDots :: Duration -> [DottedDuration]
separateDots (Duration r)
    | r == 0       = []
    | r >= 16      = (Duration 8, 0) : separateDots (Duration r - 8)
    | diff == 0    = [(Duration r, 0)]
    | n' == d' - 1 = [(Duration b, intLog2 d')]
    | otherwise    = (Duration b, 0) : separateDots (Duration diff)
    where
        (n, d) = (numerator r, denominator r)
        b = (2 ^ (intLog2 $ fromInteger n)) % d
        diff = r - b
        q = diff / b
        (n', d') = (numerator q, denominator q)

showDottedDuration :: DottedDuration -> String
showDottedDuration (Duration r, n) = pnv r ++ pds n
    where
        pnv 8 = "\\maxima"
        pnv 4 = "\\longa"
        pnv 2 = "\\breve"
        pnv r' = show $ denominator r'
        pds n = concat $ replicate n "."

instance Pretty Duration where
    pretty d = case (separateDots d) of
        [dd] -> string $ showDottedDuration dd
        _    -> error "invalid duration"

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

data Literal = FloatL Double -- #0.4
            | IntL Int -- #t
            | BoolL Bool -- ##t / ##f
            | StringL String -- etc.
            | SymbolL String -- etc.
            | SexpL String -- etc.
    deriving (Eq, Show)

instance Pretty Literal where
  pretty (FloatL d) = string $ "#" ++ show d
  pretty (IntL d) = string $ "#" ++ show d
  pretty (BoolL True) = "##t"
  pretty (BoolL False) = "##f"
  pretty (StringL d) = string $ "#\"" ++ d ++ "\""
  pretty (SymbolL d) = string $ "#\'" ++ d
  pretty (SexpL d) = string $ "#(" ++ d ++ ")"

data Assignment = Assignment String MusicL  -- foo = {a b c}
            | SymbAssignment String Literal MusicL  -- foo #'bar = baz
            | Set Assignment -- \set ...
            | Override Assignment  -- \override ...
            | Once Assignment  -- \once ...
            | Revert String  -- \revert ...
    deriving (Eq, Show)

instance Pretty Assignment where
    pretty (Assignment s e) = string s <+> "=" <+> pretty e
    pretty (SymbAssignment s l e) = string s <+> pretty l <+> "=" <+> pretty e
    pretty (Set a) = "\\set" <+> pretty a
    pretty (Override a) = "\\override" <+> pretty a
    pretty (Once a) = "\\once" <+> pretty a
    pretty (Revert s) = "\\revert" <+> pretty s

-- | A Lilypond music expression.
data MusicL
    = Note NotePitch (Maybe Duration) [Expressive]     -- ^ Single note.
    | Rest RestType (Maybe Duration)                   -- ^ Single rest.
    | Chord [NotePitch] (Maybe Duration) [Expressive]  -- ^ Single chord.
    | Bar BarLine                                      -- ^ Bar line.
    | Clef Clef                                        -- ^ Clef.
    | Key Key                                          -- ^ Key signature.
    | Time TimeSig                                     -- ^ Time signature.
    | Tempo (Maybe String) (Maybe (Duration, Integer)) -- ^ Tempo mark.
    | Sequential [MusicL]                              -- ^ Sequential composition.
    | Simultaneous Bool [MusicL]                       -- ^ Parallel composition (split voices?).
    | Repeat Bool Int MusicL (Maybe [MusicL])          -- ^ Repetition (unfold?, times, music, alternatives).
    | Tremolo Int MusicL                               -- ^ Tremolo (multiplier).
    | Times Rational MusicL                            -- ^ Stretch music (multiplier).
    | Tuplet Rational MusicL                           -- ^ Tuplet.
    | Transpose Pitch Pitch MusicL                     -- ^ Transpose music (from to).
    | Relative (Maybe Pitch) MusicL                    -- ^ Use relative octave (octave).
    | Lit Literal                                      -- ^ Single literal.
    | Assign Assignment                                -- ^ Single assignment.
    | With Assignment                                  -- ^ \with { alignAboveContext = #"main" }
    | Voice (Maybe String) MusicL                      -- ^ \new Voice
    | Staff (Maybe String) MusicL                      -- ^ \new Staff
    | Lyrics String MusicL                             -- ^ \new Lyrics \lyricsto "foo" { bar }
    | New String (Maybe String) MusicL                 -- ^ New expression.
    | Context String (Maybe String) MusicL             -- ^ Context expression.
    deriving (Eq, Show)

infixl <=>
a <=> b = sep [a,b]

-- given a printer for something with a duration, converts it into a new printer, where elements may need to split into multiple elements to allow for printable durations
durationPrinter :: Printer -> Printer -> Maybe Duration -> Printer
durationPrinter p sep Nothing  = p
durationPrinter p sep (Just d) = go (separateDots d)
    where
        go [dd] = p <> (string $ showDottedDuration dd)
        go dds  = "{" <=> (sepByS sep $ go . pure <$> dds) <=> "}"

fracPrinter :: Rational -> Printer
fracPrinter r = pretty (numerator r) <> "/" <> pretty (denominator r)

instance Pretty MusicL where
    pretty (Note p d exps) = pretty p <> pretty d <> prettyList exps
    pretty (Rest t d) = pretty t <> pretty d
    pretty (Chord ns d exps) = durationPrinter printer "~" d <> prettyList exps
        where printer = char '<' <+> nest 4 (sepByS "" $ pretty <$> ns) <+> char '>'
    pretty (Bar b) = pretty b
    pretty (Clef c) = "\\clef" <+> pretty c
    pretty (Key (p, m)) = "\\key" <+> pretty p <+> pretty m
    pretty (Time (m, n)) = "\\time" <+> (pretty m <> "/" <> pretty n)
    pretty (Tempo Nothing Nothing)           = mempty
    pretty (Tempo (Just t) Nothing)          = "\\tempo" <+> pretty t
    pretty (Tempo Nothing (Just (d, bpm)))   = "\\tempo" <+> pretty d <+> "=" <+> pretty bpm
    pretty (Tempo (Just t) (Just (d, bpm)))  = "\\tempo" <+> pretty t <+> pretty d <+> "=" <+> pretty bpm
    pretty (Sequential xs)  = "{" <=> nest 4 ((hsep . fmap pretty) xs) <=> "}"
    pretty (Simultaneous b xs) = "<<" <//> nest 4 ((sepFunc . fmap pretty) xs) <//> ">>"
        where sepFunc = if b then sepByS " \\\\" else vcat
    pretty (Repeat unfold times x alts) = "\\repeat" <=> unf unfold <=> int times <=> pretty x <=> alt alts
        where
            unf p = if p then "unfold" else "volta"
            alt Nothing      = empty
            alt (Just exps)  = "\\alternative" <> prettyList exps
    pretty (Tremolo n x) = "\\repeat tremolo" <+> pretty n <=> pretty x
    pretty (Times r x) = "\\times" <+> fracPrinter r <=> pretty x
    pretty (Tuplet r x) = "\\tuplet" <+> fracPrinter r <=> pretty x
    pretty (Transpose from to x) = "\\transpose" <+> pretty from <=> pretty to <=> pretty x
    pretty (Relative p x) = "\\relative" <=> pretty p <=> pretty x
    pretty (Lit lit) = pretty lit
    pretty (Assign as) = pretty as
    pretty (With a) = "\\with {" <+> (pretty a) <+> "}"
    pretty (Voice name x) = "\\new Voice" <+> pretty name <//> pretty x
    pretty (Staff name x) = "\\new Staff" <+> pretty name <//> pretty x
    pretty (Lyrics name x) = "\\new Lyrics \\lyricsto" <+> pretty name <//> pretty x
    pretty (New typ name x) = "\\new" <+> string typ <+> pretty name <//> pretty x
    pretty (Context typ name x) = "\\context" <+> string typ <+> pretty name <//> pretty x
    prettyList = hsep . fmap pretty

instance FromPitch MusicL where
    fromPitch p = Note (NotePitch p Nothing) (Just (1 / 4)) []

instance AdditiveGroup MusicL where
    zeroV   = Rest StdRest (Just $ 1 / 4)
    a ^+^ b = Sequential [a, b]
    negateV = error "No Music.Lilypond.Music.negateV"

instance VectorSpace MusicL where
    type Scalar MusicL = Duration
    a *^ (Note n (Just d) p)   = Note n (Just $ a * d) p
    a *^ (Rest typ (Just d))   = Rest typ (Just $ a * d)
    a *^ (Chord ns (Just d) p) = Chord ns (Just $ a * d) p
    a *^ x                     = x

-- TODO: generic fold (have func return Maybe, then apply default folding behavior when Nothing?)

-- -- | Construct a rest of default duration @1/4@.
-- --
-- --   Use the 'VectorSpace' methods to change duration.
-- --
-- rest :: MusicL
-- rest = Rest StdRest (Just $ 1 / 4)

-- -- | Construct a note of default duration @1/4@.
-- --
-- --   Use the 'VectorSpace' methods to change duration.
-- --
-- note :: Note -> Music
-- note n = Note n (Just $ 1/4) []

-- -- | Construct a chord of default duration @1/4@.
-- --
-- --   Use the 'VectorSpace' methods to change duration.
-- --
-- chord :: [Note] -> Music
-- chord ns = Chord (fmap (, []) ns) (Just $ 1/4) []


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


-- addPost :: PostEvent -> Music -> Music
-- addPost a = foldMusic' (addPost' a) id (addPost a)
--   where
--     addPost' a (Rest d es)     = Rest d (es ++ [a])
--     addPost' a (Note n d es)   = Note n d (es ++ [a])
--     addPost' a (Chord ns d es) = Chord ns d (es ++ [a])
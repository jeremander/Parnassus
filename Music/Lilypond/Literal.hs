{-# LANGUAGE OverloadedStrings #-}

module Music.Lilypond.Literal where

import Data.Char (toLower)
import Data.String (IsString(..))
import Text.Pretty


-- Utilities

string' :: String -> Printer
string' s = char '#' <> string s

prettySeq :: (Pretty a) => [a] -> Printer
prettySeq xs = "{" <+> hsep (pretty <$> xs) <+> "}"

-- Types

newtype Identifier = Identifier { getIdentifier :: String }  -- \foo
    deriving (Eq, Ord, Show)

instance Pretty Identifier where
    pretty (Identifier s) = char '\\' <> string s

newtype Symbol = Symbol { getSymbol :: String }  -- #'something
    deriving (Eq, Ord, Show)

instance Pretty Symbol where
    pretty (Symbol s) = case words s of
        []  -> ""
        [w] -> string "#'" <> string w
        _   -> string "#'(" <> string s <> char ')'

data Axis = X | Y
    deriving (Enum, Eq, Ord, Show)

instance Pretty Axis where
    pretty ax = char '#' <> string (show ax)

data AxisDir = AxisDir {
    axis :: Axis,
    dir :: Int
} deriving (Eq, Ord, Show)

instance Pretty AxisDir where
    pretty axdir@(AxisDir ax dir) = pretty ax <+> (char '#' <> string dir')
        where
            dir' = case (ax, dir) of
                        (_, 0)  -> "CENTER"
                        (X, -1) -> "LEFT"
                        (X, 1)  -> "RIGHT"
                        (Y, -1) -> "DOWN"
                        (Y, 1)  -> "UP"
                        _       -> error "invalid AxisDir"

data NumPair = NumPair Double Double
    deriving (Eq, Ord, Show)

instance Pretty NumPair where
    pretty (NumPair x y) = string $ "#'(" ++ show x ++ " . " ++ show y ++ ")"

data Font
    = AbsFontsize Double Markup
    | Bold Markup
    | Box Markup
    | Caps Markup
    | Dynamic Markup
    | Finger Markup
    | FontCaps Markup
    | Fontsize Double Markup
    | Huge Markup
    | Italic Markup
    | Large Markup
    | Larger Markup
    | Magnify Double Markup
    | Medium Markup
    | NormalSizeSub Markup
    | NormalSizeSuper Markup
    | NormalText Markup
    | Normalsize Markup
    | Number Markup
    | Replace Symbol Markup
    | Roman Markup
    | Sans Markup
    | Simple Markup
    | Small Markup
    | SmallCaps Markup
    | Smaller Markup
    | Sub Markup
    | Super Markup
    | Teeny Markup
    | TextFont Markup
    | Tiny Markup
    | Typewriter Markup
    | Underline Markup
    | Upright Markup
    deriving (Eq, Ord, Show)

instance Pretty Font where
    pretty (AbsFontsize sz e)   = "\\abs-fontsize" <+> pretty (FloatL sz) <+> pretty e
    pretty (Bold e)             = "\\bold" <+> pretty e
    pretty (Box e)              = "\\box" <+> pretty e
    pretty (Caps e)             = "\\caps" <+> pretty e
    pretty (Dynamic e)          = "\\dynamic" <+> pretty e
    pretty (Finger e)           = "\\finger" <+> pretty e
    pretty (FontCaps e)         = "\\fontCaps" <+> pretty e
    pretty (Fontsize inc e)     = "\\fontsize" <+> pretty (FloatL inc) <+> pretty e
    pretty (Huge e)             = "\\huge" <+> pretty e
    pretty (Italic e)           = "\\italic" <+> pretty e
    pretty (Large e)            = "\\large" <+> pretty e
    pretty (Larger e)           = "\\larger" <+> pretty e
    pretty (Magnify sz e)       = "\\magnify" <+> pretty (FloatL sz) <+> pretty e
    pretty (Medium e)           = "\\medium" <+> pretty e
    pretty (NormalSizeSub e)    = "\\normal-size-sub" <+> pretty e
    pretty (NormalSizeSuper e)  = "\\normal-size-super" <+> pretty e
    pretty (NormalText e)       = "\\normal-text" <+> pretty e
    pretty (Normalsize e)       = "\\normalsize" <+> pretty e
    pretty (Number e)           = "\\number" <+> pretty e
    pretty (Replace repls e)    = "\\replace" <+> pretty repls <+> pretty e
    pretty (Roman e)            = "\\roman" <+> pretty e
    pretty (Sans e)             = "\\sans" <+> pretty e
    pretty (Simple e)           = "\\simple" <+> pretty e
    pretty (Small e)            = "\\small" <+> pretty e
    pretty (SmallCaps e)        = "\\smallCaps" <+> pretty e
    pretty (Smaller e)          = "\\smaller" <+> pretty e
    pretty (Sub e)              = "\\sub" <+> pretty e
    pretty (Super e)            = "\\super" <+> pretty e
    pretty (Teeny e)            = "\\teeny" <+> pretty e
    pretty (TextFont e)         = "\\text" <+> pretty e
    pretty (Tiny e)             = "\\tiny" <+> pretty e
    pretty (Typewriter e)       = "\\typewriter" <+> pretty e
    pretty (Underline e)        = "\\underline" <+> pretty e
    pretty (Upright e)          = "\\upright" <+> pretty e

data Alignment
    = CenterAlign Markup
    | CenterColumn [Markup]
    | Column [Markup]
    | Combine MarkupExpr MarkupExpr
    | Concat [Markup]
    | DirColumn [Markup]
    | FillLine [Markup]
    | FillWithPattern Double Int MarkupExpr MarkupExpr MarkupExpr
    | GeneralAlign AxisDir Markup
    | Halign Int Markup
    | HcenterIn Double Markup
    | Hspace Double
    | JustifyField Symbol
    | Justify [Markup]
    | JustifyString String
    | LeftAlign Markup
    | LeftColumn [Markup]
    | Line [Markup]
    | Lower Double Markup
    | PadAround Double Markup
    | PadMarkup Double Markup
    | PadToBox NumPair NumPair Markup
    | PadX Double Markup
    | PutAdjacent AxisDir MarkupExpr MarkupExpr
    | Raise Double Markup
    | RightAlign Markup
    | RightColumn [Markup]
    | Rotate Double Markup
    | Translate NumPair Markup
    | TranslateScaled NumPair Markup
    | Vcenter Markup
    | Vspace Double
    | WordwrapField Symbol
    | Wordwrap [Markup]
    | WordwrapString String
    deriving (Eq, Ord, Show)

instance Pretty Alignment where
    pretty (CenterAlign e)            = "\\center-align" <+> pretty e
    pretty (CenterColumn es)          = "\\center-column" <+> prettySeq es
    pretty (Column es)                = "\\column" <+> prettySeq es
    pretty (Combine e1 e2)            = "\\combine" <+> pretty e1 <+> pretty e2
    pretty (Concat es)                = "\\dir-column" <+> prettySeq es
    pretty (DirColumn es)             = "\\dir-column" <+> prettySeq es
    pretty (FillLine es)              = "\\fill-line" <+> prettySeq es
    pretty (FillWithPattern sp dir pat left right) = "\\fill-with-pattern" <+> pretty (FloatL sp) <+> pretty (IntL dir) <+> pretty pat <+> pretty left <+> pretty right
    pretty (GeneralAlign axdir e)     = "\\general-align" <+> pretty axdir <+> pretty e
    pretty (Halign dir e)             = "\\halign" <+> pretty (IntL dir) <+> pretty e
    pretty (HcenterIn length e)       = "\\hcenter-in" <+> pretty (FloatL length) <+> pretty e
    pretty (Hspace amt)               = "\\hspace" <+> pretty (FloatL amt)
    pretty (JustifyField sym)         = "\\justify-field" <+> pretty sym
    pretty (Justify es)               = "\\justify" <+> prettySeq es
    pretty (JustifyString s)          = "\\justify-string" <+> pretty (StringL s)
    pretty (LeftAlign e)              = "\\left-align" <+> pretty e
    pretty (LeftColumn es)            = "\\left-column" <+> prettySeq es
    pretty (Line es)                  = "\\line" <+> prettySeq es
    pretty (Lower amt e)              = "\\lower" <+> pretty (FloatL amt) <+> pretty e
    pretty (PadAround amt e)          = "\\pad-around" <+> pretty (FloatL amt) <+> pretty e
    pretty (PadMarkup amt e)          = "\\pad-markup" <+> pretty (FloatL amt) <+> pretty e
    pretty (PadToBox xext yext e)     = "\\pad-to-box" <+> pretty xext <+> pretty yext <+> pretty e
    pretty (PadX amt e)               = "\\pad-x" <+> pretty (FloatL amt) <+> pretty e
    pretty (PutAdjacent axdir e1 e2)  = "\\put-adjacent" <+> pretty axdir <+> pretty e1 <+> pretty e2
    pretty (Raise amt e)              = "\\raise" <+> pretty (FloatL amt) <+> pretty e
    pretty (RightAlign e)             = "\\right-align" <+> pretty e
    pretty (RightColumn es)           = "\\right-column" <+> prettySeq es
    pretty (Rotate amt e)             = "\\rotate" <+> pretty (FloatL amt) <+> pretty e
    pretty (Translate offset e)       = "\\translate" <+> pretty offset <+> pretty e
    pretty (TranslateScaled offset e) = "\\translate-scaled" <+> pretty offset <+> pretty e
    pretty (Vcenter e)                = "\\vcenter" <+> pretty e
    pretty (Vspace amt)               = "\\vspace" <+> pretty amt
    pretty (WordwrapField sym)        = "\\wordwrap-field" <+> pretty sym
    pretty (Wordwrap es)              = "\\wordwrap" <+> prettySeq es
    pretty (WordwrapString s)         = "\\wordwrap-string" <+> pretty (StringL s)

data Graphic
    = ArrowHead AxisDir Bool
    | BeamG Double Double Double
    | Bracket Markup
    | Circle Markup
    | DrawCircle Double Double Bool
    | DrawDashedLine NumPair
    | DrawDottedLine NumPair
    | DrawHline
    | DrawLine NumPair
    | Ellipse Markup
    | Epsfile Axis Double FilePath
    | FilledBox NumPair NumPair Double
    | Hbracket Markup
    | Oval Markup
    | Parenthesize Markup
    | Path Double [String]  -- TODO: exact path commands
    | Postscript String
    | RoundedBox Markup
    | Scale NumPair Markup
    | Triangle Bool
    | WithUrl String Markup
    deriving (Eq, Ord, Show)

instance Pretty Graphic where
    pretty (ArrowHead axdir fill)     = "\\arrow-head" <+> pretty axdir <+> pretty (BoolL fill)
    pretty (BeamG width slope thick)  = "\\beam" <+> hsep (pretty . FloatL <$> [width, slope, thick])
    pretty (Bracket e)                = "\\bracket" <+> pretty e
    pretty (Circle e)                 = "\\circle" <+> pretty e
    pretty (DrawCircle r thick fill)  = "\\draw-circle" <+> hsep (pretty <$> [FloatL r, FloatL thick, BoolL fill])
    pretty (DrawDashedLine pair)      = "\\draw-dashed-line" <+> pretty pair
    pretty (DrawDottedLine pair)      = "\\draw-dotted-line" <+> pretty pair
    pretty DrawHline                  = "\\draw-hline"
    pretty (DrawLine pair)            = "\\draw-line" <+> pretty pair
    pretty (Ellipse e)                = "\\ellipse" <+> pretty e
    pretty (Epsfile ax size path)     = "\\epsfile" <+> pretty ax <+> (pretty $ FloatL size) <+> string path
    pretty (FilledBox xext yext blot) = "\\filled-box" <+> pretty xext <+> pretty yext <+> (pretty $ FloatL blot)
    pretty (Hbracket e)               = "\\hbracket" <+> pretty e
    pretty (Oval e)                   = "\\oval" <+> pretty e
    pretty (Parenthesize e)           = "\\parenthesize" <+> pretty e
    pretty (Path thick cmds)          = "\\path" <+> (pretty $ FloatL thick) <+> (hsep $ string <$> cmds)
    pretty (Postscript s)             = "\\postscript" <+> (pretty $ StringL s)
    pretty (RoundedBox e)             = "\\rounded-box" <+> pretty e
    pretty (Scale factors e)          = "\\scale" <+> pretty factors <+> pretty e
    pretty (Triangle fill)            = "\\triangle" <+> (pretty $ BoolL fill)
    pretty (WithUrl url e)            = "\\with-url" <+> (pretty $ StringL url) <+> pretty e

-- * Tweaks (modifying properties)

data Tweak =
      Override String Literal
    | OverrideSym Symbol
    | OverrideSym' String Symbol Literal
    | Revert String
    | Tweak String Literal
    | TweakSym Symbol
    deriving (Eq, Ord, Show)

instance Pretty Tweak where
    pretty (Override s val) = "\\override" <+> string s <+> char '=' <+> pretty val
    pretty (OverrideSym s)  = "\\override" <+> pretty s
    pretty (OverrideSym' s sym val) = "\\override" <+> string s <+> pretty sym <+> char '=' <+> pretty val
    pretty (Revert s)       = "\\revert" <+> string s
    pretty (Tweak s val)    = "\\tweak" <+> string s <+> char '=' <+> pretty val
    pretty (TweakSym s)     = "\\tweak" <+> pretty s

data MarkupExpr
    = MarkupAlign Alignment
    | MarkupGraphic Graphic
    | MarkupFont Font
    | MarkupQuote String
    | MarkupText String
    | MarkupTweak Tweak
    | MarkupVar LitAssignment
    deriving (Eq, Ord, Show)

instance Pretty MarkupExpr where
    pretty (MarkupAlign a)   = pretty a
    pretty (MarkupFont f)    = pretty f
    pretty (MarkupGraphic g) = pretty g
    pretty (MarkupQuote s)   = (string . show) s
    pretty (MarkupText s)    = string s
    pretty (MarkupTweak t)   = pretty t
    pretty (MarkupVar (LitAssignment name _)) = char '\\' <> string name

data Markup
    = MarkupExpr MarkupExpr
    | MarkupList [Markup]
    deriving (Eq, Ord, Show)

instance Pretty Markup where
    pretty (MarkupExpr expr)    = pretty expr
    pretty (MarkupList markups) = prettySeq markups

class HasMarkup a where
    markup :: a -> Markup

instance HasMarkup Markup where
    markup = id

instance HasMarkup a => HasMarkup [a] where
    markup = MarkupList . fmap markup

instance IsString MarkupExpr where
    fromString = MarkupText

instance IsString Markup where
    fromString = MarkupExpr . MarkupText

data MeasureUnit = MM | CM | IN | PT
    deriving (Bounded, Enum, Eq, Ord, Show)

instance Pretty MeasureUnit where
    pretty = string . ('\\' :) . fmap toLower . show

data Literal =
              FloatL Double -- #0.4
            | IntL Int -- #3
            | MeasureL Double MeasureUnit -- 3\cm
            | BoolL Bool -- ##t / ##f
            | StringL String -- etc.
            | SymbolL Symbol -- etc.
            | SexpL String -- Scheme expression (just stores it as a flat string)
            | MarkupL Markup  -- \markup { }
    deriving (Eq, Ord, Show)

instance Pretty Literal where
    pretty (FloatL d) = string $ "#" ++ show d
    pretty (IntL d) = string $ "#" ++ show d
    pretty (MeasureL d u) = string (show d ++ "\\") <> pretty u
    pretty (BoolL True) = "##t"
    pretty (BoolL False) = "##f"
    pretty (StringL d) = string $ "#\"" ++ d ++ "\""
    pretty (SymbolL d) = pretty d
    pretty (SexpL d) = string $ "#(" ++ d ++ ")"
    pretty (MarkupL m) = "\\markup" <+> (char '{' <+> pretty m <+> char '}')

data LitAssignment = LitAssignment String Literal
    deriving (Eq, Ord, Show)

instance Pretty LitAssignment where
    pretty (LitAssignment name val) = string name <+> "=" <+> pretty val
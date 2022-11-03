{-# LANGUAGE
    FlexibleContexts,
    FlexibleInstances,
    InstanceSigs,
    OverloadedStrings,
    UndecidableInstances
    #-}

module Music.Lilypond.Parse where

import Control.Monad (ap, void)
import Control.Monad.Trans (lift)
import Data.Char (isAlphaNum, isAscii, isSpace, toLower, toUpper)
import Data.Default (Default(..))
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Data.Ratio ((%))
import Data.Sort (sortOn)
import Data.Tuple (swap)
import Euterpea (Mode(..), Pitch, PitchClass(..))
import System.Directory (findFile)
import Text.Parsec
import Text.Parsec.Number (floating2, int, sign)
import Text.Parsec.Pos (initialPos)
import Text.Pretty (Pretty(..))

import Misc.Utils (enumerate)
import Music.Dynamics
import Music.Pitch
import Music.Rhythm
import Music.Lilypond.Literal
import Music.Lilypond.MusicL
import Music.Lilypond.Score
import Music.Lilypond.Symbols


data LilypondState = LilypondState {
    includePaths :: [FilePath],
    language :: Language,
    hdrAssignments :: M.Map String Literal
} deriving (Eq, Show)

-- Mac OS specific include path
defaultLilypondState :: LilypondState
defaultLilypondState = LilypondState {
    includePaths = ["/Applications/LilyPond.app/Contents/Resources/share/lilypond/current/ly"],
    language = def,
    hdrAssignments = M.empty
}

-- consumes spaces or comments
sorc :: (Stream s m Char) => ParsecT s u m ()
sorc = skipMany $ comment <|> space'
    where
        space' = void $ satisfy isSpace
        comment = string "%" >> manyTill anyChar newline >> return ()

bracket' :: (Stream s m Char) => Char -> Char -> ParsecT s u m a -> ParsecT s u m a
bracket' c1 c2 = between (char c1 <* sorc) (sorc *> char c2)

braces' :: (Stream s m Char) => ParsecT s u m a -> ParsecT s u m a
braces' = bracket' '{' '}'

parseBool :: (Stream s m Char) => ParsecT s u m Bool
parseBool = try (True <$ string "#t") <|> (False <$ string "#f")

parseFloat :: (Stream s m Char) => ParsecT s u m Double
parseFloat = char '#' *> ap sign (floating2 False)

parseInt :: (Stream s m Char) => ParsecT s u m Int
parseInt = char '#' *> int

escape :: (Stream s m Char) => ParsecT s u m String
escape = do
    d <- char '\\'
    c <- oneOf "\\\"0nrvtbf" -- all the characters which can be escaped
    return [d, c]

nonEscape :: (Stream s m Char) => ParsecT s u m Char
nonEscape = noneOf "\\\"\0\n\r\v\t\b\f"

character :: (Stream s m Char) => ParsecT s u m String
character = fmap return nonEscape <|> escape

parseString :: (Stream s m Char) => ParsecT s u m String
parseString = do
    char '"'
    strings <- many character
    char '"'
    return $ concat strings

parseString' :: (Stream s m Char) => ParsecT s u m String
parseString' = char '#' *> parseString

parens' :: (Stream s m Char) => ParsecT s u m a -> ParsecT s u m a
parens'  = between (char '(' <* spaces) (spaces *> char ')')

-- parses an arbitrary string prefixed by a backslash, wrapping the result in an Identifier
parseIdent :: (Stream s m Char) => ParsecT s u m String -> ParsecT s u m Identifier
parseIdent p = Identifier <$> ((char '\\') *> p)

-- parses a specific string prefixed by a backslash, wrapping the result in an Identifier
parseIdent' :: (Stream s m Char) => String -> ParsecT s u m Identifier
parseIdent' = parseIdent . string

validIdentifiers = ['a'..'z'] ++ ['A'..'Z'] ++ ",.;:-+/"

parseToken :: (Stream s m Char) => ParsecT s u m String
parseToken = many1 $ oneOf validIdentifiers <|> satisfy (not . isAscii)

-- parses an arbitrary identifier
parseIdentifier :: (Stream s m Char) => ParsecT s u m Identifier
parseIdentifier = parseIdent parseToken

parseSymbol :: (Stream s m Char) => ParsecT s u m Symbol
parseSymbol = fmap Symbol $ string "#'" *> ((parens' $ manyTill anyChar (lookAhead $ char ')')) <|> parseToken)

parseAxis :: (Stream s m Char) => ParsecT s u m Axis
parseAxis = char '#' *> ((X <$ char 'X') <|> (Y <$ char 'Y'))

parseAxisDir :: (Stream s m Char) => ParsecT s u m AxisDir
parseAxisDir = do
    ax <- parseAxis
    dir <- sorc *> char '#' *>
            ((0 <$ string "CENTER") <|>
            case ax of
                X -> (-1 <$ string "LEFT") <|> (1 <$ string "RIGHT")
                Y -> (-1 <$ string "DOWN") <|> (1 <$ string "UP"))
    return $ AxisDir ax dir

parseNumPair :: (Stream s m Char) => ParsecT s u m NumPair
parseNumPair = string "#'(" *> sorc *> (NumPair <$> floating2 False <*> (sorc *> char '.' *> sorc *> floating2 False)) <* sorc <* char ')'

parseFont :: (Stream s m Char) => ParsecT s LilypondState m Font
parseFont =
        fontParser2 AbsFontsize "abs-fontsize"
    <|> fontParser Bold "bold"
    <|> fontParser Box "box"
    <|> fontParser Caps "caps"
    <|> fontParser Dynamic "dynamic"
    <|> fontParser Finger "finger"
    <|> fontParser FontCaps "fontCaps"
    <|> fontParser2 Fontsize "fontsize"
    <|> fontParser Huge "huge"
    <|> fontParser Italic "italic"
    <|> fontParser Large "large"
    <|> fontParser Larger "larger"
    <|> fontParser2 Magnify "magnify"
    <|> fontParser Medium "medium"
    <|> fontParser NormalSizeSub "normal-size-sub"
    <|> fontParser NormalSizeSuper "normal-size-super"
    <|> fontParser NormalText "normal-text"
    <|> fontParser Normalsize "normalsize"
    <|> fontParser Number "number"
    <|> fontParser3 Replace "replace"
    <|> fontParser Roman "roman"
    <|> fontParser Sans "sans"
    <|> fontParser Simple "simple"
    <|> fontParser Small "small"
    <|> fontParser SmallCaps "smallCaps"
    <|> fontParser Smaller "smaller"
    <|> fontParser Sub "sub"
    <|> fontParser Super "super"
    <|> fontParser Teeny "teeny"
    <|> fontParser TextFont "text"
    <|> fontParser Tiny "tiny"
    <|> fontParser Typewriter "typewriter"
    <|> fontParser Underline "underline"
    <|> fontParser Upright "upright"
    where
        fontParser con s = try $ con <$> (parseIdent' s *> sorc *> parseMarkup)
        fontParser2 con s = try $ con <$> (parseIdent' s *> sorc *> floating2 False) <*> (sorc *> parseMarkup)
        fontParser3 con s = try $ con <$> (parseIdent' s *> sorc *> parseSymbol) <*> (sorc *> parseMarkup)

parseAlignment :: (Stream s m Char) => ParsecT s LilypondState m Alignment
parseAlignment =
        alignParser1 CenterAlign "center-align"
    <|> alignParser2 CenterColumn "center-column"
    <|> alignParser2 Column "column"
    <|> try (Combine <$> (parseIdent' "combine" *> sorc *> parseMarkupExpr) <*> (sorc *> parseMarkupExpr))
    <|> alignParser2 Concat "concat"
    <|> alignParser2 DirColumn "dir-column"
    <|> alignParser2 FillLine "fill-line"
    <|> try (FillWithPattern <$> (parseIdent' "fill-with-pattern" *> sorc *> parseFloat) <*> (sorc *> parseInt) <*> (sorc *> parseMarkupExpr) <*> (sorc *> parseMarkupExpr) <*> (sorc *> parseMarkupExpr))
    <|> try (GeneralAlign <$> (parseIdent' "general-align" *> sorc *> parseAxisDir) <*> (sorc *> parseMarkup))
    <|> try (Halign <$> (parseIdent' "halign" *> sorc *> parseInt) <*> (sorc *> parseMarkup))
    <|> alignParser3 HcenterIn "hcenter-in"
    <|> try (Hspace <$> (parseIdent' "hspace" *> sorc *> parseFloat))
    <|> try (JustifyField <$> (parseIdent' "justify-field" *> sorc *> parseSymbol))
    <|> alignParser2 Justify "justify"
    <|> try (JustifyString <$> (parseIdent' "justify-string" *> sorc *> parseString'))
    <|> alignParser1 LeftAlign "left-align"
    <|> alignParser2 LeftColumn "left-column"
    <|> alignParser2 Line "line"
    <|> alignParser3 Lower "lower"
    <|> alignParser3 PadAround "pad-around"
    <|> alignParser3 PadMarkup "pad-markup"
    <|> try (PadToBox <$> (parseIdent' "padToBox" *> sorc *> parseNumPair) <*> (sorc *> parseNumPair) <*> (sorc *> parseMarkup))
    <|> alignParser3 PadX "pad-x"
    <|> try (PutAdjacent <$> (parseIdent' "put-adjacent" *> sorc *> parseAxisDir) <*> (sorc *> parseMarkupExpr) <*> (sorc *> parseMarkupExpr))
    <|> alignParser3 Raise "raise"
    <|> alignParser1 RightAlign "right-align"
    <|> alignParser2 RightColumn "right-column"
    <|> alignParser3 Rotate "rotate"
    <|> try (Translate <$> (parseIdent' "translate" *> sorc *> parseNumPair) <*> (sorc *> parseMarkup))
    <|> try (TranslateScaled <$> (parseIdent' "translate-scaled" *> sorc *> parseNumPair) <*> (sorc *> parseMarkup))
    <|> alignParser1 Vcenter "vcenter"
    <|> try (Vspace <$> (parseIdent' "vspace" *> sorc *> parseFloat))
    <|> try (WordwrapField <$> (parseIdent' "wordwrap-field" *> sorc *> parseSymbol))
    <|> alignParser2 Wordwrap "wordwrap"
    <|> try (WordwrapString <$> (parseIdent' "wordwrap-string" *> sorc *> parseString'))
    where
        alignParser1 con s = try $ con <$> (parseIdent' s *> sorc *> parseMarkup)
        alignParser2 con s = try $ con <$> (parseIdent' s *> sorc *> braces' (many1 $ parseMarkup <* sorc))
        alignParser3 con s = try $ con <$> (parseIdent' s *> sorc *> parseFloat) <*> (sorc *> parseMarkup)

parseGraphic :: (Stream s m Char) => ParsecT s LilypondState m Graphic
parseGraphic =
        try (ArrowHead <$> (parseIdent' "arrow-head" *> sorc *> parseAxisDir) <*> (sorc *> parseBool))
    <|> try (BeamG <$> parseFloat <*> (sorc *> parseFloat) <*> (sorc *> parseFloat))
    <|> graphicParser Bracket "bracket"
    <|> graphicParser Circle "circle"
    <|> try (DrawCircle <$> parseFloat <*> (sorc *> parseFloat) <*> (sorc *> parseBool))
    <|> try (DrawDashedLine <$> (parseIdent' "draw-dashed-line" *> sorc *> parseNumPair))
    <|> try (DrawDottedLine <$> (parseIdent' "draw-dotted-line" *> sorc *> parseNumPair))
    <|> try (DrawHline <$ parseIdent' "draw-hline")
    <|> try (DrawLine <$> (parseIdent' "draw-line" *> sorc *> parseNumPair))
    <|> graphicParser Ellipse "ellipse"
    <|> try (Epsfile <$> (parseIdent' "epsfile" *> sorc *> parseAxis) <*> (sorc *> parseFloat) <*> (sorc *> parseString'))
    <|> try (FilledBox <$> (parseIdent' "filled-box" *> sorc *> parseNumPair) <*> (sorc *> parseNumPair) <*> (sorc *> parseFloat))
    <|> graphicParser Hbracket "hbracket"
    <|> graphicParser Oval "oval"
    <|> graphicParser Parenthesize "parenthesize"
    <|> try (Path <$> (parseIdent' "path" *> sorc *> parseFloat) <*> (sorc *> sepBy parseString' sorc))
    <|> try (Postscript <$> (parseIdent' "postscript" *> sorc *> parseString'))
    <|> graphicParser RoundedBox "rounded-box"
    <|> try (Scale <$> (parseIdent' "scale" *> sorc *> parseNumPair) <*> (sorc *> parseMarkup))
    <|> try (Triangle <$> (parseIdent' "triangle" *> sorc *> parseBool))
    <|> try (WithUrl <$> (parseIdent' "with-url" *> sorc *> parseString') <*> (sorc *> parseMarkup))
    where
        graphicParser con s = try $ con <$> (parseIdent' s *> sorc *> parseMarkup)

parseTweak :: (Stream s m Char) => ParsecT s LilypondState m Tweak
parseTweak =
        try (parseIdent' "override" *> sorc *> (
                (try $ OverrideSym <$> parseSymbol)
            <|> (try $ OverrideSym' <$> parseToken <*> (sorc *> parseSymbol) <*> parseVal)
            <|> (Override <$> parseToken <*> parseVal)))
         <|> try (Revert <$> (parseIdent' "revert" *> sorc *> parseToken))
         <|> try (parseIdent' "tweak" *> sorc *> ((try $ TweakSym <$> parseSymbol) <|> (Tweak <$> parseToken <*> (sorc *> optional (char '=') *> sorc *> parseLiteral))))
    where
        parseVal = sorc *> char '=' *> sorc *> parseLiteral

parseMarkupVar :: (Stream s m Char) => ParsecT s LilypondState m MarkupExpr
parseMarkupVar = do
    name <- getIdentifier <$> parseIdentifier
    assignments <- hdrAssignments <$> getState
    let val = M.lookup name assignments
    case val of
        Nothing   -> fail $ "no variable \\" ++ name
        Just val' -> return $ MarkupVar $ LitAssignment name val'

parseMarkupExpr :: (Stream s m Char) => ParsecT s LilypondState m MarkupExpr
parseMarkupExpr =
        MarkupQuote <$> parseString
    <|> try (MarkupAlign <$> parseAlignment)
    <|> try (MarkupFont <$> parseFont)
    <|> try (MarkupGraphic <$> parseGraphic)
    <|> try (MarkupTweak <$> parseTweak)
    <|> try parseMarkupVar
    <|> MarkupText <$> parseToken

parseMarkup :: (Stream s m Char) => ParsecT s LilypondState m Markup
parseMarkup =   MarkupList <$> braces' (many1 $ parseMarkup <* sorc)
            <|> MarkupExpr <$> parseMarkupExpr

parseSexp :: (Stream s m Char) => ParsecT s u m String
parseSexp = tail . init <$> parseWithParens
    where
        parseWithParens = (:) <$> char '(' <*> parseRest
        parseRest = do
            c <- lookAhead anyChar
            case c of
                '(' -> (++) <$> parseWithParens <*> parseRest
                ')' -> string ")"
                _   -> (:) <$> anyChar <*> parseRest

parseMeasureUnit :: (Stream s m Char) => ParsecT s u m MeasureUnit
parseMeasureUnit = choice [try $ u <$ string (show $ pretty u) | u <- enumerate]

parseLiteral :: (Stream s m Char) => ParsecT s LilypondState m Literal
parseLiteral = try (MarkupL <$> (parseIdent' "markup" *> sorc *> parseMarkup))
            <|> try (BoolL <$> parseBool)
            <|> try (MeasureL <$> floating2 False <*> parseMeasureUnit)
            <|> try (FloatL <$> parseFloat)
            <|> try (IntL <$> ap sign int)
            <|> try (StringL <$> parseString)
            <|> try (SymbolL <$> parseSymbol)
            <|> try (SexpL <$> (char '#' *> parseSexp))
            <|> (char '#' *> parseLiteral)

parseLitAssignment :: (Stream s m Char) => ParsecT s LilypondState m LitAssignment
parseLitAssignment = do
    name <- parseToken
    sorc *> parseEq *> sorc
    val <- parseLiteral
    modifyState $ \st -> st {hdrAssignments = M.insert name val (hdrAssignments st)}
    return $ LitAssignment name val

-- * Musical Notation

parseBarline :: (Stream s m Char) => ParsecT s u m BarLine
parseBarline = (BarCheck <$ char '|') <|> (BarLine <$> (parseIdent' "bar" *> sorc *> parseString))

parseBeam :: (Stream s m Char) => ParsecT s u m Beam
parseBeam = (BeamOn <$ char '[') <|> (BeamOff <$ char ']')

parseSlur :: (Stream s m Char) => ParsecT s u m Slur
parseSlur = (SlurOn <$ char '(') <|> (SlurOff <$ char ')') <|> (char '\\' *> ((PhraseSlurOn <$ char '(') <|> (PhraseSlurOff <$ char ')')))

parseDirection :: (Stream s m Char) => ParsecT s u m Direction
parseDirection = (Below <$ char '_') <|> (Default <$ char '-') <|> (Above <$ char '^')

parseArticulation :: (Stream s m Char) => ParsecT s u m Articulation
parseArticulation = charParser Accent '>'
                <|> charParser Marcato '^'
                <|> charParser Staccatissimo '!'
                <|> charParser Staccato '.'
                <|> charParser Tenuto '-'
                <|> charParser Portato '_'
                <|> charParser Stopped '+'
                <|> identParser LeftHeel "leftheel"
                <|> identParser RightHeel "rightheel"
                <|> identParser LeftToe "lefttoe"
                <|> identParser RightToe "righttoe"
                <|> identParser ReverseTurn "reverseturn"
                <|> identParser PrallPrall "prallprall"
                <|> identParser PrallMordent "prallmordent"
                <|> identParser UpPrall "upprall"
                <|> identParser DownPrall "downprall"
                <|> identParser UpMordent "upmordent"
                <|> identParser DownMordent "downmordent"
                <|> identParser PrallDown "pralldown"
                <|> identParser PrallUp "prallup"
                <|> identParser LinePrall "lineprall"
                <|> identParser SignumCongruentiae "signumCongruentiae"
                <|> identParser ShortFermata "shortfermata"
                <|> identParser LongFermata "longfermata"
                <|> identParser VeryLongFermata "verylongfermata"
                <|> identParser VarCoda "varcoda"
                <|> read . cap . getIdentifier <$> (parseIdent $ choice $ try . string <$> tokens)
            where
                charParser con c = con <$ char c
                identParser con s = try $ con <$ (char '\\' *> string s)
                tokens = sortOn (negate . length) $ fmap toLower . show <$> (enumerate::[Articulation])

-- * Language

parseLanguage :: (Stream s m Char) => ParsecT s u m Language
parseLanguage = choice [try $ lang <$ string (show $ pretty lang) | lang <- enumerate]

-- * Pitch

parsePitchClass' :: (Stream s m Char) => Language -> ParsecT s u m PitchClass
parsePitchClass' lang = do
    base <- (nameMapInv M.!) <$> (choice $ try . string <$> nameStrs)
    acc <- parseFlats <|> parseSharps <|> return []
    return $ read $ show base ++ concat acc
    where
        (nameMap, (flats, sharps)) = languageNoteMap M.! lang
        nameMapInv' = M.fromList $ swap <$> M.toList nameMap
        -- handle Germanic cases
        nameMapInv = if "b" `M.member` nameMapInv' then nameMapInv' else M.insert "b" Bf nameMapInv'
        nameStrs = M.keys nameMapInv
        flats' = sortOn (negate . length) flats
        sharps' = sortOn (negate . length) sharps
        parseFlat = choice $ try . string <$> flats'
        parseSharp = choice $ try . string <$> sharps'
        parseFlats = map (const "f") <$> (choice $ try . flip count parseFlat <$> [2, 1])
        parseSharps = map (const "s") <$> (choice $ try . flip count parseSharp <$> [2, 1])

parsePitchClass :: (Stream s m Char) => ParsecT s LilypondState m PitchClass
parsePitchClass = do
    lang <- language <$> getState
    parsePitchClass' lang

parseMode :: (Stream s m Char) => ParsecT s u m Mode
parseMode = (try $ Major <$ string "\\major") <|> (try $ Minor <$ string "\\minor")

-- | Parses an octave, possibly with an octave check.
parseOctave :: (Stream s m Char) => ParsecT s u m (Octave, Maybe OctaveCheck)
parseOctave = swap <$> ((,) <$> (fmap (const OctaveCheck) <$> (optionMaybe $ char '=')) <*> parseOctave')
    where
        parseUpOctave = fromIntegral . length <$> many1 (char '\'')
        parseDownOctave = fromIntegral . (* (-1)) . length <$> many1 (char ',')
        parseOctave' = (+3) <$> (parseUpOctave <|> parseDownOctave <|> pure 0)

parseNotePitch :: (Stream s m Char) => ParsecT s LilypondState m NotePitch
parseNotePitch = do
    pc <- parsePitchClass
    (oct, check) <- parseOctave
    return $ NotePitch (pc, oct) check

-- * Dynamics

parseDynamicFixed :: (Stream s m Char) => ParsecT s u m DynamicFixed
parseDynamicFixed = read . fmap toUpper . getIdentifier <$> (parseIdent $ choice $ try . string <$> tokens)
    where tokens = sortOn (negate . length) $ fmap toLower . show <$> (enumerate::[DynamicFixed])

parseDynamicMotion :: (Stream s m Char) => ParsecT s u m DynamicMotion
parseDynamicMotion = go . getIdentifier <$> (parseIdent $ choice $ string <$> [">", "<", "!"])
    where
        go "<" = Crescendo
        go ">" = Decrescendo
        go _   = EndDynamicMotion

parseDynamics :: (Stream s m Char) => ParsecT s u m Dynamics
parseDynamics = (try $ DynMotion <$> parseDynamicMotion) <|> (DynFixed <$> parseDynamicFixed)

parseExpressive :: (Stream s m Char) => ParsecT s LilypondState m Expressive
parseExpressive =   Tie <$ (char '~' <* sorc)
                <|> try (Beam <$> parseBeam)
                <|> try (Slur <$> parseSlur)
                <|> try (Glissando <$ string "\\glissando")
                <|> do
                        d <- optionMaybe $ parseDirection <* sorc
                        let d' = fromMaybe Default d
                        -- d <- option Default (parseDirection <* sorc)
                        try (Articulation d' <$> parseArticulation)
                            <|> try (Dynamics d' <$> parseDynamics)
                            <|> try (Text d' <$> parseString)
                            <|> try (Text d' <$> maybe (fail "") (const parseToken) d)
                            <|> Markup d' <$> (string "\\markup" *> sorc *> parseMarkup)

-- * Duration

-- | Parses the denominator of a time signature (a power of 2).
parseBaseDur :: (Num a, Read a, Stream s m Char) => ParsecT s u m a
parseBaseDur = read <$> (choice $ try . string . show <$> (reverse $ (2^) <$> [0..6]))

-- | Parses a time signature.
parseTimeSig :: (Stream s m Char) => ParsecT s u m TimeSig
parseTimeSig = TimeSig <$> ((,) <$> (string "\\time" *> sorc *> int) <*> (sorc *> char '/' *> parseBaseDur))

-- parses duration (1 / n)
parseNumDur :: (Stream s m Char) => ParsecT s u m Rational
parseNumDur = (1 %) <$> parseBaseDur

longDurs = [("breve", 2),
            ("longa", 4),
            ("maxima", 8)]

parseLongDur :: (Stream s m Char) => ParsecT s u m Rational
parseLongDur = do
    name <- char '\\' *> (choice $ try . string . fst <$> longDurs)
    return $ fromMaybe (error "invalid duration") (lookup name longDurs)

parseDuration :: (Stream s m Char) => ParsecT s u m Duration
parseDuration = do
    r <- parseNumDur <|> parseLongDur
    nd <- length <$> many (char '.')
    m <- option 1 (try $ sorc *> char '*' *> sorc *> int)
    return $ Duration r nd m

parseRestType :: (Stream s m Char) => ParsecT s u m RestType
parseRestType = go <$> (choice $ char <$> ['r', 'R', 's'])
    where
        go 'r' = StdRest
        go 'R' = FullRest
        go _   = Skip

parseStdClef :: (Stream s m Char) => ParsecT s u m StdClef
parseStdClef = go <$> parseToken
    where
        go "violin" = Treble
        go "GG" = GG
        go "C" = Alto
        go "varC" = AltovarC
        go "baritonevarF" = Varbaritone
        go "F" = Bass
        go s = read $ cap s

parseClef :: (Stream s m Char) => ParsecT s u m Clef
parseClef = try (CustomClef <$> parseString) <|> (StdClef <$> parseStdClef)

parseTempo :: (Stream s m Char) => ParsecT s u m Tempo
parseTempo = string "\\tempo" *> sorc *> (Tempo <$> (optionMaybe parseToken <* sorc) <*> (optionMaybe $ (,) <$> (parseDuration <* sorc) <*> (parseEq *> sorc *> int)))

parseStaff :: (Stream s m Char) => ParsecT s u m Staff
parseStaff = read <$> (choice $ try . string . show <$> (enumerate::[Staff]))

reservedVars = sortOn (negate . length) ["alternative" ,"book", "bookpart", "clef", "context", "header", "key", "language", "lyricsto", "markup", "new", "once", "override", "partcombine", "relative", "repeat", "revert", "set", "tempo", "time", "times", "version", "width", "with"]

parseReservedVar :: (Stream s m Char) => ParsecT s u m String
parseReservedVar = char '\\' *> choice (try . string <$> reservedVars) <* notFollowedBy (satisfy isAlphaNum)

parseVar :: (Stream s m Char) => ParsecT s u m Variable
parseVar = Variable <$> (notFollowedBy parseReservedVar *> char '\\' *> parseToken)

parsePitch :: (Stream s m Char) => ParsecT s LilypondState m Pitch
parsePitch = (toPitch :: (NotePitch -> Pitch)) <$> parseNotePitch

parseEq :: (Stream s m Char) => ParsecT s u m ()
parseEq = void $ char '='

parseAssignment :: (FromPitch a, Stream s m Char) => ParsecT s LilypondState m (Assignment a)
parseAssignment =
        try (Set <$> (string "\\set" *> sorc *> parseAssignment))
    <|> try (Once <$> (string "\\once" *> sorc *> parseAssignment))
    <|> try (SymbAssignment <$> (parseToken <* sorc) <*> (parseLiteral <* sorc) <*> (parseEq *> sorc *> parseMusic))
    <|> try (PropAssignment <$> parseTweak)
    <|> try (Assignment <$> (parseToken <* sorc) <*> (parseEq *> sorc *> parseMusic))

parseWithBlock :: (FromPitch a, Stream s m Char) => ParsecT s LilypondState m (WithBlock a)
parseWithBlock = string "\\with" *> sorc *> (WithBlock <$> braces' parseAssignment)

parseNewItem :: (Stream s m Char) => ParsecT s LilypondState m NewItem
parseNewItem =
        try (Voice <$ string "Voice")
    <|> try (Lyrics <$ (string "Lyrics" *> sorc *> string "\\lyricsto"))
    <|> try (NewStaff <$> parseStaff)
    <|> NewItem <$> parseToken

parseContext :: (FromPitch a, Stream s m Char) => ParsecT s LilypondState m (Context a)
parseContext = do
    string "\\context" *> sorc
    c <- lookAhead anyChar
    pref <- if c == '{'
                then return Nothing
                else Just <$> ((,) <$> parseToken <*> (sorc *> optionMaybe (parseEq *> sorc *> parseString)))
    mus <- sorc *> parseMusic
    return $ Context pref mus

parseMusic :: (FromPitch a, Stream s m Char) => ParsecT s LilypondState m (MusicL a)
parseMusic =
        try (Assign <$> parseAssignment)
    <|> try (Note <$> parseNotePitch' <*> optionMaybe (try parseDuration) <*> (many $ try parseExpressive))
    <|> try (Rest <$> parseRestType <*> optionMaybe (try parseDuration) <*> (many $ try parseExpressive))
    <|> try (Chord <$> (bracket' '<' '>' (endBy parseNotePitch' sorc)) <*> optionMaybe parseDuration <*> many parseExpressive)
    <|> try (Bar <$> parseBarline)
    <|> try (string "\\clef" *> sorc *> (Clef <$> parseClef))
    <|> try (string "\\key" *> sorc *> (Key <$> ((,) <$> (parsePitchClass <* sorc) <*> parseMode)))
    <|> try (string "\\time" *> sorc *> (Time . TimeSig <$> ((,) <$> int <*> (char '/' *> int))))
    <|> try (Tmp <$> parseTempo)
    <|> try (Sequential <$> braces' (endBy parseMusic sorc))
    <|> try (Simultaneous True <$> brackSim (sepBy (parseMusic <* sorc) (string "\\\\" <* sorc)))
    <|> try (Simultaneous False <$> brackSim (endBy parseMusic sorc))
    <|> try (string "\\repeat" *> sorc *>
            ((Repeat <$> ((== "unfold") <$> (string "unfold" <|> string "volta") <* sorc) <*> (int <* sorc) <*> (parseMusic <* sorc) <*> (optionMaybe $ string "\\alternative" *> sorc *> sepBy parseMusic sorc))
            <|> (Tremolo <$> (string "tremolo" *> sorc *> int <* sorc) <*> parseMusic)))
    <|> try (string "\\partcombine" *> sorc *> (PartCombine <$> parseMusic <*> (sorc *> parseMusic)))
    <|> try (string "\\times" *> sorc *> (Times <$> (parseFrac <* sorc) <*> parseMusic))
    <|> try (string "\\tuplet" *> sorc *> (Tuplet <$> (parseFrac <* sorc) <*> parseMusic))
    <|> try (string "\\tupletSpan" *> sorc *> (TupletSpan <$> ((Just <$> (try parseDuration)) <|> Nothing <$ string "\\default")))
    <|> try (string "\\transpose" *> sorc *> (Transpose <$> parsePitch <*> (sorc *> parsePitch) <*> (sorc *> parseMusic)))
    <|> try (string "\\relative" *> sorc *> (Relative <$> (optionMaybe parsePitch) <*> (sorc *> parseMusic)))
    <|> try (string "\\new" *> sorc *> (New <$> parseNewItem <*> (sorc *> optional (parseEq *> sorc) *> optionMaybe parseString) <*> (sorc *> optionMaybe parseWithBlock) <*> (sorc *> parseMusic)))
    <|> try (Ctx <$> parseContext)
    <|> try (Var <$> parseVar)
    <|> try (Lit <$> parseLiteral)
    where
        brackSim = between (string "<<" <* sorc) (sorc *> string ">>")
        parseFrac = (%) <$> int <*> (char '/' *> int)
        parseNotePitch' = fromPitch . toPitch <$> parseNotePitch

parseHeader :: (Stream s m Char) => ParsecT s LilypondState m Header
parseHeader = string "\\header" *> sorc *> braces' (Header <$> endBy parseLitAssignment sorc)

parseMaybeHeader :: (Stream s m Char) => ParsecT s LilypondState m (Maybe Header)
parseMaybeHeader = try (Just <$> parseHeader) <|> return Nothing

parseLayoutItem :: (FromPitch a, Stream s m Char) => ParsecT s LilypondState m (LayoutItem a)
parseLayoutItem =   try (LayoutAssignment <$> parseLitAssignment)
                <|> try (LayoutVar <$> parseVar)
                <|> (LayoutContext <$> parseContext)

parseLayout :: (FromPitch a, Stream s m Char) => ParsecT s LilypondState m (Layout a)
parseLayout = string "\\layout" *> sorc *> braces' (Layout <$> endBy parseLayoutItem sorc)

parseMidiItem :: (FromPitch a, Stream s m Char) => ParsecT s LilypondState m (MidiItem a)
parseMidiItem =   try (MidiTempo <$> parseTempo)
              <|> (MidiContext <$> parseContext)

parseMidi :: (FromPitch a, Stream s m Char) => ParsecT s LilypondState m (Midi a)
parseMidi = string "\\midi" *> sorc *> braces' (Midi <$> endBy parseMidiItem sorc)

parseScoreItem :: (FromPitch a, Stream s m Char) => ParsecT s LilypondState m (ScoreItem a)
parseScoreItem =    try (ScoreMusic <$> parseMusic)
                <|> try (ScoreHeader <$> parseHeader)
                <|> try (ScoreLayout <$> parseLayout)
                <|> (ScoreMidi <$> parseMidi)

parseScore :: (FromPitch a, Stream s m Char) => ParsecT s LilypondState m (Score a)
parseScore =    try (string "\\score" *> sorc *> braces' (Score <$> endBy parseScoreItem sorc))
            <|> Score . pure . ScoreMusic <$> parseMusic

parseBookPart :: (FromPitch a, Stream s m Char) => ParsecT s LilypondState m (BookPart a)
parseBookPart = try (string "\\bookpart" *> sorc *> braces' (BookPart <$> parseMaybeHeader <*> endBy parseScore sorc))
                <|> (BookPart Nothing <$> endBy1 parseScore sorc)

parseBook :: (FromPitch a, Stream s m Char) => ParsecT s LilypondState m (Book a)
parseBook = (try (string "\\book") *> sorc *> braces' (Book <$> parseMaybeHeader <*> endBy parseBookPart sorc))
        <|> (Book Nothing <$> (endBy1 parseBookPart sorc))

parseTopLevel :: (FromPitch a) => ParsecT String LilypondState IO (TopLevel a)
parseTopLevel = try (string "\\version" *> sorc *> (Version <$> parseString))
            <|> (try (string "\\language") *> sorc *> parseLang)
            <|> (try (string "\\include") *> sorc *> parseInclude)
            <|> try (AssignmentTop <$> parseAssignment)
            <|> try (HeaderTop <$> parseHeader)
            <|> (BookTop <$> parseBook)
    where
        parseLang = do
            lang <- between (char '"') (char '"') parseLanguage
            modifyState (\st -> st {language = lang})
            return $ Lang lang
        parseInclude = do
            path <- parseString
            st <- getState
            path' <- lift $ findFile (includePaths st) path
            case path' of
                Nothing -> fail $ "Failed to locate included file \"" ++ path ++ "\""
                (Just path'') -> do
                    incl <- lift $ readFile path''
                    curPosition <- getPosition
                    curInput <- getInput
                    setPosition $ initialPos path''
                    setInput incl
                    lp <- parseLilypond <* eof
                    setPosition curPosition
                    setInput curInput
                    return $ Include path lp

parseLilypond :: (FromPitch a) => ParsecT String LilypondState IO (Lilypond a)
parseLilypond = Lilypond <$> (sorc *> endBy parseTopLevel sorc <* eof)
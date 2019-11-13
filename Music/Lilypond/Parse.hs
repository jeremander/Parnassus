{-# LANGUAGE
    FlexibleContexts,
    FlexibleInstances,
    InstanceSigs,
    OverloadedStrings,
    TemplateHaskell,
    UndecidableInstances
    #-}

module Music.Lilypond.Parse where

import Control.Lens (makeLenses, set)
import Control.Monad (ap, void)
import Control.Monad.Trans (lift)
import Data.Char (isSpace, toLower, toUpper)
import Data.Default (Default(..))
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Data.Ratio ((%))
import Data.Sort (sortOn)
import Data.Tuple (swap)
import Euterpea (Mode(..), Pitch, PitchClass(..))
import System.Directory (findFile)
import Text.Parsec
import Text.Parsec.Number (floating, int, sign)
import Text.Parsec.Token (float, integer, stringLiteral)
import qualified Text.Pretty as P
import Text.Pretty (Pretty(..))

import Misc.Utils (enumerate)
import Music.Dynamics
import Music.Pitch
import Music.Rhythm
import Music.Lilypond.IO
import Music.Lilypond.Music
import Music.Lilypond.Score

import System.IO.Unsafe
import System.FilePath.Posix


makeLenses ''Header


data LilypondState = LilypondState {
    includePaths :: [FilePath],
    language :: Language
} deriving (Eq, Show)

-- Mac OSX specific include path
defaultLilypondState :: LilypondState
defaultLilypondState = LilypondState {
    includePaths = ["/Applications/LilyPond.app/Contents/Resources/share/lilypond/current/ly"],
    language = def
}

-- * Utilities

-- capitalizes the first letter of a string
cap :: String -> String
cap cs = [if (i == 0) then toUpper c else c | (i, c) <- zip [0..] cs]

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


-- * Identifier

parseIdent :: (Stream s m Char) => ParsecT s u m String -> ParsecT s u m Identifier
parseIdent p = Identifier <$> ((char '\\') *> p)

validIdentifiers = ['a'..'z'] ++ ['A'..'Z'] ++ ['.', '-']

parseToken :: (Stream s m Char) => ParsecT s u m String
parseToken = many1 $ oneOf validIdentifiers

-- parses an arbitrary identifier
parseIdentifier :: (Stream s m Char) => ParsecT s u m Identifier
parseIdentifier = parseIdent parseToken

-- * Literal

parseBool :: (Stream s m Char) => ParsecT s u m Bool
parseBool = try (True <$ string "#t") <|> (False <$ string "#f")

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

parseSymbol :: (Stream s m Char) => ParsecT s u m String
parseSymbol = char '\'' *> (many $ oneOf validIdentifiers)

parens' :: (Stream s m Char) => ParsecT s u m a -> ParsecT s u m a
parens'  = between (char '(' <* spaces) (spaces *> char ')')

sexpChars = validIdentifiers ++ ['0'..'9'] ++ [' ', '-', '\'', '\"', ':', '#'] -- ???? TODO: fix

parseSexp :: (Stream s m Char) => ParsecT s u m String
parseSexp = parens' $ many $ oneOf sexpChars

parseLiteral :: (Stream s m Char) => ParsecT s u m Literal
parseLiteral =  try (BoolL <$> parseBool)
            <|> try (FloatL <$> ap sign floating)
            <|> try (IntL <$> ap sign int)
            <|> try (StringL <$> parseString)
            <|> try (SymbolL <$> parseSymbol)
            <|> try (SexpL <$> parseSexp)
            <|> (char '#' *> parseLiteral)

-- * Markup

parseMarkup :: (Stream s m Char) => ParsecT s u m Markup
parseMarkup =   MarkupList <$> braces' (many1 $ parseMarkup <* sorc)
            <|> MarkupQuote <$> parseString
            <|> markupParser Bold "bold"
            <|> markupParser Box "box"
            <|> markupParser Caps "caps"
            <|> markupParser DynamicsFont "dynamics"
            <|> markupParser FingeringFont "fingering"
            <|> floatParser Fontsize "fontsize"
            <|> markupParser Huge "huge"
            <|> markupParser Italic "italic"
            <|> markupParser Large "large"
            <|> markupParser Larger "larger"
            <|> floatParser Magnify "magnify"
            <|> markupParser Medium "medium"
            <|> markupParser Roman "roman"
            <|> markupParser Sans "sans"
            <|> markupParser Sub "sub"
            <|> markupParser Super "super"
            <|> markupParser TextFont "text"
            <|> markupParser Tiny "tiny"
            <|> markupParser TypewriterFont "typewriter"
            <|> markupParser Upright "upright"
            <|> MarkupText <$> parseToken
        where
            identParser s = char '\\' *> string s *> sorc
            floatParser con s = try $ con <$> (identParser s *> char '#' *> ap sign floating <* sorc) <*> parseMarkup
            markupParser con s = try $ con <$> (identParser s *> parseMarkup)

parseBarline :: (Stream s m Char) => ParsecT s u m BarLine
parseBarline = (BarCheck <$ char '|') <|> (BarLine <$> (string "\\bar" *> sorc *> parseString))

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

parseExpressive :: (Stream s m Char) => ParsecT s u m Expressive
parseExpressive =   Tie <$ char '~'
                <|> try (Beam <$> parseBeam)
                <|> try (Slur <$> parseSlur)
                <|> try (Glissando <$ string "\\glissando")
                <|> do
                        d <- option Default (parseDirection <* sorc)
                        try (Articulation d <$> parseArticulation) <|> try (Dynamics d <$> parseDynamics) <|> try (Text d <$> parseString) <|> Markup d <$> (string "\\markup" *> sorc *> parseMarkup)

-- * Duration

-- | Parses the denominator of a time signature (a power of 2).
parseBaseDur :: (Num a, Read a, Stream s m Char) => ParsecT s u m a
parseBaseDur = read <$> (choice $ try . string . show <$> (reverse $ (2^) <$> [0..6]))

-- | Parses a time signature.
parseTimeSig :: (Stream s m Char) => ParsecT s u m TimeSig
parseTimeSig = (,) <$> (string "\\time" *> sorc *> int) <*> (sorc *> char '/' *> parseBaseDur)

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

reservedVariables = ["incipitwidth", "htitle", "hcomposer", "title",
                     "subtitle", "subsubtitle", "composer", "opus",
                     "poet", "copyright"]

parseReservedVar :: (Stream s m Char) => ParsecT s u m String
parseReservedVar = choice $ try . string <$> reservedVariables

parsePitch :: (Stream s m Char) => ParsecT s LilypondState m Pitch
parsePitch = (toPitch :: (NotePitch -> Pitch)) <$> parseNotePitch

staffNames = ["Staff", "PianoStaff", "ChoirStaff"]

parseStaffName :: (Stream s m Char) => ParsecT s u m String
parseStaffName = choice $ try . string <$> staffNames

parseEq :: (Stream s m Char) => ParsecT s u m ()
parseEq = void $ char '='

parseAssignment :: (Stream s m Char) => ParsecT s LilypondState m Assignment
parseAssignment =
        try (Set <$> (string "\\set" *> sorc *> parseAssignment))
    <|> try (Override <$> (string "\\override" *> sorc *> parseAssignment))
    <|> try (Once <$> (string "\\once" *> sorc *> parseAssignment))
    <|> try (Revert <$> (string "\\revert" *> parseToken))
    <|> try (SymbAssignment <$> (parseToken <* sorc) <*> (parseLiteral <* sorc) <*> (parseEq *> sorc *> parseMusic))
    <|> try (Assignment <$> (parseReservedVar <* sorc) <*> (Lit . IntL <$> int))
    <|> try (Assignment <$> (parseReservedVar <* sorc) <*> (Lit . StringL <$> parseString))
    <|> try (Assignment <$> (parseToken <* sorc) <*> (parseEq *> sorc *> parseMusic))

parseMusic :: (Stream s m Char) => ParsecT s LilypondState m MusicL
parseMusic =
        try (Assign <$> parseAssignment)
    <|> try (Note <$> parseNotePitch <*> optionMaybe (try parseDuration) <*> (many $ try parseExpressive))
    <|> try (Rest <$> parseRestType <*> optionMaybe parseDuration)
    <|> try (Chord <$> (bracket' '<' '>' (endBy parseNotePitch sorc)) <*> optionMaybe parseDuration <*> many parseExpressive)
    <|> try (Bar <$> parseBarline)
    <|> try (string "\\clef" *> sorc *> (Clef <$> parseClef))
    <|> try (string "\\key" *> sorc *> (Key <$> ((,) <$> (parsePitchClass <* sorc) <*> parseMode)))
    <|> try (string "\\time" *> sorc *> (Time <$> ((,) <$> int <*> (char '/' *> int))))
    <|> try (string "\\tempo" *> sorc *> (Tempo <$> (optionMaybe parseToken <* sorc) <*> (optionMaybe $ (,) <$> (parseDuration <* sorc) <*> (parseEq *> sorc *> int))))
    <|> try (Sequential <$> braces' (endBy parseMusic sorc))
    <|> try (Simultaneous True <$> brackSim (sepBy (parseMusic <* sorc) (string "\\\\" <* sorc)))
    <|> try (Simultaneous False <$> brackSim (endBy parseMusic sorc))
    <|> try (string "\\repeat" *> sorc *>
            ((Repeat <$> ((== "unfold") <$> (string "unfold" <|> string "volta") <* sorc) <*> (int <* sorc) <*> (parseMusic <* sorc) <*> (optionMaybe $ string "\\alternative" *> sorc *> sepBy parseMusic sorc))
            <|> (Tremolo <$> (string "tremolo" *> sorc *> int <* sorc) <*> parseMusic)))
    <|> try (string "\\times" *> sorc *> (Times <$> (parseFrac <* sorc) <*> parseMusic))
    <|> try (string "\\tuplet" *> sorc *> (Tuplet <$> (parseFrac <* sorc) <*> parseMusic))
    <|> try (string "\\transpose" *> sorc *> (Transpose <$> parsePitch <*> (sorc *> parsePitch) <*> (sorc *> parseMusic)))
    <|> try (string "\\relative" *> sorc *> (Relative <$> (optionMaybe parsePitch) <*> (sorc *> parseMusic)))
    <|> try (string "\\with" *> sorc *> (With <$> braces' parseAssignment))
    <|> try (string "\\new" *> sorc *> choice [
            string "Voice" *> sorc *> (Voice <$> (optionMaybe $ parseEq *> sorc *> parseString) <*> (sorc *> parseMusic)),
            parseStaffName *> sorc *> (Staff <$> (optionMaybe $ sorc *> parseString) <*> (sorc *> parseMusic)),
            string "Lyrics" *> sorc *> string "\\lyricsto" *> (Lyrics <$> (sorc *> parseString) <*> (sorc *> parseMusic)),
            New <$> (sorc *> parseToken <* sorc) <*> (optionMaybe $ parseEq *> sorc *> parseString) <*> (sorc *> parseMusic)
        ])
    <|> try (string "\\context" *> sorc *> (Context <$> (sorc *> parseToken <* sorc) <*> (optionMaybe $ parseEq *> sorc *> parseString) <*> (sorc *> parseMusic)))
    <|> try (char '\\' *> (Var <$> parseToken))
    <|> try (Lit <$> parseLiteral)
    where
        brackSim = between (string "<<" <* sorc) (sorc *> string ">>")
        parseFrac = (%) <$> int <*> (char '/' *> int)

parseScore :: (Stream s m Char) => ParsecT s LilypondState m Score
parseScore = try (string "\\score" *> sorc *> (Score <$> braces' parseMusic))
         <|> Score <$> parseMusic

parseHeader :: (Stream s m Char) => ParsecT s u m Header
parseHeader = string "\\header" *> sorc *> braces' (fieldSetter <*> pure def)
    where
        parseField' (setter, field) = set setter . Just <$> (string field *> sorc *> parseEq *> sorc *> parseLiteral)
        parseField = choice $ try . parseField' <$> [
            (dedication, "dedication"),
            (title, "title"),
            (subtitle, "subtitle"),
            (subsubtitle, "subsubtitle"),
            (instrument, "instrument"),
            (poet, "poet"),
            (composer, "composer"),
            (meter, "meter"),
            (arranger, "arranger"),
            (tagline, "tagline"),
            (copyright, "copyright")
            ]
        fieldSetters = endBy parseField sorc
        fieldSetter = foldr (.) id . reverse <$> fieldSetters

parseBookPart :: (Stream s m Char) => ParsecT s LilypondState m BookPart
parseBookPart = (try (string "\\bookpart") *> sorc *> braces' (BookPart <$> (sorc *> optionMaybe parseHeader <* sorc) <*> (sepBy parseScore sorc)))
            <|> (BookPart Nothing <$> (sepBy1 parseScore sorc))

parseBook :: (Stream s m Char) => ParsecT s LilypondState m Book
parseBook = (try (string "\\book") *> sorc *> braces' (Book <$> (sorc *> optionMaybe parseHeader <* sorc) <*> (sepBy parseBookPart sorc)))
        <|> (Book Nothing <$> (sepBy1 parseBookPart sorc))

parseTopLevel :: ParsecT String LilypondState IO TopLevel
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
                    curInput <- getInput
                    setInput incl
                    lp <- parseLilypond <* eof
                    setInput curInput
                    return $ Include path lp

parseLilypond :: ParsecT String LilypondState IO Lilypond
parseLilypond = Lilypond <$> (sorc *> endBy parseTopLevel sorc <* eof)

----

inPath :: FilePath
inPath = "/Users/jeremander/Programming/Music/Parnassus/tunes/lilypond/BWV528/SonataIV.ly"

outPath :: FilePath
outPath = replaceBaseName inPath (base ++ "2")
    where
        base = takeBaseName inPath

lp :: IO (Either ParseError Lilypond)
lp = do
    s <- readFile inPath
    runParserT parseLilypond defaultLilypondState inPath s

main :: IO ()
main = do
    s <- readFile inPath
    res <- runParserT parseLilypond defaultLilypondState inPath s
    case res of
        (Left err) -> print err
        (Right lp) -> do
            writeLilypond lp outPath
            putStrLn $ "Successfully saved to " ++ outPath

test = unsafePerformIO $ readFile "/Users/jeremander/Programming/Music/Parnassus/tunes/lilypond/BWV528/test.ly"
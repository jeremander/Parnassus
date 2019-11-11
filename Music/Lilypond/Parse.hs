{-# LANGUAGE
    FlexibleContexts,
    OverloadedStrings,
    TemplateHaskell
    #-}

module Music.Lilypond.Parse where

import Control.Lens (makeLenses, set)
import Control.Monad (ap, void)
import Data.Char (isSpace, toLower, toUpper)
import Data.Default (def)
import Data.Maybe (fromMaybe)
import Data.Ratio ((%))
import Data.Sort (sortOn)
import Data.Tuple (swap)
import Euterpea (Mode(..), Pitch, PitchClass)
import Text.Parsec.Class
import Text.Parsec.Number (floating, int, sign)
import Text.Parsec.Token (float, integer, stringLiteral)
import qualified Text.Pretty as P
import Text.Pretty (Pretty(..))

import Misc.Utils (enumerate)
import Music.Dynamics
import Music.Pitch
import Music.Rhythm
import Music.Lilypond.Music
import Music.Lilypond.Score

import System.IO.Unsafe


makeLenses ''Header


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

instance HasParser Identifier where
    parser = parseIdent parseToken

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

instance HasParser Literal where
    parser =    try (BoolL <$> parseBool)
            <|> try (FloatL <$> ap sign floating)
            <|> try (IntL <$> ap sign int)
            <|> try (StringL <$> parseString)
            <|> try (SymbolL <$> parseSymbol)
            <|> try (SexpL <$> parseSexp)
            <|> (char '#' *> parser)

-- * Markup

instance HasParser Markup where
    parser =   MarkupList <$> braces' (many1 $ parser <* sorc)
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
            floatParser con s = try $ con <$> (identParser s *> char '#' *> ap sign floating <* sorc) <*> parser
            markupParser con s = try $ con <$> (identParser s *> parser)

instance HasParser BarLine where
    parser = (BarCheck <$ char '|') <|> (BarLine <$> (string "\\bar" *> sorc *> parseString))

instance HasParser Beam where
    parser = (BeamOn <$ char '[') <|> (BeamOff <$ char ']')

instance HasParser Slur where
    parser = (SlurOn <$ char '(') <|> (SlurOff <$ char ')') <|> (char '\\' *> ((PhraseSlurOn <$ char '(') <|> (PhraseSlurOff <$ char ')')))

instance HasParser Direction where
    parser = (Below <$ char '_') <|> (Default <$ char '-') <|> (Above <$ char '^')

instance HasParser Articulation where
    parser =   charParser Accent '>'
           <|> charParser Marcato '^'
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

instance HasParser Expressive where
    parser =   Tie <$ char '~'
           <|> try (Beam <$> parser)
           <|> try (Slur <$> parser)
           <|> try (Glissando <$ string "\\glissando")
           <|> do
                d <- option Default (parser <* sorc)
                try (Articulation d <$> parser) <|> try (Dynamics d <$> parser) <|> try (Text d <$> parseString) <|> Markup d <$> (string "\\markup" *> sorc *> parser)

-- * Pitch

instance HasParser PitchClass where
    parser = do
        pc <- toUpper <$> oneOf "abcdefgh"
        let parseSharps = choice $ try . flip count (string "is") <$> [2, 1]
        let parseFlats = choice $ try . flip count (string "es") <$> [2, 1]
        accs <- map (\acc -> if (acc == "is") then "s" else "f") <$> (parseSharps <|> parseFlats <|> pure [])
        let convertPc c = if (c == 'H') then 'B' else c  -- convert German notation
        return $ read $ convertPc pc : concat accs

instance HasParser Mode where
    parser = (try $ Major <$ string "\\major") <|> (try $ Minor <$ string "\\minor")

-- | Parses an octave, possibly with an octave check.
parseOctave :: (Stream s m Char) => ParsecT s u m (Octave, Maybe OctaveCheck)
parseOctave = swap <$> ((,) <$> (fmap (const OctaveCheck) <$> (optionMaybe $ char '=')) <*> parseOctave')
    where
        parseUpOctave = fromIntegral . length <$> many1 (char '\'')
        parseDownOctave = fromIntegral . (* (-1)) . length <$> many1 (char ',')
        parseOctave' = (+3) <$> (parseUpOctave <|> parseDownOctave <|> pure 0)

instance HasParser NotePitch where
    parser = do
        pc <- parser
        (oct, check) <- parseOctave
        return $ NotePitch (pc, oct) check

-- * Dynamics

instance HasParser DynamicFixed where
    parser = read . fmap toUpper . getIdentifier <$> (parseIdent $ choice $ try . string <$> tokens)
        where tokens = sortOn (negate . length) $ fmap toLower . show <$> (enumerate::[DynamicFixed])

instance HasParser DynamicMotion where
    parser = go . getIdentifier <$> (parseIdent $ choice $ string <$> [">", "<", "!"])
        where
            go "<" = Crescendo
            go ">" = Decrescendo
            go _   = EndDynamicMotion

instance HasParser Dynamics where
    parser = (try $ DynMotion <$> parser) <|> (DynFixed <$> parser)


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

-- applies some number of dots to a duration
applyDots :: Rational -> Int -> Rational
applyDots r n = ((3 % 2) ^^ n) * r

instance HasParser Duration where
    parser = do
        d <- parseNumDur <|> parseLongDur
        n <- length <$> many (char '.')
        return $ Duration $ applyDots d n

instance HasParser RestType where
    parser = go <$> (choice $ char <$> ['r', 'R', 's'])
        where
            go 'r' = StdRest
            go 'R' = FullRest
            go _   = Skip

instance HasParser StdClef where
    parser = go <$> parseToken
        where
            go "violin" = Treble
            go "GG" = GG
            go "C" = Alto
            go "varC" = AltovarC
            go "baritonevarF" = Varbaritone
            go "F" = Bass
            go s = read $ cap s

instance HasParser Clef where
    parser = try (CustomClef <$> parseString) <|> (StdClef <$> parser)

reservedVariables = ["incipitwidth", "htitle", "hcomposer", "title",
                     "subtitle", "subsubtitle", "composer", "opus",
                     "poet", "copyright"]

parseReservedVar :: (Stream s m Char) => ParsecT s u m String
parseReservedVar = choice $ try . string <$> reservedVariables

parsePitch :: (Stream s m Char) => ParsecT s u m Pitch
parsePitch = (toPitch :: (NotePitch -> Pitch)) <$> parser

staffNames = ["Staff", "PianoStaff", "ChoirStaff"]

parseStaffName :: (Stream s m Char) => ParsecT s u m String
parseStaffName = choice $ try . string <$> staffNames

parseEq :: (Stream s m Char) => ParsecT s u m ()
parseEq = void $ char '='

instance HasParser Assignment where
    parser =
            try (Set <$> (string "\\set" *> sorc *> parser))
        <|> try (Override <$> (string "\\override" *> sorc *> parser))
        <|> try (Once <$> (string "\\once" *> sorc *> parser))
        <|> try (Revert <$> (string "\\revert" *> parseToken))
        <|> try (SymbAssignment <$> (parseToken <* sorc) <*> (parser <* sorc) <*> (parseEq *> sorc *> parser))
        <|> try (Assignment <$> (parseReservedVar <* sorc) <*> (Lit . IntL <$> int))
        <|> try (Assignment <$> (parseReservedVar <* sorc) <*> (Lit . StringL <$> parseString))
        <|> try (Assignment <$> (parseToken <* sorc) <*> (parseEq *> sorc *> parser))

instance HasParser MusicL where
    parser =
            try (Assign <$> parser)
        <|> try (Note <$> parser <*> optionMaybe parser <*> many parser)
        <|> try (Rest <$> parser <*> optionMaybe parser)
        <|> try (Chord <$> (bracket' '<' '>' (endBy parser sorc)) <*> optionMaybe parser <*> many parser)
        <|> try (Bar <$> parser)
        <|> try (string "\\clef" *> sorc *> (Clef <$> parser))
        <|> try (string "\\key" *> sorc *> (Key <$> ((,) <$> (parser <* sorc) <*> parser)))
        <|> try (string "\\time" *> sorc *> (Time <$> ((,) <$> int <*> (char '/' *> int))))
        <|> try (string "\\tempo" *> sorc *> (Tempo <$> (optionMaybe parseToken <* sorc) <*> (optionMaybe $ (,) <$> (parser <* sorc) <*> (parseEq *> sorc *> int))))
        <|> try (Sequential <$> braces' (endBy parser sorc))
        <|> try (Simultaneous True <$> brackSim (sepBy (parser <* sorc) (string "\\\\" <* sorc)))
        <|> try (Simultaneous False <$> brackSim (endBy parser sorc))
        <|> try (string "\\repeat" *> sorc *>
                ((Repeat <$> ((== "unfold") <$> (string "unfold" <|> string "volta") <* sorc) <*> (int <* sorc) <*> (parser <* sorc) <*> (optionMaybe $ string "\\alternative" *> sorc *> sepBy parser sorc))
                <|> (Tremolo <$> (string "tremolo" *> sorc *> int <* sorc) <*> parser)))
        <|> try (string "\\times" *> sorc *> (Times <$> (parseFrac <* sorc) <*> parser))
        <|> try (string "\\tuplet" *> sorc *> (Tuplet <$> (parseFrac <* sorc) <*> parser))
        <|> try (string "\\transpose" *> sorc *> (Transpose <$> parsePitch <*> (sorc *> parsePitch) <*> (sorc *> parser)))
        <|> try (string "\\relative" *> sorc *> (Relative <$> (optionMaybe parsePitch) <*> (sorc *> parser)))
        <|> try (string "\\with" *> sorc *> (With <$> braces' parser))
        <|> try (string "\\new" *> sorc *> choice [
                string "Voice" *> sorc *> (Voice <$> (optionMaybe $ parseEq *> sorc *> parseString) <*> (sorc *> parser)),
                parseStaffName *> sorc *> (Staff <$> (optionMaybe $ sorc *> parseString) <*> (sorc *> parser)),
                string "Lyrics" *> sorc *> string "\\lyricsto" *> (Lyrics <$> (sorc *> parseString) <*> (sorc *> parser)),
                New <$> (sorc *> parseToken <* sorc) <*> (optionMaybe $ parseEq *> sorc *> parseString) <*> (sorc *> parser)
            ])
        <|> try (string "\\context" *> sorc *> (Context <$> (sorc *> parseToken <* sorc) <*> (optionMaybe $ parseEq *> sorc *> parseString) <*> (sorc *> parser)))
        <|> try (Lit <$> parser)
        where
            brackSim = between (string "<<" <* sorc) (sorc *> string ">>")
            parseFrac = (%) <$> int <*> (char '/' *> int)

instance HasParser Score where
    parser = string "\\score" *> sorc *> (Score <$> braces' parser)

instance HasParser Header where
    parser = string "\\default" *> sorc *> braces' (fieldSetter <*> pure def)
        where
            parseField' (setter, field) = set setter . Just <$> (string field *> sorc *> parseEq *> sorc *> parser)
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

instance HasParser BookPart where
    parser = try (string "\\bookpart" *> sorc *> (BookPart <$> (optionMaybe parser <* sorc) <*> (sepBy parser sorc)))
             <|> (BookPart Nothing <$> (sepBy1 parser sorc))

instance HasParser Book where
    parser = try (Book Nothing <$> (sepBy1 parser sorc))
             <|> (string "\\book" *> sorc *> (Book <$> (optionMaybe parser <* sorc) <*> (sepBy parser sorc)))

instance HasParser TopLevel where
    parser =
            try (string "\\version" *> sorc *> (Version <$> parseString))
        <|> try (string "\\include" *> sorc *> (Include <$> parseString))
        <|> try (AssignmentTop <$> parser)
        <|> try (HeaderTop <$> parser)
        <|> (BookTop <$> parser)

instance HasParser Lilypond where
    parser = Lilypond <$> sepBy parser sorc


s = unsafePerformIO $ readFile "/Users/jeremander/Programming/Music/Parnassus/tunes/lilypond/BWV528/Adagio.ly"
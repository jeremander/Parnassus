{-|
This module handles reading and writing AnaMark @.tun@ files.
See [the specification](https://www.mark-henning.de/files/am/Tuning_File_V2_Doc.pdf).

A good tool for creating customized @.tun@ files is available here: [https://sevish.com/scaleworkshop](https://sevish.com/scaleworkshop)
-}

{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Music.Tuning.Tun where

import Control.Applicative ((<|>))
import Data.Attoparsec.Text (Parser)
import qualified Data.Attoparsec.Text as A
import Data.Char (toLower)
import Data.Either (rights)
import Data.List (foldl')
import qualified Data.Map as M
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Scientific (floatingOrInteger, Scientific, toRealFloat)
import Data.Sort (sortOn)
import Data.Text (Text, pack, strip, unpack)
import Prelude hiding (id)
import System.FilePath.Posix (takeBaseName)
import Text.Pretty ((</>))
import qualified Text.Pretty as P

import Misc.Utils (IdxOrSpan, parseIdxOrSpans, safeHead)
import Music.Tuning (NamedTuning(NamedTuning), Tuning(..))


-- ** @.tun@ Data

-- | A comment line.
newtype Comment = Comment String
    deriving (Eq, Show)

instance P.Pretty Comment where
    pretty (Comment comment) = P.string $ ";" ++ comment

-- | A key in a key/value pair.
type Key = String

-- | A value in a key/value pair. Can be a number or a quoted string.
data Val = IntVal Integer | SciVal Scientific | StringVal String
    deriving (Eq, Show)

getIntVal :: Val -> Maybe Integer
getIntVal (IntVal val) = Just val
getIntVal _            = Nothing

getSciVal :: Val -> Maybe Scientific
getSciVal (SciVal val) = Just val
getSciVal _            = Nothing

getStringVal :: Val -> Maybe String
getStringVal (StringVal val) = Just val
getStringVal _               = Nothing

instance P.Pretty Val where
    pretty (IntVal val)    = P.pretty val
    pretty (SciVal val)    = P.string $ show val
    pretty (StringVal val) = P.string $ "\"" ++ val ++ "\""

-- | A key/value pair.
newtype KeyVal = KeyVal (Key, Val)
    deriving (Eq, Show)

instance P.Pretty KeyVal where
    pretty (KeyVal (key, val)) = P.string (key ++ " = ") <> P.pretty val

-- | A section title.
newtype SectionTitle = SectionTitle String
    deriving (Eq, Show)

instance P.Pretty SectionTitle where
    pretty (SectionTitle title) = P.string $ "[" ++ title ++ "]"

-- | A line within a section. Can be a comment or a key/value pair.
data SectionItem = SectionComment Comment | SectionKeyVal KeyVal
    deriving (Eq, Show)

instance P.Pretty SectionItem where
    pretty (SectionComment comment) = P.pretty comment
    pretty (SectionKeyVal keyVal)   = P.pretty keyVal

-- | A section of a @.tun@ file, consisting of a title and 'SectionItem's.
data Section = Section {
    title :: SectionTitle,
    items :: [SectionItem]
} deriving (Eq, Show)

instance P.Pretty Section where
    pretty (Section {title, items}) = P.pretty title </> P.vcat (P.pretty <$> items)

-- | A @.tun@ file, consisting of multiple 'Section's.
newtype Tun = Tun [Section]
    deriving (Eq, Show)

instance P.Pretty Tun where
    pretty (Tun sections) = P.vcat $ P.pretty <$> sections

-- ** Parsing

skip :: Parser ()
skip = A.skipSpace

newlines :: Parser ()
newlines = A.many1' A.endOfLine >> pure ()

isKeyChar :: Char -> Bool
isKeyChar c = c `notElem` ("\n=" :: String)

parseComment :: Parser Comment
parseComment = Comment <$> (A.char ';' *> A.many' (A.notChar '\n'))

parseString :: Parser String
parseString = A.char '"' *> A.many' (A.notChar '"') <* A.char '"'

parseVal :: Parser Val
parseVal = do
    val <- Left <$> A.scientific <|> Right <$> parseString
    return $ case val of
        Left num -> case floatingOrInteger num of
            Left _    -> SciVal num
            Right int -> IntVal int
        Right s  -> StringVal s

parseKeyVal :: Parser KeyVal
parseKeyVal = do
    key <- strip . pack <$> A.many1 (A.satisfy isKeyChar)
    skip *> A.char '=' *> skip
    val <- parseVal
    return $ KeyVal (unpack key, val)

parseSectionTitle :: Parser SectionTitle
parseSectionTitle = SectionTitle <$> (A.char '[' *> A.many' (A.notChar ']') <* A.char ']')

parseSectionItem :: Parser SectionItem
parseSectionItem = SectionComment <$> parseComment <|> SectionKeyVal <$> parseKeyVal

parseSection :: Parser Section
parseSection = Section <$> parseSectionTitle <*> (skip *> A.sepBy parseSectionItem newlines)

parseCommentOrSection :: Parser (Either Comment Section)
parseCommentOrSection = A.eitherP parseComment parseSection

parseTun :: Parser Tun
parseTun = Tun . rights <$> (A.many' A.endOfLine *> A.sepBy parseCommentOrSection newlines <* A.many' A.endOfLine)

parseFull :: Parser a -> Text -> Either String a
parseFull parser = A.parseOnly $ parser <* A.endOfInput


-- ** Interpretation

-- | Case insensitive string equality.
lowEq :: String -> String -> Bool
lowEq s1 s2 = (toLower <$> s1) == (toLower <$> s2)

data Scale = Scale {
    format :: String,
    formatVersion :: Integer,
    formatSpecs :: String
} deriving (Eq, Show)

data Info = Info {
    name :: String,
    id :: String,
    filename :: String,
    author :: String,
    location :: String,
    contact :: String,
    date :: String,
    editor :: String,
    editorSpecs :: String,
    description :: String,
    keyword :: [String],
    history :: String,
    geography :: String,
    instrument :: String,
    composition :: [String],
    comments :: String
} deriving (Eq, Show)

newtype Assignment = Assignment [IdxOrSpan]
    deriving (Eq, Show)

-- | Adjustments (in cents from base frequency) for each MIDI note 0-127.
newtype TuningCents = TuningCents [Scientific]
    deriving (Eq, Show)

data SectionData =
      SecScaleBegin Scale
    | SecScaleEnd
    | SecInfo Info
    | SecAssignment Assignment
    | SecTuning TuningCents
    | SecExactTuning Scientific TuningCents
    | SecEditorSpecifics [SectionItem]
    deriving (Eq, Show)

newtype TunData = TunData [SectionData]
    deriving (Eq, Show)

-- | Gets all the values for a particular key in a list of 'SectionItem's.
lookupItems :: Key -> [SectionItem] -> [Val]
lookupItems key = mapMaybe go
    where
        go (SectionComment _)                   = Nothing
        go (SectionKeyVal (KeyVal (key', val))) = if (key' `lowEq` key) then Just val else Nothing

-- | Gets all the integer values for a particular key in a list of 'SectionItem's.
lookupIntItems :: Key -> [SectionItem] -> [Integer]
lookupIntItems key items = mapMaybe getIntVal (lookupItems key items)

-- | Gets all the 'Scientific' values for a particular key in a list of 'SectionItem's.
lookupSciItems :: Key -> [SectionItem] -> [Scientific]
lookupSciItems key items = mapMaybe getSciVal (lookupItems key items)

-- | Gets all the string values for a particular key in a list of 'SectionItem's.
lookupStrItems :: Key -> [SectionItem] -> [String]
lookupStrItems key items = mapMaybe getStringVal (lookupItems key items)

-- | Gets the first value for a particular key in a list of 'SectionItem's.
lookupItem :: Key -> [SectionItem] -> Maybe Val
lookupItem key = safeHead . lookupItems key

-- | Gets the first integer value for a particular key in a list of 'SectionItem's.
lookupIntItem :: Key -> [SectionItem] -> Maybe Integer
lookupIntItem key = safeHead . lookupIntItems key

-- | Gets the first 'Scientific' value for a particular key in a list of 'SectionItem's.
lookupSciItem :: Key -> [SectionItem] -> Maybe Scientific
lookupSciItem key = safeHead . lookupSciItems key

-- | Gets the first string value for a particular key in a list of 'SectionItem's.
lookupStrItem :: Key -> [SectionItem] -> Maybe String
lookupStrItem key = safeHead . lookupStrItems key

getIntItem :: Key -> Integer -> [SectionItem] -> Integer
getIntItem key def = fromMaybe def . lookupIntItem key

getSciItem :: Key -> Scientific -> [SectionItem] -> Scientific
getSciItem key def = fromMaybe def . lookupSciItem key

getStrItem :: Key -> String -> [SectionItem] -> String
getStrItem key def = fromMaybe def . lookupStrItem key

interpretSecScaleBegin :: [SectionItem] -> SectionData
interpretSecScaleBegin items = SecScaleBegin $ Scale {
    format = getStrItem "Format" "AnaMark-TUN" items,
    formatVersion = getIntItem "FormatVersion" 100 items,
    formatSpecs = getStrItem "FormatSpecs" "http://www.mark-henning.de/eternity/tuningspecs.html" items
    }

interpretSecInfo :: [SectionItem] -> SectionData
interpretSecInfo items = SecInfo $ Info {
    name = getStrItem "Name" "" items,
    id = getStrItem "ID" "ID_" items,
    filename = getStrItem "Filename" "" items,
    author = getStrItem "Author" "" items,
    location = getStrItem "Location" "" items,
    contact = getStrItem "Contact" "" items,
    date = getStrItem "Date" "" items,
    editor = getStrItem "Editor" "" items,
    editorSpecs = getStrItem "EditorSpecs" "" items,
    description = getStrItem "Description" "" items,
    keyword = lookupStrItems "Keyword" items,
    history = getStrItem "History" "" items,
    geography = getStrItem "Geography" "" items,
    instrument = getStrItem "Instrument" "" items,
    composition = lookupStrItems "Composition" items,
    comments = getStrItem "Comments" "" items
}

interpretSecAssignment :: [SectionItem] -> SectionData
interpretSecAssignment items = SecAssignment $ Assignment $ fromMaybe [] $ do
    channelStr <- lookupStrItem "MIDIChannels" items
    case A.parseOnly parseIdxOrSpans (pack channelStr) of
        Left _ -> Nothing
        Right idxOrSpans -> Just idxOrSpans

parseNoteNum :: A.Parser (Maybe Int)
parseNoteNum = do
    i <- A.string "note " *> A.decimal
    return $ if (i <= 127) then (Just i) else Nothing

getCentMap :: [SectionItem] -> M.Map Int Scientific
getCentMap items = centMap
    where
        insertCents tbl noteNum (Just cents) = if (noteNum <= 127) then M.insert noteNum cents tbl else tbl
        insertCents tbl _ Nothing            = tbl
        getCents (IntVal cents) = Just $ fromIntegral cents
        getCents (SciVal cents) = Just cents
        getCents _              = Nothing
        go tbl (SectionComment _) = tbl
        go tbl (SectionKeyVal (KeyVal (key, val))) = case A.parseOnly parseNoteNum (pack key) of
            Right (Just noteNum) -> insertCents tbl noteNum (getCents val)
            _                    -> tbl
        centMap = foldl' go M.empty items

interpretSecTuning :: [SectionItem] -> SectionData
interpretSecTuning items = SecTuning $ TuningCents cents
    where
        centMap = getCentMap items
        cents = [fromMaybe (fromIntegral $ 100 * noteNum) (M.lookup noteNum centMap) | noteNum <- [0..127]]

interpretSecExactTuning :: [SectionItem] -> Maybe SectionData
interpretSecExactTuning items = if M.null centMap then Nothing else Just (SecExactTuning baseFreq cents)
    where
        baseFreq = getSciItem "BaseFreq" (read "8.1757989156437073336") items
        centMap = getCentMap items
        -- auto-completion for periodic scales
        maxNoteNum = maximum $ 0 : M.keys centMap
        maxNoteCents = M.findWithDefault 0 maxNoteNum centMap
        getCents noteNum
            | noteNum <= maxNoteNum = M.findWithDefault (fromIntegral $ 100 * noteNum) noteNum centMap
            | otherwise             = getCents (noteNum - maxNoteNum) + maxNoteCents
        cents = TuningCents $ getCents <$> [0..127]

interpretSection :: Section -> Maybe SectionData
interpretSection (Section {title = SectionTitle t, items})
    | t `lowEq` "Scale Begin"      = Just $ interpretSecScaleBegin items
    | t `lowEq` "Scale End"        = Just SecScaleEnd
    | t `lowEq` "Assignment"       = Just $ interpretSecAssignment items
    | t `lowEq` "Tuning"           = Just $ interpretSecTuning items
    | t `lowEq` "Exact Tuning"     = interpretSecExactTuning items
    | t `lowEq` "Editor Specifics" = Just $ SecEditorSpecifics items
    | otherwise                    = Nothing

-- | Extracts a name from 'TunData', if present.
tunDataName :: TunData -> Maybe String
tunDataName (TunData sections) = safeHead results
    where
        nameFromInfo (SecInfo info) = Just $ name info
        nameFromInfo _              = Nothing
        results = mapMaybe nameFromInfo sections

-- | Interprets a parsed 'Tun' object as 'TunData'.
interpretTun :: Tun -> TunData
interpretTun (Tun sections) = TunData $ mapMaybe interpretSection sections

-- | Loads a @.tun@ file as 'TunData'.
loadTun :: FilePath -> IO TunData
loadTun path = do
    text <- readFile path
    return $ case parseFull parseTun (pack text) of
        Left err  -> error $ "loading TUN file" ++ path ++ ": " ++ err
        Right tun -> interpretTun tun

-- ** 'Tuning' conversion

tuningFromCents :: Scientific -> TuningCents -> Tuning
tuningFromCents baseFreq (TuningCents cents) = Tuning [toRealFloat baseFreq * (2 ** (toRealFloat c / 1200)) | c <- cents]

-- | Returns a section's tuning data, if it is contained in the section, along with a priority indicator for the section (higher means more priority).
tuningFromSectionData :: SectionData -> Maybe (Int, Tuning)
tuningFromSectionData (SecTuning tuningCents)               = Just (0, tuningFromCents 0.0 tuningCents)
tuningFromSectionData (SecExactTuning baseFreq tuningCents) = Just (1, tuningFromCents baseFreq tuningCents)
tuningFromSectionData _                                     = Nothing

-- | Given 'TunData', attempts to extract a 'Tuning'.
tuningFromTunData :: TunData -> Either String Tuning
tuningFromTunData (TunData sections) = if null results
    then Left "TUN file must contain a valid [Tuning] or [Exact Tuning] section"
    else Right $ snd $ last results
    where results = sortOn fst $ mapMaybe tuningFromSectionData sections

-- | Loads a @.tun@ file as a 'NamedTuning'.
--   If no name is present in the file, uses the base name of the filename.
loadNamedTuning :: FilePath -> IO NamedTuning
loadNamedTuning path = do
    tunData <- loadTun path
    let name = fromMaybe (takeBaseName path) (tunDataName tunData)
    case tuningFromTunData tunData of
        Left err     -> fail err
        Right tuning -> return $ NamedTuning name tuning
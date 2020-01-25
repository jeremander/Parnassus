import Data.Foldable (foldl')
import Data.List.Split (splitOn)
import Data.Maybe
import Data.Ratio
import System.FilePath.Posix
import Text.Parsec

import Euterpea (AbsPitch, absPitch, Dur, Note1)

import Music.Lilypond
import Music.Pitch (toPitch)
import Music.Types.MusicT (MusicT(..))

import System.IO.Unsafe


inPath :: FilePath
inPath = "/Users/jeremander/Programming/Music/Parnassus/tunes/lilypond/BWV528/SonataIV-tmp.ly"

outPath :: FilePath
outPath = replaceBaseName inPath (base ++ "-twohands")
    where base = takeBaseName inPath

parseBwv528 :: IO (Lilypond NotePitch)
parseBwv528 = do
    s <- readFile inPath
    let st = defaultLilypondState {includePaths = takeDirectory inPath : includePaths defaultLilypondState}
    res <- runParserT parseLilypond st inPath s
    case res of
        Left err -> error "parse error"
        Right lp -> do
            putStrLn "Splicing include files..."
            let lp' = spliceIncludes lp
            return lp'

isClef :: MusicL' -> Bool
isClef (Clef _) = True
isClef _        = False

makeBassClef :: MusicL' -> MusicL'
makeBassClef (Clef _) = Clef $ StdClef Bass
makeBassClef x = x

stripClefs :: MusicL' -> MusicL'
stripClefs (Sequential xs) = Sequential $ filter (not . isClef) (stripClefs <$> xs)
-- define only the recursive cases that are needed
stripClefs (Simultaneous b xs) = Simultaneous b $ filter (not . isClef) (stripClefs <$> xs)
stripClefs (PartCombine x1 x2) = PartCombine (stripClefs x1) (stripClefs x2)
stripClefs (Times r x) = Times r $ stripClefs x
stripClefs (Relative p x) = Relative p $ stripClefs x
stripClefs x = x

type PitchRange = (AbsPitch, AbsPitch)

combinePitchRange :: PitchRange -> PitchRange -> PitchRange
combinePitchRange (p1, p2) (p1', p2') = (min p1 p1', max p2 p2')

pitchRangeFold :: [Maybe PitchRange] -> Maybe PitchRange
pitchRangeFold = foldl' f Nothing
    where
        f Nothing r = r
        f r Nothing = r
        f (Just r1) (Just r2) = Just $ combinePitchRange r1 r2

pitchRange' :: NotePitch -> PitchRange
pitchRange' x = (pc, pc)
    where pc = absPitch $ toPitch x

pitchRange :: MusicL' -> Maybe PitchRange
pitchRange (Note x _ _) = Just $ pitchRange' x
pitchRange (Chord xs _ _) = pitchRangeFold $ Just . pitchRange' <$> xs
pitchRange (Sequential xs) = pitchRangeFold $ pitchRange <$> xs
pitchRange (Simultaneous _ xs) = pitchRangeFold $ pitchRange <$> xs
pitchRange (PartCombine x1 x2) = pitchRangeFold $ pitchRange <$> [x1, x2]
pitchRange (Repeat _ _ x xs) = pitchRangeFold $ pitchRange x : (pitchRange <$> (concat $ maybeToList xs))
pitchRange (Tremolo _ x) = pitchRange x
pitchRange (Times _ x) = pitchRange x
pitchRange (Tuplet _ x) = pitchRange x
pitchRange (Relative _ x) = pitchRange x
pitchRange _ = Nothing

-- distance between highest and lowest pitch if two pitch ranges were to be combined
pitchRangeDistance :: Maybe PitchRange -> Maybe PitchRange -> Int
pitchRangeDistance Nothing _ = 0
pitchRangeDistance _ Nothing = 0
pitchRangeDistance (Just (p1, p2)) (Just (p1', p2')) = max p2 p2' - min p1 p1'

-- given bass, alto, soprano parts, returns True if alto is closer to bass than soprano
choosePart :: MusicL' -> MusicL' -> MusicL' -> Bool
choosePart bass alto sop = pitchRangeDistance br ar <= pitchRangeDistance ar sr
    where [br, ar, sr] = pitchRange <$> [bass, alto, sop]

-- combines three voices into two (acting on MusicL')
threeToTwo :: Dur -> [MusicL'] -> [MusicL']
threeToTwo d parts = [left, right]
    where
        [bass, alto, sop] = take 3 parts
        -- fix tuplet settings, which somehow break during part combination
        -- (triplets only occur in allegro section)
        tupletSettings = [TupletSpan $ Just $ fromRational (1 % 8), Assign $ PropAssignment $ OverrideSym' "TupletNumber" (Symbol "transparent") (BoolL True)]
        (Relative p (Sequential altoElts)) = alto
        -- split parts by measure, glue alto measures to their closest (bass or soprano) measure in terms of pitch distance
        alto' = Relative p (Sequential $ tupletSettings ++ (stripClefs <$> altoElts))
        [bassMeasures, altoMeasures, sopMeasures] = split d . fillDurations <$> [bass, alto', sop]
        measures = zip3 bassMeasures altoMeasures sopMeasures
        measures' = [if choosePart b a s then (Simultaneous True [b, a], s) else (b, Simultaneous True [a, s]) | (b, a, s) <- measures]
        (leftMeasures, rightMeasures) = unzip measures'
        (left, right) = (line leftMeasures, line rightMeasures)

getAssignment :: TopLevel' -> (String, MusicL')
getAssignment (AssignmentTop (Assignment name val)) = (name, val)

-- combines three voices into two (acting on TopLevel')
threeToTwo' :: Dur -> [TopLevel'] -> [TopLevel']
threeToTwo' d tops = [leftPart, rightPart]
    where
        [sop, alto, bass] = take 3 tops
        (sopName, sopPart) = getAssignment sop
        (altoName, altoPart) = getAssignment alto
        (_, bassPart) = getAssignment bass
        [left, right] = threeToTwo d [bassPart, altoPart, sopPart]
        leftPart = AssignmentTop $ Assignment altoName left
        rightPart = AssignmentTop $ Assignment sopName right

slice :: Int -> Int -> [a] -> [a]
slice start end = take (end - start) . drop start

-- reduces staves from three to two
reduceStaves :: Score' -> Score'
reduceStaves (Score scoreItems) = Score $ ScoreMusic (Simultaneous False manualElts) : tail scoreItems
    where
        (ScoreMusic (Simultaneous _ elts)) = head scoreItems
        -- convert treble to bass clef in left hand
        processStaffElt :: MusicL' -> Maybe MusicL'
        processStaffElt (New (NewStaff GrandStaff) _ _ (Simultaneous _ staves)) = Just elt'
            where
                leftStaff = staves !! 2
                (New _ _ _ (Sequential leftStaffElts)) = leftStaff
                -- deactivate added text from \partcombine
                noText = WithBlock (Assignment "printPartCombineTexts" (Lit (BoolL False)))
                leftStaffElts' = makeBassClef <$> leftStaffElts
                leftStaff' = New (NewStaff Staff) (Just "left") (Just noText) (Sequential leftStaffElts')
                elt' = New (NewStaff GrandStaff) Nothing Nothing (Simultaneous False $ take 2 staves ++ [leftStaff'])
        -- discard pedal staff
        processStaffElt (New (NewStaff Staff) (Just _) _ _) = Nothing
        processStaffElt elt = Just elt
        manualElts = mapMaybe processStaffElt elts

convertBwv528 :: Lilypond' -> Lilypond'
convertBwv528 lp = lp'
    where
        (Lilypond tops) = lp
        -- merge bass and alto parts for each section
        -- varIndices = [[6, 7, 8], [9, 10, 11], [15, 16, 17], [22, 23, 24]]
        introParts = threeToTwo' 1 $ slice 6 9 tops
        adagioParts = threeToTwo' (3 % 4) $ slice 9 12 tops
        andanteParts = threeToTwo' 1 $ slice 15 18 tops
        allegroParts = threeToTwo' (3 % 8) $ slice 22 25 tops
        -- reduce staves from three to two
        (BookTop (Book _ [BookPart _ scores])) = last tops
        -- bookTop = BookTop (Book Nothing [BookPart Nothing $ reduceStaves <$> scores])
        bookTop = BookTop (Book Nothing [BookPart Nothing $ reduceStaves <$> slice 0 1 scores])
        -- tops' = slice 0 6 tops ++ introParts ++ adagioParts ++ slice 12 15 tops ++ andanteParts ++ slice 18 22 tops ++ allegroParts ++ slice 25 27 tops ++ [bookTop]
        tops' = slice 0 6 tops ++ introParts ++ adagioParts ++ slice 25 27 tops ++ [bookTop]
        lp' = Lilypond tops'

main :: IO ()
main = do
    putStrLn $ "Reading Lilypond file " ++ inPath
    lp <- parseBwv528
    putStrLn "Converting..."
    let lp' = convertBwv528 lp
    writeLilypond lp' outPath
    putStrLn $ "Successfully saved to " ++ outPath


{-# NOINLINE test #-}
-- test :: ()
test = ()
    where
        lp = unsafePerformIO parseBwv528
        (Lilypond tops) = lp
        [sop, alto, bass] = snd . getAssignment <$> slice 22 25 tops
        -- TODO: memory of last duration must be better preserved
        (Relative _ (Sequential bad')) = sop
        bad = Sequential $ slice 26 33 bad'
        items = unLine bad
        bad2 = Sequential [items !! 0, items !! 6]
        [bassMeasures, altoMeasures, sopMeasures] = split (3 % 8) . fillDurations <$> [bass, alto, sop]
        measures = zip3 bassMeasures altoMeasures sopMeasures
        measures' = [if choosePart b a s then (Simultaneous True [b, a], s) else (b, Simultaneous True [a, s]) | (b, a, s) <- measures]


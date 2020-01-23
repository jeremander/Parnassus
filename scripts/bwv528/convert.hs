import Data.List.Split (splitOn)
import Data.Maybe
import Data.Ratio
import System.FilePath.Posix
import Text.Parsec

import Music.Lilypond


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

splitMeasures :: MusicL' -> [MusicL']
splitMeasures (Relative p x) = Relative p <$> splitMeasures x
splitMeasures (Sequential xs) = Sequential <$> splitOn [Bar BarCheck] xs
-- have a fallback
splitMeasures x = [x]

-- data MusicL
--     = Note NotePitch (Maybe Duration) [Expressive]     -- ^ Single note.
--     | Rest RestType (Maybe Duration) [Expressive]      -- ^ Single rest.
--     | Chord [NotePitch] (Maybe Duration) [Expressive]  -- ^ Single chord.
--     | Bar BarLine                                      -- ^ Bar line.
--     | Clef Clef                                        -- ^ Clef.
--     | Key Key                                          -- ^ Key signature.
--     | Time TimeSig                                     -- ^ Time signature.
--     | Tmp Tempo                                        -- ^ Tempo mark.
--     | Sequential [MusicL]                              -- ^ Sequential composition.
--     | Simultaneous Bool [MusicL]                       -- ^ Parallel composition (split voices?).
--     | PartCombine MusicL MusicL                        -- ^ Combines two musical expressions into single staff
--     | Repeat Bool Int MusicL (Maybe [MusicL])          -- ^ Repetition (unfold?, times, music, alternatives).
--     | Tremolo Int MusicL                               -- ^ Tremolo (multiplier).
--     | Times Rational MusicL                            -- ^ Stretch music (multiplier).
--     | Tuplet Rational MusicL                           -- ^ Tuplet.
--     | TupletSpan (Maybe Duration)                      -- \tupletSpan (duration)
--     | Transpose Pitch Pitch MusicL                     -- ^ Transpose music (from to).
--     | Relative (Maybe Pitch) MusicL                    -- ^ Use relative octave (octave).
--     | Lit Literal                                      -- ^ Single literal.
--     | Assign Assignment                                -- ^ Single assignment.
--     | New NewItem (Maybe String) (Maybe WithBlock) MusicL  -- ^ \new (Voice/Staff/Lyrics, etc.)
--     | Ctx Context                                      -- ^ Context expression.
--     | Var Variable                                     -- ^ Variable occurrence.
--     deriving (Eq, Show)

threeToTwo :: [MusicL'] -> [MusicL']
threeToTwo parts = twoParts
    where
        [bass, alto, sop] = take 3 parts
        -- fix tuplet settings, which somehow break during part combination
        -- (triplets only occur in allegro section)
        tupletSettings = [TupletSpan $ Just $ fromRational (1 % 8), Assign $ PropAssignment $ OverrideSym' "TupletNumber" (Symbol "transparent") (BoolL True)]
        (Relative p (Sequential altoElts)) = alto
        alto' = Relative p (Sequential $ tupletSettings ++ altoElts)
        -- strip all clef changes in alto & bass, merge them together
        twoParts = [stripClefs $ Simultaneous True [alto', bass], sop]
        -- the below is better, but tuplets are still messed up for now
        -- twoParts = [stripClefs $ PartCombine alto' bass, sop]
        -- TODO: split alto into two voices, glue one to bass, one to sop

getAssignment :: TopLevel' -> (String, MusicL')
getAssignment (AssignmentTop (Assignment name val)) = (name, val)

threeToTwo' :: [TopLevel'] -> [TopLevel']
threeToTwo' tops = [leftPart, rightPart]
    where
        [sop, alto, bass] = take 3 tops
        (sopName, sopPart) = getAssignment sop
        (altoName, altoPart) = getAssignment alto
        (_, bassPart) = getAssignment bass
        [left, right] = threeToTwo [bassPart, altoPart, sopPart]
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
        introParts = threeToTwo' $ slice 6 9 tops
        adagioParts = threeToTwo' $ slice 9 12 tops
        andanteParts = threeToTwo' $ slice 15 18 tops
        allegroParts = threeToTwo' $ slice 22 25 tops
        -- reduce staves from three to two
        (BookTop (Book _ [BookPart _ scores])) = last tops
        bookTop = BookTop (Book Nothing [BookPart Nothing $ reduceStaves <$> scores])
        tops' = slice 0 6 tops ++ introParts ++ adagioParts ++ slice 12 15 tops ++ andanteParts ++ slice 18 22 tops ++ allegroParts ++ slice 25 27 tops ++ [bookTop]
        lp' = Lilypond tops'
        -- lp' = lp

main :: IO ()
main = do
    putStrLn $ "Reading Lilypond file " ++ inPath
    lp <- parseBwv528
    putStrLn "Converting..."
    let lp' = convertBwv528 lp
    writeLilypond lp' outPath
    putStrLn $ "Successfully saved to " ++ outPath
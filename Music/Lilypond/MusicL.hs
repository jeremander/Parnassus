{-# LANGUAGE
    FlexibleInstances,
    InstanceSigs,
    MultiParamTypeClasses,
    OverloadedStrings
#-}

module Music.Lilypond.MusicL where

import Data.Char (isAlpha, toUpper)
import Data.List (nub, sort)
import Data.Maybe (fromMaybe)
import Control.Monad (liftM2)
import Data.Ratio ((%), denominator, numerator)
import Data.Tuple.Select (sel3)
import qualified Text.Pretty as P
import Text.Pretty (Pretty(..), Printer, (<+>), (<//>), char, hsep, nest, sep, sepByS, string, vcat)
import qualified Euterpea as E
import Euterpea (Music(..), Pitch, PitchClass(..), mFold)

import Misc.Utils (notImpl)
import Music.Lilypond.Literal (Literal(..), Markup(..), MarkupExpr(..), Tweak)
import Music.Lilypond.Symbols (BarLine, Clef, Expressive(..), Staff)
import Music.Pitch (FromPitch(..), PrettyPitch(..), ToPitch(..), Key, staffDistance, trans)
import Music.Rhythm (Duration(..), TimeSig, splitDur)
import Music.Types.MusicT (MusicT(..), ToMidi(..))


-- Utilities

-- capitalizes the first letter of a string
cap :: String -> String
cap cs = [if (i == 0) then toUpper c else c | (i, c) <- zip [0..] cs]

nubSort :: (Ord a) => [a] -> [a]
nubSort = nub . sort

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

-- tempo name (e.g. allegro), and a specification of what note duration is one beat, along with BPM
data Tempo = Tempo (Maybe String) (Maybe (Duration, Integer))
    deriving (Eq, Show)

instance Pretty Tempo where
    pretty (Tempo Nothing Nothing)          = mempty
    pretty (Tempo (Just t) Nothing)         = "\\tempo" <+> pretty t
    pretty (Tempo Nothing (Just (d, bpm)))  = "\\tempo" <+> pretty d <+> "=" <+> pretty bpm
    pretty (Tempo (Just t) (Just (d, bpm))) = "\\tempo" <+> pretty t <+> pretty d <+> "=" <+> pretty bpm

data RestType
    = StdRest  -- ^ standard rest
    | FullRest -- ^ multi-measure rest
    | Skip     -- ^ invisible rest
    deriving (Eq, Ord, Show)

instance Pretty RestType where
    pretty StdRest  = "r"
    pretty FullRest = "R"
    pretty Skip     = "s"

data Assignment a = Assignment String (MusicL a)  -- foo = {a b c}
            | SymbAssignment String Literal (MusicL a)  -- foo #'bar = baz
            | PropAssignment Tweak  -- \override, etc.
            | Set (Assignment a) -- \set ...
            | Once (Assignment a)  -- \once ...
    deriving (Eq, Show)

instance (PrettyPitch a) => PrettyPitch (Assignment a) where
    prettyPitch lang (Assignment s e) = string s <+> "=" <+> prettyPitch lang e
    prettyPitch lang (SymbAssignment s l e) = string s <+> pretty l <+> "=" <+> prettyPitch lang e
    prettyPitch _    (PropAssignment t) = pretty t
    prettyPitch lang (Set a) = "\\set" <+> prettyPitch lang a
    prettyPitch lang (Once a) = "\\once" <+> prettyPitch lang a

newtype WithBlock a = WithBlock (Assignment a)
    deriving (Eq, Show)

instance (PrettyPitch a) => PrettyPitch (WithBlock a) where
    prettyPitch lang (WithBlock a) = "\\with {" <+> (prettyPitch lang a) <+> "}"

data NewItem = Voice | NewStaff Staff | Lyrics | NewItem String
    deriving (Eq, Show)

instance Pretty NewItem where
    pretty Voice            = "Voice"
    pretty (NewStaff staff) = pretty staff
    pretty Lyrics           = "Lyrics \\lyricsto"
    pretty (NewItem s)      = string s

-- \context [type] [name] [expression]
data Context a = Context (Maybe (String, (Maybe String))) (MusicL a)
    deriving (Eq, Show)

instance (PrettyPitch a) => PrettyPitch (Context a) where
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
data MusicL a =
      Note a (Maybe Duration) [Expressive]             -- ^ Single note.
    | Rest RestType (Maybe Duration) [Expressive]      -- ^ Single rest.
    | Chord [a] (Maybe Duration) [Expressive]          -- ^ Single chord.
    | Bar BarLine                                      -- ^ Bar line.
    | Clef Clef                                        -- ^ Clef.
    | Key Key                                          -- ^ Key signature.
    | Time TimeSig                                     -- ^ Time signature.
    | Tmp Tempo                                        -- ^ Tempo mark.
    | Sequential [MusicL a]                            -- ^ Sequential composition.
    | Simultaneous Bool [MusicL a]                     -- ^ Parallel composition (split voices?).
    | PartCombine (MusicL a) (MusicL a)                -- ^ Combines two musical expressions into single staff
    | Repeat Bool Int (MusicL a) (Maybe [MusicL a])    -- ^ Repetition (unfold?, times, music, alternatives).
    | Tremolo Int (MusicL a)                           -- ^ Tremolo (multiplier).
    | Times Rational (MusicL a)                        -- ^ Stretch music (multiplier).
    | Tuplet Rational (MusicL a)                       -- ^ Tuplet.
    | TupletSpan (Maybe Duration)                      -- \tupletSpan (duration)
    | Transpose Pitch Pitch (MusicL a)                 -- ^ Transpose music (from to).
    | Relative (Maybe Pitch) (MusicL a)                -- ^ Use relative octave (octave).
    | Lit Literal                                      -- ^ Single literal.
    | Assign (Assignment a)                            -- ^ Single assignment.
    | New NewItem (Maybe String) (Maybe (WithBlock a)) (MusicL a)  -- ^ \new (Voice/Staff/Lyrics, etc.)
    | Ctx (Context a)                                  -- ^ Context expression.
    | Var Variable                                     -- ^ Variable occurrence.
    deriving (Eq, Show)

infixl <=>
a <=> b = sep [a,b]

fracPrinter :: Rational -> Printer
fracPrinter r = pretty (numerator r) <> "/" <> pretty (denominator r)

instance (PrettyPitch a) => PrettyPitch (MusicL a) where
    prettyPitch lang (Note p d exps) = prettyPitch lang p <> pretty d <> prettyList exps
    prettyPitch _  (Rest t d exps) = pretty t <> pretty d <> prettyList exps
    prettyPitch lang (Chord ns d exps) = (char '<' <+> nest 2 (sepByS "" $ prettyPitch lang <$> ns) <+> char '>') <> pretty d <> prettyList exps
    prettyPitch _ (Bar b) = pretty b
    prettyPitch _ (Clef c) = "\\clef" <+> pretty c
    prettyPitch lang (Key (p, m)) = "\\key" <+> prettyPitch lang p <+> pretty m
    prettyPitch _ (Time (m, n)) = "\\time" <+> (pretty m <> "/" <> pretty n)
    prettyPitch _ (Tmp tempo) = pretty tempo
    prettyPitch lang (Sequential xs) = "{" <+> (hsep . fmap (prettyPitch lang)) xs <+> "}"
    prettyPitch lang (Simultaneous b xs) = "<<" <//> nest 2 ((sepFunc . fmap (prettyPitch lang)) xs) <//> ">>"
        where sepFunc = if b then sepByS " \\\\" else vcat
    prettyPitch lang (PartCombine x1 x2) = "\\partcombine" <+> prettyPitch lang x1 <+> prettyPitch lang x2
    prettyPitch lang (Repeat unfold times x alts) = "\\repeat" <=> unf unfold <=> P.int times <=> prettyPitch lang x <=> alt alts
        where
            unf p = if p then "unfold" else "volta"
            alt Nothing      = P.empty
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

-- convenience aliases

type Assignment' = Assignment NotePitch
type WithBlock' = WithBlock NotePitch
type Context' = Context NotePitch
type MusicL' = MusicL NotePitch

-- MusicT instance

-- extracts data from Note (pitch data, duration, presence of a tie)
getNoteData :: MusicL a -> Maybe (a, Maybe E.Dur, Bool)
getNoteData (Note x d exprs) = Just (x, toRational <$> d, Tie `elem` exprs)
getNoteData m                = Nothing

dur' :: MusicL a -> Maybe Duration
dur' (Note _ d _) = d
dur' (Rest _ d _) = d
dur' (Chord _ d _) = d
dur' x = Nothing

instance (FromPitch a, ToPitch a) => MusicT MusicL a where
    toMusic :: MusicL a -> Music a
    toMusic note@(Note {}) = E.Prim $ E.Note (toRational d') x
        where
            (Just (x, d, hasTie)) = getNoteData note
            d' = fromMaybe (error "no duration") d
    toMusic (Rest _ d _) = E.Prim $ E.Rest (toRational d')
        where d' = fromMaybe (error "no duration") d
    toMusic (Chord xs d exprs) = E.chord [toMusic $ Note x d exprs | x <- xs]
    toMusic (Key (pc, mode)) = E.Modify (E.KeySig pc mode) empty
    toMusic (Sequential xs) = foldr (:+:) empty $ toMusic <$> xs
    toMusic (Simultaneous _ xs) = foldr (:=:) empty $ toMusic <$> xs
    toMusic (PartCombine x y) = toMusic x :=: toMusic y
    toMusic (Repeat _ n x alts) = foldr (:+:) empty $ toMusic <$> xs
        where
            heads = replicate n x
            tails = case alts of
                Nothing    -> repeat empty
                Just alts' -> replicate (max 0 $ n - length alts') empty ++ alts'
            xs = concat [[h, t] | (h, t) <- zip heads tails]
    toMusic (Times r x) = E.Modify (E.Tempo (1 / r)) (toMusic x)
    toMusic (Tuplet r x) = E.Modify (E.Tempo r) (toMusic x)
    toMusic (Transpose p1 p2 x) = E.Modify (E.Transpose (E.absPitch p2 - E.absPitch p1)) (toMusic x)
    toMusic (Relative _ x) = toMusic x
    toMusic (New _ _ _ x) = toMusic x
    toMusic (Ctx (Context _ x)) = toMusic x
    toMusic _ = empty
    fromMusic :: Music a -> MusicL a
    fromMusic = mFold prim (/+/) (/=/) modify
    prim :: E.Primitive a -> MusicL a
    prim (E.Note d p) = case durs of
        []   -> empty
        [d'] -> Note p (Just d') []
        _    -> Sequential [Note p (Just d') expr | (d', expr) <- zip durs exprs]
        where
            durs = splitDur d
            exprs = [[Tie] | _ <- init durs] ++ [[]]
    prim (E.Rest d) = case durs of
        []   -> empty
        [d'] -> Rest StdRest (Just d') []
        _    -> Sequential [Rest StdRest (Just d') [] | d' <- durs]
        where durs = splitDur d
    empty :: MusicL a
    empty = Sequential []
    modify :: E.Control -> MusicL a -> MusicL a
    modify (E.Tempo r) = Times (1 / r)
    modify (E.Transpose ap) = Transpose (C, 4) (trans ap (C, 4))
    modify (E.Instrument inst) = id  -- TODO: implement this (convert from enum to Lilypond)
    modify (E.Phrase attrs) = \x -> foldr modify' x attrs
        where
            -- TODO: dynamics/articulation conversion is tricky since Lilypond always puts it after the note
            -- thus conversion may only be possible if the Modify applies to a single note
            modify' attr = id
    modify (E.KeySig pc mode) = (/+/) $ Key (pc, mode)
    modify (E.Custom s) = (/+/) $ Lit (MarkupL (MarkupExpr (MarkupText s)))
    (/+/) :: MusicL a -> MusicL a -> MusicL a
    (/+/) x1 x2 = Sequential (unLine x1 ++ unLine x2)
    (/=/) :: MusicL a -> MusicL a -> MusicL a
    (/=/) x1 x2 = Simultaneous False (unChord x1 ++ unChord x2)
    (/*/) :: MusicL a -> Int -> MusicL a
    (/*/) x n = Repeat False n x Nothing
    line :: Eq a => [MusicL a] -> MusicL a
    line xs = Sequential $ foldr go [] xs'
        where
            xs' = filter (not . isEmpty) $ concatMap unLine xs
            pitchSet ps = nubSort $ toPitch <$> ps
            removeTies = filter (not . (== Tie))
            maybePlus = liftM2 (+)
            -- gets initial set of pitches
            pitches (Note p _ _) = [toPitch p]
            pitches (Chord ps _ _) = pitchSet ps
            pitches (Simultaneous _ ys) = nubSort $ concatMap pitches ys
            pitches _ = []
            -- whether element is tied to the next one
            isTied (Note _ _ exprs) = Tie `elem` exprs
            isTied (Chord _ _ exprs) = Tie `elem` exprs
            isTied (Sequential ys@(_:_)) = isTied $ last ys
            isTied (Simultaneous _ ys) = all isTied ys
            isTied _ = False
            -- extends the duration of the first note of a musical element
            extend d1 (Note p d2 exprs) = Note p (maybePlus d1 d2) exprs
            extend d1 (Rest rt d2 exprs) = Rest rt (maybePlus d1 d2) exprs
            extend d1 (Chord ps d2 exprs) = Chord ps (maybePlus d1 d2) exprs
            extend d1 (Simultaneous b ys) = Simultaneous b (extend d1 <$> ys)
            -- if same note or chord is tied to another, extend its duration
            go x@(Note p1 d1 exprs1) (y@(Note p2 d2 exprs2):ys) = if isTied x && (p1 == p2)
                then (Note p2 (maybePlus d1 d2) (nubSort $ removeTies exprs1 ++ exprs2)) : ys
                else x : y : ys
            go x@(Chord ps1 d1 exprs1) (y@(Chord ps2 d2 exprs2):ys) = if isTied x && (pitchSet ps1 == pitchSet ps2)
                then (Chord ps1 (maybePlus d1 d2) (nubSort $ removeTies exprs1 ++ exprs2)) : ys
                else x : y : ys
            go x@(Chord ps1 d1 exprs1) (y@(Simultaneous {}):ys) = if isTied x && (pitches x == pitches y)
                then extend d1 y : ys
                else x : y : ys
            -- TODO: fix this?
            go x@(Simultaneous {}) (y@(Simultaneous {}):ys) = if isTied x && (pitches x == pitches y)
                then extend (dur' x) y : ys
                else x : y : ys
            -- if two rests are in succession, join them
            go x@(Rest t1 d1 exprs1) (y@(Rest t2 d2 exprs2):ys) = if (t1 == t2)
                then (Rest t1 (maybePlus d1 d2) (nubSort $ exprs1 ++ exprs2)) : ys
                else x : y : ys
            -- default
            go x ys = x : ys
    chord :: Eq a => [MusicL a] -> MusicL a
    chord xs = chord' $ foldr go [] xs'
        where
            xs' = filter (not . isEmpty) $ concatMap unChord xs
            mkNoteChord [p] d exprs = Note (fromPitch p) d exprs
            mkNoteChord ps d exprs  = Chord (fromPitch <$> ps) d exprs
            -- if two notes/chords have the same duration & expressives, merge them into a chord
            go x@(Note p1 d1 exprs1) (y@(Note p2 d2 exprs2):ys) = if (d1 == d2) && (exprs1 == exprs2)
                then mkNoteChord (nubSort $ toPitch <$> [p1, p2]) d1 exprs1 : ys
                else x : y : ys
            go x@(Note p1 d1 exprs1) (y@(Chord ps2 d2 exprs2):ys) = if (d1 == d2) && (exprs1 == exprs2)
                then mkNoteChord (nubSort $ toPitch <$> p1 : ps2) d1 exprs1 : ys
                else x : y : ys
            go x@(Chord ps1 d1 exprs1) (y@(Note p2 d2 exprs2):ys) = if (d1 == d2) && (exprs1 == exprs2)
                then mkNoteChord (nubSort $ toPitch <$> p2 : ps1) d1 exprs1 : ys
                else x : y : ys
            go x@(Chord ps1 d1 exprs1) (y@(Chord ps2 d2 exprs2):ys) = if (d1 == d2) && (exprs1 == exprs2)
                then mkNoteChord (nubSort $ toPitch <$> ps1 ++ ps2) d1 exprs1 : ys
                else x : y : ys
            -- if a rest overlaps something with the same duration, remove the rest
            go x@(Rest {}) (y:ys) = if (dur x == dur y) then y : ys else x : y : ys
            go x (y@(Rest {}):ys) = if (dur x == dur y) then x : ys else x : y : ys
            -- extract singleton Sequentials
            go (Sequential [x]) ys = go x ys
            -- default
            go x ys = x : ys
            chord' [] = empty
            chord' [y] = y
            chord' ys = Simultaneous False ys
    unLine :: MusicL a -> [MusicL a]
    unLine (Sequential xs) = xs
    unLine x = [x]
    unChord :: MusicL a -> [MusicL a]
    unChord (Simultaneous _ xs) = xs
    unChord x = [x]
    dur :: MusicL a -> E.Dur
    dur (Note _ d _) = toRational $ fromMaybe (error "no duration") d
    dur (Rest _ d _) = toRational $ fromMaybe (error "no duration") d
    dur (Chord _ d _) = toRational $ fromMaybe (error "no duration") d
    dur (Tmp _) = notImpl "Tempo mark"
    dur (Sequential xs) = sum $ dur <$> xs
    dur (Simultaneous _ xs) = maximum $ dur <$> xs
    dur (PartCombine x y) = max (dur x) (dur y)
    dur (Repeat _ n x alts) = fromIntegral n * dur x + d
        where
            d = case alts of
                Nothing    -> 0
                Just alts' -> sum $ dur <$> take n alts'
    dur (Tremolo n x) = fromIntegral n * dur x
    dur (Times r x) = r * dur x
    dur (Tuplet r x) = (dur x) / r
    dur (Transpose _ _ x) = dur x
    dur (Relative _ x) = dur x
    dur (New _ _ _ x) = dur x
    dur (Ctx (Context _ x)) = dur x
    dur _ = 0
    bisect :: Eq a => E.Dur -> MusicL a -> (MusicL a, MusicL a)
    bisect d m | d <= 0            = (empty, m)
    bisect d (Note x d' exprs) = (Note x (f1 <$> d') exprs', Note x (fromRational <$> d'') exprs)
        where
            f1 = fromRational . min d . toRational
            d'' = max 0 . subtract d . toRational <$> d'
            exprs' = case d'' of
                Nothing -> exprs
                Just d2 -> if (d2 > 0) then Tie : exprs else exprs
    bisect d (Rest rt d' exprs) = (Rest rt (f1 <$> d') exprs, Rest rt (f2 <$> d') exprs)
        where (f1, f2) = (fromRational . min d . toRational, fromRational . max 0 . subtract d . toRational)
    bisect d (Chord xs d' exprs) = (Chord xs (f1 <$> d') exprs', Chord xs (fromRational <$> d'') exprs)
        where
            f1 = fromRational . min d . toRational
            d'' = max 0 . subtract d . toRational <$> d'
            exprs' = case d'' of
                Nothing -> exprs
                Just d2 -> if (d2 > 0) then Tie : exprs else exprs
    bisect d (Sequential []) = (empty, empty)
    bisect d (Sequential ms) = (line left', line right')
        where
            durs = dur <$> ms
            cumDurs = scanl (+) 0 durs
            items = zip3 (tail cumDurs) durs ms
            (items1, items2) = span (\(cd, _, _) -> cd - d <= 0) items
            emptyRight = null items2
            (cd', d', mid) = if emptyRight then (0, 0, empty) else (head items2)
            (midLeft, midRight) = bisect (d' + d - cd') mid
            (left, right) = (sel3 <$> items1, sel3 <$> items2)
            left' = if (isEmpty midLeft) then left else (left ++ [midLeft])
            right' = midRight : (if emptyRight then [] else tail right)
    bisect d (Simultaneous b ms) = (Simultaneous b lefts, Simultaneous b rights)
        where (lefts, rights) = unzip $ bisect d <$> ms
    bisect d (PartCombine m1 m2) = (PartCombine left1 left2, PartCombine right1 right2)
        where
            (left1, right1) = bisect d m1
            (left2, right2) = bisect d m2
    bisect d (Repeat _ n mus alts) = bisect d (Sequential $ replicate n mus ++ fromMaybe [] alts)
    bisect d (Tremolo n mus) = bisect d (Sequential $ replicate n mus)
    bisect d (Times r mus) = (Times r left, Times r right)
        where (left, right) = bisect (d / r) mus
    bisect d (Tuplet r mus) = (Tuplet r left, Tuplet r right)
        where (left, right) = bisect (d * r) mus
    bisect d (Transpose p1 p2 mus) = (Transpose p1 p2 left, Transpose p1 p2 right)
        where (left, right) = bisect d mus
    -- TODO: detect last note on left side to set correct relative pitch on right side
    bisect d x@(Relative _ _) = bisect d $ unRelative x
    bisect d x = (x, empty)
    transpose :: E.AbsPitch -> MusicL a -> MusicL a
    transpose i = Transpose pc1 pc2
        where
            pc1 = (C, 4)
            pc2 = E.pitch $ E.absPitch pc1 + i

instance ToMidi MusicL

-- durations are optional for notes and rests, but they can be inferred from the preceding note
-- this function fills in the missing durations
fillDurations :: (FromPitch a, ToPitch a) => MusicL a -> MusicL a
fillDurations = go Nothing
    where
        go d = snd . fillDur d
        fillDur d (Note p Nothing exprs) = (d, Note p d exprs)
        fillDur d (Rest rt Nothing exprs) = (d, Rest rt d exprs)
        fillDur d (Chord ys Nothing exprs) = (d, Chord ys d exprs)
        fillDur d (Sequential xs) = (d', Sequential $ snd <$> pairs)
            where
                fillDur' (d, _) x = fillDur d x
                pairs = tail $ scanl fillDur' (d, empty) xs
                d' = fst $ last pairs
        fillDur d (Simultaneous b xs) = (Nothing, Simultaneous b $ go d <$> xs)
        fillDur d (PartCombine x1 x2) = (Nothing, PartCombine (go d x1) (go d x2))
        fillDur d (Repeat b n x xs) = (Nothing, Repeat b n (go d x) (fmap (go d) <$> xs))
        fillDur d (Tremolo n x) = (d', Tremolo n x')
            where (d', x') = fillDur d x
        fillDur d (Times r x) = (d', Times r x')
            where (d', x') = fillDur d x
        fillDur d (Tuplet r x) = (d', Tuplet r x')
            where (d', x') = fillDur d x
        fillDur d (Transpose p1 p2 x) = (d', Transpose p1 p2 x')
            where (d', x') = fillDur d x
        fillDur d (Relative p x) = (d', Relative p x')
            where (d', x') = fillDur d x
        fillDur d x = (d', x)
            where d' = fromMaybe d $ return <$> dur' x

-- given a possible relative pitch context, and a pitch, returns the corresponding pitch whose octave is nearest to the pitch context, in terms of staff distance
-- this replicates the functionality of Lilypond's \relative mode
nearestRelPitch :: Maybe Pitch -> Pitch -> Pitch
nearestRelPitch Nothing p2 = p2
nearestRelPitch (Just p1) (pc2, oct2) = (pc2, oct2')
    where
        d = staffDistance p1 (pc2, oct2)  -- oct 3 has no octave marker
        shift = round $ toRational d / 7
        oct2' = oct2 - shift + (oct2 - 3)

-- converts all pitches to absolute pitches
unRelative :: (FromPitch a, ToPitch a) => MusicL a -> MusicL a
unRelative = go Nothing
    where
        go p = snd . unRel p
        pitch' (Note p _ _) = Just $ toPitch p
        pitch' x = Nothing
        unRel p (Note p' d exprs) = (Just p'', Note (fromPitch p'') d exprs)
            where p'' = nearestRelPitch p (toPitch p')
        unRel p (Sequential xs) = (p', Sequential $ snd <$> pairs)
            where
                unRel' (p, _) x = unRel p x
                pairs = tail $ scanl unRel' (p, empty) xs
                p' = fst $ last pairs
        unRel p (Simultaneous b xs) = (Nothing, Simultaneous b $ go p <$> xs)
        unRel p (PartCombine x1 x2) = (Nothing, PartCombine (go p x1) (go p x2))
        unRel p (Repeat b n x xs) = (Nothing, Repeat b n (go p x) (fmap (go p) <$> xs))
        unRel p (Tremolo n x) = (p', Tremolo n x')
            where (p', x') = unRel p x
        unRel p (Times r x) = (p', Times r x')
            where (p', x') = unRel p x
        unRel p (Tuplet r x) = (p', Tuplet r x')
            where (p', x') = unRel p x
        unRel p (Transpose p1 p2 x) = (p', Transpose p1 p2 x')
            where (p', x') = unRel p x
        unRel _ (Relative p x) = unRel p x
        unRel p x = (p, x)
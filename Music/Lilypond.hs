module Music.Lilypond (
    module Music.Lilypond.IO,
    module Music.Lilypond.Literal,
    module Music.Lilypond.MusicL,
    module Music.Lilypond.Parse,
    module Music.Lilypond.Score,
    module Music.Lilypond.Symbols
) where

import Music.Lilypond.IO
import Music.Lilypond.Literal
import Music.Lilypond.MusicL
import Music.Lilypond.Parse
import Music.Lilypond.Score
import Music.Lilypond.Symbols


-- * Lilypond conversion

-- TODO: enable this

-- toLilypond' :: (ToPitch a) => Music a -> LP.MusicL
-- toLilypond' = mFold f combineSeq combinePar g
--     where
--         f :: (ToPitch a) => Primitive a -> LP.MusicL
--         f (Note d x) = LP.Note (LP.NotePitch (toPitch x) Nothing) (Just $ fromRational d) []
--         f (Rest d) = LP.Rest LP.StdRest (Just $ fromRational d) []
--         combineSeq :: LP.MusicL -> LP.MusicL -> LP.MusicL
--         combineSeq = LP.sequential
--         combinePar :: LP.MusicL -> LP.MusicL -> LP.MusicL
--         combinePar = LP.simultaneous
--         g :: Control -> LP.MusicL -> LP.MusicL
--         g (Tempo r) m = LP.Sequential [t, m]
--             where
--                 bpm = round $ 120 * r
--                 t = LP.Tmp $ LP.Tempo Nothing (Just $ (fromRational $ 1 % 4, bpm))
--         g (Transpose d) m = LP.Transpose p p' m
--             where
--                 p = (C, 4)
--                 p' = pitch $ absPitch p + d
--         g (Instrument inst) m = LP.Sequential [LP.Assign $ LP.Set $ LP.Assignment "Staff.instrumentName" $ LP.Lit $ LP.StringL $ show inst, m]
--         -- g (Instrument inst) m = LP.Sequential [LP.Assign $ LP.Set "Staff.instrumentName" $ LP.StringL $ show inst, m]
--         g (KeySig p mode) m   = LP.Sequential [LP.Key $ simplifyMode (p, mode), m]
--         g _ m                 = m  -- TODO: PhraseAttribute

-- -- | Converts a Parnassus musical object to a Lilypond object.
-- toLilypond :: (MusicT m a, ToPitch a) => m a -> String -> TimeSig -> LP.Lilypond
-- toLilypond mus title (n, d) = LP.setHeader hdr $ LP.toLilypond mus'
--     where
--         mus' = LP.Sequential [LP.Time (n, d), toLilypond' $ toMusic mus]
--         hdr = LP.Header [LP.LitAssignment "title" (LP.StringL title)]
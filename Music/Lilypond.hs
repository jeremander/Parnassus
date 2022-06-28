module Music.Lilypond (
    fromLilypond,
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
import Music.Pitch (FromPitch, ToPitch)
import Music.Types.MusicT (MusicT(..))


-- * Lilypond conversion

-- NOTE: these routines are very limited in functionality, at present.

-- toLilypond :: (MusicT m a, ToPitch a) => m a -> String -> TimeSig -> LP.Lilypond
-- toLilypond mus title (n, d) = LP.setHeader hdr $ LP.toLilypond mus'
--     where
--         mus' = LP.Sequential [LP.Time (n, d), toLilypond' $ toMusic mus]
--         hdr = LP.Header [LP.LitAssignment "title" (LP.StringL title)]

fromLilypond :: (Eq a, FromPitch a, ToPitch a) => Lilypond a -> MusicL a
fromLilypond (Lilypond topLevels) = line $ fromTopLevel <$> topLevels
    where
        fromTopLevel (BookTop book) = fromBook book
        fromTopLevel _              = empty
        fromBook (Book _ bookParts) = line $ fromBookPart <$> bookParts
        fromBookPart (BookPart _ scores) = line $ fromScore <$> scores
        fromScore (Score scoreItems) = line $ fromScoreItem <$> scoreItems
        fromScoreItem (ScoreMusic mus) = mus
        fromScoreItem _                = empty

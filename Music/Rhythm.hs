{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module Music.Rhythm (
    Duration(..),
    TimeSig,
    splitDur,
    validDur
) where

import Data.Ratio ((%), denominator, numerator)
import Euterpea (Dur)
import Text.Pretty (Pretty(..), string)


-- | Time duration.
--   Notated time in fractions, in @[2^^i | i <- [-10..3]]@, along with a number of dots and a multiplier.
data Duration = Duration Rational Int Int
    deriving (Eq, Ord, Show)

-- floor(log2(x))
intLog2 :: (Integral a, Integral b) => a -> b
intLog2 = truncate . logBase 2 . fromIntegral

-- | Separates a 'Dur' into a sequence of 'Duration's (with multiplier 1).
splitDur :: Dur -> [Duration]
splitDur r
    | r == 0       = []
    | r >= 16      = Duration 8 0 1 : splitDur (r - 8)
    | diff == 0    = [Duration r 0 1]
    | n' == d' - 1 = [Duration b (intLog2 d') 1]
    | otherwise    = Duration b 0 1 : splitDur diff
    where
        (n, d) = (numerator r, denominator r)
        b = (2 ^ (intLog2 $ fromInteger n)) % d
        diff = r - b
        q = diff / b
        (n', d') = (numerator q, denominator q)

-- | Returns True if a duration is valid for Lilypond output.
validDur :: Dur -> Bool
validDur r = length (splitDur r) <= 1

instance Real Duration where
    toRational (Duration r nd m) = r * ((3 / 2) ^^ nd) * (fromIntegral m)

instance Fractional Duration where
    fromRational r          = Duration r 0 1
    recip (Duration r _ _ ) = Duration (1 / r) 0 1

instance Num Duration where
    (+) d1 d2 = fromRational (toRational d1 + toRational d2)
    (-) d1 d2 = fromRational (toRational d1 - toRational d2)
    (*) d1 d2 = fromRational (toRational d1 * toRational d2)
    abs d = fromRational $ abs $ toRational d
    signum d = fromRational $ signum $ toRational d
    fromInteger = fromRational . fromIntegral

instance Pretty Duration where
    -- LilyPond rendering disallows certain duration values
    pretty (Duration r nd m) = case (splitDur r) of
        [Duration r' nd' _] -> string (showDur r' (nd' + nd)) <> (if (m == 1) then "" else (string $ "*" ++ show m))
        _                   -> error "invalid duration"
        where
            go 8 = "\\maxima"
            go 4 = "\\longa"
            go 2 = "\\breve"
            go x = show $ denominator x
            showDur r' nd' = go r' ++ concat (replicate nd' ".")

-- | Time signature
type TimeSig = (Int, Int)

{-# LANGUAGE NamedFieldPuns #-}

import Data.Array ((!), listArray)
import Data.Counter (count, Counter)
import Data.Default (Default(..))
import qualified Data.Map as M
import Data.Maybe (catMaybes, mapMaybe)
import Data.Ratio ((%))
import qualified Data.Set as S
import Data.Sort (sort, sortOn)
import qualified Data.Vector as V

import Euterpea (absPitch, AbsPitch, PitchClass(..))

import Math.Optimize ((^-^), FinDiffParams(..), fdsaGradientDescent, mkFinDiffFuncs)
import Music.Pitch (ToPitch(..))
import Music.Tuning (intervalRatios, just5Asym2aScale, Scale(..), ScaleInterval, toCents, twelveTetScale)
import Music.Types (MusicD(..), Tied(..), ToMusicD(..))
import Music.Types.MusicD (extractTied, isTied)
import Misc.Utils (log2, mkArray, pairwise, ToDouble(..))


-- * Tuning Optimization

-- ** Helper Functions

-- | Takes reciprocal of a rational if necessary so that it is \( \geq 1 \).
posRatio :: RealFrac a => a -> a
posRatio r = if r >= 1 then r else 1 / r

-- ** Ideal Scales

-- | Ideal intervals @[0..11]@ (can allow multiple possibilities)
newtype IdealScale a = IdealScale {unIdealScale :: [[a]]}
    deriving (Eq, Show)

strict5Limit :: IdealScale Rational
strict5Limit = IdealScale $ pure <$> unScale just5Asym2aScale

lax5Limit :: IdealScale Rational
lax5Limit = IdealScale [[1], [16 % 15], [9 % 8, 10 % 9], [6 % 5], [5 % 4], [4 % 3], [25 % 18, 45 % 32, 36 % 25, 64 % 45], [3 % 2], [8 % 5], [5 % 3], [9 % 5, 16 % 9], [15 % 8]]


-- ** Optimization

-- | Measures deviations between a scale's intervals and some "ideal" intervals, as ratios.
intervalDevs :: (Fractional a, Ord a, RealFrac a) => [Int] -> IdealScale a -> Scale a -> [(ScaleInterval, a)]
intervalDevs intervals (IdealScale ideal) scale = devs
    where
        intervalSet = S.fromList intervals
        idealArr = listArray (0, length ideal - 1) ideal
        idealIntervals i j = idealArr ! ((j - i) `mod` 12)
        bestInterval r xs = head $ sortOn posRatio [r / x | x <- xs]
        devs = [((i, j), bestInterval r (idealIntervals i j)) | ((i, j), r) <- intervalRatios scale, ((j - i) `mod` 12) `S.member` intervalSet]

-- | Measures deviations between a scale's intervals and some "ideal" intervals, in cents.
--   Sorts by decreasing abs deviation.
intervalDevCents :: (RealFrac a, ToDouble a) => [Int] -> IdealScale a -> Scale a -> [(ScaleInterval, Double)]
intervalDevCents intervals ideal scale = sortOn (negate . abs . snd) [((i, j), toCents dev) | ((i, j), dev) <- intervalDevs intervals ideal scale]

data TunOpt = TunOpt {
    pnorm :: Double,
    idealScale :: IdealScale Double
} deriving Show

type Interval = (AbsPitch, AbsPitch)

instance Default TunOpt where
    def = TunOpt 2.0 (IdealScale $ fmap fromRational <$> unIdealScale strict5Limit)

diatonicIntervals :: [Interval]
diatonicIntervals = [(60, 60 + i) | i <- [0, 2, 4, 5, 7, 9, 11, 12]]

-- | Takes interval notes mod 12.
collapseInterval :: Interval -> Interval
collapseInterval (i, j) = (i `mod` 12, j `mod` 12)

-- | Gives \(p\)-norm distance between 2 scales in log space.
scaleDistance :: Double -> Scale Double -> Scale Double -> Double
scaleDistance p (Scale scale1) (Scale scale2) = dist
    where
        scale1' = V.fromList $ log2 <$> scale1
        scale2' = V.fromList $ log2 <$> scale2
        dist = (** (1.0 / p)) $ V.sum $ (** p) . abs <$> (scale1' ^-^ scale2')

-- | Computes \(p\)-norm loss between a given scale and an ideal scale.
pNormLoss :: TunOpt -> Counter Interval Int -> PitchClass -> Scale Double -> Double
pNormLoss (TunOpt {pnorm, idealScale}) intervalCtr pc (Scale scale) = loss
    where
        base = absPitch (pc, 4) `mod` 12  -- base pitch class
        base' = 12 - base
        scaleArr = log2 <$> mkArray scale
        idealDiffArr = mkArray $ fmap log2 <$> unIdealScale idealScale
        err :: Interval -> Int -> Double
        err (i, j) ct = fromIntegral ct * (minAbsErr ** pnorm)
            where
                i' = (i + base') `mod` 12
                iDiff = scaleArr ! i'
                j' = (j + base') `mod` 12
                jDiff = scaleArr ! j'
                actualDiff = if j' >= i' then jDiff - iDiff else 1.0 + jDiff - iDiff
                dist = j - i
                (_, dist') = dist `divMod` 12
                idealDiffs = idealDiffArr ! dist'
                minAbsErr = minimum $ abs . (actualDiff -) <$> idealDiffs
        intervalCtPairs = M.toList intervalCtr
        lossTerms = [err interval ct | (interval, ct) <- intervalCtPairs]
        loss = sum lossTerms ** (1.0 / pnorm)

tuningFinDiffParams = FinDiffParams {a = 0.0001, b = 50.0, alpha = 0.602, c = 0.0001, gamma = 0.101}
tuningFinDiffFuncs = mkFinDiffFuncs tuningFinDiffParams

-- | Optimizes a 12-tone scale to minimize \(p\)-norm loss with an ideal scale.
optimizeScale :: TunOpt -> [Interval] -> PitchClass -> Scale Double
optimizeScale tunOpt intervals pc = vecToScale xf
    where
        vecToScale = Scale . fmap (2.0 **) . (0.0 :) . V.toList
        intervalCtr = count $ collapseInterval <$> intervals
        lossFunc = pNormLoss tunOpt intervalCtr pc . vecToScale
        x0 = V.fromList $ log2 <$> tail (unScale twelveTetScale)  -- start with equal temperament
        (_, xf) = fdsaGradientDescent def tuningFinDiffFuncs lossFunc x0

musicToIntervals :: (ToPitch a, ToMusicD m a) => m a -> [Interval]
musicToIntervals mus = vertIntervals ++ horizIntervals
    where
        (MusicD _ _ arr) = toMusicD mus
        orderPair (i, j) = if i <= j then (i, j) else (j, i)
        ap = absPitch . toPitch
        -- gets interval between each untied note and each other note in a chord
        chordIntervals notes = intervals
            where
                untiedPcs = sort $ ap <$> mapMaybe extractTied (filter (not . isTied) notes)
                tiedPcs = sort $ ap <$> mapMaybe extractTied (filter isTied notes)
                intervals = [orderPair (untiedPc, tiedPc) | untiedPc <- untiedPcs, tiedPc <- tiedPcs]
        vertIntervals = concatMap chordIntervals arr
        seqInterval (Untied _ p1) (Untied _ p2) = Just (ap p1, ap p2)
        seqInterval (Tied p1)     (Untied _ p2) = Just (ap p1, ap p2)
        seqInterval _             _             = Nothing
        adjChordIntervals (notes1, notes2) = catMaybes [seqInterval note1 note2 | note1 <- notes1, note2 <- notes2]
        horizIntervals = concatMap adjChordIntervals $ pairwise arr

-- | Optimizes a 12-tone scale (starting on C) for a given piece of music.
optimizeScaleForMusic :: (ToPitch a, ToMusicD m a) => TunOpt -> m a -> Scale Double
optimizeScaleForMusic tunOpt mus = optimizeScale tunOpt (musicToIntervals mus) C
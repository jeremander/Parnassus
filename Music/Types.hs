{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Music.Types (
    module Music.Types.MusicT,
    module Music.Types.MusicD,
    module Music.Types.MusicU,
    MusicD1,
    MusicU1
) where

import qualified Data.List
import Euterpea (Control(..), Music(..), Note1, Pitch, Primitive(..))

import Music.Pitch (ToPitch(..))
import Music.Types.MusicT
import Music.Types.MusicD
import Music.Types.MusicU
import Music.Lilypond.MusicL


-- Types --

type MusicU1 = MusicU Note1
type MusicD1 = MusicD Note1

-- MusicT type conversions

convUtoD :: (Ord a, ToPitch a) => MusicU a -> MusicD a
convUtoD mus = MusicD q ctl m''
    where
        g :: Control -> MusicD a -> MusicD a
        g c (MusicD q' ctl' x') = MusicD q' (c : ctl') x'
        (MusicD q ctl m') = mFoldU (MusicD 0 [] []) (primD $ durGCD mus) (foldr1 (/+/)) (foldr1 (/=/)) g mus
        m'' = Data.List.nub . filter (not . isRest) <$> m'  -- dedupe identical notes, eliminate rests in a chord

instance (Ord a, ToPitch a) => ToMusicU MusicD a where
    toMusicU = fromMusic . toMusic
    fromMusicU = convUtoD

instance (Ord a, ToPitch a) => ToMusicD MusicU a where
    toMusicD = convUtoD
    fromMusicD = fromMusic . toMusic

unConjD :: (MusicT m a, ToMusicD m a) => (MusicD a -> MusicD a) -> (m a -> m a)
unConjD f = fromMusicD . f . toMusicD

-- Quantizable Instances --

instance (Ord a, ToPitch a) => Quantizable MusicU a where
    -- improve memory efficiency by quantizing parallel sections separately
    quantize :: Rational -> MusicU a -> MusicU a
    quantize q mus = case mus of
        Empty            -> Empty
        ParU ms          -> chord $ quantize' <$> ms
        ModifyU ctl mus' -> ModifyU ctl $ quantize q mus'
        _                -> quantize' mus  -- TODO: make more efficient
        where quantize' = (unConjD $ quantize q)

instance (Ord a, ToPitch a) => Quantizable Music a where
    quantize :: Rational -> Music a -> Music a
    quantize q = fromMusicU . quantize q . toMusicU
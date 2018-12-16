module Parnassus.MusicBase where

import Euterpea

-- Type class for basic music interface
class MusicT m where
    toMusic :: m a -> Music a
    fromMusic :: Music a -> m a
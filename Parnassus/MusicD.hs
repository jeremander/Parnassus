module Parnassus.MusicD where

type Tied a = (a, Bool)

-- "dense" music data structure
type MusicD a = [[Tied a]]

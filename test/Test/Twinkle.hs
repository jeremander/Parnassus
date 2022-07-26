module Test.Twinkle where

import Euterpea hiding (line, play)

import Music.Types

twinkle :: MusicU Pitch
twinkle = fromMusic $ line $ map ($ qn) (section1 ++ section2 ++ section2 ++ section1)
    where
        section1 = [c 4, c 4, g 4, g 4, a 4, a 4, g 4, prim . Rest, f 4, f 4, e 4, e 4, d 4, d 4, c 4, prim . Rest]
        section2 = [g 4, g 4, f 4, f 4, e 4, e 4, d 4, prim . Rest]

twinkleBass :: MusicU Pitch
twinkleBass = fromMusic $ line $ section1 ++ section2 ++ section2 ++ section1
    where
        section1 = [c 3 hn, e 2 hn, f 2 qn, a 2 qn, c 3 qn, prim $ Rest qn, f 3 hn, c 3 hn, g 3 qn, g 2 qn, c 3 qn, prim $ Rest qn]
        section2 = [e 3 hn, a 3 hn, f 3 en, e 3 en, f 3 en, fs 3 en, g 3 qn, g 2 qn]

twinkle2 = twinkle /=/ twinkleBass
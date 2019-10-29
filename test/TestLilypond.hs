{-# LANGUAGE OverloadedStrings #-}

module TestLilypond where

import Data.VectorSpace
import Music.Pitch.Literal.Pitch

import Music.Lilypond.Music (Articulation(..), Clef(..), Markup(..), Music(..), PostEvent(..), addArticulation, addDynamics, addMarkup, addPost, chord, markup, rest, toValue)
import Music.Lilypond.Dynamics (Dynamics(..))
import Music.Lilypond.IO (writeLilypond)


test1 = Simultaneous False [
        New "StaffGroup" Nothing (Simultaneous False [
            New "Staff" Nothing (Relative c' $ Sequential [
                Set "Staff.instrumentName" (toValue "Violin I"),
                (addDynamics FF c), d, e
                ]),
            New "Staff" Nothing (Sequential [
                Set "Staff.instrumentName" (toValue "Violin II"),
                Clef Bass, c, g_, c])
        ])
    ]

test2 =
    Simultaneous False
        [ Relative g' (Sequential [
            addMarkup ([Bold "Hello", Italic (markup [MarkupText "cruel", Bold $ MarkupText "world"])]) rest,
            addArticulation Mordent $ chord [c,e,g]^*2,
            d^*1,
            e^*2,
            c^*(3/2),
            fs^*(1/2)
            ])
        , Sequential [Tremolo 4 (Sequential [c^/4,d^/4]), Tremolo 4 (Sequential [c^/4,d^/4])]
        , Sequential [rest,c^*2,d^*1,e^*2,c^*(3/2),fs^*(1/2)]
        , Sequential [rest,c^*2,d^*1,e^*2,c^*(3/2),fs^*(1/2)]
        , Relative g (Sequential [rest,c^*2,d^*1,e^*2,c^*(3/2),fs^*(1/2)])
        , Sequential
            [ Times (4/5) (Sequential
                [
                    rest,
                    addArticulation Accent $ addPost BeginSlur $ addPost BeginCresc $ c^*2,
                    d^*1,
                    addPost Tie $ e^*1
                ])
            , Times (4/5) (Sequential
                [
                    addPost BeginDim $ addPost EndCrescDim $ e^*1,
                    c^*(3/2),
                    addPost EndSlur $ fs^*(1/2),
                    addPost EndCrescDim $ c^*2
                ])
            ]
        ]

testLilypond :: IO ()
testLilypond = do
    writeLilypond test1 "test1.pdf"
    writeLilypond test2 "test2.pdf"
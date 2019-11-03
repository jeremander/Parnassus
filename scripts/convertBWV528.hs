import System.IO.Unsafe
import Parnassus.Music

noteDur (Prim (Note d _)) = d
noteDur _ = 0

title = "BWV528"
x = unsafePerformIO $ fromMidiFile "/Users/jeremander/Desktop/bwv528.mid" :: Music1
z = removeTempos $ removeInstruments x
z2 = quantize (1 % 32) z
z3 = toMusicD z2
z4 = toMusic1 z3
mus = modify (KeySig B Minor) $ applyControls $ transpose (-3) z4
toPDF mus title (4,4)


z = head $ unChord x
z2 = removeTempos $ removeInstruments z
z3 = unLine z2 !! 2
z4 = chord $ take 400 $ unChord z3
z5 = quantize (1 % 32) z3
z6 = toMusicD z5
-- z7 = line $ toMusic1 <$> unLine z6
z7 = toMusic1 z6

-- remove residual lines
z8 = unChord z7 !! 1
-- lineDurs = [sum $ noteDur <$> unLine ln | ln <- unChord z7]
-- maxDur = maximum $ [sum $ noteDur <$> ln | ln <- unChord z7]

mus = modify (KeySig B Minor) $ applyControls $ transpose (-3) z8
-- toLilypond mus title (4,4)
toPDF mus title (4,4)
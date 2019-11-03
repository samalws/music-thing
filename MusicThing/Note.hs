module MusicThing.Note where

import MusicThing.Sound
import MusicThing.Combiner
import MusicThing.SoundSequence

data Note = Note {freqVal :: Double} deriving (Eq)
type Tone = Note -> Sound

rest :: Note
rest = Note (-1)

equalTempNote :: Int -> Note
equalTempNote = Note . (* 440) . ((2 ** (1 / 12)) **) . fromIntegral

letterNote :: Int -> Int -> Note
letterNote halfSteps octave = equalTempNote (halfSteps + (octave - 4) * 12)

_C = letterNote (-9)
_Db = letterNote (-8)
_D = letterNote (-7)
_Eb = letterNote (-6)
_E = letterNote (-5)
_F = letterNote (-4)
_Gb = letterNote (-3)
_G = letterNote (-2)
_Ab = letterNote (-1)
_A = letterNote 0
_Bb = letterNote 1
_B = letterNote 2

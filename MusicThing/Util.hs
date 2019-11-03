module MusicThing.Util where

import MusicThing.Sound
import MusicThing.Filter
import MusicThing.Combiner
import MusicThing.Note
import MusicThing.Instrument
import System.Random
import Data.Random.Normal
import Data.Hashable

sineWaveTone :: Tone
sineWaveTone (Note freq) = funcToSound $ sin . (* (pi * 2 * freq))

squareWaveTone :: Tone
squareWaveTone (Note freq) = funcToSound $ (\time -> if (floor (time * freq * 2) `mod` 2 == 1) then 1 else -1)

sawToothWaveTone :: Tone
sawToothWaveTone (Note freq) = funcToSound $ (\time -> time - (fromInteger (floor time) :: Double)) . (* freq)

staticSound :: Int -> Sound
staticSound salt = funcToSound $ fst . normal . mkStdGen . hashWithSalt salt

instrumentTone :: Tone
instrumentTone = instrumentToTone sineWaveTone [(1, 1.5), (1.5, 1), (2, 0.5)]

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

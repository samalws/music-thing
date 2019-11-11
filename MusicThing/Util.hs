module MusicThing.Util where

import MusicThing.Sound
import MusicThing.Filter
import MusicThing.Combiner
import MusicThing.Note
import MusicThing.Instrument
import MusicThing.NoteSet
import System.Random
import Data.Random.Normal
import Data.Hashable

sineWaveTone :: Tone
sineWaveTone freq = sin . (* (pi * 2 * freq))

squareWaveTone :: Tone
squareWaveTone freq time = if (floor (time * freq * 2) `mod` 2 == 1) then 1 else -1

sawToothWaveTone :: Tone
sawToothWaveTone freq time = (time - (fromInteger (floor time) :: Double)) * freq

staticSound :: Int -> Sound
staticSound salt = fst . normal . mkStdGen . hashWithSalt salt

instrumentTone :: Tone
instrumentTone = instrumentToTone sineWaveTone [(1, 1.5), (1.5, 1), (2, 0.5)]

crescendo :: Double -> Double -> Time -> TimeSensitiveFilter
crescendo a1 a2 length startTime = timeAmpFuncToFilter (\time ->
	(* (((a2 - a1) * (time - startTime) / length) + a1)))

aMinorScale :: NoteSet
aMinorScale = listToNoteSet $ map (. (+ 4)) [_A . (+ (-1)), _B . (+ (-1)), _C, _D, _E, _Gb, _Ab, _A]

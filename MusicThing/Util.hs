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
sawToothWaveTone freq time = (time * freq - (fromInteger (floor $ freq * time) :: Double)) * 2 - 1

staticSound :: Int -> Sound
staticSound salt = fst . normal . mkStdGen . hashWithSalt salt

crescendo :: Double -> Double -> Time -> TimeSensitiveFilter
crescendo a1 a2 length startTime = timeAmpFuncToFilter (\time ->
	(* (((a2 - a1) * (time - startTime) / length) + a1)))

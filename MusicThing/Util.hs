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
sineWaveTone (Note freq) = sin . (* (pi * 2 * freq))

squareWaveTone :: Tone
squareWaveTone (Note freq) time = if (floor (time * freq * 2) `mod` 2 == 1) then 1 else -1

sawToothWaveTone :: Tone
sawToothWaveTone (Note freq) time = (time - (fromInteger (floor time) :: Double)) * freq

staticSound :: Int -> Sound
staticSound salt = fst . normal . mkStdGen . hashWithSalt salt

instrumentTone :: Tone
instrumentTone = instrumentToTone sineWaveTone [(1, 1.5), (1.5, 1), (2, 0.5)]

crescendo :: Double -> Double -> Time -> TimeSensitiveFilter
crescendo a1 a2 length startTime = timeAmpFuncToFilter (\time ->
	(* (((a2 - a1) * (time - startTime) / length) + a1)))

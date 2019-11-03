module MusicThing.Util where

import MusicThing.Sound
import MusicThing.Filter
import MusicThing.Combiner
import System.Random
import Data.Random.Normal
import Data.Hashable

sineWaveSound :: Double -> Sound
sineWaveSound freq = funcToSound $ sin . (* (pi * 2 * freq))

squareWaveSound :: Double -> Sound
squareWaveSound freq = funcToSound $ (\time -> if (floor (time * freq * 2) `mod` 2 == 1) then 1 else -1)

sawToothWaveSound :: Double -> Sound
sawToothWaveSound freq = funcToSound $ (\time -> time - (fromInteger (floor time) :: Double)) . (* freq)

staticSound :: Int -> Sound
staticSound salt = funcToSound $ fst . normal . mkStdGen . hashWithSalt salt

amplitudeMultiply :: Double -> Filter
amplitudeMultiply = ampFuncToFilter . funcToAmpFunc . (*)

timeFuncToFilter :: (Time -> Bool) -> Filter
timeFuncToFilter f = timeAmpFuncToFilter (\time -> funcToAmpFunc $ if (f time) then id else (const 0))

specificTimeRange :: Time -> Time -> Filter
specificTimeRange t1 t2 = timeFuncToFilter (\time -> time >= t1 && time <= t2)

addSoundsList :: [Sound] -> Sound
addSoundsList = combineSoundsList $ ampCombinationFuncToCombiner $ funcToAmpCombinationFunc (+)

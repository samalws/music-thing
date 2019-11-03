module MusicThing.Combiner where

import MusicThing.Sound
import MusicThing.Filter

type Combiner = Sound -> Sound -> Sound
type AmpCombinationFunc = Amplitude -> AmpFunc

funcToAmpCombinationFunc :: (Double -> Double -> Double) -> AmpCombinationFunc
funcToAmpCombinationFunc f = funcToAmpFunc . f . ampVal

ampCombinationFuncToCombiner :: AmpCombinationFunc -> Combiner
ampCombinationFuncToCombiner f sound = timeAmpFuncToFilter $ f . (at sound)

combineSoundsList :: Combiner -> [Sound] -> Sound
combineSoundsList f [] = zeroSound
combineSoundsList f [sound] = sound
combineSoundsList combiner (sound:sounds) = combiner sound $ combineSoundsList combiner sounds

addCombiner :: Combiner
addCombiner = ampCombinationFuncToCombiner $ funcToAmpCombinationFunc (+)

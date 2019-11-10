module MusicThing.Combiner where

import MusicThing.Sound
import MusicThing.Filter

type Combiner = Sound -> Sound -> Sound
type AmpCombinationFunc = Amplitude -> AmpFunc

ampCombinationFuncToCombiner :: AmpCombinationFunc -> Combiner
ampCombinationFuncToCombiner f sound = timeAmpFuncToFilter $ f . sound

combineSoundsList :: Combiner -> [Sound] -> Sound
combineSoundsList f [] = zeroSound
combineSoundsList f [sound] = sound
combineSoundsList combiner (sound:sounds) = combiner sound $ combineSoundsList combiner sounds

addCombiner :: Combiner
addCombiner = ampCombinationFuncToCombiner $ (+)

averageSound :: [Sound] -> Sound
averageSound sounds = amplitudeMultiply (1 / (fromIntegral $ length sounds)) $ combineSoundsList addCombiner sounds

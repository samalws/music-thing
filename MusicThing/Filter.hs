module MusicThing.Filter where

import MusicThing.Sound

type Filter = Sound -> Sound
type AmpFunc = Amplitude -> Amplitude

funcToAmpFunc :: (Double -> Double) -> AmpFunc
funcToAmpFunc f = Amplitude . f . ampVal

timeAmpFuncToFilter :: (Time -> AmpFunc) -> Filter
timeAmpFuncToFilter f sound = Sound (\time -> f time $ sound `at` time)

ampFuncToFilter :: AmpFunc -> Filter
ampFuncToFilter = timeAmpFuncToFilter . const

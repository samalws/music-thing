module MusicThing.Filter where

import MusicThing.Sound

type Filter = Sound -> Sound
type TimeFunc = Time -> Time
type AmpFunc = Amplitude -> Amplitude

funcToTimeFunc :: (Double -> Double) -> TimeFunc
funcToTimeFunc f = Time . f . timeVal

funcToAmpFunc :: (Double -> Double) -> AmpFunc
funcToAmpFunc f = Amplitude . f . ampVal

timeAmpFuncToFilter :: (Time -> AmpFunc) -> Filter
timeAmpFuncToFilter f sound = Sound (\time -> f time $ sound `at` time)

timeFuncToFilter :: TimeFunc -> Filter
timeFuncToFilter f sound = Sound $ (at sound) . f

ampFuncToFilter :: AmpFunc -> Filter
ampFuncToFilter = timeAmpFuncToFilter . const

timeConstraintFilter :: (Time -> Bool) -> Filter
timeConstraintFilter f = timeAmpFuncToFilter (\time -> if (f time) then id else (const $ Amplitude 0))

lengthFilter :: Time -> Filter
lengthFilter time = timeConstraintFilter (\time2 -> time2 >= (Time 0) && time2 <= time)

timeOffsetFilter :: Time -> Filter
timeOffsetFilter (Time time) = timeFuncToFilter $ funcToTimeFunc $ (\x -> x - time)

amplitudeMultiply :: Double -> Filter
amplitudeMultiply = ampFuncToFilter . funcToAmpFunc . (*)

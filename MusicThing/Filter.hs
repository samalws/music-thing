module MusicThing.Filter where

import MusicThing.Sound

type Filter = Sound -> Sound
type TimeSensitiveFilter = Time -> Filter
type TimeFunc = Time -> Time
type AmpFunc = Amplitude -> Amplitude

combineFilters :: Filter -> Filter -> Filter
combineFilters = (.)

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

filterToTimeSensitiveFilter :: Filter -> TimeSensitiveFilter
filterToTimeSensitiveFilter = const

idFilter :: Filter
idFilter = ampFuncToFilter $ funcToAmpFunc id

amplitudeMultiply :: Double -> Filter
amplitudeMultiply = ampFuncToFilter . funcToAmpFunc . (*)

cutoffTimeSensitiveFilter :: Time -> TimeSensitiveFilter -> TimeSensitiveFilter
cutoffTimeSensitiveFilter length t_s_Filter startTime sound = let cutoffTime = Time (timeVal startTime + timeVal length) in
	Sound (\time ->
		if (time >= startTime && time <= cutoffTime) then
			t_s_Filter startTime sound `at` time
		else sound `at` time
	)

offsetTimeSensitiveFilter :: Time -> TimeSensitiveFilter -> TimeSensitiveFilter
offsetTimeSensitiveFilter time filter time2 = filter $ Time $ timeVal time + timeVal time2

combineTimeSensitiveFilters :: TimeSensitiveFilter -> TimeSensitiveFilter -> TimeSensitiveFilter
combineTimeSensitiveFilters filter1 filter2 time = combineFilters (filter1 time) (filter2 time)

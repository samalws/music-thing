module MusicThing.Filter where

import MusicThing.Sound

type Filter = Sound -> Sound
type TimeSensitiveFilter = Time -> Filter
type TimeFunc = Time -> Time
type AmpFunc = Amplitude -> Amplitude

combineFilters :: Filter -> Filter -> Filter
combineFilters = (.)

timeAmpFuncToFilter :: (Time -> AmpFunc) -> Filter
timeAmpFuncToFilter f sound time = f time $ sound time

timeFuncToFilter :: TimeFunc -> Filter
timeFuncToFilter f sound = sound . f

ampFuncToFilter :: AmpFunc -> Filter
ampFuncToFilter = (.)

timeConstraintFilter :: (Time -> Bool) -> Filter
timeConstraintFilter f = timeAmpFuncToFilter (\time -> if (f time) then id else (const 0))

lengthFilter :: Time -> Filter
lengthFilter time = timeConstraintFilter (\time2 -> time2 >= 0 && time2 <= time)

timeOffsetFilter :: Time -> Filter
timeOffsetFilter time = timeFuncToFilter (\x -> x - time)

filterToTimeSensitiveFilter :: Filter -> TimeSensitiveFilter
filterToTimeSensitiveFilter = const

idFilter :: Filter
idFilter = ampFuncToFilter id

amplitudeMultiply :: Double -> Filter
amplitudeMultiply = ampFuncToFilter . (*)

cutoffTimeSensitiveFilter :: Time -> TimeSensitiveFilter -> TimeSensitiveFilter
cutoffTimeSensitiveFilter length t_s_Filter startTime sound time =
	(if (time >= startTime && time <= startTime + length) then t_s_Filter startTime else id) sound time

offsetTimeSensitiveFilter :: Time -> TimeSensitiveFilter -> TimeSensitiveFilter
offsetTimeSensitiveFilter time filter = filter . (time +)

combineTimeSensitiveFilters :: TimeSensitiveFilter -> TimeSensitiveFilter -> TimeSensitiveFilter
combineTimeSensitiveFilters filter1 filter2 time = combineFilters (filter1 time) (filter2 time)

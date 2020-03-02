module MusicThing.Filters where

import MusicThing.Types
import MusicThing.Sounds

idFilter :: Filter
idFilter = id

freqFilter :: Frequency -> Filter
freqFilter freq sound time = sound $ freq * time

volumeFilter :: Amplitude -> Filter
volumeFilter vol sound = sound . (* vol)

timeVolumeFilter :: (Time -> Amplitude) -> Filter
timeVolumeFilter ampFunc sound time = sound time * ampFunc time

startTimeFilter :: Time -> Filter
startTimeFilter startTime sound time = (if (time < startTime) then zeroSound else sound) time

endTimeFilter :: Time -> Filter
endTimeFilter endTime sound time = (if (time > endTime) then zeroSound else sound) time

timeRangeFilter :: TimeRange -> Filter
timeRangeFilter (startTime, endTime) = startTimeFilter startTime . endTimeFilter endTime

cutoffFilterMod :: TimeRange -> Filter -> Filter
cutoffFilterMod (startTime, endTime) filter sound time = (if (time < startTime || time > endTime) then idFilter else filter) sound time

crescendoFilter :: TimeRange -> (Amplitude, Amplitude) -> Filter
crescendoFilter timeRange@(startTime, endTime) (startAmp, endAmp) = cutoffFilterMod timeRange $ timeVolumeFilter
	(\time -> (endTime - startTime)*(endAmp-startAmp)*(time-startTime) + startAmp)

decrescendoFilter = crescendoFilter

addSounds :: Sound -> Filter
addSounds sound1 sound2 time = sound1 time + sound2 time

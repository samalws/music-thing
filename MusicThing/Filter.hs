module MusicThing.Filter where

import MusicThing.Types
import MusicThing.Sound

idFilter :: Filter
idFilter = id

freqFilter :: Frequency -> Filter
freqFilter freq = (. (* freq))

volumeFilter :: Amplitude -> Filter
volumeFilter vol = ((* vol) .)

timeVolumeFilter :: (Time -> Amplitude) -> Filter
timeVolumeFilter ampFunc sound time = sound time * ampFunc time

startTimeFilter :: Time -> Filter
startTimeFilter startTime sound time = (if (time < startTime) then zeroSound else sound) time

endTimeFilter :: Time -> Filter
endTimeFilter endTime sound time = (if (time > endTime) then zeroSound else sound) time

timeRangeFilter :: TimeRange -> Filter
timeRangeFilter (startTime, endTime) = startTimeFilter startTime . endTimeFilter endTime

offsetTimeFilter :: Time -> Filter
offsetTimeFilter time = (. (+ time))

cutoffFilterMod :: TimeRange -> Filter -> Filter
cutoffFilterMod (startTime, endTime) filter sound time = (if (time < startTime || time > endTime) then idFilter else filter) sound time

crescendoFilter :: TimeRange -> (Amplitude, Amplitude) -> Filter
crescendoFilter timeRange@(startTime, endTime) (startAmp, endAmp) = cutoffFilterMod timeRange $ timeVolumeFilter
	(\time -> (endAmp-startAmp)*(time-startTime)/(endTime - startTime) + startAmp)

decrescendoFilter = crescendoFilter

cutoffFilter :: Amplitude -> Filter
cutoffFilter cutoff sound time = min cutoff $ max (-cutoff) $ sound time

addSounds :: Sound -> Sound -> Sound
addSounds sound1 sound2 time = sound1 time + sound2 time

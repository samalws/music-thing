module MusicThing.Instrument where

import MusicThing.Types
import MusicThing.Sounds
import MusicThing.Filters
import MusicThing.Tones

instrument :: [(Frequency, Amplitude)] -> Tone -> Tone
instrument fas tone = volumeFilter (1 / ampSum) . (foldToneList addSounds $ map mapFn fas) where
	mapFn = ($ tone) . (.) . (\(freq, amp) -> freqFilter freq . volumeFilter amp)
	ampSum = foldl (+) 0 $ map snd fas

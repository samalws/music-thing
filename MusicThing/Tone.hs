module MusicThing.Tone where

import MusicThing.Types
import MusicThing.Sound
import MusicThing.Filter
import Data.Maybe

sineWaveTone :: Tone
sineWaveTone freq = sin . (* (pi * 2 * freq))

squareWaveTone :: Tone
squareWaveTone freq time = if (floor (time * freq * 2) `mod` 2 == 1) then 1 else -1

sawToothWaveTone :: Tone
sawToothWaveTone freq time = (time * freq - (fromInteger (floor $ freq * time) :: Double)) * 2 - 1

foldToneList :: (Sound -> Sound -> Sound) -> [Tone] -> Tone
foldToneList f l freq = foldl f zeroSound $ map ($ freq) l

instrument :: [(Frequency, Amplitude)] -> Tone -> Tone
instrument fas tone = volumeFilter (1 / ampSum) . (foldToneList addSounds $ map mapFn fas) where
	mapFn = ($ tone) . (.) . (\(freq, amp) -> freqFilter freq . volumeFilter amp)
	ampSum = foldl (+) 0 $ map snd fas

maybeTone :: Tone -> (Maybe Frequency -> Sound)
maybeTone = (maybe zeroSound $)

addTones :: Tone -> Tone -> Tone
addTones a b freq = addSounds (a freq) (b freq)

filterTone :: Filter -> Tone -> Tone
filterTone f t freq = f $ t freq

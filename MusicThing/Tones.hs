module MusicThing.Tones where

import MusicThing.Types
import MusicThing.Sounds

sineWaveTone :: Tone
sineWaveTone freq = sin . (* (pi * 2 * freq))

squareWaveTone :: Tone
squareWaveTone freq time = if (floor (time * freq * 2) `mod` 2 == 1) then 1 else -1

sawToothWaveTone :: Tone
sawToothWaveTone freq time = (time * freq - (fromInteger (floor $ freq * time) :: Double)) * 2 - 1

foldToneList :: (Sound -> Filter) -> [Tone] -> Tone
foldToneList f l freq = foldl f zeroSound $ map ($ freq) l

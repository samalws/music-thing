module MusicThing.Melody where

import MusicThing.Types
import MusicThing.LengthSound
import MusicThing.Tone

singleMelody :: [(Time, Maybe Frequency)] -> Tone -> LengthSound
singleMelody notes tone = foldl appendLengthSounds zeroLengthSound $ map (fmap (maybeTone tone $)) notes

multipleMelody :: [[(Time, Maybe Frequency)]] -> Tone -> LengthSound
multipleMelody melodies tone = averageLengthSounds $ map (($ tone) . singleMelody) melodies

module MusicThing.Melody where

import MusicThing.Types
import MusicThing.LengthSound
import MusicThing.Tone
import Data.Tuple

changeMelodyLengths :: Time -> Melody -> Melody
changeMelodyLengths timeMult = map $ swap . fmap (* timeMult) . swap

singleMelody :: Time -> Melody -> Tone -> LengthSound
singleMelody pruneLength notes tone = foldl appendLengthSounds zeroLengthSound $
	map (pruneEndOfLengthSound pruneLength . fmap (maybeTone tone $)) notes

multipleMelody :: Time -> [Melody] -> Tone -> LengthSound
multipleMelody pruneLength melodies tone = averageLengthSounds $ map (($ tone) . singleMelody pruneLength) melodies

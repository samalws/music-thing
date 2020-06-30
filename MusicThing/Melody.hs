module MusicThing.Melody where

import MusicThing.Types
import MusicThing.LengthSound
import MusicThing.Tone
import Data.Tuple

changeMelodyLengths :: Time -> Melody -> Melody
changeMelodyLengths timeMult = map $ swap . fmap (* timeMult) . swap

singleMelody :: Time -> Bool -> Melody -> Tone -> LengthSound
-- boolean is whether to also prune start
singleMelody pruneLength True notes tone = foldl appendLengthSounds zeroLengthSound $
	map (pruneStartOfLengthSound pruneLength . pruneEndOfLengthSound pruneLength . fmap (maybeTone tone $)) notes
singleMelody pruneLength False notes tone = foldl appendLengthSounds zeroLengthSound $
	map (pruneEndOfLengthSound pruneLength . fmap (maybeTone tone $)) notes

multipleMelody :: Time -> Bool -> [Melody] -> Tone -> LengthSound
multipleMelody pruneLength alsoPruneStart melodies tone = averageLengthSounds $ map (($ tone) . singleMelody pruneLength alsoPruneStart) melodies

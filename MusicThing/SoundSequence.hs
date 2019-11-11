module MusicThing.SoundSequence where

import MusicThing.Sound
import MusicThing.Filter
import MusicThing.Combiner
import MusicThing.LengthSound

type SoundSequence = [LengthSound]

soundSequenceToLengthSound :: SoundSequence -> LengthSound
soundSequenceToLengthSound [] = (zeroSound, 0)
soundSequenceToLengthSound [(sound, length)] = (lengthFilter length sound, length)
soundSequenceToLengthSound (a:b) = (addCombiner sound1 sound2, length1 + length2) where
	(sound1, length1) = soundSequenceToLengthSound [a]
	(unoffsetSound2, length2) = soundSequenceToLengthSound b
	sound2 = timeOffsetFilter length1 unoffsetSound2

doubleToTimeInSequence :: [(a, Double)] -> [(a, Time)]
doubleToTimeInSequence = map (\(thing, t) -> (thing, t))

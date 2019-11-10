module MusicThing.SoundSequence where

import MusicThing.Sound
import MusicThing.Filter
import MusicThing.Combiner

type SoundSequence = [(Sound, Time)]

soundSequenceToSound :: SoundSequence -> Sound
soundSequenceToSound [] = zeroSound
soundSequenceToSound [(sound, length)] = lengthFilter length sound
soundSequenceToSound (a:b) = addCombiner (soundSequenceToSound [a]) $ timeOffsetFilter (snd a) (soundSequenceToSound b)

doubleToTimeInSequence :: [(a, Double)] -> [(a, Time)]
doubleToTimeInSequence = map (\(thing, t) -> (thing, t))

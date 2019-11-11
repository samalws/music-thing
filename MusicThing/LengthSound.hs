module MusicThing.LengthSound where

import MusicThing.Sound
import MusicThing.Filter
import MusicThing.Combiner

type LengthSound = (Sound, Time)

filterLengthSound :: Filter -> LengthSound -> LengthSound
filterLengthSound filter (sound, length) = (filter sound, length)

combineLengthSounds :: Combiner -> LengthSound -> LengthSound -> LengthSound
combineLengthSounds combiner (sound1, length1) (sound2, length2) = (combiner sound1 sound2, max length1 length2)

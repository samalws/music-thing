module MusicThing.LengthSound where

import MusicThing.Types
import MusicThing.Sound
import MusicThing.Filter

zeroLengthSound :: LengthSound
zeroLengthSound = (0, zeroSound)

appendLengthSounds :: LengthSound -> LengthSound -> LengthSound
appendLengthSounds (length1, sound1) (length2, sound2) = (length1 + length2, addSounds (endTimeFilter length1 sound1) (offsetTimeFilter (-length1) $ startTimeFilter 0 sound2))

overlapLengthSounds :: LengthSound -> LengthSound -> LengthSound
overlapLengthSounds (length1, sound1) (length2, sound2) = (max length1 length2, addSounds (endTimeFilter length1 sound1) (endTimeFilter length2 sound2))

averageLengthSounds :: [LengthSound] -> LengthSound
averageLengthSounds sounds = fmap (volumeFilter (1 / (fromIntegral $ length sounds))) $ foldl overlapLengthSounds zeroLengthSound sounds

lengthSoundToCutoffSound :: LengthSound -> Sound
lengthSoundToCutoffSound (length, sound) = timeRangeFilter (0, length) sound

timeRangeToLengthSound :: TimeRange -> Sound -> LengthSound
timeRangeToLengthSound (start, end) sound = appendLengthSounds (start, zeroSound) (end - start, sound)

pruneStartOfLengthSound :: Time -> LengthSound -> LengthSound
pruneStartOfLengthSound time (length, sound) = (length, decrescendoFilter (0, time) (0, 1) sound)

pruneEndOfLengthSound :: Time -> LengthSound -> LengthSound
pruneEndOfLengthSound time (length, sound) = (length, decrescendoFilter (length-time, length) (1, 0) sound)

filterLengthSound :: Filter -> LengthSound -> LengthSound
filterLengthSound f (l, s) = (l, f s)

replicateLengthSound :: Int -> LengthSound -> LengthSound
replicateLengthSound n = foldr appendLengthSounds zeroLengthSound . replicate n

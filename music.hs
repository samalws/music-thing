import MusicThing.Sound
import MusicThing.Filter
import MusicThing.Combiner
import MusicThing.Util
import MusicThing.File
import MusicThing.Note
import MusicThing.SoundSequence
import MusicThing.NoteSequence
import MusicThing.FilterSequence
import MusicThing.LengthSound
import MusicThing.Melody
import MusicThing.NoteSet
import MusicThing.NoteLength
import MusicThing.Instrument

noteLength = 1
instrumentTone1 = instrumentToTone sineWaveTone [(1, 1.5), (1.5, 1), (2, 0.5), (4/3, 0.25), (3, 0.25)]
chord1 = listToNoteSet $ map (. (+ 4)) [_C, _E, _G, _B]
melody1 = multiplySequenceNoteLength noteLength $ noteSetMembersToMelody [0, 1, 0, 1, 0, 1, 0, 0]
melody2 = multiplySequenceNoteLength noteLength $ noteSetMembersToMelody [0, 1, 0, 2, 0, 3, 0, 4]
noteSequ1 = melodyToNoteSequence chord1 melody1
noteSequ2 = melodyToNoteSequence chord1 melody2
soundSequ1 = noteSequenceToSoundSequence instrumentTone1 noteSequ1
soundSequ2 = noteSequenceToSoundSequence instrumentTone1 noteSequ2
ls1 = soundSequenceToLengthSound soundSequ1
ls2 = soundSequenceToLengthSound soundSequ2
repeated1 = soundSequenceToLengthSound $ replicate 5 ls1
repeated2 = soundSequenceToLengthSound $ replicate 5 ls2
ls1len = snd ls1
filter1 = timeSensitiveFilterSequenceToFilter [(filterToTimeSensitiveFilter $ amplitudeMultiply 1, ls1len * 3), (crescendo 1 0 ls1len, ls1len), (filterToTimeSensitiveFilter $ amplitudeMultiply 0, ls1len)]
filter2 = timeSensitiveFilterSequenceToFilter [(filterToTimeSensitiveFilter $ amplitudeMultiply 0, ls1len), (crescendo 0 1 ls1len, ls1len), (filterToTimeSensitiveFilter $ amplitudeMultiply 1, ls1len * 3)]
filtered1 = filterLengthSound filter1 repeated1
filtered2 = filterLengthSound filter2 repeated2
lsCombined = filterLengthSound (amplitudeMultiply 0.5) $ combineLengthSounds addCombiner filtered1 filtered2
mainSound = filterLengthSound (amplitudeMultiply 0.1) lsCombined

main = songToWav "a" mainSound

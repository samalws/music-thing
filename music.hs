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

noteSequ1 :: NoteSequence
noteSequ2 :: NoteSequence
soundSequ1 :: SoundSequence
soundSequ2 :: SoundSequence
ls1 :: LengthSound
ls2 :: LengthSound
filtr :: Filter
ls :: LengthSound
noteSequ1 = [(_A 4, 1), (_B 4, 1), (_C 5, 1), (_D 5, 1)]
noteSequ2 = [(_A 3, 1), (_B 3, 1), (_C 4, 1), (_D 4, 1)]
soundSequ1 = noteSequenceToSoundSequence instrumentTone noteSequ1
soundSequ2 = noteSequenceToSoundSequence squareWaveTone noteSequ2
ls1 = soundSequenceToLengthSound soundSequ1
ls2 = filterLengthSound (amplitudeMultiply 0.1) $ soundSequenceToLengthSound soundSequ2
filtr = timeSensitiveFilterSequenceToFilter [(filterToTimeSensitiveFilter $ amplitudeMultiply 0.5, 1), (crescendo 0.5 1 2, 2)]
ls = filterLengthSound ((amplitudeMultiply 0.1) . filtr) $ combineLengthSounds addCombiner ls1 ls2

main = songToWav "a" ls

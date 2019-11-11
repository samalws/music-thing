import MusicThing.Sound
import MusicThing.Filter
import MusicThing.Util
import MusicThing.File
import MusicThing.Note
import MusicThing.NoteSequence
import MusicThing.FilterSequence

noteSequ1 = [(_A 4, 1), (_B 4, 1), (_C 5, 1), (_D 5, 1)]
noteSequ2 = [(_A 3, 1), (_B 3, 1), (_C 4, 1), (_D 4, 1)]
soundSequ1 = noteSequenceToSoundSequence instrumentTone noteSequ1
soundSequ2 = noteSequenceToSoundSequence squareWaveTone noteSequ2
lengthSound1 = soundSequenceToLengthSound soundSequ1
lengthSound2 = soundSequenceToLengthSound soundSequ2
filtr = timeSensitiveFilterSequenceToFilter [(filterToTimeSensitiveFilter $ amplitudeMultiply 0.5, 1), (crescendo 0.5 1 2, 2)]
sound = amplitudeMultiply 0.5 $ filtr $ noteDoubleSequencesToSound instrumentTone [sequ1, sequ2]

main = songToWav "a" 4 sound

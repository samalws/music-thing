import MusicThing.Sound
import MusicThing.Filter
import MusicThing.Util
import MusicThing.File
import MusicThing.Note
import MusicThing.NoteSequence
import MusicThing.FilterSequence

sequ1 :: [(Note, Double)]
sequ1 = [(_A 4, 1), (_B 4, 1), (_C 5, 1), (_D 5, 1)]
sequ2 = [(_A 3, 1), (_B 3, 1), (_C 4, 1), (_D 4, 1)]
filtr = filterSequenceToFilter [(idFilter, (Time 1)), (amplitudeMultiply 0, (Time 1))]
sound = amplitudeMultiply 0.5 $ filtr $ noteDoubleSequencesToSound instrumentTone [sequ1, sequ2]

main = songToWav "a" 4 sound

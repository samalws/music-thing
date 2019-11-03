import MusicThing.Sound
import MusicThing.Filter
import MusicThing.Util
import MusicThing.File
import MusicThing.Note
import MusicThing.SoundSequence

sequ1 :: [(Note, Double)]
sequ1 = [(_A 4, 1), (_B 4, 1), (rest, 1), (_D 5, 1)]
sequ2 = [(_A 3, 1), (_B 3, 1), (rest, 1), (_D 4, 1)]
sound = amplitudeMultiply 0.5 $ noteDoubleSequencesToSound instrumentTone [sequ1, sequ2]

main = songToWav "a" 4 sound

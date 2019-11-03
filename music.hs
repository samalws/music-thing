import MusicThing.Sound
import MusicThing.Filter
import MusicThing.Util
import MusicThing.File
import MusicThing.Note
import MusicThing.SoundSequence

sequ = [(_A 4, 1), (_B 4, 1), (_C 5, 1), (_D 5, 1)]
sound = amplitudeMultiply 0.5 $ noteDoubleSquenceToSound instrumentTone sequ

main = songToWav "a" 4 sound

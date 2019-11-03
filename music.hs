import MusicThing.Sound
import MusicThing.Util
import MusicThing.File
import MusicThing.Note
import MusicThing.SoundSequence

sound = amplitudeMultiply 0.05 $ soundSequenceToSound $ noteSequenceToSoundSequence sineWaveSound [((_A 4), (Time 1)), ((_B 4), (Time 1))]

main = songToWav "a" 4 sound

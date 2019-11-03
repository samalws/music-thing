import MusicThing.Sound
import MusicThing.Util
import MusicThing.File
import MusicThing.Note

sound = amplitudeMultiply 0.05 $ noteSequenceToSound [((_A 4), (Time 1)), ((_B 4), (Time 1))] sineWaveSound

main = songToWav "a" 4 sound

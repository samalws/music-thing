import MusicThing.Sound
import MusicThing.Util
import MusicThing.Sample

sound = amplitudeMultiply 0.05 $ staticSound 880654

main = putStr $ sampleToDat $ makeSample (Time $ 1 / 44100) (Time 1) sound

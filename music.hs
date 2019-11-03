import MusicThing.Util
import MusicThing.File

sound = amplitudeMultiply 0.05 $ staticSound 880654

main = songToWav "a" 1 sound

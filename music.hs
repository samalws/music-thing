import MusicThing.Types
import MusicThing.File
import MusicThing.Tone

main = songToWav "a" (5, instrument [(1, 1), (1.5, 0.5)] sineWaveTone 440)

module MusicThing.Sound where

type Time = Double
type Amplitude = Double
type Sound = Time -> Amplitude

zeroSound :: Sound
zeroSound = const 0

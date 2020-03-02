module MusicThing.Types where

type Time = Double
type Amplitude = Double
type Sound = Time -> Amplitude
type Filter = Sound -> Sound
type LengthSound = (Time, Sound)
type Frequency = Double
type Tone = Frequency -> Sound
type TimeRange = (Time, Time)

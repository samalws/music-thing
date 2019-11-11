module MusicThing.Instrument where

import MusicThing.Filter
import MusicThing.Combiner
import MusicThing.Note

type Instrument = [(Double, Double)]

instrumentToTone :: Tone -> Instrument -> Tone
instrumentToTone tone instrument freq =
	amplitudeMultiply (1 / (sum $ map snd instrument)) $
	combineSoundsList addCombiner $ map
		(\(freqMult, vol) -> amplitudeMultiply vol $ tone $ freqMult * freq)
		instrument

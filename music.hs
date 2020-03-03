import MusicThing.Types
import MusicThing.File
import MusicThing.Tone
import MusicThing.LengthSound
import MusicThing.Melody

scale :: Double -> Maybe Double
scale n = Just $ 220 * (12 ** (n / 12))

melody1 :: Melody
melody1 = [(4, Nothing),
	(1, scale 4), (0.5, Nothing), (2, scale 2), (0.5, Nothing),
	(1, scale 3), (0.5, Nothing), (2, scale 4), (0.5, Nothing),
	(1, scale 1.5), (0.5, Nothing), (2, scale 2), (0.5, Nothing)]

instrument1 :: Tone
instrument1 = instrument [(1, 1), (1.5, 0.5), (2, 0.5)] sineWaveTone

lengthSound :: LengthSound
lengthSound = singleMelody 0.1 (concat $ replicate 3 $ changeMelodyLengths 0.5 melody1) instrument1

main = songToWav "a" lengthSound

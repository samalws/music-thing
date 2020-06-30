import MusicThing.Types
import MusicThing.File
import MusicThing.Tone
import MusicThing.Sound
import MusicThing.LengthSound
import MusicThing.Melody
import MusicThing.Filter
import Data.Time

--------------------------------------------- UTILS

type Scale = ([Double], Double)

scale :: Scale -> Int -> Double
scale (notes, octaveFreq) n
	| n < 0             = (scale (notes, octaveFreq) (n + length notes)) / octaveFreq
	| n >= length notes = (scale (notes, octaveFreq) (n - length notes)) * octaveFreq
	| otherwise         = notes !! n

mapScale :: Scale -> [(a, Maybe Int)] -> [(a, Maybe Double)]
mapScale s = map (\(x, y) -> (x, scale s <$> y))

increaseOneOctave :: Double -> Maybe Double -> Maybe Double
increaseOneOctave octaveFreq = (>>= Just . (* octaveFreq))

coolInstrumentMaker :: [(Double, Double)] -> [(Double, Double)]
coolInstrumentMaker = concat . map (\(note, amp) -> [(note*0.9, amp*0.05), (note*0.95, amp*0.1), (note, amp), (note*1.1, amp*0.1), (note*1.1, amp*0.05)])

-- proportion sine -> tone containing both sine and sawtooth
sineSawTone :: Amplitude -> Tone
sineSawTone a = addTones (filterTone (volumeFilter a) sineWaveTone) (filterTone (volumeFilter (1-a)) sawToothWaveTone)

-- proportion sine -> tone containing both sine and square
sineSquareTone :: Amplitude -> Tone
sineSquareTone a = addTones (filterTone (volumeFilter a) sineWaveTone) (filterTone (volumeFilter (1-a)) squareWaveTone)

-- http://thoughtmountain.com/orchestra/Sounds_of_the_Orchestra.html
-- [(1, 1), (2, 0.7), (3, 0.35), (4, 0.4), (5, 0.05), (6, 0.25), (7, 0.1), (8, 0.02)]: piano-ish
-- [(1, 0.37), (2, 0.52), (3, 0.12), (4, 0.21), (5, 0.14), (6, 0.04), (7, 0.05),
-- 	(8, 0.08), (9, 0.05), (10, 0.07), (11, 0.11), (12, 0.02), (14, 0.4)]: harpsichord-ish

linearDecay :: Time -> Sound
linearDecay cutoffTime time
	| time < 0 = 0
	| time > cutoffTime = 0
	| otherwise = 1 - (time / cutoffTime)

decayFilter :: Filter
decayFilter = timeVolumeFilter $ linearDecay 0.1

--------------------------------------------- SCALES

scale1 :: Scale
scale1 = (map (* 220) [1, 4/3, 1.5, 5/3], 2)

--------------------------------------------- INSTRUMENTS

-- weird :) sounding one
instrument1 :: Tone
instrument1 = instrument (coolInstrumentMaker [(1, 1), (1.5, 0.1), (2, 0.5), (3, 0.05)]) sineWaveTone

-- organ sounding one
instrument2 :: Tone
instrument2 = instrument [(1, 0.5), (2, 1), (3, 0.2), (4, 0.1), (5, 0.02), (6, 0.08)] sineWaveTone

-- organ sounding one
instrument3 :: Tone
instrument3 = instrument [(1, 1), (2, 0.5), (3, 0.05), (4, 0.3), (5, 0.03), (6, 0.2)] sineWaveTone

-- organ sounding one
instrument4 :: Tone
instrument4 = instrument [(1, 1), (2, 1), (3, 0.05), (4, 0.3), (5, 0.03), (6, 0.2)] sineWaveTone

-- increases in complexity with O(6^n)
instrument5s :: Int -> Tone
instrument5s 0 = sineWaveTone
instrument5s n = instrument [(0.5, 0.2), (1, 1), (2, 0.3)] $ instrument5s (n-1)

-- glassy sounding one
instrument6 :: Tone
instrument6 = instrument [(1, 1), (2, 1), (3, 0.05), (4, 0.3), (5, 0.03), (6, 0.2)] $ sineSawTone 0.95

--------------------------------------------- MELODIES

j = Just

melody1 :: Melody
melody1 = mapScale scale1 [(1, j 0), (1, j 1), (1, j 3), (1, j 4), (1, Nothing), (1, j 2), (1, j 3), (1, j 4)]

melody2 :: Melody
melody2 = mapScale scale1 [(3, j 0), (1, j 1), (1, Nothing), (1, j 0), (1, j 1), (1, j 2)]

melody3 :: Melody
melody3 = map (\(a, b) -> (a, increaseOneOctave 2 b)) melody1

melody4 :: Melody
melody4 = mapScale scale1 [(1, j 0), (1, j 2), (1, j 0), (1, j 3), (1, j 2), (1, j 1)]

melody5 :: Melody
melody5 = mapScale scale1 [(1, j 2), (1, j 3), (1, j 5), (9, Nothing), (1, j 2), (1, j 3), (1, j 5), (1, j 2), (1, j 3), (1, j 5), (6, Nothing)]

melody5l :: Melody
melody5l = mapScale scale1 [(1, j (-3)), (1, j (-2)), (1, j (-2)), (9, Nothing), (1, j (-3)), (1, j (-2)), (1, j (-2)), (1, j (-3)), (1, j (-2)), (1, j (-2)), (6, Nothing)]

--------------------------------------------- SOUNDS

-- standard measure has length 12 because i say so
-- key: m = measure, f = fast, l = low

ls0 :: LengthSound
ls0 = singleMelody 0.1 False (changeMelodyLengths 1 melody4) instrument1
-- length: 6
ls0m = replicateLengthSound 2 ls0

ls0f :: LengthSound
ls0f = singleMelody 0.1 True (changeMelodyLengths (1/2) melody4) instrument1
-- length: 3
ls0fm = replicateLengthSound 4 ls0f

ls1 :: LengthSound
ls1 = singleMelody 0.1 True (changeMelodyLengths (1/6) melody5) instrument6
-- length: 4
ls1m = replicateLengthSound 3 ls1

ls1l :: LengthSound
ls1l = singleMelody 0.1 True (changeMelodyLengths (1/6) melody5l) instrument2
-- length: 4
ls1lm = replicateLengthSound 3 ls1l

drumLs :: LengthSound
drumLs = (0.5, decayFilter $ volumeFilter 0.25 $ staticSound 0)
drumLsM = replicateLengthSound 24 drumLs


mainLs :: LengthSound
mainLs = replicateLengthSound 1 $ averageLengthSounds [ls0m, ls0fm, ls1m, ls1lm, ls1lm, drumLsM]

main = do
	start <- getCurrentTime
	songToWav "a" mainLs
	end <- getCurrentTime
	print $ diffUTCTime end start
	putStrLn "done"

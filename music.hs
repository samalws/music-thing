import MusicThing.Types
import MusicThing.File
import MusicThing.Tone
import MusicThing.Sound
import MusicThing.LengthSound
import MusicThing.Melody
import MusicThing.Filter

scaleNotes :: [Double]
scaleNotes = map (* 220) [1, 4/3, 1.5, 5/3]

octaveFreq :: Double
octaveFreq = 2

scale :: Int -> Double
scale n
	| n < 0                  = scale (n + length scaleNotes) / octaveFreq
	| n >= length scaleNotes = scale (n - length scaleNotes) * octaveFreq
	| otherwise              = scaleNotes !! n

mScale :: Int -> Maybe Double
mScale = Just . scale

increaseOneOctave :: Maybe Double -> Maybe Double
increaseOneOctave = (>>= Just . (* octaveFreq))

melody1 :: Melody
melody1 = [(1, mScale 0), (1, mScale 1), (1, mScale 3), (1, mScale 4), (1, Nothing), (1, mScale 2), (1, mScale 3), (1, mScale 4)]

melody2 :: Melody
melody2 = [(3, mScale 0), (1, mScale 1), (1, Nothing), (1, mScale 0), (1, mScale 1), (1, mScale 2)]

melody3 :: Melody
melody3 = map (\(a, b) -> (a, increaseOneOctave b)) melody1

coolInstrumentMaker :: [(Double, Double)] -> [(Double, Double)]
coolInstrumentMaker = concat . map (\(note, amp) -> [(note*0.9, amp*0.05), (note*0.95, amp*0.1), (note, amp), (note*1.1, amp*0.1), (note*1.1, amp*0.05)])

instrument1 :: Tone
instrument1 = instrument (coolInstrumentMaker [(1, 1), (1.5, 0.1), (2, 0.5), (3, 0.05)]) sineWaveTone

ls0 :: LengthSound
ls0 = singleMelody 0.1 (changeMelodyLengths 0.3 melody2) instrument1

ls1 :: LengthSound
ls1 = replicateLengthSound 6 ls0

ls2 :: LengthSound
ls2 = appendLengthSounds (fst ls0 * 2, zeroSound) $ replicateLengthSound 3 $ singleMelody 0.1 (changeMelodyLengths 0.3 melody1) instrument1

ls3 :: LengthSound
ls3 = appendLengthSounds (fst ls0 * 3, zeroSound) $ replicateLengthSound 2 $ filterLengthSound (volumeFilter 0.5) $ singleMelody 0.1 (changeMelodyLengths 0.3 melody3) instrument1

mainLs :: LengthSound
mainLs = averageLengthSounds [ls1, ls2, ls3]

main = do
	songToWav "a" mainLs
	putStrLn "done"

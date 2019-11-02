import Numeric
import System.Random
import Data.Random.Normal
import Data.Hashable

data Time = Time {timeVal :: Double} deriving (Eq, Ord, Show)
data Amplitude = Amplitude {ampVal :: Double} deriving (Eq, Ord, Show)
data Sound = Sound {at :: Time -> Amplitude}
data Sample = Sample {stepVal :: Time, ampsVal :: [Amplitude]} deriving (Show)
type Filter = Sound -> Sound
type Combiner = Sound -> Sound -> Sound
type AmpFunc = Amplitude -> Amplitude
type AmpCombinationFunc = Amplitude -> AmpFunc

appendToSample :: Amplitude -> Sample -> Sample
appendToSample amp samp = Sample (stepVal samp) (amp:(ampsVal samp))

makeSampleHelper :: Time {-starting point-} -> Time {-step length-} -> Time {-length-} -> Sound -> Sample
makeSampleHelper startingPoint step length sound
	| startingPoint > length = Sample step []
	| otherwise = (sound `at` startingPoint) `appendToSample`
		(makeSampleHelper (Time $ timeVal startingPoint + timeVal step) step length sound)

makeSample = makeSampleHelper (Time 0)

sampleToDatHead :: Sample -> String
sampleToDatHead samp = "; Sample Rate " ++ show (round (1 / (timeVal $ stepVal samp)) :: Int) ++ "\n; Channels 1\n"

showNumNicelyForDat :: Double -> String
showNumNicelyForDat num = Numeric.showFFloat (Just 10) num ""

sampleToDatBody :: Time -> Sample -> String
sampleToDatBody _ (Sample _ []) = ""
sampleToDatBody time samp = (showNumNicelyForDat $ timeVal time) ++ " " ++ (showNumNicelyForDat $ ampVal $ head $ ampsVal $ samp) ++ "\n" ++
	(sampleToDatBody (Time $ timeVal time + (timeVal $ stepVal samp)) (Sample (stepVal samp) (tail $ ampsVal samp)))

sampleToDat :: Sample -> String
sampleToDat samp = sampleToDatHead samp ++ sampleToDatBody (Time 0) samp

funcToSound :: (Double -> Double) -> Sound
funcToSound f = Sound $ Amplitude . f . timeVal

soundToFunc :: Sound -> (Double -> Double)
soundToFunc sound = ampVal . (at sound) . Time

funcToAmpFunc :: (Double -> Double) -> AmpFunc
funcToAmpFunc f = Amplitude . f . ampVal

funcToAmpCombinationFunc :: (Double -> Double -> Double) -> AmpCombinationFunc
funcToAmpCombinationFunc f = funcToAmpFunc . f . ampVal

timeAmpFuncToFilter :: (Time -> AmpFunc) -> Filter
timeAmpFuncToFilter f sound = Sound (\time -> f time $ sound `at` time)

ampFuncToFilter :: AmpFunc -> Filter
ampFuncToFilter = timeAmpFuncToFilter . const

zeroSound :: Sound
zeroSound = funcToSound $ const 0

sineWaveSound :: Double -> Sound
sineWaveSound freq = funcToSound $ sin . (* (pi * 2 * freq))

squareWaveSound :: Double -> Sound
squareWaveSound freq = funcToSound $ (\time -> if (floor (time * freq * 2) `mod` 2 == 1) then 1 else -1)

sawToothWaveSound :: Double -> Sound
sawToothWaveSound freq = funcToSound $ (\time -> time - (fromInteger (floor time) :: Double)) . (* freq)

staticSound :: Int -> Sound
staticSound salt = funcToSound $ fst . normal . mkStdGen . hashWithSalt salt

amplitudeMultiply :: Double -> Filter
amplitudeMultiply = ampFuncToFilter . funcToAmpFunc . (*)

timeFuncToFilter :: (Time -> Bool) -> Filter
timeFuncToFilter f = timeAmpFuncToFilter (\time -> funcToAmpFunc $ if (f time) then id else (const 0))

specificTimeRange :: Time -> Time -> Filter
specificTimeRange t1 t2 = timeFuncToFilter (\time -> time >= t1 && time <= t2)

ampCombinationFuncToCombiner :: AmpCombinationFunc -> Combiner
ampCombinationFuncToCombiner f sound = timeAmpFuncToFilter $ f . (at sound)

combineSoundsList :: Combiner -> [Sound] -> Sound
combineSoundsList f [] = zeroSound
combineSoundsList f [sound] = sound
combineSoundsList combiner (sound:sounds) = combiner sound $ combineSoundsList combiner sounds

addSoundsList :: [Sound] -> Sound
addSoundsList = combineSoundsList $ ampCombinationFuncToCombiner $ funcToAmpCombinationFunc (+)

sound = amplitudeMultiply 0.05 $ staticSound 440

main = putStr $ sampleToDat $ makeSample (Time $ 1 / 44100) (Time 1) sound

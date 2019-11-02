import Numeric

data Time = Time {timeVal :: Double} deriving (Eq, Ord, Show)
data Amplitude = Amplitude {ampVal :: Double} deriving (Eq, Ord, Show)
data Sound = Sound {at :: Time -> Amplitude}
data Sample = Sample {stepVal :: Time, ampsVal :: [Amplitude]} deriving (Show)

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

zeroSound :: Sound
zeroSound = funcToSound $ const 0

sineWaveSound :: Double -> Sound
sineWaveSound freq = funcToSound $ sin . (* (pi * 2 * freq))

amplitudeMultiply :: Double -> Sound -> Sound
amplitudeMultiply factor sound = funcToSound $ (* factor) . (soundToFunc sound)

combineSounds :: (Double -> Double -> Double) -> Sound -> Sound -> Sound
combineSounds f sound1 sound2 = funcToSound $ (\t -> (soundToFunc sound1 t) `f` (soundToFunc sound2 t))

addSounds :: Sound -> Sound -> Sound
addSounds = combineSounds (+)

sound1 = sineWaveSound 440
sound2 = sineWaveSound (440*1.5)
sound3 = amplitudeMultiply 0.05 $ addSounds sound1 sound2

main = putStr $ sampleToDat $ makeSample (Time $ 1 / 44100) (Time 3) sound3

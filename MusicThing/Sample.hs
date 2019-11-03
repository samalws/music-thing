module MusicThing.Sample (Sample, makeSample, sampleToDat) where

import MusicThing.Sound
import Numeric

data Sample = Sample {stepVal :: Time, ampsVal :: [Amplitude]} deriving (Show)

appendToSample :: Amplitude -> Sample -> Sample
appendToSample amp samp = Sample (stepVal samp) (amp:(ampsVal samp))

makeSampleHelper :: Time {-starting point-} -> Time {-step length-} -> Time {-length-} -> Sound -> Sample
makeSampleHelper startingPoint step length sound
	| startingPoint > length = Sample step []
	| otherwise = (sound `at` startingPoint) `appendToSample`
		(makeSampleHelper (Time $ timeVal startingPoint + timeVal step) step length sound)

makeSample :: Time {-step length-} -> Time {-length-} -> Sound -> Sample
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

module MusicThing.Sample (Sample, makeSample, sampleToDat) where

import MusicThing.Types
import Numeric

data Sample = Sample {stepVal :: Time, ampsVal :: [Amplitude]} deriving (Show)

appendToSample :: Amplitude -> Sample -> Sample
appendToSample amp samp = Sample (stepVal samp) (amp:(ampsVal samp))

makeSampleHelper :: Time {-starting point-} -> Time {-step length-} -> LengthSound -> Sample
makeSampleHelper startingPoint step (sound, length)
	| startingPoint > length = Sample step []
	| otherwise = (sound startingPoint) `appendToSample`
		(makeSampleHelper (startingPoint + step) step (sound, length))

makeSample :: Time {-step length-} -> LengthSound -> Sample
makeSample = makeSampleHelper 0

sampleToDatHead :: Sample -> String
sampleToDatHead samp = "; Sample Rate " ++ show (round (1 / (stepVal samp)) :: Int) ++ "\n; Channels 1\n"

showNumNicelyForDat :: Double -> String
showNumNicelyForDat num = Numeric.showFFloat (Just 10) num ""

sampleToDatBody :: Time -> Sample -> String
sampleToDatBody _ (Sample _ []) = ""
sampleToDatBody time samp = (showNumNicelyForDat time) ++ " " ++ (showNumNicelyForDat $ head $ ampsVal $ samp) ++ "\n" ++
	(sampleToDatBody (time + (stepVal samp)) (Sample (stepVal samp) (tail $ ampsVal samp)))

sampleToDat :: Sample -> String
sampleToDat samp = sampleToDatHead samp ++ sampleToDatBody 0 samp

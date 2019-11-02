data Time = Time {time :: Double} deriving (Eq, Ord, Show)
data Amplitude = Amplitude {amplitude :: Double} deriving (Eq, Ord, Show)
data Sound = Sound {at :: Time -> Amplitude}
data Sample = Sample Time {-step time-} [Amplitude] {-amplitudes-}

appendToSample :: Amplitude -> Sample -> Sample
appendToSample amp (Sample step amps) = Sample step (amp:amps)

makeSampleHelper :: Time {-starting point-} -> Time {-step length-} -> Time {-length-} -> Sound -> Sample
makeSampleHelper startingPoint step length sound
	| startingPoint >= length = Sample step []
	| otherwise = (sound `at` startingPoint) `appendToSample`
		(makeSampleHelper (Time $ time startingPoint + time step) step (Time $ time length - time step) sound)

main = print ""

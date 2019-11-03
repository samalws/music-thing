module MusicThing.Sound where

data Time = Time {timeVal :: Double} deriving (Eq, Ord, Show)
data Amplitude = Amplitude {ampVal :: Double} deriving (Eq, Ord, Show)
data Sound = Sound {at :: Time -> Amplitude}

soundToFunc :: Sound -> (Double -> Double)
soundToFunc sound = ampVal . (at sound) . Time

funcToSound :: (Double -> Double) -> Sound
funcToSound f = Sound $ Amplitude . f . timeVal

zeroSound :: Sound
zeroSound = funcToSound $ const 0

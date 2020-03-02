module MusicThing.Sounds where

import MusicThing.Types
import System.Random
import Data.Random.Normal
import Data.Hashable

zeroSound :: Sound
zeroSound = const 0

staticSound :: Int -> Sound
staticSound salt = fst . normal . mkStdGen . hashWithSalt salt

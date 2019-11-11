module MusicThing.File (songToWav) where

import MusicThing.Sound
import MusicThing.Sample
import MusicThing.LengthSound
import System.IO
import System.Process

writeLengthSoundToDat :: FilePath -> LengthSound -> IO ()
writeLengthSoundToDat path lengthSound = writeFile path $ sampleToDat $ makeSample (1 / 44100) lengthSound

datToWav :: FilePath -> FilePath -> IO ()
datToWav path1 path2 = callCommand ("sox " ++ path1 ++ " " ++ path2)

songToWav :: FilePath -> LengthSound -> IO ()
songToWav path lengthSound = let pathDat = path ++ ".dat" in
	writeLengthSoundToDat pathDat lengthSound >>
	datToWav pathDat (path ++ ".wav")

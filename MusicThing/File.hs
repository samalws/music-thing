module MusicThing.File where

import MusicThing.Types
import MusicThing.Sample
import System.IO
import System.Process

writeSoundToDat :: FilePath -> LengthSound -> IO ()
writeSoundToDat path lengthSound = writeFile path $ sampleToDat $ makeSample (1 / 44100) lengthSound

datToWav :: FilePath -> FilePath -> IO ()
datToWav path1 path2 = callCommand ("sox " ++ path1 ++ " " ++ path2)

songToWav :: FilePath -> LengthSound -> IO ()
songToWav path lengthSound = let pathDat = path ++ ".dat" in
	writeSoundToDat pathDat lengthSound >>
	datToWav pathDat (path ++ ".wav")

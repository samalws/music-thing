module MusicThing.File where

import MusicThing.Sound
import MusicThing.Sample
import System.IO
import System.Process

writeSoundToDat :: FilePath -> Double {-length-} -> Sound -> IO ()
writeSoundToDat path length sound = writeFile path $ sampleToDat $ makeSample (Time $ 1 / 44100) (Time length) sound

datToWav :: FilePath -> FilePath -> IO ()
datToWav path1 path2 = callCommand ("sox " ++ path1 ++ " " ++ path2)

songToWav :: FilePath -> Double {-length-} -> Sound -> IO ()
songToWav path length sound = let pathDat = path ++ ".dat" in
	writeSoundToDat pathDat length sound >>
	datToWav pathDat (path ++ ".wav")

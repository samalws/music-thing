module MusicThing.Note where

import MusicThing.Sound
import MusicThing.Combiner
import MusicThing.SoundSequence

data Note = Note {freqVal :: Double} deriving (Eq)
type Tone = Note -> Sound
type NoteSequence = [(Note, Time)]

rest :: Note
rest = Note (-1)

noteSequenceToSoundSequence :: Tone -> NoteSequence -> SoundSequence
noteSequenceToSoundSequence tone = map (\(note, t) -> if (note == rest) then (zeroSound, t) else (tone note, t))

equalTempNote :: Int -> Note
equalTempNote = Note . (* 440) . ((2 ** (1 / 12)) **) . fromIntegral

noteDoubleSequenceToSound :: Tone -> [(Note, Double)] -> Sound
noteDoubleSequenceToSound tone = soundSequenceToSound . noteSequenceToSoundSequence tone . doubleToTimeInSequence

noteDoubleSequencesToSound :: Tone -> [[(Note, Double)]] -> Sound
noteDoubleSequencesToSound tone = averageSound . (map $ noteDoubleSequenceToSound tone)

module MusicThing.Note where

import MusicThing.Sound
import MusicThing.Filter
import MusicThing.Combiner

data Note = Note {freqVal :: Double}
type Tone = Note -> Sound
type NoteSequence = [(Note, Time)]

noteSequenceToSound :: NoteSequence -> Tone -> Sound
noteSequenceToSound [] _ = zeroSound
noteSequenceToSound [(note, length)] tone = lengthFilter length $ tone note
noteSequenceToSound (a:b) tone = addCombiner (noteSequenceToSound [a] tone) $ timeOffsetFilter (snd a) $ noteSequenceToSound b tone

equalTempNote :: Int -> Note
equalTempNote = Note . (* 440) . ((2 ** (1 / 12)) **) . fromIntegral

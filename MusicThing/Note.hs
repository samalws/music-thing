module MusicThing.Note where

import MusicThing.Sound
import MusicThing.SoundSequence

data Note = Note {freqVal :: Double}
type Tone = Note -> Sound
type NoteSequence = [(Note, Time)]

noteSequenceToSoundSequence :: Tone -> NoteSequence -> SoundSequence
noteSequenceToSoundSequence tone = map (\(note, t) -> (tone note, t))

equalTempNote :: Int -> Note
equalTempNote = Note . (* 440) . ((2 ** (1 / 12)) **) . fromIntegral

noteDoubleSquenceToSound :: Tone -> [(Note, Double)] -> Sound
noteDoubleSquenceToSound tone = soundSequenceToSound . noteSequenceToSoundSequence tone . doubleToTimeInSequence

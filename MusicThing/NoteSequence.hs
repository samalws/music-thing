module MusicThing.NoteSequence where

import MusicThing.Sound
import MusicThing.Note
import MusicThing.Combiner
import MusicThing.SoundSequence

type NoteSequence = [(Note, Time)]

noteSequenceToSoundSequence :: Tone -> NoteSequence -> SoundSequence
noteSequenceToSoundSequence tone = map (\(note, t) -> if (note == rest) then (zeroSound, t) else (tone note, t))

module MusicThing.Melody where

import MusicThing.Sound
import MusicThing.NoteSequence
import MusicThing.Chord

type Melody = [(ChordMember, Time)]

melodyToNoteSequence :: Chord -> Melody -> NoteSequence
melodyToNoteSequence chord = map (\(chordMember, time) -> (chord chordMember, time))

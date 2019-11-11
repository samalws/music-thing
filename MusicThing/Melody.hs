module MusicThing.Melody where

import MusicThing.Sound
import MusicThing.NoteSequence
import MusicThing.NoteSet

type Melody = [(NoteSetMember, Time)]

melodyToNoteSequence :: NoteSet -> Melody -> NoteSequence
melodyToNoteSequence noteSet = map (\(noteSetMember, time) -> (noteSet noteSetMember, time))

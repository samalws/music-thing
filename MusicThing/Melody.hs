module MusicThing.Melody where

import MusicThing.Sound
import MusicThing.NoteSequence
import MusicThing.NoteSet

type Melody = [(NoteSetMember, Time)]

noteSetMembersToMelody :: [NoteSetMember] -> Melody
noteSetMembersToMelody = map (\x -> (x, 1))

melodyToNoteSequence :: NoteSet -> Melody -> NoteSequence
melodyToNoteSequence noteSet = map (\(noteSetMember, time) -> (noteSet noteSetMember, time))

module MusicThing.NoteSet where

import MusicThing.Note

type NoteSetMember = Int
type NoteSet = NoteSetMember -> Note

listToNoteSet :: [Octave -> Note] -> NoteSet
listToNoteSet arr noteSetMember = (arr !! (noteSetMember `mod` length arr)) (noteSetMember `div` length arr)

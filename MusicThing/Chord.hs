module MusicThing.Chord where

import MusicThing.Note

type ChordMember = Int
type Chord = ChordMember -> Note

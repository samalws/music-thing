module MusicThing.NoteLength where

import MusicThing.Sound

multiplySequenceNoteLength :: Time -> [(a, Time)] -> [(a, Time)]
multiplySequenceNoteLength time = map (\(x, y) -> (x, y * time))

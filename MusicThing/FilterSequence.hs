module MusicThing.FilterSequence where

import MusicThing.Sound
import MusicThing.Filter

type FilterSequence = [(Filter, Time)]
type TimeSensitiveFilterSequence = [(TimeSensitiveFilter, Time)]

timeSensitiveFilterSequenceToTimeSensitiveFilter :: TimeSensitiveFilterSequence -> TimeSensitiveFilter
timeSensitiveFilterSequenceToTimeSensitiveFilter [] = filterToTimeSensitiveFilter idFilter
timeSensitiveFilterSequenceToTimeSensitiveFilter [(filter, length)] = cutoffTimeSensitiveFilter length filter
timeSensitiveFilterSequenceToTimeSensitiveFilter (a:b) = timeSensitiveFilterSequenceToTimeSensitiveFilter [a] `combineTimeSensitiveFilters` (offsetTimeSensitiveFilter (snd a) $ timeSensitiveFilterSequenceToTimeSensitiveFilter b)

timeSensitiveFilterSequenceToFilter :: TimeSensitiveFilterSequence -> Filter
timeSensitiveFilterSequenceToFilter = ($ 0) . timeSensitiveFilterSequenceToTimeSensitiveFilter

filterSequenceToTimeSensitiveFilter :: FilterSequence -> TimeSensitiveFilter
filterSequenceToTimeSensitiveFilter = timeSensitiveFilterSequenceToTimeSensitiveFilter . map (\(filter, time) -> (filterToTimeSensitiveFilter filter, time))

filterSequenceToFilter :: FilterSequence -> Filter
filterSequenceToFilter = ($ 0) . filterSequenceToTimeSensitiveFilter

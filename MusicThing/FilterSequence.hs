module MusicThing.FilterSequence where

import MusicThing.Sound
import MusicThing.Filter

type FilterSequence = [(Filter, Time)]
type TimeSensitiveFilterSequence = [(TimeSensitiveFilter, Time)]

timeOffsetFilterSequenceToTimeSensitiveFilter :: TimeSensitiveFilterSequence -> TimeSensitiveFilter
timeOffsetFilterSequenceToTimeSensitiveFilter [] = filterToTimeSensitiveFilter idFilter
timeOffsetFilterSequenceToTimeSensitiveFilter [(filter, length)] = cutoffTimeSensitiveFilter length filter
timeOffsetFilterSequenceToTimeSensitiveFilter (a:b) = timeOffsetFilterSequenceToTimeSensitiveFilter [a] `combineTimeSensitiveFilters` (offsetTimeSensitiveFilter (snd a) $ timeOffsetFilterSequenceToTimeSensitiveFilter b)

timeOffsetFilterSequenceToFilter :: TimeSensitiveFilterSequence -> Filter
timeOffsetFilterSequenceToFilter = ($ Time 0) . timeOffsetFilterSequenceToTimeSensitiveFilter

filterSequenceToTimeSensitiveFilter :: FilterSequence -> TimeSensitiveFilter
filterSequenceToTimeSensitiveFilter = timeOffsetFilterSequenceToTimeSensitiveFilter . map (\(filter, time) -> (filterToTimeSensitiveFilter filter, time))

filterSequenceToFilter :: FilterSequence -> Filter
filterSequenceToFilter = ($ Time 0) . filterSequenceToTimeSensitiveFilter

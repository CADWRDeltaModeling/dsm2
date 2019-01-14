#ifndef PATTERN_MATCH_HPP_
#define PATTERN_MATCH_HPP_

#ifdef _WIN32
#define pattern_match PATTERN_MATCH
#define pattern_match_index PATTERN_MATCH_INDEX
#define pattern_match_clear PATTERN_MATCH_CLEAR
#define pattern_match_count PATTERN_MATCH_COUNT
#define append_match APPEND_MATCH
#else
#define pattern_match pattern_match_
#define pattern_match_index pattern_match_index_
#define pattern_match_clear pattern_match_clear_
#define pattern_match_count pattern_match_count_
#define append_match append_match_
#endif
/* This module defines pattern matching routines that are used
   to match object identifiers such as channel numbers and reservoir names
   against regular expression patterns. At this time, they are used in 
   support of the groups module.
*/



#endif

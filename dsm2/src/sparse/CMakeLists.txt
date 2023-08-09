cmake_minimum_required (VERSION 3.2.2)
enable_language(C)
# search by file pattern and include in library
file(GLOB sources *.c)
list(FILTER sources EXCLUDE REGEX "sptest\.c")
add_library(sparse ${sources})
set_target_properties(sparse PROPERTIES POSITION_INDEPENDENT_CODE ON)
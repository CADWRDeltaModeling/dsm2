cmake_minimum_required (VERSION 3.2.2)
enable_language(C)
# search by file pattern and include in library
file(GLOB sources *.c)
add_library(sparse STATIC ${sources})

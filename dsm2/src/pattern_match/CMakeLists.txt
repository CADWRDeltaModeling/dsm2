cmake_minimum_required (VERSION 3.2.2)
enable_language(CXX)
# add pre build command
# search by file pattern and include in library
file(GLOB sources *.cpp)
include_directories("${BOOST_DIR}")
add_library(pattern_match_cpp STATIC ${sources})
target_compile_options(pattern_match_cpp PRIVATE "-fPIC")

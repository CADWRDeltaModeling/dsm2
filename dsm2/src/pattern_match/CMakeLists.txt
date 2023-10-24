cmake_minimum_required(VERSION 3.21)
enable_language(CXX)

add_library(pattern_match)
file(GLOB SOURCES *.cpp)
target_sources(pattern_match PRIVATE ${SOURCES})
target_include_directories(pattern_match PRIVATE ${Boost_INCLUDE_DIRS})

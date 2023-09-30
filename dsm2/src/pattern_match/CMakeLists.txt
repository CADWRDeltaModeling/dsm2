cmake_minimum_required(VERSION 3.17)
project(pattern_match LANGUAGES CXX)

add_library(${PROJECT_NAME})
file(GLOB SOURCES *.cpp)
target_sources(${PROJECT_NAME} PRIVATE ${SOURCES})
target_include_directories(${PROJECT_NAME} PRIVATE ${Boost_INCLUDE_DIRS})

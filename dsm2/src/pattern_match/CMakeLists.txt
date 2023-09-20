cmake_minimum_required(VERSION 3.17)
project(pattern_match LANGUAGES CXX)

# add pre build command
# search by file pattern and include in library
add_library(${PROJECT_NAME})
file(GLOB SOURCES *.cpp)
target_sources(${PROJECT_NAME} PRIVATE ${SOURCES})
target_include_directories(${PROJECT_NAME}
    PRIVATE ${BOOST_INCLUDE_DIR})

# set_target_properties(${PROJECT_NAME} PROPERTIES
# COMPILE_OPTIONS "$<$<CONFIG:Debug>:/MTd>$<$<CONFIG:Release>:/MT>"
# )

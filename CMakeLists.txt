cmake_minimum_required(VERSION 3.21)
project(dsm2 LANGUAGES C CXX Fortran)

set(CMAKE_DEBUG_POSTFIX "d")
if (WIN32)
  add_compile_options($<$<CONFIG:DEBUG>:$<$<COMPILE_LANGUAGE:Fortran>:/CB>>)
else()
  add_compile_options($<$<CONFIG:DEBUG>:$<$<COMPILE_LANGUAGE:Fortran>:-CB>>)
endif()

find_package(Python3 3.9 REQUIRED COMPONENTS Interpreter)

# Add Fortran-lang stdlib and fypp as submodules if they exist, otherwise fetch them
if(EXISTS ${CMAKE_CURRENT_SOURCE_DIR}/fypp/CMakeLists.txt)
  add_subdirectory("fypp")
else()
  set("fypp-url" "https://github.com/aradi/fypp")
  message(STATUS "Retrieving fypp from ${fypp-url}")
  include(FetchContent)
  FetchContent_Declare(
    "fypp"
    GIT_REPOSITORY "${fypp-url}"
    GIT_TAG "HEAD"
  )
  FetchContent_MakeAvailable("fypp")
  list(PREPEND CMAKE_PROGRAM_PATH "${fypp_SOURCE_DIR}/bin")
endif()

# Include the stdlib project
if(EXISTS ${CMAKE_CURRENT_SOURCE_DIR}/stdlib/CMakeLists.txt)
  add_subdirectory("stdlib")
else()
  set("stdlib-url" "https://github.com/fortran-lang/stdlib")
  message(STATUS "Retrieving stdlib from ${stdlib-url}")
  include(FetchContent)
  FetchContent_Declare(
    "fortran_stdlib"
    GIT_REPOSITORY "${stdlib-url}"
    GIT_TAG "v0.7.0"
  )
  FetchContent_MakeAvailable("fortran_stdlib")
  add_library(fortran_stdlib::fortran_stdlib ALIAS fortran_stdlib)
endif()

find_package(Boost REQUIRED)
set(HDF5_USE_STATIC_LIBRARIES TRUE)
find_package(HDF5 REQUIRED COMPONENTS HL C Fortran)

add_subdirectory(oprule)
add_subdirectory(input_storage)
add_subdirectory(dsm2)

cmake_minimum_required (VERSION 3.21)
enable_language(CXX Fortran)
# search by file pattern and include in library
#set (CMAKE_Fortran_FLAGS "/extend-source:132")
include_directories("${CMAKE_Fortran_MODULE_DIRECTORY}")
file(GLOB sources *.f90)
add_library(tools ${sources})
add_dependencies(tools gtm_core)
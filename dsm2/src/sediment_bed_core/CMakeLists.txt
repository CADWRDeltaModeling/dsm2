cmake_minimum_required (VERSION 3.2.2)
enable_language(CXX Fortran)
# add pre build command
# search by file pattern and include in library
#set (CMAKE_Fortran_FLAGS "/extend_source:132")
include_directories("${CMAKE_Fortran_MODULE_DIRECTORY}")
file(GLOB sources *.f90)
add_library(sediment_bed_core STATIC ${sources})

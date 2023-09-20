cmake_minimum_required (VERSION 3.17)
enable_language(CXX Fortran)
# add pre build command
# search by file pattern and include in library
#set (CMAKE_Fortran_FLAGS "/extend-source:132")
include_directories("${CMAKE_Fortran_MODULE_DIRECTORY}")
file(GLOB sources *.f90)
add_library(sediment_bed_core ${sources})
add_dependencies(sediment_bed_core transport)
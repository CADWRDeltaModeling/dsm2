cmake_minimum_required (VERSION 3.17)
enable_language(CXX Fortran)
# add pre build command
#add_custom_command(OUTPUT version.fi PRE_BUILD COMMAND python version_generate.py WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR})
# search by file pattern and include in library
#set (CMAKE_Fortran_FLAGS "/extend-source:132")
include_directories("${CMAKE_Fortran_MODULE_DIRECTORY}")
file(GLOB sources *.f90)
add_library(mercury ${sources})
add_dependencies(mercury gtm_core sediment_bed)

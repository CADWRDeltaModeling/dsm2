cmake_minimum_required (VERSION 3.2.2)
enable_language(CXX Fortran)
# add pre build command
#add_custom_command(OUTPUT version.fi PRE_BUILD COMMAND python version_generate.py WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR})
# search by file pattern and include in library
#set (CMAKE_Fortran_FLAGS "/fixed /extend_source:132")
include_directories("${CMAKE_Fortran_MODULE_DIRECTORY}" "..\\hydrolib" "..\\timevar" "../${HDF5_DIR}/include/static")
file(GLOB sources *.f)
add_library(fixed STATIC ${sources})

cmake_minimum_required (VERSION 3.2.2)
enable_language(CXX Fortran)
# add pre build command
#add_custom_command(OUTPUT version.fi PRE_BUILD COMMAND python version_generate.py WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR})
# search by file pattern and include in library
file(GLOB sources *.f)
# fortran flags
if (WIN32)
   set (CMAKE_Fortran_FLAGS "/extend_source:132")
else (WIN32)
   set (CMAKE_Fortran_FLAGS "-extend-source 132")
endif (WIN32)
#include directories
include_directories("../fixed" "../${HDF5_DIR}/include/static")
add_library(hdf_tidefile_fortran STATIC ${sources} "dsm2_tidefile_input_storage_fortran.f90")
file(GLOB cxx_sources *.cpp)
include_directories("../${HDF5_DIR}/include" "${DSM2_TOP_DIR}/input_storage/src/" "${BOOST_DIR}")
add_library(hdf_tidefile_cpp STATIC ${cxx_sources})
target_compile_options(hdf_tidefile_fortran PRIVATE "-fPIC")
target_compile_options(hdf_tidefile_cpp PRIVATE "-fPIC")
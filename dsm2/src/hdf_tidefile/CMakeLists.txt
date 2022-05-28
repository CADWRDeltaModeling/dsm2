cmake_minimum_required (VERSION 3.2.2)
enable_language(CXX Fortran)
# add pre build command
#add_custom_command(OUTPUT version.fi PRE_BUILD COMMAND python version_generate.py WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR})
# search by file pattern and include in library
file(GLOB sources *.f *.f90)
# fortran flags
if (WIN32)
   set (CMAKE_Fortran_FLAGS "/extend_source:132")
else (WIN32)
   set (CMAKE_Fortran_FLAGS "-extend-source 132")
endif (WIN32)
#include directories
add_library(hdf_tidefile_fortran ${sources} "dsm2_tidefile_input_storage_fortran.f90")
target_include_directories(hdf_tidefile_fortran PRIVATE "${CMAKE_SOURCE_DIR}../fixed" "../${HDF5_DIR}/include/static")
target_compile_options(hdf_tidefile_fortran PRIVATE "-fPIC")
add_dependencies(hdf_tidefile_fortran hdf_tidefile_cpp)

file(GLOB cxx_sources *.cpp)
add_library(hdf_tidefile_cpp ${cxx_sources})
target_include_directories(hdf_tidefile_cpp PRIVATE "${DSM2_TOP_DIR}/input_storage/src/" "${BOOST_DIR}")
target_compile_options(hdf_tidefile_cpp PRIVATE  "-fPIC")
add_dependencies(hdf_tidefile_cpp input_storage)

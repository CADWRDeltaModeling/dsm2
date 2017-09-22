cmake_minimum_required (VERSION 3.2.2)
enable_language(CXX Fortran)
# add pre build command
#add_custom_command(OUTPUT version.fi PRE_BUILD COMMAND python version_generate.py WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR})
# search by file pattern and include in library
file(GLOB sources *.f)
set (CMAKE_Fortran_FLAGS "/extend_source:132")
include_directories("../fixed")
add_library(hdf_tidefile_fortran STATIC ${sources} "dsm2_tidefile_input_storage_fortran.f90")
file(GLOB cxx_sources *.cpp)
include_directories("${DSM2_THIRD_PARTY_DIR}/hdf5_183_xp32_vs2008_ivf101/include" "${DSM2_TOP_DIR}/input_storage/src/" "${DSM2_THIRD_PARTY_DIR}/boost_1_39")
add_library(hdf_tidefile_cpp STATIC ${cxx_sources})

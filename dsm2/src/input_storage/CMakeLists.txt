cmake_minimum_required (VERSION 3.2.2)
enable_language(CXX Fortran)
# add pre build command
#add_custom_command(OUTPUT version.fi PRE_BUILD COMMAND python version_generate.py WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR})
# search by file pattern and include in library
file(GLOB sources *.cpp)
set(DSM2_TOP_DIR "../../../")
set(DSM2_THIRD_PARTY_DIR "${DSM2_TOP_DIR}/third_party")
include_directories("${DSM2_THIRD_PARTY_DIR}/hdf5_183_xp32_vs2008_ivf101/include" "${DSM2_TOP_DIR}/input_storage/src/" "${DSM2_THIRD_PARTY_DIR}/boost_1_39")
add_library(input_storage_cpp STATIC ${sources})
file(GLOB fortran_header_sources *.fi)
include_directories(".")
add_library(input_storage STATIC "input_storage_fortran.f90")

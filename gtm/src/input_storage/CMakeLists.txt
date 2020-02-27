cmake_minimum_required (VERSION 3.2.2)
enable_language(CXX Fortran)
# add pre build command
#add_custom_command(OUTPUT version.fi PRE_BUILD COMMAND python version_generate.py WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR})
# search by file pattern and include in library
file(GLOB sources *.cpp)
set(DSM2_TOP_DIR "${DSM2_TOP_DIR}..")
set(DSM2_THIRD_PARTY_DIR "${DSM2_TOP_DIR}/third_party")
add_library(input_storage_cpp STATIC ${sources})
target_include_directories(input_storage_cpp PRIVATE "${HDF5_DIR}/include"
    PRIVATE "${DSM2_TOP_DIR}/input_storage/src/" PRIVATE "${BOOST_DIR}")
if (WIN32)
set (CMAKE_Fortran_FLAGS "/extend_source:132")
else (WIN32)
set (CMAKE_Fortran_FLAGS "-extend_source 132")
endif (WIN32)
add_library(input_storage STATIC "input_storage_fortran.f90")
# file(GLOB fortran_header_sources *.fi)
target_include_directories(input_storage PRIVATE "." PRIVATE "${HDF5_DIR}/include/static")

cmake_minimum_required (VERSION 3.2.2)
enable_language(CXX Fortran)
add_custom_command(OUTPUT input_storage_fortran.f90 
  COMMAND python generate.py 
  DEPENDS generate.py
  WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR})
# search by file pattern and include in library
file(GLOB sources *.cpp)
set(DSM2_TOP_DIR "../../../")
set(DSM2_THIRD_PARTY_DIR "${DSM2_TOP_DIR}/third_party")
include_directories("${HDF5_DIR}/include" "${DSM2_TOP_DIR}/input_storage/src/" "${BOOST_DIR}")
include_directories("${DSM2_TOP_DIR}/input_storage/src/")
add_library(input_storage_cpp STATIC ${sources})
if (WIN32)
   set (CMAKE_Fortran_FLAGS "/extend_source:132")
else (WIN32)
   set (CMAKE_Fortran_FLAGS "-extend_source 132")
endif (WIN32)

file(GLOB fortran_header_sources *.fi)
include_directories("." "../${HDF5_DIR}/include/static")
add_library(input_storage STATIC ${CMAKE_CURRENT_SOURCE_DIR}/input_storage_fortran.f90)

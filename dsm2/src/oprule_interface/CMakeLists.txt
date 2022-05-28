cmake_minimum_required(VERSION 3.2.2)
enable_language(CXX Fortran)
# add pre build command add_custom_command(OUTPUT version.fi PRE_BUILD COMMAND
# python version_generate.py WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR})
# search by file pattern and include in library
set(DSM2_TOP_DIR "../../..")
if(DEFINED THIRD_PARTY_DIR)
  set(DSM2_THIRD_PARTY_DIR "${THIRD_PARTY_DIR}")
else()
  set(DSM2_THIRD_PARTY_DIR "${DSM2_TOP_DIR}/third_party")
endif()
# include_directories("${HDF5_DIR}/include" "${BOOST_DIR}")
# include_directories("${DSM2_TOP_DIR}/oprule"
# "${DSM2_TOP_DIR}/input_storage/src/"
# "${DSM2_THIRD_PARTY_DIR}/loki_0.1.7/include/loki")
file(GLOB sources *.cpp)
add_library(oprule_interface_cpp ${sources})
target_include_directories(
  oprule_interface_cpp
  PRIVATE "${DSM2_TOP_DIR}/oprule"
          "${DSM2_THIRD_PARTY_DIR}/loki_0.1.7/include/loki")

file(GLOB fortran_header_sources *.fi)
if(WIN32)
  set(CMAKE_Fortran_FLAGS "/extend_source:132")
else(WIN32)
  set(CMAKE_Fortran_FLAGS "-extend-source 132 -fPIC")
  set(CMAKE_CXX_FLAGS "-fPIC")
endif(WIN32)
file(GLOB fortran_sources *.f)
add_library(oprule_interface_fortran ${fortran_sources})
add_dependencies(oprule_interface_fortran hydrolib oprule_interface_cpp)

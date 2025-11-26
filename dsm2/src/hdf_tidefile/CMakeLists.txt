cmake_minimum_required(VERSION 3.21)
enable_language(CXX Fortran)

file(GLOB CXX_SOURCES *.cpp)
add_library(hdf_tidefile_cpp)
target_sources(hdf_tidefile_cpp PRIVATE ${CXX_SOURCES})
target_include_directories(
  hdf_tidefile_cpp PRIVATE ${CMAKE_CURRENT_SOURCE_DIR}/../../../input_storage/src
  ${Boost_INCLUDE_DIRS} ${HDF5_INCLUDE_DIRS})

file(GLOB Fortran_SOURCES *.f90)
add_library(hdf_tidefile)
target_sources(hdf_tidefile PRIVATE ${Fortran_SOURCES})
target_include_directories(hdf_tidefile PRIVATE
  ${HDF5_Fortran_INCLUDE_DIRS})
target_link_libraries(hdf_tidefile PUBLIC common dsm2_input_storage ${HDF5_LIBRARIES})
add_dependencies(hdf_tidefile hdf_tidefile_cpp dsm2_input_storage gtm_core
  common)

cmake_minimum_required(VERSION 3.17)
project(hdf_tidefile LANGUAGES CXX Fortran)

file(GLOB CXX_SOURCES *.cpp)
add_library(${PROJECT_NAME}_cpp)
target_sources(${PROJECT_NAME}_cpp PRIVATE ${CXX_SOURCES})
target_include_directories(
  ${PROJECT_NAME}_cpp PRIVATE ${CMAKE_CURRENT_SOURCE_DIR}/../../../input_storage/src
  ${Boost_INCLUDE_DIRS} ${HDF5_INCLUDE_DIRS})

file(GLOB Fortran_SOURCES *.f90)
add_library(${PROJECT_NAME})
target_sources(${PROJECT_NAME} PRIVATE ${Fortran_SOURCES})
target_include_directories(${PROJECT_NAME} PRIVATE
  ${HDF5_Fortran_INCLUDE_DIRS})
add_dependencies(${PROJECT_NAME} ${PROJECT_NAME}_cpp input_storage gtm_core
  common)

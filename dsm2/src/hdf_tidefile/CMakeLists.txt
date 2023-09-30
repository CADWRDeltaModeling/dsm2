cmake_minimum_required(VERSION 3.17)
project(hdf_tidefile LANGUAGES CXX Fortran)

set(LIB_CXX ${PROJECT_NAME}_cpp)
file(GLOB CXX_SOURCES *.cpp)
add_library(${LIB_CXX})
target_sources(${LIB_CXX} PRIVATE ${CXX_SOURCES})
target_include_directories(
  ${LIB_CXX} PRIVATE ${CMAKE_CURRENT_SOURCE_DIR}/../../../input_storage/src
                     ${Boost_INCLUDE_DIRS} ${HDF5_INCLUDE_DIRS})

file(GLOB Fortran_SOURCES *.f90)
add_library(${PROJECT_NAME})
target_sources(${PROJECT_NAME} PRIVATE ${Fortran_SOURCES})
target_include_directories(${PROJECT_NAME} PRIVATE ${Fortran_MODULE_DIR}
                                                   ${HDF5_Fortran_INCLUDE_DIRS})
add_dependencies(${PROJECT_NAME} ${PROJECT_NAME}_cpp input_storage gtm_core
                 common)

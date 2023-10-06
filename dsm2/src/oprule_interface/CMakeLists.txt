cmake_minimum_required(VERSION 3.17)
project(oprule_interface LANGUAGES CXX Fortran)

set(LIB_FORT ${PROJECT_NAME})
add_library(${LIB_FORT})
file(GLOB FORTRAN_SOURCES *.f *.f90 *.fi)
target_sources(${LIB_FORT} PRIVATE ${FORTRAN_SOURCES})
target_include_directories(${LIB_FORT} PRIVATE ${HDF5_Fortran_INCLUDE_DIRS}
                                               ${Fortan_MODULE_DIRECTORY})

add_dependencies(${LIB_FORT} hydrolib)
set(LIB_CXX ${PROJECT_NAME}_cpp)
add_library(${LIB_CXX})
file(GLOB SOURCES *.cpp)
target_sources(${LIB_CXX} PRIVATE ${SOURCES})
target_include_directories(
  ${LIB_CXX}
  PRIVATE ${Boost_INCLUDE_DIRS} ${CMAKE_CURRENT_SOURCE_DIR}/../../../oprule
          ${DSM2_OPRULE_INTERFACE} ${Loki_INCLUDE_DIRS})
add_dependencies(${LIB_CXX} ${LIB_FORT})

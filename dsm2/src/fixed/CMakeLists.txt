cmake_minimum_required(VERSION 3.17)
project(fixed LANGUAGES Fortran)

add_library(${PROJECT_NAME})
file(GLOB SOURCES *.f90)
target_sources(${PROJECT_NAME} PRIVATE ${SOURCES})
target_include_directories(${PROJECT_NAME} PRIVATE ${HDF5_Fortran_INCLUDE_DIRS})

add_dependencies(${PROJECT_NAME} hdf_tidefile common)

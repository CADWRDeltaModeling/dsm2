cmake_minimum_required(VERSION 3.17)
project(hydrolib LANGUAGES Fortran)

file(GLOB SOURCES *.f90 *.F90)
add_library(${PROJECT_NAME})
target_sources(${PROJECT_NAME} PRIVATE ${SOURCES})

target_include_directories(${PROJECT_NAME} PRIVATE ${CMAKE_Fortran_MODULE_DIRECTORY}
                                                   ${HDF5_Fortran_INCLUDE_DIRS})

add_dependencies(${PROJECT_NAME} hdf_tidefile input_storage common)

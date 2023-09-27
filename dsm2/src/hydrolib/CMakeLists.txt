cmake_minimum_required(VERSION 3.17)
project(hydrolib LANGUAGES Fortran)

file(GLOB SOURCES *.f90 *.F90)
add_library(${PROJECT_NAME})
target_sources(${PROJECT_NAME} PRIVATE ${SOURCES})

target_include_directories(${PROJECT_NAME} PRIVATE
    ${Fortran_MODULE_DIRECTORY}
    ${HDF5_Fortran_MODULE_PATH})

add_dependencies(${PROJECT_NAME} klu_fortran hdf_tidefile gtm_core common)

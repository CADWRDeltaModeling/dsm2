cmake_minimum_required(VERSION 3.17)
project(gtm_core LANGUAGES Fortran)

add_library(${PROJECT_NAME})
file(GLOB SOURCES *.F90 *.f90)
target_sources(${PROJECT_NAME} PRIVATE ${SOURCES})

target_include_directories(${PROJECT_NAME} PUBLIC
    ${Fortan_MODULE_DIRECTORY}
    ${HDF5_Fortran_MODULE_PATH})

add_dependencies(${PROJECT_NAME} common)
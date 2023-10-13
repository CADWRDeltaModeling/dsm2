cmake_minimum_required(VERSION 3.17)
project(common LANGUAGES Fortran)
# add pre build command
add_custom_target(version_file
    BYPRODUCTS dsm2_version.f90
    COMMAND python ${CMAKE_CURRENT_SOURCE_DIR}/version_generate.py)

file(GLOB SOURCES *.F90 *.f90)
add_library(${PROJECT_NAME} ${SOURCES})
set_source_files_properties(network.F90 PROPERTIES Fortran_PREPROCESS ON)
add_dependencies(${PROJECT_NAME} version_file)

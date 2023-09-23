cmake_minimum_required(VERSION 3.17)
project(common LANGUAGES Fortran)
# add pre build command
add_custom_target(version_file
    BYPRODUCTS dsm2_version.f90
    COMMAND python ${CMAKE_CURRENT_SOURCE_DIR}/version_generate.py)

file(GLOB SOURCES *.f90)
add_library(${PROJECT_NAME} ${SOURCES})
add_dependencies(${PROJECT_NAME} version_file)

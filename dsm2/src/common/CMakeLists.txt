cmake_minimum_required(VERSION 3.21)
enable_language(Fortran)
# add pre build command
add_custom_target(version_file
    BYPRODUCTS dsm2_version.inc
    COMMAND ${Python3_EXECUTABLE} ${CMAKE_CURRENT_SOURCE_DIR}/version_generate.py
    WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR})

file(GLOB SOURCES *.F90 *.f90)
add_library(common ${SOURCES})
target_link_libraries(common PRIVATE  fortran_stdlib::fortran_stdlib)
set_source_files_properties(network.F90 PROPERTIES Fortran_PREPROCESS ON)
add_dependencies(common version_file)

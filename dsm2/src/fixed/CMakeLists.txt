cmake_minimum_required(VERSION 3.21)
enable_language(Fortran)

add_library(fixed)
file(GLOB SOURCES *.f90)
target_sources(fixed PRIVATE ${SOURCES})
target_include_directories(fixed PRIVATE ${HDF5_Fortran_INCLUDE_DIRS})
target_link_libraries(fixed PUBLIC hdf_tidefile dsm2_input_storage common)

add_dependencies(fixed hdf_tidefile common)

cmake_minimum_required(VERSION 3.21)
enable_language(Fortran)

add_library(fixed)
file(GLOB SOURCES *.f90)
target_sources(fixed PRIVATE ${SOURCES})
target_include_directories(fixed PRIVATE ${HDF5_Fortran_INCLUDE_DIRS})
target_link_libraries(fixed PUBLIC hydrolib hdf_tidefile dsm2_input_storage oprule_interface common)

add_dependencies(fixed hydrolib hdf_tidefile dsm2_input_storage oprule_interface common)

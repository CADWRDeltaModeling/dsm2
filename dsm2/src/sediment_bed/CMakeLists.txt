cmake_minimum_required(VERSION 3.21)
enable_language(CXX Fortran)
file(GLOB SOURCES *.f90)
add_library(sediment_bed ${SOURCES})
target_include_directories(sediment_bed PUBLIC ${HDF5_Fortran_INCLUDE_DIRS})
add_dependencies(sediment_bed sediment_bed_core process_io)

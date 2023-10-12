cmake_minimum_required(VERSION 3.17)
enable_language(CXX Fortran)
file(GLOB SOURCES *.f90)
add_library(sediment_bed_core ${SOURCES})
target_include_directories(sediment_bed_core
                           PUBLIC ${HDF5_Fortran_INCLUDE_DIRS})
add_dependencies(sediment_bed_core gtm_core)

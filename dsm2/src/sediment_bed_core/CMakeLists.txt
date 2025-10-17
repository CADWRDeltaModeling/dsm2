cmake_minimum_required(VERSION 3.21)
enable_language(CXX Fortran)
file(GLOB SOURCES *.f90)
add_library(sediment_bed_core ${SOURCES})
target_include_directories(sediment_bed_core PUBLIC
                           ${HDF5_Fortran_INCLUDE_DIRS})
target_link_libraries(sediment_bed_core PUBLIC gtm_core)
add_dependencies(sediment_bed_core gtm_core)

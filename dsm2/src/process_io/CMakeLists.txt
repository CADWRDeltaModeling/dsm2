cmake_minimum_required(VERSION 3.21)
enable_language(CXX Fortran)
file(GLOB sources *.f90)
add_library(process_io ${sources})
add_dependencies(process_io gtm_core dsm2_input_storage)
target_include_directories(process_io PUBLIC
    ${HDF5_Fortran_INCLUDE_DIRS})
target_link_libraries(process_io PUBLIC gtm_core dsm2_input_storage qual_core)

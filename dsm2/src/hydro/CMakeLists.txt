cmake_minimum_required(VERSION 3.21)
enable_language(Fortran)

add_library(fourpt)
target_sources(fourpt PRIVATE fourpt.f90 dsm2_hydro_init.f90)
target_include_directories(fourpt PRIVATE ${HDF5_Fortran_INCLUDE_DIRS})
target_link_libraries(fourpt PUBLIC hydrolib oprule_interface common fortran_stdlib::fortran_stdlib)
add_dependencies(fourpt hydrolib oprule_interface common)

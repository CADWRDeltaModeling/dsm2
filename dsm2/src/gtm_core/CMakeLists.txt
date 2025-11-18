cmake_minimum_required(VERSION 3.21)
enable_language(Fortran)

add_library(gtm_core)
file(GLOB SOURCES *.F90 *.f90)
target_sources(gtm_core PRIVATE ${SOURCES})

target_include_directories(gtm_core PUBLIC ${HDF5_Fortran_INCLUDE_DIRS})

target_link_libraries(gtm_core PUBLIC common ${HDF5_LIBRARIES})
add_dependencies(gtm_core common)

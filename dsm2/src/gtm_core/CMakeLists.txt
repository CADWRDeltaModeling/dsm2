cmake_minimum_required(VERSION 3.21)
enable_language(Fortran)

add_library(gtm_core)
file(GLOB SOURCES *.F90 *.f90)
target_sources(gtm_core PRIVATE ${SOURCES})
set_source_files_properties(common_xsect.F90 PROPERTIES Fortran_PREPROCESS ON)

target_include_directories(gtm_core PUBLIC ${Fortan_MODULE_DIRECTORY}
                                                  ${HDF5_Fortran_INCLUDE_DIRS})

add_dependencies(gtm_core common)

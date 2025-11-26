cmake_minimum_required(VERSION 3.21)
enable_language(Fortran)

file(GLOB SOURCES *.f90 *.F90)
add_library(hydrolib)
target_sources(hydrolib PRIVATE ${SOURCES})
set_source_files_properties(channel_xsect_tbl.F90 PROPERTIES Fortran_PREPROCESS ON)
target_include_directories(hydrolib PRIVATE ${CMAKE_Fortran_MODULE_DIRECTORY}
  ${HDF5_Fortran_INCLUDE_DIRS})
target_link_libraries(hydrolib PUBLIC hdf_tidefile dsm2_input_storage common)
add_dependencies(hydrolib hdf_tidefile dsm2_input_storage common)

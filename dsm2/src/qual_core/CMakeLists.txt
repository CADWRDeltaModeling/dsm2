add_library(qual_core)
file(GLOB QUALCORE_SOURCE *.f90)
target_sources(
  qual_core
  PRIVATE ${QUALCORE_SOURCE})
target_link_libraries(qual_core PUBLIC fixed hydrolib common)
target_include_directories(qual_core PUBLIC ${HDF5_Fortran_INCLUDE_DIRS})
add_dependencies(qual_core hdf_tidefile fixed hydrolib)

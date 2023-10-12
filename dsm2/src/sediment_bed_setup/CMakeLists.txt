cmake_minimum_required(VERSION 3.17)
file(GLOB SOURCES *.f90)
add_executable(sed_bed_init ${SOURCES})
target_link_libraries(
  sed_bed_init PRIVATE gtm_core common ${HDF5_HL_LIBRARIES} ${HEClib_LIBRARIES}
                       ${HDF5_LIBRARIES})
add_dependencies(sed_bed_init process_io)

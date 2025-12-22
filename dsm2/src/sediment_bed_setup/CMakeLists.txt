file(GLOB SOURCES *.f90)
add_executable(sed_bed_init ${SOURCES})
set_target_properties(sed_bed_init PROPERTIES LINKER_LANGUAGE Fortran
                                              INSTALL_RPATH "\${ORIGIN}/../lib")
target_link_libraries(
  sed_bed_init PRIVATE process_io gtm_core common ${HDF5_HL_LIBRARIES} ${HDF5_LIBRARIES})
add_dependencies(sed_bed_init process_io)

if(WIN32)
  target_link_options(sed_bed_init PRIVATE "/STACK:8000000")
endif(WIN32)

# Build Qual
set(QUAL_BIN qual)
add_executable(${QUAL_BIN})
if(NOT WIN32)
  set_target_properties(${QUAL_BIN} PROPERTIES LINKER_LANGUAGE Fortran INSTALL_RPATH "\${ORIGIN}/../lib")
endif()
target_sources(${QUAL_BIN} PRIVATE qual_driver.f90)
target_link_libraries(
  ${QUAL_BIN}
  PRIVATE qual_core
          fixed
          hydrolib
          oprule_interface
          oprule_interface_cpp
          dsm2_input_storage
          dsm2_input_storage_cpp
          hdf_tidefile
          hdf_tidefile_cpp
          common
          pattern_match
          input_storage
          ${Boost_LIBRARIES}
          ${HDF5_HL_LIBRARIES}
          ${HDF5_LIBRARIES}
          ${KLU_LIBRARIES}
          ${HEClib_LIBRARIES})
add_dependencies(${QUAL_BIN} qual_core)

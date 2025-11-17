set(HYDRO_BIN hydro)
add_executable(${HYDRO_BIN} hydro_driver.f90)
set_target_properties(${HYDRO_BIN} PROPERTIES LINKER_LANGUAGE Fortran
                                              INSTALL_RPATH "\${ORIGIN}/../lib")
add_dependencies(${HYDRO_BIN} fourpt)
target_link_libraries(
  ${HYDRO_BIN}
  PRIVATE fourpt
          hydrolib
          fixed
          hdf_tidefile
          hdf_tidefile_cpp
          oprule_interface_cpp
          oprule_interface
          transport
          dsm2_input_storage
          dsm2_input_storage_cpp
          klu_fortran
          common
          pattern_match
          input_storage
          oprule_parser
          oprule
          ${Boost_LIBRARIES}
          ${HDF5_HL_LIBRARIES}
          ${HDF5_LIBRARIES}
          ${KLU_LIBRARIES}
          ${HEClib_LIBRARIES}
          fortran_stdlib::fortran_stdlib)
target_compile_options(
  ${HYDRO_BIN} PRIVATE "$<TARGET_PROPERTY:MKL::MKL,INTERFACE_COMPILE_OPTIONS>")
target_link_libraries(${HYDRO_BIN} PRIVATE "$<LINK_ONLY:MKL::MKL>")

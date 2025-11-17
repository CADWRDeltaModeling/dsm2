cmake_minimum_required(VERSION 3.21)
enable_language(Fortran)

add_executable(hydro_gtm)
add_dependencies(hydro_gtm fourpt gtm_main)
target_sources(hydro_gtm PRIVATE hydro_gtm_driver.f90)
target_include_directories(hydro_gtm PRIVATE ${CMAKE_Fortran_MODULE_DIRECTORY}
                                             ${HDF5_Fortran_INCLUDE_DIRS})
set_target_properties(hydro_gtm PROPERTIES LINKER_LANGUAGE Fortran)
if(WIN32)
  target_link_options(hydro_gtm PRIVATE "/STACK:90000000")
endif(WIN32)
target_link_libraries(
  hydro_gtm
  PRIVATE hydro_gtm_core
          hydro_gtm_cpp
          fourpt
          ${GTM_MAIN}
          ${GTM_MERCURY}
          ${GTM_SEDIMENT_BED}
          ${GTM_SEDIMENT_BED_CORE}
          ${GTM_SEDIMENT}
          ${GTM_TRANSPORT}
          ${GTM_CORE}
          ${GTM_PROCESS_IO}
          hydrolib
          fixed
          hdf_tidefile
          hdf_tidefile_cpp
          dsm2_input_storage
          dsm2_input_storage_cpp
          oprule_interface_cpp
          oprule_interface
          common
          klu_fortran
          pattern_match
          input_storage
          oprule_parser
          oprule
          ${Boost_LIBRARIES}
          ${HDF5_HL_LIBRARIES}
          ${HDF5_Fortran_LIBRARIES}
          ${KLU_LIBRARIES}
          ${HEClib_LIBRARIES})
target_compile_options(
  hydro_gtm PRIVATE "$<TARGET_PROPERTY:MKL::MKL,INTERFACE_COMPILE_OPTIONS>")
target_link_libraries(hydro_gtm PRIVATE "$<LINK_ONLY:MKL::MKL>")

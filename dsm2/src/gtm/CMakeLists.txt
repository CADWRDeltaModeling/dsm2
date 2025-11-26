cmake_minimum_required(VERSION 3.21)
enable_language(Fortran)
add_library(gtm_main)
target_sources(
  gtm_main
  PRIVATE boundary_advection_network.f90 boundary_diffusion_network.f90
          gtm_source.f90 gtm_subs.f90 gtm.f90 state_variables_network.f90)
target_include_directories(gtm_main PUBLIC ${HDF5_Fortran_INCLUDE_DIRS})
target_link_libraries(gtm_main PUBLIC transport mercury process_io)
add_dependencies(gtm_main mercury sediment_bed sediment gtm_core)

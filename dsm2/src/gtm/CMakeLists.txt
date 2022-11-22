cmake_minimum_required(VERSION 3.2.2)
enable_language(Fortran)
# add pre build command add_custom_command(OUTPUT version.fi PRE_BUILD COMMAND
# python version_generate.py WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR})
# search by file pattern and include in library
if(WIN32)
  set(CMAKE_Fortran_FLAGS "/free")
else(WIN32)
  set(CMAKE_Fortran_FLAGS "-free")
endif(WIN32)
add_library(gtm_main)
target_sources(
  gtm_main
  PRIVATE boundary_advection_network.f90
          boundary_diffusion_network.f90
          gtm_source.f90
          gtm_subs.f90
          gtm.f90
          state_variables_network.f90)
add_dependencies(gtm_main gtm_core process_io mercury)

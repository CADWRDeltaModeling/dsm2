cmake_minimum_required(VERSION 3.17)
enable_language(Fortran)
# add pre build command add_custom_command(OUTPUT version.fi PRE_BUILD COMMAND
# python version_generate.py WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR})
# search by file pattern and include in library
if(WIN32)
  set(CMAKE_Fortran_FLAGS "/free")
else(WIN32)
  set(CMAKE_Fortran_FLAGS "-free")
endif(WIN32)
add_library(qual_core)
target_sources(
  qual_core
  PRIVATE balanceflo.f90
          boundary.f90
          heat.f90
          node_rate.f90
          qual.f90
          route.f90
          calscsk.f90
          interpx.f90
          qual_hdf_output.f90
          checkerror.f90
          print_outqual.f90
          rate_chanres.f90
          kinetic.f90
          process_tide.f90
          res_rate.f90
          bltminit.f90
          get_output.f90
          masstrack.f90
          restart_file.f90)
add_dependencies(qual_core fixed)

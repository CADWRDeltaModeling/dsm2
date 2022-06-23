cmake_minimum_required(VERSION 3.2.2)
enable_language(Fortran)
# search by file pattern and include in library
if(WIN32)
  set(CMAKE_Fortran_FLAGS "/free")
else(WIN32)
  set(CMAKE_Fortran_FLAGS "-free")
endif(WIN32)
include_directories("${CMAKE_Fortran_MODULE_DIRECTORY}" "../hdf_tidefile"
                    "../fixed" "../${HDF5_DIR}/include/static")
file(GLOB sources *.f *.f90)
add_library(hydrolib STATIC ${sources})
add_dependencies(hydrolib hdf_tidefile_fortran common)

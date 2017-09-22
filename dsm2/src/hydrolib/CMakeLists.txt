cmake_minimum_required (VERSION 3.2.2)
enable_language(Fortran)
# add pre build command
#add_custom_command(OUTPUT version.fi PRE_BUILD COMMAND python version_generate.py WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR})
# search by file pattern and include in library
include_directories("${CMAKE_Fortran_MODULE_DIRECTORY}" "../hdf_tidefile" "../fixed" "${DSM2_THIRD_PARTY_DIR}/hdf5_183_xp32_vs2008_ivf101/include")
file(GLOB sources *.f)
add_library(hydrolib STATIC ${sources})

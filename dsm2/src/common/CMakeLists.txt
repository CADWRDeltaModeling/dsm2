cmake_minimum_required (VERSION 3.2.2)
enable_language(Fortran)
# add pre build command
add_custom_target(generate_version COMMAND python version_generate.py WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR})
# search by file pattern and include in library
if (WIN32)
   set (CMAKE_Fortran_FLAGS "/free")
else (WIN32)
   set (CMAKE_Fortran_FLAGS "-free")
endif (WIN32)
set(GTM_CORE_DIR "../gtm_core")
file(GLOB sources *.f ${GTM_CORE_DIR}/gtm_precision.f90 ${GTM_CORE_DIR}/common_xsect.f90 ${GTM_CORE_DIR}/common_variables.f90 ${GTM_CORE_DIR}/gtm_precision.f90 ${GTM_CORE_DIR}/gtm_logging.f90 ${GTM_CORE_DIR}/error_handling.f90)
add_library(common STATIC ${sources} "version.fi")
add_dependencies(common generate_version)

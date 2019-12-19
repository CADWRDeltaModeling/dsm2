cmake_minimum_required (VERSION 3.2.2)
enable_language(CXX)
# add pre build command
#add_custom_command(OUTPUT version.fi PRE_BUILD COMMAND python version_generate.py WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR})
# search by file pattern and include in library
set(DSM2_TOP_DIR "../../../")
if (DEFINED THIRD_PARTY_DIR)
  set(DSM2_THIRD_PARTY_DIR "${THIRD_PARTY_DIR}")
else()
  set(DSM2_THIRD_PARTY_DIR "${DSM2_TOP_DIR}/third_party")
endif()
set(SUITE_SPARSE_DIR "${DSM2_THIRD_PARTY_DIR}/SuiteSparse")
include_directories("${SUITE_SPARSE_DIR}/KLU/Include" "${SUITE_SPARSE_DIR}/AMD/Include" "${SUITE_SPARSE_DIR}/BTF/Include" "${SUITE_SPARSE_DIR}/COLAMD/Include" "${SUITE_SPARSE_DIR}/SuiteSparse_config")
add_library(klu_fortran STATIC "klu_fortran.c")

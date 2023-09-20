cmake_minimum_required(VERSION 3.17)
project(fixed LANGUAGES Fortran)

# add pre build command
# add_custom_command(OUTPUT version.fi PRE_BUILD COMMAND python version_generate.py WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR})
# search by file pattern and include in library
add_library(${PROJECT_NAME})
file(GLOB SOURCES *.f *.f90)
target_sources(${PROJECT_NAME} PRIVATE ${SOURCES})
target_include_directories(${PROJECT_NAME} PRIVATE
    ${DSM2_CORE_DIR}
    ${Fortran_MODULE_DIRECTORY}
    ${DSM2_HYDROLIB}
    ${HDF5_Fortran_MODULE_PATH})

# target_link_libraries(${PROJECT_NAME} PRIVATE
# hdf_tidefile_fortran gtm_core input_storage common)

# target_link_libraries(fixed gtm_core)
add_dependencies(${PROJECT_NAME} hdf_tidefile common)

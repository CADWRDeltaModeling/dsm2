cmake_minimum_required(VERSION 3.17)
project(fourpt LANGUAGES Fortran)

# add pre build command add_custom_command(OUTPUT version.fi PRE_BUILD COMMAND
# python version_generate.py WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR})
# search by file pattern and include in library
add_library(${PROJECT_NAME})
target_sources(${PROJECT_NAME} PRIVATE fourpt.f90 dsm2_hydro_init.f90)
target_include_directories(${PROJECT_NAME} PRIVATE
    ${Fortran_MODULE_DIR}
    ${HDF5_Fortran_MODULE_PATH})
add_dependencies(${PROJECT_NAME} hydrolib oprule_interface_fortran)
# target_link_libraries(${PROJECT_NAME} PRIVATE hdf_tidefile hydrolib common)

cmake_minimum_required(VERSION 3.17)
project(gtm_core LANGUAGES Fortran)

# add pre build command
# add_custom_command(OUTPUT version.fi PRE_BUILD COMMAND python version_generate.py WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR})
# search by file pattern and include in library
# set (CMAKE_Fortran_FLAGS "/extend-source:132")
# include_directories("${CMAKE_Fortran_MODULE_DIRECTORY}")
# set(DSM2_COMMONS_DIR "../common")
add_library(${PROJECT_NAME})
file(GLOB SOURCES *.f90) # ${DSM2_COMMONS_DIR}/grid_data.f90 ${DSM2_COMMONS_DIR}/type_defs.f90 ${DSM2_COMMONS_DIR}/constants.f90)
target_sources(${PROJECT_NAME} PRIVATE ${SOURCES})

target_include_directories(${PROJECT_NAME} PUBLIC
    ${Fortan_MODULE_DIRECTORY}
    ${HDF5_Fortran_MODULE_PATH})

add_dependencies(${PROJECT_NAME} common)
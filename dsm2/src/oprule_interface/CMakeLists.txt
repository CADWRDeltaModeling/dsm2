cmake_minimum_required(VERSION 3.17)
project(oprule_interface
    LANGUAGES CXX Fortran)

# add pre build command add_custom_command(OUTPUT version.fi PRE_BUILD COMMAND
# python version_generate.py WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR})
# search by file pattern and include in library
set(LIB_CXX ${PROJECT_NAME}_cpp)
add_library(${LIB_CXX})
file(GLOB SOURCES *.cpp)
target_sources(${LIB_CXX} PRIVATE ${SOURCES})
target_include_directories(${LIB_CXX} PRIVATE
    ${BOOST_INCLUDE_DIR}
    ${DSM2_TOP_DIR}/oprule
    ${DSM2_OPRULE_INTERFACE}
    ${DSM2_THIRD_PARTY_DIR}/loki_0.1.7/include/loki)

# set_target_properties(oprule_interface_cpp PROPERTIES
# COMPILE_OPTIONS "$<$<CONFIG:Debug>:/MTd>$<$<CONFIG:Release>:/MT>"
# )
set(LIB_FORT ${PROJECT_NAME}_fortran)
add_library(${LIB_FORT})
file(GLOB FORTRAN_SOURCES *.f *.f90 *.fi)
target_sources(${LIB_FORT} PRIVATE
    ${FORTRAN_SOURCES})
target_include_directories(${LIB_FORT} PRIVATE
    ${HDF5_Fortran_MODULE_PATH}
    ${CMAKE_Fortan_MODULE_DIRECTORY})

add_dependencies(${LIB_FORT} ${LIB_CXX} hydrolib)

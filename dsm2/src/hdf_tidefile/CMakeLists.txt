cmake_minimum_required(VERSION 3.17)
project(hdf_tidefile LANGUAGES CXX Fortran)

set(LIB_CXX ${PROJECT_NAME}_cpp)
file(GLOB CXX_SOURCES *.cpp)
add_library(${LIB_CXX})
target_sources(${LIB_CXX} PRIVATE ${CXX_SOURCES})
target_include_directories(
    ${LIB_CXX} PRIVATE ${DSM2_TOP_DIR}/input_storage/src ${BOOST_INCLUDE_DIR}
                       ${HDF5_DIR}/include)

file(GLOB Fortran_SOURCES *.f *.f90)
add_library(${PROJECT_NAME})
target_sources(${PROJECT_NAME} PRIVATE ${Fortran_SOURCES})
target_include_directories(${PROJECT_NAME} PRIVATE ${Fortran_MODULE_DIR}
                                                   ${HDF5_Fortran_MODULE_PATH})
add_dependencies(${PROJECT_NAME} input_storage gtm_core common)

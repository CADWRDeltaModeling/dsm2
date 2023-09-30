cmake_minimum_required(VERSION 3.17)
project(fourpt LANGUAGES Fortran)

add_library(${PROJECT_NAME})
target_sources(${PROJECT_NAME} PRIVATE fourpt.f90 dsm2_hydro_init.f90)
target_include_directories(${PROJECT_NAME} PRIVATE ${Fortran_MODULE_DIR}
                                                   ${HDF5_Fortran_INCLUDE_DIRS})
add_dependencies(${PROJECT_NAME} hydrolib common)

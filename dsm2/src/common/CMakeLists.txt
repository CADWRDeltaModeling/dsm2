cmake_minimum_required(VERSION 3.17)
project(common LANGUAGES Fortran)
# add pre build command
# add_custom_target(generate_version
#     COMMAND python version_generate.py
#     BYPRODUCTS version.fi
#     WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR})

# set(GTM_CORE_DIR "../gtm_core")
# include_directories(${GTM_CORE_DIR})
file(GLOB SOURCES *.f *.f90 *.fi)
add_library(${PROJECT_NAME})
target_sources(${PROJECT_NAME} PRIVATE ${SOURCES})
# add_dependencies(common generate_version)
# target_link_libraries(common ${HECLIB_LIBRARIES})

enable_language(CXX Fortran)

file(GLOB HYDRO_GTM_SOURCES *.f90)
add_library(hydro_gtm_core ${HYDRO_GTM_SOURCES})

file(GLOB HYDRO_GTM_CXX_SOURCES *.cpp)
add_library(hydro_gtm_cpp ${HYDRO_GTM_CXX_SOURCES})
target_include_directories(hydro_gtm_cpp PUBLIC
    ${CMAKE_SOURCE_DIR}/dsm2/src/oprule_interface)
target_link_libraries(hydro_gtm_cpp PRIVATE oprule_interface_cpp)
add_dependencies(hydro_gtm_cpp oprule_interface_cpp)

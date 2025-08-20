set(CMAKE_C_COMPILER
    icx
    CACHE STRING "C compiler")
set(CMAKE_CXX_COMPILER
    icpx
    CACHE STRING "C++ compiler")
set(CMAKE_Fortran_COMPILER
    ifx
    CACHE STRING "Fortran compiler")
set(CMAKE_CXX_STANDARD 14 CACHE STRING "C++ standard")
set(CMAKE_POSITION_INDEPENDENT_CODE
    ON
    CACHE BOOL "Position independent code")
set(CMAKE_Fortran_FLAGS_INIT
    "-Dhydro_1000 -traceback"
    CACHE STRING "Fortran flags to use a large array for the dense dx")

set(BOOST_ROOT
    ${CMAKE_SOURCE_DIR}/deps/boost_1_83_0
    CACHE PATH "Boost root directory")
set(HDF5_ROOT
    ${CMAKE_SOURCE_DIR}/deps/hdf5-1.14.2
    CACHE PATH "HDF5 root directory")
set(HEClib_ROOT
    ${CMAKE_SOURCE_DIR}/deps/heclib-6
    CACHE PATH "HEClib root directory")
set(KLU_ROOT
    ${CMAKE_SOURCE_DIR}/deps/suitesparse-4.0.2
    CACHE PATH "KLU root directory")
set(Loki_ROOT
    ${CMAKE_SOURCE_DIR}/deps/loki-0.1.7
    CACHE PATH "Loki root directory")

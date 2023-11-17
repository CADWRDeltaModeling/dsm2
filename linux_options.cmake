set(JAVA_HOME
    ${CMAKE_SOURCE_DIR}/deps/jdk/jdk-21.0.1
    CACHE PATH "Java home")
set(CMAKE_C_COMPILER
    icc
    CACHE STRING "C compiler")
set(CMAKE_CXX_COMPILER
    icpc
    CACHE STRING "C++ compiler")
set(CMAKE_Fortran_COMPILER
    ifort
    CACHE STRING "Fortran compiler")
set(CMAKE_CXX_STANDARD 11 CACHE STRING "C++ standard")
set(CMAKE_POSITION_INDEPENDENT_CODE
    ON
    CACHE BOOL "Position independent code")
set(CMAKE_Fortran_FLAGS_INIT
    "-Dhydro_1000"
    CACHE STRING "Fortran flags to use a large array for the dense dx")

set(BOOST_ROOT
    ${CMAKE_SOURCE_DIR}/deps/boost-1.83.0
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

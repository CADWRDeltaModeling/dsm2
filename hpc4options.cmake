set(JAVA_HOME
    $ENV{HOME}/local/jdk/jdk1.6.0_45_64bit
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
# set(CMAKE_CXX_STANDARD 17 CACHE STRING "C++ standard")
set(CMAKE_POSITION_INDEPENDENT_CODE
    ON
    CACHE BOOL "Position independent code")

set(BOOST_ROOT
    $ENV{HOME}/local/boost-1.83.0
    CACHE PATH "Boost root directory")
set(HDF5_ROOT
    $ENV{HOME}/local/hdf5-1.10.10
    CACHE PATH "HDF5 root directory")
set(HEClib_ROOT
    $ENV{HOME}/local/heclib-6
    CACHE PATH "HEClib root directory")
set(KLU_ROOT
    $ENV{HOME}/local/suitesparse-4.0.2
    CACHE PATH "KLU root directory")
set(Loki_ROOT
    $ENV{HOME}/local/loki-0.1.7
    CACHE PATH "Loki root directory")

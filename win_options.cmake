set(JAVA_HOME
    "C:/Program Files/Java/jdk1.6.0_45"
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

set(THIRD_PARTY_DIR
    "C:/Users/foobar/third_party"
    CACHE PATH "Third party directory")
set(BOOST_ROOT
    "${THIRD_PARTY_DIR}/boost-1.83.0"
    CACHE PATH "Boost root directory")
set(HDF5_ROOT
    "${THIRD_PARTY_DIR}/hdf5-1.14.2"
    CACHE PATH "HDF5 root directory")
set(HEClib_ROOT
    "${THIRD_PARTY_DIR}/heclib-6"
    CACHE PATH "HEClib root directory")
set(KLU_ROOT
    "${THIRD_PARTY_DIR}/suitesparse-4.0.2"
    CACHE PATH "KLU root directory")
set(Loki_ROOT
    "${THIRD_PARTY_DIR}/loki-0.1.7"
    CACHE PATH "Loki root directory")

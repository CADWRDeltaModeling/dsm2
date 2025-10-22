# ~~~~
# Example of CMake settings
# Please change the values to match your environment
# This setting is used for building with MSVC 2022 and Intel compilers 2025.
# ~~~~
set(CMAKE_C_COMPILER
    cl
    CACHE STRING "C compiler")
set(CMAKE_CXX_COMPILER
    cl
    CACHE STRING "C++ compiler")
# CMake often cannot find the Intel Fortran compiler automatically
# You may need to set the full path manually
set(CMAKE_Fortran_COMPILER
    "C:/Program Files (x86)/Intel/oneAPI/compiler/latest/bin/ifx.exe"
    CACHE STRING "Fortran compiler")

# set(CMAKE_CXX_STANDARD 17 CACHE STRING "C++ standard")
set(CMAKE_POSITION_INDEPENDENT_CODE
    ON
    CACHE BOOL "Position independent code")
set(CMAKE_Fortran_FLAGS_INIT
    "/Dhydro_1000 /traceback"
    CACHE STRING "Fortran flags to use a large array for the dense dx")

# Dependent libraries. Assuming they are under deps directory.
set(THIRD_PARTY_DIR
    "${CMAKE_SOURCE_DIR}/deps"
    CACHE PATH "Third party directory")
set(BOOST_ROOT
    "${THIRD_PARTY_DIR}/boost-1.83.0"
    CACHE PATH "Boost root directory")
set(HDF5_ROOT
    "${THIRD_PARTY_DIR}/HDF5-1.14.2-win64"
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

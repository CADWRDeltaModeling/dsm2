# Builds only the oprule libraries and their tests (no HDF5, Fortran stdlib or DSM2 model needed).
#   cmake -S oprule/test/standalone -B <build> -DBOOST_ROOT=<boost> -DCMAKE_CXX_STANDARD=14
cmake_minimum_required(VERSION 3.21)
project(oprule_tests LANGUAGES C CXX Fortran)
enable_testing()
find_package(Boost REQUIRED)
set(OPRULE_BUILD_TESTS ON)
add_subdirectory(${CMAKE_CURRENT_SOURCE_DIR}/../.. oprule)

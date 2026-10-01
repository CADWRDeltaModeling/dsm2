# Builds only the oprule libraries and their tests (no HDF5, Fortran stdlib or DSM2 model needed).
#   cmake -S oprule/test/standalone -B <build> -DBOOST_ROOT=<boost> -DCMAKE_CXX_STANDARD=14
cmake_minimum_required(VERSION 3.21)
project(oprule_tests LANGUAGES C CXX Fortran)
enable_testing()
find_package(Boost REQUIRED)
# Optional: test the HDF5 sink too. -DOPRULE_HDF5_ROOT=<install tree with include/ and lib/libhdf5.a>
set(OPRULE_HDF5_ROOT "" CACHE PATH "HDF5 install used to build the HDF5 log sink")
if(OPRULE_HDF5_ROOT)
  set(HDF5_ROOT ${OPRULE_HDF5_ROOT})
  set(HDF5_USE_STATIC_LIBRARIES TRUE)
  find_package(HDF5 REQUIRED COMPONENTS C)
endif()
set(OPRULE_BUILD_TESTS ON)
add_subdirectory(${CMAKE_CURRENT_SOURCE_DIR}/../.. oprule)

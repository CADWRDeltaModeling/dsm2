cmake_minimum_required(VERSION 3.17)

project(DSM2 LANGUAGES Fortran C CXX)
set(CMAKE_Fortan_MODULE_DIRECTORY ${PROJECT_BINARY_DIR}/modules)
set(CMAKE_DEBUG_POSTFIX d)
set(CMAKE_Fortran_MODULE_DIRECTORY ${PROJECT_BINARY_DIR}/modules)

# Options for MSVC
if(MSVC)
  set(CMAKE_MSVC_RUNTIME_LIBRARY
      "MultiThreaded$<$<CONFIG:Debug>:Debug>DLL"
      CACHE STRING "MSVC runtime library" FORCE)
  set(CMAKE_CXX_FLAGS_DEBUG_INIT
      /MDd
      CACHE STRING "Flags used by the compiler during all build types." FORCE)
  set(CMAKE_CXX_FLAGS_RELEASE_INIT
      /MD
      CACHE STRING "Flags used by the compiler during all build types." FORCE)
  set(CMAKE_Fortran_FLAGS_DEBUG_INIT
      /MDd
      CACHE STRING "Flags used by the compiler during all build types." FORCE)
  set(CMAKE_Fortran_FLAGS_RELEASE_INIT
      /MD
      CACHE STRING "Flags used by the compiler during all build types." FORCE)
endif()

list(APPEND CMAKE_MODULE_PATH "${CMAKE_CURRENT_SOURCE_DIR}/../cmake")
message(STATUS "CMAKE_MODULE_PATH: ${CMAKE_MODULE_PATH}")

# Find libraries
find_package(Boost REQUIRED COMPONENTS filesystem regex system)

set(HDF5_USE_STATIC_LIBRARIES TRUE)
find_package(HDF5 REQUIRED COMPONENTS HL Fortran)

find_package(HEClib REQUIRED)

find_package(KLU REQUIRED)

find_package(Loki REQUIRED)

set(MKL_LINK static)
set(MKL_ARCH intel64)
set(MKL_THREADING sequential)
set(MKL_INTERFACE lp64)
find_package(MKL CONFIG REQUIRED)

# Setting to find oprule and input_storage. These will be consolidated later.
if(MSVC)
  set(STATIC_LIB_PREFIX "")
  set(STATIC_LIB_SUFFIX "lib")
else(MSVC)
  set(STATIC_LIB_PREFIX "lib")
  set(STATIC_LIB_SUFFIX "a")
endif(MSVC)

# Oprule/Input storage
find_library(
  OPRULE_DEBUG
  NAMES
    ${STATIC_LIB_PREFIX}oprule${TOOLSET}${COMPILE_INDICATOR}${CMAKE_DEBUG_POSTFIX}.${STATIC_LIB_SUFFIX}
  HINTS "${CMAKE_CURRENT_SOURCE_DIR}/../../oprule/build"
  PATH_SUFFIXES "Debug"
  NO_DEFAULT_PATH)
find_library(
  OPRULE_PARSER_DEBUG
  NAMES
    ${STATIC_LIB_PREFIX}oprule_parser${TOOLSET}${COMPILE_INDICATOR}${CMAKE_DEBUG_POSTFIX}.${STATIC_LIB_SUFFIX}
  HINTS "${CMAKE_CURRENT_SOURCE_DIR}/../../oprule/build"
  PATH_SUFFIXES "Debug"
  NO_DEFAULT_PATH)
find_library(
  OPRULE_RELEASE
  NAMES
    ${STATIC_LIB_PREFIX}oprule${TOOLSET}${COMPILE_INDICATOR}.${STATIC_LIB_SUFFIX}
  HINTS "${CMAKE_CURRENT_SOURCE_DIR}/../../oprule/build"
  PATH_SUFFIXES "Release"
  NO_DEFAULT_PATH)
find_library(
  OPRULE_PARSER_RELEASE
  NAMES
    ${STATIC_LIB_PREFIX}oprule_parser${TOOLSET}${COMPILE_INDICATOR}.${STATIC_LIB_SUFFIX}
  HINTS "${CMAKE_CURRENT_SOURCE_DIR}/../../oprule/build"
  PATH_SUFFIXES "Release"
  NO_DEFAULT_PATH)

message(
  STATUS
    ${STATIC_LIB_PREFIX}input_storage${TOOLSET}${COMPILE_INDICATOR}${CMAKE_DEBUG_POSTFIX}.${STATIC_LIB_SUFFIX}
)
find_library(
  INPUT_STORAGE_DEBUG
  NAMES
    ${STATIC_LIB_PREFIX}input_storage${TOOLSET}${COMPILE_INDICATOR}${CMAKE_DEBUG_POSTFIX}.${STATIC_LIB_SUFFIX}
  HINTS "${CMAKE_CURRENT_SOURCE_DIR}/../../input_storage/build"
  PATH_SUFFIXES "Debug"
  NO_DEFAULT_PATH)
find_library(
  INPUT_STORAGE_RELEASE
  NAMES
    ${STATIC_LIB_PREFIX}input_storage${TOOLSET}${COMPILE_INDICATOR}.${STATIC_LIB_SUFFIX}
  HINTS "${CMAKE_CURRENT_SOURCE_DIR}/../../input_storage/build"
  PATH_SUFFIXES "Release"
  NO_DEFAULT_PATH)

# Module directories for MSVC multi-config generators
if(MSVC)
  if(CMAKE_BUILD_TYPE MATCHES Debug)
    set(Fortran_MODULE_DIR ${CMAKE_Fortan_MODULE_DIRECTORY}/Debug)
  else()
    set(Fortran_MODULE_DIR ${CMAKE_Fortan_MODULE_DIRECTORY}/Release)
  endif()
else()
  set(Fortran_MODULE_DIR ${CMAKE_Fortan_MODULE_DIRECTORY})
endif()

# set(CMAKE_Fortran_FLAGS_INIT "/libs:dll /threads /MD")
set(DSM2_COMMONS_DIR ${DSM2_SOURCE_DIR}/common)
set(DSM2_FIXED_DIR ${DSM2_SOURCE_DIR}/fixed)
set(DSM2_HDF_TIDEFILE ${DSM2_SOURCE_DIR}/hdf_tidefile)
set(DSM2_HYDRO ${DSM2_SOURCE_DIR}/hydro)
set(DSM2_HYDROLIB ${DSM2_SOURCE_DIR}/hydrolib)
set(DSM2_INPUT_STORAGE ${DSM2_SOURCE_DIR}/input_storage)
set(DSM2_KLU_FORTRAN ${DSM2_SOURCE_DIR}/klu_fortran)
set(DSM2_OPRULE_INTERFACE ${DSM2_SOURCE_DIR}/oprule_interface)
set(DSM2_PATTERN_MATCH ${DSM2_SOURCE_DIR}/pattern_match)
set(DSM2_PTM ${DSM2_SOURCE_DIR}/ptm)
set(DSM2_QUAL ${DSM2_SOURCE_DIR}/qual)
set(DSM2_SPARSE ${DSM2_SOURCE_DIR}/sparse)

# set (DSM2_TIMEVAR ${DSM2_SOURCE_DIR}/timevar)
set(DSM2_PYTHON ${DSM2_SOURCE_DIR}/python)

# generate the files now
# execute_process(COMMAND "python" "generate.py" WORKING_DIRECTORY ${DSM2_INPUT_STORAGE})
add_subdirectory(${DSM2_COMMONS_DIR})
add_subdirectory(${DSM2_INPUT_STORAGE})
add_subdirectory(${DSM2_FIXED_DIR})
add_subdirectory(${DSM2_HDF_TIDEFILE})
add_subdirectory(${DSM2_HYDRO})
add_subdirectory(${DSM2_HYDROLIB})
add_subdirectory(${DSM2_KLU_FORTRAN})
add_subdirectory(${DSM2_OPRULE_INTERFACE})
add_subdirectory(${DSM2_SPARSE})
add_subdirectory(${DSM2_PATTERN_MATCH})

# GTM
set(DSM2_GTM_CORE ${DSM2_SOURCE_DIR}/gtm_core)
add_subdirectory(${DSM2_GTM_CORE})

# add_subdirectory(${DSM2_TIMEVAR})
# add_subdirectory(${DSM2_QUAL})

# Build HYDRO
set(HYDRO_BIN hydro_exe)
add_executable(${HYDRO_BIN} hydro/hydro_driver.f90)
set_target_properties(${HYDRO_BIN} PROPERTIES LINKER_LANGUAGE Fortran)
add_dependencies(${HYDRO_BIN} fourpt)
target_link_libraries(
  ${HYDRO_BIN}
  PRIVATE fourpt
          gtm_core
          hydrolib
          fixed
          hdf_tidefile
          hdf_tidefile_cpp
          oprule_interface_cpp
          oprule_interface
          input_storage
          input_storage_cpp
          klu_fortran
          common
          pattern_match
          sparse
          debug
          ${INPUT_STORAGE_DEBUG}
          optimized
          ${INPUT_STORAGE_RELEASE}
          debug
          ${OPRULE_PARSER_DEBUG}
          optimized
          ${OPRULE_PARSER_RELEASE}
          debug
          ${OPRULE_DEBUG}
          optimized
          ${OPRULE_RELEASE}
          ${Boost_LIBRARIES}
          ${HDF5_HL_LIBRARIES}
          ${HDF5_LIBRARIES}
          ${KLU_LIBRARIES}
          ${HEClib_LIBRARIES})
target_compile_options(
  ${HYDRO_BIN} PRIVATE "$<TARGET_PROPERTY:MKL::MKL,INTERFACE_COMPILE_OPTIONS>")
target_link_libraries(${HYDRO_BIN} PRIVATE "$<LINK_ONLY:MKL::MKL>")

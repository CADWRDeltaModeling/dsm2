cmake_minimum_required(VERSION 3.21)

enable_language(Fortran C CXX)
list(APPEND CMAKE_MODULE_PATH "${CMAKE_SOURCE_DIR}/cmake")
include(GetGitHash)

set(CMAKE_Fortran_MODULE_DIRECTORY ${PROJECT_BINARY_DIR}/modules)

# Options for MSVC
if(MSVC)
  set(CMAKE_MSVC_RUNTIME_LIBRARY
      "MultiThreaded$<$<CONFIG:Debug>:Debug>DLL"
      CACHE STRING "MSVC runtime library" FORCE)
  add_compile_options($<$<CONFIG:Debug>:/MDd> $<$<CONFIG:Release>:/MD>)
endif()

message(STATUS "CMAKE_MODULE_PATH: ${CMAKE_MODULE_PATH}")

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

set(INTEL_ARCH ${MKL_ARCH})
find_package(IntelRuntime REQUIRED)

# Setting to find oprule and input_storage. These will be consolidated later.
if(MSVC)
  set(STATIC_LIB_PREFIX "")
  set(STATIC_LIB_SUFFIX "lib")
else(MSVC)
  set(STATIC_LIB_PREFIX "lib")
  set(STATIC_LIB_SUFFIX "a")
endif(MSVC)

# set(CMAKE_Fortran_FLAGS_INIT "/libs:dll /threads /MD")
set(DSM2_COMMONS_DIR common)
set(DSM2_FIXED_DIR fixed)
set(DSM2_HDF_TIDEFILE hdf_tidefile)
set(DSM2_HYDRO hydro)
set(DSM2_HYDROLIB hydrolib)
set(DSM2_INPUT_STORAGE input_storage)
set(DSM2_KLU_FORTRAN klu_fortran)
set(DSM2_OPRULE_INTERFACE oprule_interface)
set(DSM2_PATTERN_MATCH pattern_match)

set(DSM2_TIMEVAR timevar)
set(DSM2_PYTHON python)

add_subdirectory(src/dsm2_defs)
add_subdirectory(src/${DSM2_COMMONS_DIR})
add_subdirectory(src/${DSM2_INPUT_STORAGE})
add_subdirectory(src/${DSM2_FIXED_DIR})
add_subdirectory(src/${DSM2_HDF_TIDEFILE})
add_subdirectory(src/${DSM2_HYDRO})
add_subdirectory(src/${DSM2_HYDROLIB})
add_subdirectory(src/${DSM2_KLU_FORTRAN})
add_subdirectory(src/${DSM2_OPRULE_INTERFACE})
add_subdirectory(src/${DSM2_PATTERN_MATCH})

# GTM
set(GTM_CORE gtm_core)
add_subdirectory(src/${GTM_CORE})

# Hydro driver
set(HYDRO_BIN hydro)
add_subdirectory(src/hydro_driver)

# Qual driver
set(QUAL_BIN qual)
add_subdirectory(src/qual_core)
add_subdirectory(src/qual_driver)

# Build GTM
set(GTM_BIN gtm)
set(GTM_MAIN gtm_main)
set(GTM_PROCESS_IO process_io)
set(GTM_SEDIMENT_BED_CORE sediment_bed_core)
set(GTM_SEDIMENT_BED sediment_bed)
set(GTM_SEDIMENT sediment)
set(GTM_MERCURY mercury)
set(GTM_TRANSPORT transport)

add_subdirectory(src/gtm)
add_subdirectory(src/${GTM_PROCESS_IO})
add_subdirectory(src/${GTM_SEDIMENT})
add_subdirectory(src/${GTM_SEDIMENT_BED_CORE})
add_subdirectory(src/${GTM_SEDIMENT_BED})
add_subdirectory(src/${GTM_MERCURY})
add_subdirectory(src/${GTM_TRANSPORT})
# add_subdirectory(${GTM_DO_MODULE}) add_subdirectory(${GTM_TOOLS})

add_executable(${GTM_BIN} src/gtm/gtm_driver.f90)
add_dependencies(${GTM_BIN} ${GTM_MAIN})
set_target_properties(${GTM_BIN} PROPERTIES LINKER_LANGUAGE Fortran
                                            INSTALL_RPATH "\${ORIGIN}/../lib")
if(WIN32)
  target_link_options(${GTM_BIN} PRIVATE "/STACK:90000000")
endif(WIN32)
target_link_libraries(
  ${GTM_BIN}
  PRIVATE ${GTM_MAIN}
          ${GTM_MERCURY}
          ${GTM_SEDIMENT_BED}
          ${GTM_SEDIMENT_BED_CORE}
          ${GTM_SEDIMENT}
          ${GTM_TRANSPORT}
          ${GTM_CORE}
          ${GTM_PROCESS_IO}
          dsm2_input_storage
          dsm2_input_storage_cpp
          common
          input_storage
          ${Boost_LIBRARIES}
          ${HDF5_HL_LIBRARIES}
          ${HDF5_LIBRARIES}
          ${HEClib_LIBRARIES})

set(GTM_SEDIMENT_BED_SETUP sediment_bed_setup)
add_subdirectory(src/${GTM_SEDIMENT_BED_SETUP})

# Hydro-GTM
add_subdirectory(src/hydro_gtm_driver)
add_subdirectory(src/hydro_gtm)

# Tests
if (BUILD_TESTING)
  enable_testing()
  add_subdirectory(tests)
endif()

# INSTALL Section
install(
  TARGETS ${HYDRO_BIN}
          ${QUAL_BIN}
          ${GTM_BIN}
          hydro_gtm
          sed_bed_init
          RUNTIME_DEPENDENCY_SET
          RUNTIME_LIB
  RUNTIME DESTINATION bin
  LIBRARY DESTINATION lib)

if(NOT WIN32)
  # USing DIRECTORIES will raise CMake warnings, but no other way to grab Intel
  # runtime is found.
  install(
    RUNTIME_DEPENDENCY_SET
    RUNTIME_LIB
    DIRECTORIES
    ${INTELRUNTIME_DIR}
    PRE_EXCLUDE_REGEXES
    "ld.+"
    "libc.+"
    "libstdc.+"
    "libgcc.+"
    "libdl.+"
    "libm.+"
    "libpthread.+"
    "librt.+")
  # libcilkrts is not included automatically.
  if(EXISTS ${INTELRUNTIME_DIR}/libcilkrts.so.5)
    install(FILES ${INTELRUNTIME_DIR}/libcilkrts.so.5 TYPE LIB)
  endif()
endif()

# CPack
set(CPACK_PACKAGE_NAME "DSM2")
set(CPACK_PACKAGE_VENDOR "Delta Modeling Section")
set(CPACK_PACKAGE_DESCRIPTION_SUMMARY "Delta Simulation Model 2")
set(CPACK_PACKAGE_VERSION_MAJOR "8")
set(CPACK_PACKAGE_VERSION_MINOR "5")
set(CPACK_PACKAGE_VERSION_PATCH "${GIT_HASH}")
set(CPACK_PACKAGE_VERSION
    "${CPACK_PACKAGE_VERSION_MAJOR}.${CPACK_PACKAGE_VERSION_MINOR}.${CPACK_PACKAGE_VERSION_PATCH}"
)
set(LICENSE_FILE ${CMAKE_SOURCE_DIR}/LICENSE.txt)
cmake_path(ABSOLUTE_PATH LICENSE_FILE)
set(CPACK_RESOURCE_FILE_LICENSE ${LICENSE_FILE})
set(CPACK_PACKAGE_INSTALL_DIRECTORY "CPack")
include(CPack)

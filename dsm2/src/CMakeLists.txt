cmake_minimum_required(VERSION 3.17)

# Set CMake module path, keep this for later
# set(CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} "${DSM2_TOP_DIR}/cmake")
PROJECT(DSM2 LANGUAGES C CXX Fortran)

option(BUILD_SHARED_LIBS OFF)
set(DSM2_TOP_DIR ${CMAKE_SOURCE_DIR}/../..)
set(CMAKE_Fortan_MODULE_DIRECTORY ${PROJECT_BINARY_DIR}/modules)
set(CMAKE_MSVC_RUNTIME_LIBRARY "MultiThreaded$<$<CONFIG:Debug>:Debug>DLL" CACHE STRING "MSVC runtime library" FORCE)
set(CMAKE_CXX_FLAGS_DEBUG_INIT /MDd CACHE STRING "Flags used by the compiler during all build types." FORCE)
set(CMAKE_CXX_FLAGS_RELEASE_INIT /MD CACHE STRING "Flags used by the compiler during all build types." FORCE)
set(CMAKE_Fortran_FLAGS_DEBUG_INIT /MDd CACHE STRING "Flags used by the compiler during all build types." FORCE)
set(CMAKE_Fortran_FLAGS_RELEASE_INIT /MD CACHE STRING "Flags used by the compiler during all build types." FORCE)

if(DEFINED THIRD_PARTY_DIR)
    set(DSM2_THIRD_PARTY_DIR "${THIRD_PARTY_DIR}")
else()
    set(DSM2_THIRD_PARTY_DIR "${DSM2_TOP_DIR}/third_party")
endif()

set(CMAKE_DEBUG_POSTFIX d)

# Boost library
if(WIN32)
    set(BOOST_VERSION 1_83)
    set(STATIC_LIB_SUFFIX "lib")
else(WIN32)
    set(BOOST_VERSION 1_69)
    set(STATIC_LIB_SUFFIX "a")
endif(WIN32)

set(BOOST_DIR ${DSM2_THIRD_PARTY_DIR}/boost_${BOOST_VERSION}_0)
set(BOOST_INCLUDE_DIR ${BOOST_DIR}/include/boost-1_83/)
set(BOOST_LIB_DIR ${BOOST_DIR}/lib)

if(WIN32)
    set(BOOST_VC_VERSION "vc143")
    set(BOOST_RELEASE_VERSION "-${BOOST_VC_VERSION}-mt-x64-${BOOST_VERSION}")
    set(BOOST_DEBUG_VERSION "-${BOOST_VC_VERSION}-mt-gd-x64-${BOOST_VERSION}")
else(WIN32)
    set(BOOST_VC_VERSION "")
    set(BOOST_RELEASE_VERSION "")
    set(BOOST_DEBUG_VERSION "")
endif(WIN32)

list(APPEND BOOST_LIB_NAMES filesystem regex "system") # unit_test_framework)

foreach(NAME IN LISTS BOOST_LIB_NAMES)
    message(STATUS "Looking for boost library ${NAME}, boost_${NAME}${BOOST_RELEASE_VERSION}.${STATIC_LIB_SUFFIX}")
    unset(LIB)
    unset(LIBD)
    find_library(LIB NAMES libboost_${NAME}${BOOST_RELEASE_VERSION}.${STATIC_LIB_SUFFIX}
        PATHS ${BOOST_LIB_DIR} NO_CACHE NO_DEFAULT_PATH)
        find_library(LIBD NAMES libboost_${NAME}${BOOST_DEBUG_VERSION}.${STATIC_LIB_SUFFIX}
        PATHS ${BOOST_LIB_DIR} NO_CACHE NO_DEFAULT_PATH)
    message(STATUS "LIB: ${LIB}")

    if(LIB)
        message(STATUS "Found boost library ${NAME}: ${LIB}")
        list(APPEND BOOST_LIBRARIES_RELEASE ${LIB})
        list(APPEND BOOST_LIBRARIES_DEBUG ${LIBD})
    else()
        message(FATAL_ERROR "Could not find boost library ${BOOST_LIBLRARY_NAME}")
    endif()
endforeach()

# HDF5
list(APPEND HDF5_LIB_NAMES
    hdf5 hdf5_hl hdf5_fortran hdf5_hl_fortran
    hdf5_f90cstub hdf5_hl_f90cstub z szaec aec)

if(MSVC)
    set(HDF5_DIR ${DSM2_THIRD_PARTY_DIR}/hdf5-1.10.10-win64)
    set(HDF5_LIB_DIR "${HDF5_DIR}/lib")
    set(HDF5_Fortran_MODULE_PATH "${HDF5_DIR}/mod/static")
    set(HDF5_LIB_PREFIX "lib")
    set(HDF5_DEBUG_SUFFIX "_D")

    foreach(NAME IN LISTS HDF5_LIB_NAMES)
        unset(LIB)
        find_library(LIB NAMES ${HDF5_LIB_PREFIX}${NAME}${HDF5_DEBUG_SUFFIX}.${STATIC_LIB_SUFFIX}
            PATHS ${HDF5_LIB_DIR} NO_CACHE NO_DEFAULT_PATH)

        if(LIB)
            list(APPEND HDF5_LIBRARIES ${LIB})
        else()
            message(FATAL_ERROR "Could not find HDF5 library ${NAME}: ${HDF5_LIB_PREFIX}${NAME}${HDF5_DEBUG_SUFFIX}.${STATIC_LIB_SUFFIX}")
        endif()
    endforeach()

else(MSVC)
    message(FATAL_ERROR "DSM2 is only supported on Windows")
endif(MSVC)

# SparseSuite KLU solver library
set(KLULIB_PATH "${DSM2_THIRD_PARTY_DIR}/SuiteSparse/KLU")

if(WIN32)
    find_library(KLULIB_DEBUG ${LIB_PREFIX}KLU.${STATIC_LIB_SUFFIX} PATHS "${KLULIB_PATH}/build/Debug" NO_DEFAULT_PATH)
    find_library(KLULIB_RELEASE ${LIB_PREFIX}KLU.${STATIC_LIB_SUFFIX} PATHS "${KLULIB_PATH}/build/Release" NO_DEFAULT_PATH)
else(WIN32)
    find_library(KLULIB_DEBUG ${LIB_PREFIX}KLU${TOOLSET}.${STATIC_LIB_SUFFIX} PATHS "${KLULIB_PATH}/build/Debug" NO_DEFAULT_PATH)
    find_library(KLULIB_RELEASE ${LIB_PREFIX}KLU${TOOLSET}.${STATIC_LIB_SUFFIX} PATHS "${KLULIB_PATH}/build/Release" NO_DEFAULT_PATH)
endif(WIN32)

# Setting up the static link option
# NOTE I am not sure if BUILD_SHARED_LIBS should take care of this automatically.
# if(MSVC)
# if(BUILD_SHARED_LIBS)
# if(CMAKE_BUILD_TYPE MATCHES Debug)
# add_compile_options(/MDd)
# else()
# add_compile_options(/MD)
# endif()
# else(BUILD_SHARED_LIBS)
# if(CMAKE_BUILD_TYPE MATCHES Debug)
# add_compile_options(/MTd)
# else()
# add_compile_options(/MT)
# endif()
# endif(BUILD_SHARED_LIBS)
# else(MSVC)
# message(FATAL_ERROR "DSM2 is only supported on Windows")
# endif(MSVC)

# HEClib
if(MSVC)
    set(LIB_PREFIX "")
    set(DEBUG_COMPILE "d")
else(MSVC)
    set(LIB_PREFIX "lib")
    set(TOOLSET "_intel")
    set(STATIC_COMPILE "")
    set(DEBUG_COMPILE "")
endif(MSVC)

set(HECLIB_DIR ${DSM2_THIRD_PARTY_DIR}/heclib)
set(HECLIB_LIB_PATH ${HECLIB_DIR}/lib)

if(MSVC)
    find_library(HECLIB_RELEASE NAMES ${LIB_PREFIX}heclib${TOOLSET}${COMPILE_INDICATOR}.${STATIC_LIB_SUFFIX} PATHS ${HECLIB_LIB_PATH} NO_DEFAULT_PATH)
    message(STATUS "HECLIB_RELEASE: ${HECLIB_RELEASE}")
    list(APPEND HECLIB_LIBRARIES_RELEASE ${HECLIB_RELEASE})
    find_library(HECLIB_DEBUG NAMES ${LIB_PREFIX}heclib${TOOLSET}${COMPILE_INDICATOR}${DEBUG_COMPILE}.${STATIC_LIB_SUFFIX} PATHS "${HECLIB_LIB_PATH}" NO_DEFAULT_PATH)
    list(APPEND HECLIB_LIBRARIES_DEBUG ${HECLIB_DEBUG})
    message(STATUS "HECLIB_LIBRARIES_RELEASE: ${HECLIB_LIBRARIES_RELEASE}")
    message(STATUS "HECLIB_LIBRARIES_DEBUG: ${HECLIB_LIBRARIES_DEBUG}")
else(MSVC)
    message(FATAL_ERROR "DSM2 is only supported on Windows")

    # find_library(HECLIB_RELEASE NAMES ${LIB_PREFIX}heclib${TOOLSET}${COMPILE_INDICATOR}.${STATIC_LIB_SUFFIX} PATHS "${HECLIB_LIB_PATH}" NO_DEFAULT_PATH)
endif(MSVC)

# Oprule/Input storage
find_library(OPRULE_DEBUG
    NAMES ${LIB_PREFIX}oprule${TOOLSET}${COMPILE_INDICATOR}${DEBUG_COMPILE}.${STATIC_LIB_SUFFIX}
    PATHS "${DSM2_TOP_DIR}/lib" NO_DEFAULT_PATH)
find_library(OPRULE_PARSER_DEBUG
    NAMES ${LIB_PREFIX}oprule_parser${TOOLSET}${COMPILE_INDICATOR}${DEBUG_COMPILE}.${STATIC_LIB_SUFFIX} PATHS "${DSM2_TOP_DIR}/lib" NO_DEFAULT_PATH)
find_library(OPRULE_RELEASE
    NAMES ${LIB_PREFIX}oprule${TOOLSET}${COMPILE_INDICATOR}.${STATIC_LIB_SUFFIX}
    PATHS "${DSM2_TOP_DIR}/lib" NO_DEFAULT_PATH)
find_library(OPRULE_PARSER_RELEASE
    NAMES ${LIB_PREFIX}oprule_parser${TOOLSET}${COMPILE_INDICATOR}.${STATIC_LIB_SUFFIX}
    PATHS "${DSM2_TOP_DIR}/lib" NO_DEFAULT_PATH)

find_library(INPUT_STORAGE_DEBUG
    NAMES ${LIB_PREFIX}input_storage${TOOLSET}${COMPILE_INDICATOR}${DEBUG_COMPILE}.${STATIC_LIB_SUFFIX}
    PATHS "${DSM2_TOP_DIR}/lib" NO_DEFAULT_PATH)
find_library(INPUT_STORAGE_RELEASE
    NAMES ${LIB_PREFIX}input_storage${TOOLSET}${COMPILE_INDICATOR}.${STATIC_LIB_SUFFIX}
    PATHS "${DSM2_TOP_DIR}/lib" NO_DEFAULT_PATH)

# # MKL
find_library(MKL_CORE_LIB NAMES mkl_core.lib PATHS)
find_library(MKL_INTEL_LIB NAMES mkl_intel_c.lib PATHS)

# Set Fortran module file directory
set(CMAKE_Fortran_MODULE_DIRECTORY ${PROJECT_BINARY_DIR}/modules)

# Shared lib build setting
if(MSVC)
    if(CMAKE_BUILD_TYPE MATCHES Debug)
        set(Fortran_MODULE_DIR
            ${CMAKE_Fortan_MODULE_DIRECTORY}/Debug)
    else()
        set(Fortran_MODULE_DIR
            ${CMAKE_Fortan_MODULE_DIRECTORY}/Release)
    endif()
else()
    set(Fortran_MODULE_DIR
        ${CMAKE_Fortan_MODULE_DIRECTORY})
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

# set (DSM2_TIMEVAR      ${DSM2_SOURCE_DIR}/timevar)
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
add_executable(${HYDRO_BIN})
add_dependencies(${HYDRO_BIN} fourpt)
target_sources(${HYDRO_BIN} PRIVATE hydro/hydro_driver.f90)
target_link_libraries(${HYDRO_BIN} PRIVATE
    gtm_core fixed input_storage_cpp input_storage
    oprule_interface_fortran oprule_interface_cpp hydrolib
    pattern_match hdf_tidefile hdf_tidefile_cpp klu_fortran common fourpt
    debug sparse
    debug ${BOOST_LIBRARIES_DEBUG} optimized ${BOOST_LIBRARIES_RELEASE}
    ${HDF5_LIBRARIES}
    debug ${OPRULE_DEBUG} optimized ${OPRULE_RELEASE}
    debug ${OPRULE_PARSER_DEBUG} optimized ${OPRULE_PARSER_RELEASE}
    debug ${INPUT_STORAGE_DEBUG} optimized ${INPUT_STORAGE_RELEASE}
    debug ${HECLIB_LIBRARIES_DEBUG} optimized ${HECLIB_LIBRARIES_RELEASE}
    debug ${KLULIB_DEBUG} optimized ${KLULIB_RELEASE}
)

# target_link_options(${HYDRO_BIN} PRIVATE
# "/NODEFAULTLIB:LIBUCRT" "/NODEFAULTLIB:MSVCRT")
set(MKL_ARCH intel64)
set(MKL_THREADING sequential)
set(MKL_INTERFACE lp64)
find_package(MKL REQUIRED)

# Should check if Intel Windows compiler, not MSVC
if(MSVC)
    # target_link_libraries(${HYDRO_BIN} PRIVATE
    # debug libcmtd.lib debug libmmtd.lib debug libifcoremtd.lib debug libircmtd.lib
    # optimized libcmt.lib optimized libmmt.lib optimized libifcoremt.lib optimized libircmt.lib)
    target_link_libraries(${HYDRO_BIN} PRIVATE ${mkl_core_file} ${mkl_intel_lp64_file} ${mkl_intel_thread_file})
    # target_link_options(${HYDRO_BIN} PRIVATE "/Qmkl-ilp64")
else(MSVC)
    message(FATAL_ERROR "DSM2 is only supported on Windows")
    target_link_options(${HYDRO_BIN} PRIVATE "-mkl=sequential")
endif(MSVC)

# include_directories("${CMAKE_Fortran_MODULE_DIRECTORY}" "../hdf_tidefile" "../fixed")
cmake_minimum_required (VERSION 3.2.2)
PROJECT(input_storage)
enable_language(C CXX Fortran)
# 
string(TIMESTAMP CONFIG_DATE "%Y-%m-%d")
#-----------------------------------------------------------------------------
# Set the core names of all the libraries
#-----------------------------------------------------------------------------
set(DSM2_TOP_DIR "../")
set(DSM2_THIRD_PARTY_DIR "${DSM2_TOP_DIR}/third_party")

# make sure that the default is a RELEASE
if (NOT CMAKE_BUILD_TYPE)
  set (CMAKE_BUILD_TYPE Debug CACHE STRING
      "Choose the type of build, options are: None Debug Release."
      FORCE)
endif (NOT CMAKE_BUILD_TYPE)
#####################
### GENERAL FLAGS ###
#####################
#- To ensure static linking for C++ code
set(CMAKE_CXX_FLAGS_RELEASE "/MT")
set(CMAKE_CXX_FLAGS_DEBUG "/MTd")
#- To ensure static linking for C code
set(CMAKE_C_FLAGS_RELEASE "/MT")
set(CMAKE_C_FLAGS_RELEASE "/MT")
set(CMAKE_C_FLAGS_DEBUG "/MTd")
#---------------LIBRARIES-------------------------------------------------
#------------------ DEPENDENCIES ---------------------
#----------------- EXECUTABLES --------------------------------------------
#----------------- FIND THE LIBRARIES -------------------
#boost libraries
#boost libraries built with ./b2 runtime-link=static
set(BOOST_VERSION 1_64)
set(BOOST_DIR ${DSM2_THIRD_PARTY_DIR}/boost_${BOOST_VERSION}_0)
set(BOOST_LIB_DIR ${BOOST_DIR}/stage/lib)
set(BOOST_VC_VERSION "vc140")
find_library(BOOST_FILESYSTEM_DEBUG NAMES libboost_filesystem-${BOOST_VC_VERSION}-mt-sgd-${BOOST_VERSION}.lib PATHS ${BOOST_LIB_DIR} NO_DEFAULT_PATH)
find_library(BOOST_FILESYSTEM_RELEASE NAMES libboost_filesystem-${BOOST_VC_VERSION}-mt-s-${BOOST_VERSION}.lib PATHS ${BOOST_LIB_DIR} NO_DEFAULT_PATH)
find_library(BOOST_REGEX_DEBUG NAMES libboost_regex-${BOOST_VC_VERSION}-mt-sgd-${BOOST_VERSION}.lib PATHS ${BOOST_LIB_DIR} NO_DEFAULT_PATH)
find_library(BOOST_REGEX_RELEASE NAMES libboost_regex-${BOOST_VC_VERSION}-mt-s-${BOOST_VERSION}.lib PATHS ${BOOST_LIB_DIR} NO_DEFAULT_PATH)
find_library(BOOST_SYSTEM_DEBUG NAMES libboost_system-${BOOST_VC_VERSION}-mt-sgd-${BOOST_VERSION}.lib PATHS ${BOOST_LIB_DIR} NO_DEFAULT_PATH)
find_library(BOOST_SYSTEM_RELEASE NAMES libboost_system-${BOOST_VC_VERSION}-mt-s-${BOOST_VERSION}.lib PATHS ${BOOST_LIB_DIR} NO_DEFAULT_PATH)
find_library(BOOST_UNITTEST_DEBUG NAMES libboost_unit_test_framework-${BOOST_VC_VERSION}-mt-sgd-${BOOST_VERSION}.lib PATHS ${BOOST_LIB_DIR} NO_DEFAULT_PATH)
find_library(BOOST_UNITTEST_RELEASE NAMES libboost_unit_test_framework-${BOOST_VC_VERSION}-mt-s-${BOOST_VERSION}.lib PATHS ${BOOST_LIB_DIR} NO_DEFAULT_PATH)
include_directories("${BOOST_DIR}")
# ---------------- HDF5 setup -------------- hdf5.lib hdf5_hl_fortran.lib hdf5_fortran.lib hdf5_f90cstub.lib hdf5_hl.lib 
add_definitions(-DH5_BUILT_AS_DYNAMIC_LIB) # needed as HDF5 was compiled with CMake and we want to link static
#set(HDF5_DIR "${DSM2_THIRD_PARTY_DIR}/hdf5_x32_vs2015")
set(HDF5_DIR "${DSM2_THIRD_PARTY_DIR}/HDF5-1.10.1-win32")
#set(HDF5_DIR "${DSM2_THIRD_PARTY_DIR}/HDF5-1.8.20-win32")
set(HDF5_LIB_PREFIX "lib")
set(HDF5_LIB_PATH "${HDF5_DIR}/lib")
set(HDF5_FORTRAN_INCLUDE_PATH "${HDF5_DIR}/include/static")
include_directories("${HDF5_DIR}/include")
find_library(HDF5 NAMES ${HDF5_LIB_PREFIX}hdf5.lib PATHS ${HDF5_LIB_PATH} NO_DEFAULT_PATH)
find_library(HDF5_CPP NAMES ${HDF5_LIB_PREFIX}hdf5_cpp.lib PATHS ${HDF5_LIB_PATH} NO_DEFAULT_PATH)
find_library(HDF5_HL_FORTRAN NAMES ${HDF5_LIB_PREFIX}hdf5_hl_fortran.lib PATHS ${HDF5_LIB_PATH} NO_DEFAULT_PATH)
find_library(HDF5_FORTRAN NAMES ${HDF5_LIB_PREFIX}hdf5_fortran.lib PATHS ${HDF5_LIB_PATH} NO_DEFAULT_PATH)
find_library(HDF5_F90CSTUB NAMES ${HDF5_LIB_PREFIX}hdf5_f90cstub.lib PATHS ${HDF5_LIB_PATH} NO_DEFAULT_PATH)
find_library(HDF5_HL_F90CSTUB NAMES ${HDF5_LIB_PREFIX}hdf5_hl_f90cstub.lib PATHS ${HDF5_LIB_PATH} NO_DEFAULT_PATH)
find_library(HDF5_HL NAMES ${HDF5_LIB_PREFIX}hdf5_hl.lib PATHS ${HDF5_LIB_PATH} NO_DEFAULT_PATH)
find_library(HDF5_HL_CPP NAMES ${HDF5_LIB_PREFIX}hdf5_hl_cpp.lib PATHS ${HDF5_LIB_PATH} NO_DEFAULT_PATH)
#szlib.lib zlib.lib 
find_library(SZLIB NAMES szip.lib PATHS "${HDF5_LIB_PATH}" NO_DEFAULT_PATH)
find_library(ZLIB NAMES zlib.lib PATHS "${HDF5_LIB_PATH}" NO_DEFAULT_PATH)
#standard libraries. no need to specify path as they are linked from default library paths FIXME: find_package
set (CMT_LIB_DEBUG "libcmtd.lib")
set (CMT_LIB_RELEASE "libcmt.lib")
#math library for Intel Composer 2013 set (MMT_LIB "libmmt.lib")
# math library for Intel VF 16  mkl_intel_c.lib mkl_sequential.lib mkl_core.lib
set (INTEL_LIB_PATH "C:/Program Files (x86)/IntelSWTools/compilers_and_libraries/windows/mkl/lib/ia32")
set(MMT_LIB "${INTEL_LIB_PATH}/mkl_intel_c.lib")
set(MMT_SQ_LIB "${INTEL_LIB_PATH}/mkl_sequential.lib")
set(MMT_CORE_LIB "${INTEL_LIB_PATH}/mkl_core.lib")
#-------------add linker flags and excluded and included libraries
if(MSVC)
  set(CMAKE_EXE_LINKER_FLAGS
      "${CMAKE_EXE_LINKER_FLAGS} /NODEFAULTLIB:libc.lib /NODEFAULTLIB:msvcrt.lib /NODEFAULTLIB:libcmt.lib /NODEFAULTLIB:msvcrtd.lib /MANIFEST:NO")
  set (CMAKE_SHARED_LINKER_FLAGS "/NODEFAULTLIB:libc.lib /NODEFAULTLIB:msvcrt.lib /MANIFEST:NO")
endif()
#include_directories()
#
file(GLOB SOURCES src/*.cpp)
list(REMOVE_ITEM SOURCES "${CMAKE_SOURCE_DIR}/src/ItemInputState.cpp")
file(GLOB HEADERS src/*.h)
include_directories(src ${HDF5_FORTRAN_INCLUDE_PATH})
add_library(input_storage STATIC ${SOURCES} ${HEADERS})
target_link_libraries(input_storage 
    debug ${BOOST_FILESYSTEM_DEBUG} optimized ${BOOST_FILESYSTEM_RELEASE}
    debug ${BOOST_REGEX_DEBUG} optimized ${BOOST_REGEX_RELEASE}
    debug ${BOOST_SYSTEM_DEBUG} optimized ${BOOST_SYSTEM_RELEASE}
    debug ${BOOST_UNITTEST_DEBUG} optimized ${BOOST_UNITTEST_RELEASE}
    ${HDF5} ${HDF5_FORTRAN} ${HDF5_F90CSTUB} ${HDF5_CPP}  
    ${HDF5_HL} ${HDF5_HL_CPP} ${HDF5_HL_F90CSTUB} ${HDF5_HL_FORTRAN} ${HDF5_HL_CPP}
    ${SZLIB} ${ZLIB}
    ${MMT_LIB} ${MMT_SQ_LIB} ${MMT_CORE_LIB}
    debug ${CMT_LIB_DEBUG} optimized ${CMT_LIB_RELEASE}
    )
set(CMAKE_DEBUG_POSTFIX "d")
set_target_properties(input_storage PROPERTIES OUTPUT_NAME "input_storage_vs14_mt")
set_target_properties(input_storage PROPERTIES DEBUG_POSTFIX "d")
add_custom_command(TARGET input_storage POST_BUILD 
  COMMAND "${CMAKE_COMMAND}" -E copy 
     "$<TARGET_FILE:input_storage>"
     "${CMAKE_SOURCE_DIR}/../lib/$<TARGET_FILE_NAME:input_storage>" 
  COMMENT "Copying to output directory")
#-------------------- EXAMPLE exe and fortran library -----------------
# search by file pattern and include in library
file(GLOB EXAMPLE_CXX_SOURCES example/*.cpp)
add_executable(example_c ${EXAMPLE_CXX_SOURCES})
set_property(TARGET example_c PROPERTY LINKER_LANGUAGE CXX)
target_link_libraries(example_c
    input_storage
    ${HDF5} ${HDF5_FORTRAN} ${HDF5_F90CSTUB} ${HDF5_CPP}  
    ${HDF5_HL} ${HDF5_HL_CPP} ${HDF5_HL_F90CSTUB} ${HDF5_HL_FORTRAN} ${HDF5_HL_CPP}
    ${SZLIB} ${ZLIB}
    debug ${BOOST_FILESYSTEM_DEBUG} optimized ${BOOST_FILESYSTEM_RELEASE}
    debug ${BOOST_REGEX_DEBUG} optimized ${BOOST_REGEX_RELEASE}
    debug ${BOOST_SYSTEM_DEBUG} optimized ${BOOST_SYSTEM_RELEASE}
    debug ${BOOST_UNITTEST_DEBUG} optimized ${BOOST_UNITTEST_RELEASE}
    ${MMT_LIB} ${MMT_SQ_LIB} ${MMT_CORE_LIB}
    debug ${CMT_LIB_DEBUG} optimized ${CMT_LIB_RELEASE}
    )
# add pre build command
add_custom_command(OUTPUT example/xsect_input_storage.fi PRE_BUILD 
    COMMAND python generate.py WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/example
    COMMENT "Generating source code from script")
# search by file pattern and include in library
file(GLOB EXAMPLE_F_SOURCES example/*.f90)
file(GLOB EXAMPLE_F_HEADERS example/*.fi)
add_library(example_f STATIC ${EXAMPLE_F_SOURCES} ${EXAMPLE_F_HEADERS})
set_property(TARGET example_f PROPERTY LINKER_LANGUAGE Fortran)
#---------------------- TEST exe and fortran ------------------------
# search by file pattern and include in library
file(GLOB TEST_CXX_SOURCES test/*.cpp)
add_executable(input_storage_test_c ${TEST_CXX_SOURCES})
#set_target_properties(input_storage_test_c PROPERTIES COMPILE_DEFINITIONS H5_BUILT_AS_DYNAMIC)
set_property(TARGET input_storage_test_c PROPERTY LINKER_LANGUAGE CXX)
#TARGET_NAMING (input_storage_test_c ${LIB_TYPE})
#TARGET_C_PROPERTIES (input_storage_test_c ${LIB_TYPE} " " " ")
target_link_libraries(input_storage_test_c
    input_storage
    debug ${BOOST_FILESYSTEM_DEBUG} optimized ${BOOST_FILESYSTEM_RELEASE}
    debug ${BOOST_REGEX_DEBUG} optimized ${BOOST_REGEX_RELEASE}
    debug ${BOOST_SYSTEM_DEBUG} optimized ${BOOST_SYSTEM_RELEASE}
    debug ${BOOST_UNITTEST_DEBUG} optimized ${BOOST_UNITTEST_RELEASE}
    ${HDF5} ${HDF5_FORTRAN} ${HDF5_F90CSTUB} ${HDF5_CPP}  
    ${HDF5_HL} ${HDF5_HL_CPP} ${HDF5_HL_F90CSTUB} ${HDF5_HL_FORTRAN} ${HDF5_HL_CPP}
    ${SZLIB} ${ZLIB}
    ${MMT_LIB} ${MMT_SQ_LIB} ${MMT_CORE_LIB}
    debug ${CMT_LIB_DEBUG} optimized ${CMT_LIB_RELEASE}
    )
# add pre build command
add_custom_command(OUTPUT channel_input_storage.fi PRE_BUILD 
    COMMAND python generate.py WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
    COMMENT "Generating source code from script")

# search by file pattern and include in library
file(GLOB TEST_F_SOURCES test/*.f90)
file(GLOB TEST_F_HEADERS test/*.fi)
add_library(input_storage_test_f STATIC ${TEST_F_SOURCES} ${TEST_F_HEADERS})
set_property(TARGET input_storage_test_f PROPERTY LINKER_LANGUAGE Fortran)

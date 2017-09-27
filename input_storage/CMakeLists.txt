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
set(CMAKE_C_FLAGS_DEBUG "/MTd")
#---------------LIBRARIES-------------------------------------------------
#------------------ DEPENDENCIES ---------------------
#----------------- EXECUTABLES --------------------------------------------
#----------------- FIND THE LIBRARIES -------------------
include_directories("${DSM2_THIRD_PARTY_DIR}/hdf5_183_xp32_vs2008_ivf101/include" "${DSM2_THIRD_PARTY_DIR}/boost_1_39")
#boost libraries
find_library(BOOST_FILESYSTEM_DEBUG NAMES libboost_filesystem-vc90-mt-sgd-1_39 PATHS ${DSM2_THIRD_PARTY_DIR}/boost_1_39/lib NO_DEFAULT_PATH)
find_library(BOOST_FILESYSTEM_RELEASE NAMES libboost_filesystem-vc90-mt-s-1_39 PATHS ${DSM2_THIRD_PARTY_DIR}/boost_1_39/lib NO_DEFAULT_PATH)
find_library(BOOST_REGEX_DEBUG NAMES libboost_regex-vc90-mt-sgd-1_39.lib PATHS ${DSM2_THIRD_PARTY_DIR}/boost_1_39/lib NO_DEFAULT_PATH)
find_library(BOOST_REGEX_RELEASE NAMES libboost_regex-vc90-mt-s-1_39.lib PATHS ${DSM2_THIRD_PARTY_DIR}/boost_1_39/lib NO_DEFAULT_PATH)
find_library(BOOST_SYSTEM_DEBUG NAMES libboost_system-vc90-mt-sgd-1_39.lib PATHS ${DSM2_THIRD_PARTY_DIR}/boost_1_39/lib NO_DEFAULT_PATH)
find_library(BOOST_SYSTEM_RELEASE NAMES libboost_system-vc90-mt-s-1_39.lib PATHS ${DSM2_THIRD_PARTY_DIR}/boost_1_39/lib NO_DEFAULT_PATH)
find_library(BOOST_UNITTEST_DEBUG NAMES libboost_unit_test_framework-vc90-mt-sgd-1_39.lib PATHS ${DSM2_THIRD_PARTY_DIR}/boost_1_39/lib NO_DEFAULT_PATH)
find_library(BOOST_UNITTEST_RELEASE NAMES libboost_unit_test_framework-vc90-mt-s-1_39.lib PATHS ${DSM2_THIRD_PARTY_DIR}/boost_1_39/lib NO_DEFAULT_PATH)
#hdf5.lib hdf5_hl_fortran.lib hdf5_fortran.lib hdf5_f90cstub.lib hdf5_hl.lib 
set(HDF5_LIB_PATH "${DSM2_THIRD_PARTY_DIR}/hdf5_183_xp32_vs2008_ivf101/lib")
find_library(HDF5 NAMES hdf5.lib PATHS ${HDF5_LIB_PATH} NO_DEFAULT_PATH)
find_library(HDF5_HL_FORTRAN NAMES hdf5_hl_fortran.lib PATHS ${HDF5_LIB_PATH} NO_DEFAULT_PATH)
find_library(HDF5_FORTRAN NAMES hdf5_fortran.lib PATHS ${HDF5_LIB_PATH} NO_DEFAULT_PATH)
find_library(HDF5_F90CSTUB NAMES hdf5_f90cstub.lib PATHS ${HDF5_LIB_PATH} NO_DEFAULT_PATH)
find_library(HDF5_HL NAMES hdf5_hl.lib PATHS ${HDF5_LIB_PATH} NO_DEFAULT_PATH)
#szlib.lib zlib.lib 
find_library(SZLIB NAMES szlib.lib PATHS "${DSM2_THIRD_PARTY_DIR}/szip21_vs6_enc/lib" NO_DEFAULT_PATH)
find_library(ZLIB NAMES zlib.lib PATHS "${DSM2_THIRD_PARTY_DIR}/zlib123_vs6/lib" NO_DEFAULT_PATH)
#standard libraries. no need to specify path as they are linked from default library paths FIXME: find_package
set (CMT_LIB_DEBUG "libcmtd.lib")
set (CMT_LIB_RELEASE "libcmt.lib")
set (MMT_LIB "libmmt.lib")
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
include_directories(src)
add_library(input_storage ${SOURCES} ${HEADERS})
target_link_libraries(input_storage 
    debug ${BOOST_FILESYSTEM_DEBUG} optimized ${BOOST_FILESYSTEM_RELEASE}
    debug ${BOOST_REGEX_DEBUG} optimized ${BOOST_REGEX_RELEASE}
    debug ${BOOST_SYSTEM_DEBUG} optimized ${BOOST_SYSTEM_RELEASE}
    debug ${BOOST_UNITTEST_DEBUG} optimized ${BOOST_UNITTEST_RELEASE}
    ${HDF5} ${HDF5_HL_FORTRAN} ${HDF5_FORTRAN} ${HDF5_F90CSTUB} ${HDF5_HL} ${SZLIB} ${ZLIB}
    ${MMT_LIB} debug ${CMT_LIB_DEBUG} optimized ${CMT_LIB_RELEASE}
    )
set(CMAKE_DEBUG_POSTFIX "d")
set_target_properties(input_storage PROPERTIES OUTPUT_NAME "input_storage_vs9_mt")
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
    debug ${BOOST_FILESYSTEM_DEBUG} optimized ${BOOST_FILESYSTEM_RELEASE}
    debug ${BOOST_REGEX_DEBUG} optimized ${BOOST_REGEX_RELEASE}
    debug ${BOOST_SYSTEM_DEBUG} optimized ${BOOST_SYSTEM_RELEASE}
    debug ${BOOST_UNITTEST_DEBUG} optimized ${BOOST_UNITTEST_RELEASE}
    ${HDF5} ${HDF5_HL_FORTRAN} ${HDF5_FORTRAN} ${HDF5_F90CSTUB} ${HDF5_HL} ${SZLIB} ${ZLIB}
    ${MMT_LIB} debug ${CMT_LIB_DEBUG} optimized ${CMT_LIB_RELEASE}
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
set_property(TARGET input_storage_test_c PROPERTY LINKER_LANGUAGE CXX)
target_link_libraries(input_storage_test_c
    input_storage
    debug ${BOOST_FILESYSTEM_DEBUG} optimized ${BOOST_FILESYSTEM_RELEASE}
    debug ${BOOST_REGEX_DEBUG} optimized ${BOOST_REGEX_RELEASE}
    debug ${BOOST_SYSTEM_DEBUG} optimized ${BOOST_SYSTEM_RELEASE}
    ${HDF5} ${HDF5_HL_FORTRAN} ${HDF5_FORTRAN} ${HDF5_F90CSTUB} ${HDF5_HL} ${SZLIB} ${ZLIB}
    ${MMT_LIB} debug ${CMT_LIB_DEBUG} optimized ${CMT_LIB_RELEASE}
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

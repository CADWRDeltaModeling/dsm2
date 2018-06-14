cmake_minimum_required (VERSION 3.2.2)
PROJECT(OpRuleAll)
enable_language(C CXX)
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
#--- Fortran compiler options
if (Fortran_COMPILER_NAME MATCHES "gfortran.*")
  # gfortran
  set (CMAKE_Fortran_FLAGS_RELEASE "-funroll-all-loops -fno-f2c -O3")
  set (CMAKE_Fortran_FLAGS_DEBUG   "-fno-f2c -O0 -g")
elseif (Fortran_COMPILER_NAME MATCHES "ifort.*")
  # ifort (untested)
  if (CMAKE_SYSTEM_NAME MATCHES "Windows.*")
      set (CMAKE_Fortran_FLAGS "/free")  
      set (CMAKE_Fortran_FLAGS_RELEASE "/f77rtl /O3")
      #set (CMAKE_Fortran_FLAGS_DEBUG   "/Od /f77rtl /debug:full /Qfp-stack-check /dbglibs /check:all /traceback /warn:unused")
      set (CMAKE_Fortran_FLAGS_DEBUG   "/f77rtl /check:all /traceback /warn:unused")
  else (CMAKE_SYSTEM_NAME MATCHES "Windows.*")
      # free format
      set (CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS}" "-free")  
      set (CMAKE_Fortran_FLAGS_RELEASE "-f77rtl -O3")
      set (CMAKE_Fortran_FLAGS_DEBUG   "-f77rtl -O0 -g")
  endif (CMAKE_SYSTEM_NAME MATCHES "Windows.*")
elseif (Fortran_COMPILER_NAME MATCHES "g77")
  # g77
  set (CMAKE_Fortran_FLAGS_RELEASE "-funroll-all-loops -fno-f2c -O3 -m32")
  set (CMAKE_Fortran_FLAGS_DEBUG   "-fno-f2c -O0 -g -m32")
else (Fortran_COMPILER_NAME MATCHES "gfortran.*")
  message ("CMAKE_Fortran_COMPILER full path: " ${CMAKE_Fortran_COMPILER})
  message ("Fortran compiler: " ${Fortran_COMPILER_NAME})
  message ("No optimized Fortran compiler flags are known, we just try -O2...")
  set (CMAKE_Fortran_FLAGS_RELEASE "-O2")
  set (CMAKE_Fortran_FLAGS_DEBUG   "-O0 -g")
endif (Fortran_COMPILER_NAME MATCHES "gfortran.*")
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
set(HDF5_DIR "${DSM2_THIRD_PARTY_DIR}/HDF5-1.8.20-win32")
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
find_library(SZLIB NAMES ${HDF5_LIB_PREFIX}szip.lib PATHS "${HDF5_LIB_PATH}" NO_DEFAULT_PATH)
find_library(ZLIB NAMES ${HDF5_LIB_PREFIX}zlib.lib PATHS "${HDF5_LIB_PATH}" NO_DEFAULT_PATH)
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
#--------------------- OPRULE LIB ------------------------------------
file(GLOB SOURCES lib/rule/*.cpp)
#list(REMOVE_ITEM SOURCES "${CMAKE_SOURCE_DIR}/src/ItemInputState.cpp")
file(GLOB HEADERS oprule/rule/*.h)
include_directories(${CMAKE_SOURCE_DIR})
add_library(OperatingRule ${SOURCES} ${HEADERS})
target_link_libraries(OperatingRule 
    debug ${BOOST_FILESYSTEM_DEBUG} optimized ${BOOST_FILESYSTEM_RELEASE}
    debug ${BOOST_REGEX_DEBUG} optimized ${BOOST_REGEX_RELEASE}
    debug ${BOOST_SYSTEM_DEBUG} optimized ${BOOST_SYSTEM_RELEASE}
    debug ${BOOST_UNITTEST_DEBUG} optimized ${BOOST_UNITTEST_RELEASE}
    ${HDF5} ${HDF5_HL_FORTRAN} ${HDF5_FORTRAN} ${HDF5_F90CSTUB} ${HDF5_HL} ${SZLIB} ${ZLIB}
    ${MMT_LIB} debug ${CMT_LIB_DEBUG} optimized ${CMT_LIB_RELEASE}
    )
set(CMAKE_DEBUG_POSTFIX "d")
set_target_properties(OperatingRule PROPERTIES OUTPUT_NAME "oprule_vs14_mt")
set_target_properties(OperatingRule PROPERTIES DEBUG_POSTFIX "d")
add_custom_command(TARGET OperatingRule POST_BUILD 
  COMMAND "${CMAKE_COMMAND}" -E copy 
     "$<TARGET_FILE:OperatingRule>"
     "${CMAKE_SOURCE_DIR}/../lib/$<TARGET_FILE_NAME:OperatingRule>" 
  COMMENT "Copying OperatingRule lib to output directory")
#--------------------- OPRULE PARSER LIB ------------------------------------
# add pre build command
add_custom_command(OUTPUT ${CMAKE_CURRENT_SOURCE_DIR}/lib/parser/op_rule.cpp PRE_BUILD 
    COMMAND flex -L -Pop_rule -oop_rule.cpp ${CMAKE_CURRENT_SOURCE_DIR}/lib/parser/op_rule.l
    WORKING_DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}/lib/parser"
    COMMENT "Generating op_rule.cpp using flex")
add_custom_command(OUTPUT ${CMAKE_CURRENT_SOURCE_DIR}/lib/parser/op_rule_tab.cpp PRE_BUILD 
    COMMAND bison -l -v -p op_rule -d  ${CMAKE_CURRENT_SOURCE_DIR}/lib/parser/op_rule.y
    COMMAND move /Y op_rule.tab.c ${CMAKE_CURRENT_SOURCE_DIR}/lib/parser/op_rule_tab.cpp
    COMMAND move /Y op_rule.tab.h ${CMAKE_CURRENT_SOURCE_DIR}/lib/parser/op_rule_tab.h
    WORKING_DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}/lib/parser"
    COMMENT "Generating op_rule_tab.cpp and op_rule_tab.h using bison" VERBATIM)
file(GLOB SOURCES lib/parser/*.cpp)
list(REMOVE_ITEM SOURCES "${CMAKE_SOURCE_DIR}/lib/parser/Parser-notyet.cpp")
list(REMOVE_ITEM SOURCES "${CMAKE_SOURCE_DIR}/lib/parser/ModelTimeNodeFactory.cpp")
list(REMOVE_ITEM SOURCES "${CMAKE_SOURCE_DIR}/lib/parser/ModelActionFactory.cpp")
file(GLOB HEADERS lib/parser/*.h)
list(REMOVE_ITEM HEADERS "${CMAKE_SOURCE_DIR}/lib/parser/op_rule_tab.h")
list(REMOVE_ITEM HEADERS "${CMAKE_SOURCE_DIR}/lib/parser/op_rule_tab.h")
file(GLOB PARSER_HEADERS oprule/parser/*.h)
list(REMOVE_ITEM PARSER_HEADERS "${CMAKE_SOURCE_DIR}/oprule/parser/op_rule.tab.h")
list(REMOVE_ITEM PARSER_HEADERS "${CMAKE_SOURCE_DIR}/oprule/parser/ModelActionFactory.h")
list(REMOVE_ITEM PARSER_HEADERS "${CMAKE_SOURCE_DIR}/oprule/parser/ParseError.h")
list(REMOVE_ITEM PARSER_HEADERS "${CMAKE_SOURCE_DIR}/oprule/parser/Parser-notyet.h")
list(REMOVE_ITEM PARSER_HEADERS "${CMAKE_SOURCE_DIR}/oprule/parser/ptr_wrapper.h")
include_directories(src)
add_library(OpRuleParser ${SOURCES} ${HEADERS} ${PARSER_HEADERS} "oprule/rule/ActionSet.h")
target_link_libraries(OpRuleParser 
    debug ${BOOST_FILESYSTEM_DEBUG} optimized ${BOOST_FILESYSTEM_RELEASE}
    debug ${BOOST_REGEX_DEBUG} optimized ${BOOST_REGEX_RELEASE}
    debug ${BOOST_SYSTEM_DEBUG} optimized ${BOOST_SYSTEM_RELEASE}
    debug ${BOOST_UNITTEST_DEBUG} optimized ${BOOST_UNITTEST_RELEASE}
    ${HDF5} ${HDF5_HL_FORTRAN} ${HDF5_FORTRAN} ${HDF5_F90CSTUB} ${HDF5_HL} ${SZLIB} ${ZLIB}
    ${MMT_LIB} debug ${CMT_LIB_DEBUG} optimized ${CMT_LIB_RELEASE}
    )
set(CMAKE_DEBUG_POSTFIX "d")
set_target_properties(OpRuleParser PROPERTIES OUTPUT_NAME "oprule_parser_vs14_mt")
set_target_properties(OpRuleParser PROPERTIES DEBUG_POSTFIX "d")
add_custom_command(TARGET OpRuleParser POST_BUILD 
  COMMAND "${CMAKE_COMMAND}" -E copy 
     "$<TARGET_FILE:OpRuleParser>"
     "${CMAKE_SOURCE_DIR}/../lib/$<TARGET_FILE_NAME:OpRuleParser>" 
  COMMENT "Copying OperatingRule parser lib to output directory")

#-------------------- TestOperatingRule exe -----------------
# search by file pattern and include in library
file(GLOB TEST_OPRULE_CXX_SOURCES test/rule/*.cpp)
list(REMOVE_ITEM TEST_OPRULE_CXX_SOURCES "${CMAKE_SOURCE_DIR}/test/rule/ModelStateTestFixture.cpp")
list(REMOVE_ITEM TEST_OPRULE_CXX_SOURCES "${CMAKE_SOURCE_DIR}/test/rule/TestStateAction.cpp")
file(GLOB TEST_OPRULE_CXX_HEADERS test/rule/*.h)
add_executable(TestOperatingRule ${TEST_OPRULE_CXX_SOURCES} ${TEST_OPRULE_CXX_HEADERS})
set_property(TARGET TestOperatingRule PROPERTY LINKER_LANGUAGE CXX)
target_link_libraries(TestOperatingRule
    OperatingRule OpRuleParser
    debug ${BOOST_FILESYSTEM_DEBUG} optimized ${BOOST_FILESYSTEM_RELEASE}
    debug ${BOOST_REGEX_DEBUG} optimized ${BOOST_REGEX_RELEASE}
    debug ${BOOST_SYSTEM_DEBUG} optimized ${BOOST_SYSTEM_RELEASE}
    debug ${BOOST_UNITTEST_DEBUG} optimized ${BOOST_UNITTEST_RELEASE}
    ${HDF5} ${HDF5_HL_FORTRAN} ${HDF5_FORTRAN} ${HDF5_F90CSTUB} ${HDF5_HL} ${SZLIB} ${ZLIB}
    ${MMT_LIB} debug ${CMT_LIB_DEBUG} optimized ${CMT_LIB_RELEASE}
    )
#-------------------- TestParser exe -----------------
# search by file pattern and include in library
file(GLOB TEST_PARSER_CXX_SOURCES test/parser/*.cpp)
file(GLOB TEST_PARSER_CXX_HEADERS test/parser/*.h)
add_executable(TestParser ${TEST_PARSER_CXX_SOURCES} ${TEST_PARSER_CXX_HEADERS})
set_property(TARGET TestParser PROPERTY LINKER_LANGUAGE CXX)
target_link_libraries(TestParser
    OperatingRule OpRuleParser
    debug ${BOOST_FILESYSTEM_DEBUG} optimized ${BOOST_FILESYSTEM_RELEASE}
    debug ${BOOST_REGEX_DEBUG} optimized ${BOOST_REGEX_RELEASE}
    debug ${BOOST_SYSTEM_DEBUG} optimized ${BOOST_SYSTEM_RELEASE}
    debug ${BOOST_UNITTEST_DEBUG} optimized ${BOOST_UNITTEST_RELEASE}
    ${HDF5} ${HDF5_HL_FORTRAN} ${HDF5_FORTRAN} ${HDF5_F90CSTUB} ${HDF5_HL} ${SZLIB} ${ZLIB}
    ${MMT_LIB} debug ${CMT_LIB_DEBUG} optimized ${CMT_LIB_RELEASE}
    )

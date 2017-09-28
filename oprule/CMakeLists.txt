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
set_target_properties(OperatingRule PROPERTIES OUTPUT_NAME "oprule_vs9_mt")
set_target_properties(OperatingRule PROPERTIES DEBUG_POSTFIX "d")
add_custom_command(TARGET OperatingRule POST_BUILD 
  COMMAND "${CMAKE_COMMAND}" -E copy 
     "$<TARGET_FILE:OperatingRule>"
     "${CMAKE_SOURCE_DIR}/../lib/$<TARGET_FILE_NAME:OperatingRule>" 
  COMMENT "Copying OperatingRule lib to output directory")
#--------------------- OPRULE PARSER LIB ------------------------------------
# add pre build command
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
set_target_properties(OpRuleParser PROPERTIES OUTPUT_NAME "oprule_parser_vs9_mt")
set_target_properties(OpRuleParser PROPERTIES DEBUG_POSTFIX "d")
add_custom_command(TARGET OpRuleParser PRE_BUILD 
    COMMAND flex -L -Pop_rule -oop_rule.cpp ${CMAKE_CURRENT_SOURCE_DIR}/lib/parser/op_rule.l
    WORKING_DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}/lib/parser"
    COMMENT "Generating op_rule.cpp using flex")
add_custom_command(TARGET OpRuleParser PRE_BUILD 
    COMMAND bison -l -v -p op_rule -d  ${CMAKE_CURRENT_SOURCE_DIR}/lib/parser/op_rule.y
    COMMAND move /Y op_rule.tab.c ${CMAKE_CURRENT_SOURCE_DIR}/lib/parser/op_rule_tab.cpp
    COMMAND move /Y op_rule.tab.h ${CMAKE_CURRENT_SOURCE_DIR}/lib/parser/op_rule_tab.h
    WORKING_DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}/lib/parser"
    COMMENT "Generating op_rule_tab.cpp and op_rule_tab.h using bison" VERBATIM)
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

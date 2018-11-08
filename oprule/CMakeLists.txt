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
# include the common macros for compiler flags and third party libraries
include(${DSM2_TOP_DIR}CmakeLibraryMacro.txt)
set_compiler_flags()
#---------------LIBRARIES-------------------------------------------------
find_libs()
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
file(GLOB SOURCES lib/parser/*.cpp)
list(REMOVE_ITEM SOURCES "${CMAKE_SOURCE_DIR}/lib/parser/Parser-notyet.cpp")
list(REMOVE_ITEM SOURCES "${CMAKE_SOURCE_DIR}/lib/parser/ModelTimeNodeFactory.cpp")
list(REMOVE_ITEM SOURCES "${CMAKE_SOURCE_DIR}/lib/parser/ModelActionFactory.cpp")
list(APPEND SOURCES "${CMAKE_SOURCE_DIR}/lib/parser/op_rule.cpp")
list(APPEND SOURCES "${CMAKE_SOURCE_DIR}/lib/parser/op_rule_tab.cpp")
file(GLOB HEADERS lib/parser/*.h)
list(APPEND HEADERS lib/parser/op_rule_tab.cpp)
file(GLOB PARSER_HEADERS oprule/parser/*.h)
list(REMOVE_ITEM PARSER_HEADERS "${CMAKE_SOURCE_DIR}/oprule/parser/op_rule.tab.h")
list(REMOVE_ITEM PARSER_HEADERS "${CMAKE_SOURCE_DIR}/oprule/parser/ModelActionFactory.h")
list(REMOVE_ITEM PARSER_HEADERS "${CMAKE_SOURCE_DIR}/oprule/parser/ParseError.h")
list(REMOVE_ITEM PARSER_HEADERS "${CMAKE_SOURCE_DIR}/oprule/parser/Parser-notyet.h")
list(REMOVE_ITEM PARSER_HEADERS "${CMAKE_SOURCE_DIR}/oprule/parser/ptr_wrapper.h")
include_directories(src)
add_library(OpRuleParser ${SOURCES} ${HEADERS} ${PARSER_HEADERS} "oprule/rule/ActionSet.h")
# add pre build command
add_custom_command(OUTPUT "${CMAKE_SOURCE_DIR}/lib/parser/op_rule.cpp"  
    COMMAND flex -L -Pop_rule -oop_rule.cpp ${CMAKE_CURRENT_SOURCE_DIR}/lib/parser/op_rule.l
    WORKING_DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}/lib/parser"
    COMMENT "Generating op_rule.cpp using flex")
add_custom_command(OUTPUT "${CMAKE_SOURCE_DIR}/lib/parser/op_rule_tab.cpp" PRE_BUILD 
    COMMAND bison -l -v -p op_rule -d  ${CMAKE_CURRENT_SOURCE_DIR}/lib/parser/op_rule.y
    COMMAND move /Y op_rule.tab.c ${CMAKE_CURRENT_SOURCE_DIR}/lib/parser/op_rule_tab.cpp
    COMMAND move /Y op_rule.tab.h ${CMAKE_CURRENT_SOURCE_DIR}/lib/parser/op_rule_tab.h
    WORKING_DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}/lib/parser"
    COMMENT "Generating op_rule_tab.cpp and op_rule_tab.h using bison" VERBATIM)
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

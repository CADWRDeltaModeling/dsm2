cmake_minimum_required(VERSION 3.21)
enable_language(C CXX Fortran)

string(TIMESTAMP CONFIG_DATE "%Y-%m-%d")

# OPRULE LIB
file(GLOB TEST_OPRULE_CXX_SOURCES lib/rule/*.cpp)
add_library(oprule ${TEST_OPRULE_CXX_SOURCES})
target_include_directories(
  oprule PUBLIC ${CMAKE_CURRENT_SOURCE_DIR} ${HDF5_INCLUDE_DIRS}
                ${Boost_INCLUDE_DIRS})


# --------------------- OPRULE PARSER LIB
file(GLOB OPRULE_PARSER_SOURCES lib/parser/*.cpp)
add_library(oprule_parser ${OPRULE_PARSER_SOURCES} op_rule.cpp op_rule_tab.cpp)
target_include_directories(oprule_parser PUBLIC ${CMAKE_CURRENT_SOURCE_DIR}
                                                ${Boost_INCLUDE_DIRS})

add_custom_command(
  OUTPUT op_rule.cpp
  COMMAND flex -L -P op_rule -o op_rule.cpp
          ${CMAKE_CURRENT_SOURCE_DIR}/lib/parser/op_rule.l
  DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/lib/parser/op_rule.l
  # WORKING_DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}/lib/parser"
  COMMENT "Generating op_rule.cpp using flex")
add_custom_command(
  OUTPUT op_rule_tab.cpp
  COMMAND bison -l -v -p op_rule -d
          ${CMAKE_CURRENT_SOURCE_DIR}/lib/parser/op_rule.y
  COMMAND ${CMAKE_COMMAND} -E rename op_rule.tab.c op_rule_tab.cpp
  DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/lib/parser/op_rule.y
  # WORKING_DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}/lib/parser"
  COMMENT "Generating op_rule_tab.cpp and op_rule_tab.h using bison"
  VERBATIM)


# add_custom_command(TARGET OpRuleParser POST_BUILD
#   COMMAND "${CMAKE_COMMAND}" -E copy
#   "$<TARGET_FILE:OpRuleParser>"
#   "${CMAKE_SOURCE_DIR}/../lib/$<TARGET_FILE_NAME:OpRuleParser>"
#   COMMENT "Copying OperatingRule parser lib to output directory")

# -------------------- TestOperatingRule exe -----------------
# search by file pattern and include in library
# file(GLOB TEST_OPRULE_CXX_SOURCES test/rule/*.cpp)
# list(REMOVE_ITEM TEST_OPRULE_CXX_SOURCES "${CMAKE_SOURCE_DIR}/test/rule/ModelStateTestFixture.cpp")
# list(REMOVE_ITEM TEST_OPRULE_CXX_SOURCES "${CMAKE_SOURCE_DIR}/test/rule/TestStateAction.cpp")
# file(GLOB TEST_OPRULE_CXX_HEADERS test/rule/*.h)
# add_executable(TestOperatingRule ${TEST_OPRULE_CXX_SOURCES} ${TEST_OPRULE_CXX_HEADERS})
# set_property(TARGET TestOperatingRule PROPERTY LINKER_LANGUAGE CXX)
# target_link_libraries(TestOperatingRule
# OperatingRule OpRuleParser
# debug ${BOOST_FILESYSTEM_DEBUG} optimized ${BOOST_FILESYSTEM_RELEASE}
# debug ${BOOST_REGEX_DEBUG} optimized ${BOOST_REGEX_RELEASE}
# debug ${BOOST_SYSTEM_DEBUG} optimized ${BOOST_SYSTEM_RELEASE}
# debug ${BOOST_UNITTEST_DEBUG} optimized ${BOOST_UNITTEST_RELEASE}
# ${HDF5} ${HDF5_HL_FORTRAN} ${HDF5_FORTRAN} ${HDF5_F90CSTUB} ${HDF5_HL} ${SZLIB} ${ZLIB}
# ${MMT_LIB} ${CMT_LIB_RELEASE}
# )
# -------------------- TestParser exe -----------------
# search by file pattern and include in library
# file(GLOB TEST_PARSER_CXX_SOURCES test/parser/*.cpp)
# file(GLOB TEST_PARSER_CXX_HEADERS test/parser/*.h)
# add_executable(TestParser ${TEST_PARSER_CXX_SOURCES} ${TEST_PARSER_CXX_HEADERS})
# set_property(TARGET TestParser PROPERTY LINKER_LANGUAGE CXX)
# target_link_libraries(TestParser
# OperatingRule OpRuleParser
# debug ${BOOST_FILESYSTEM_DEBUG} optimized ${BOOST_FILESYSTEM_RELEASE}
# debug ${BOOST_REGEX_DEBUG} optimized ${BOOST_REGEX_RELEASE}
# debug ${BOOST_SYSTEM_DEBUG} optimized ${BOOST_SYSTEM_RELEASE}
# debug ${BOOST_UNITTEST_DEBUG} optimized ${BOOST_UNITTEST_RELEASE}
# ${HDF5} ${HDF5_HL_FORTRAN} ${HDF5_FORTRAN} ${HDF5_F90CSTUB} ${HDF5_HL} ${SZLIB} ${ZLIB}
# ${MMT_LIB} ${CMT_LIB_RELEASE}
# )

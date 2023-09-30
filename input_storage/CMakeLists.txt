cmake_minimum_required(VERSION 3.17)
project(input_storage LANGUAGES C CXX)

set(CMAKE_DEBUG_POSTFIX "d")

string(TIMESTAMP CONFIG_DATE "%Y-%m-%d")

find_package(Boost REQUIRED)
find_package(HDF5 REQUIRED)



file(GLOB SOURCES src/*.cpp)
list(REMOVE_ITEM SOURCES "${CMAKE_SOURCE_DIR}/src/ItemInputState.cpp")
add_library(input_storage ${SOURCES})
target_include_directories(input_storage PUBLIC ${Boost_INCLUDE_DIRS}
                                                ${HDF5_INCLUDE_DIRS})

# Post build command
# add_custom_command(TARGET input_storage POST_BUILD
#   COMMAND "${CMAKE_COMMAND}" -E copy
#   "$<TARGET_FILE:input_storage>"
#   "${CMAKE_SOURCE_DIR}/../lib/$<TARGET_FILE_NAME:input_storage>"
#   COMMENT "Copying to output directory")

# -------------------- EXAMPLE exe and fortran library -----------------
# search by file pattern and include in library
# file(GLOB EXAMPLE_CXX_SOURCES example/*.cpp)
# add_executable(example_c ${EXAMPLE_CXX_SOURCES})
# set_property(TARGET example_c PROPERTY LINKER_LANGUAGE CXX)
# add pre build command
# add_custom_command(TARGET example_c PRE_BUILD
# COMMAND python generate.py WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/example
# COMMENT "Generating source code from script")
# target_link_libraries(example_c
# input_storage
# ${HDF5} ${HDF5_FORTRAN} ${HDF5_F90CSTUB} ${HDF5_CPP}
# ${HDF5_HL} ${HDF5_HL_CPP} ${HDF5_HL_F90CSTUB} ${HDF5_HL_FORTRAN} ${HDF5_HL_CPP}
# ${SZLIB} ${ZLIB}
# debug ${BOOST_FILESYSTEM_DEBUG} optimized ${BOOST_FILESYSTEM_RELEASE}
# debug ${BOOST_REGEX_DEBUG} optimized ${BOOST_REGEX_RELEASE}
# debug ${BOOST_SYSTEM_DEBUG} optimized ${BOOST_SYSTEM_RELEASE}
# debug ${BOOST_UNITTEST_DEBUG} optimized ${BOOST_UNITTEST_RELEASE}
# ${MMT_LIB} ${MMT_SQ_LIB} ${MMT_CORE_LIB}
# debug ${CMT_LIB_DEBUG} optimized ${CMT_LIB_RELEASE}
# )
# search by file pattern and include in library
# file(GLOB EXAMPLE_F_SOURCES example/*.f90)
# file(GLOB EXAMPLE_F_HEADERS example/*.fi)
# add_library(example_f ${EXAMPLE_F_SOURCES} ${EXAMPLE_F_HEADERS})
# set_property(TARGET example_f PROPERTY LINKER_LANGUAGE Fortran)
# ---------------------- TEST exe and fortran ------------------------
# search by file pattern and include in library
# file(GLOB TEST_CXX_SOURCES test/*.cpp)
# add_executable(input_storage_test_c ${TEST_CXX_SOURCES})
# set_property(TARGET input_storage_test_c PROPERTY LINKER_LANGUAGE CXX)
# target_link_libraries(input_storage_test_c
# input_storage
# debug ${BOOST_FILESYSTEM_DEBUG} optimized ${BOOST_FILESYSTEM_RELEASE}
# debug ${BOOST_REGEX_DEBUG} optimized ${BOOST_REGEX_RELEASE}
# debug ${BOOST_SYSTEM_DEBUG} optimized ${BOOST_SYSTEM_RELEASE}
# debug ${BOOST_UNITTEST_DEBUG} optimized ${BOOST_UNITTEST_RELEASE}
# ${HDF5} ${HDF5_FORTRAN} ${HDF5_F90CSTUB} ${HDF5_CPP}
# ${HDF5_HL} ${HDF5_HL_CPP} ${HDF5_HL_F90CSTUB} ${HDF5_HL_FORTRAN} ${HDF5_HL_CPP}
# ${SZLIB} ${ZLIB}
# ${MMT_LIB} ${MMT_SQ_LIB} ${MMT_CORE_LIB}
# debug ${CMT_LIB_DEBUG} optimized ${CMT_LIB_RELEASE}
# )
# add pre build command
# add_custom_command(TARGET input_storage_test_c PRE_BUILD
# COMMAND python generate.py WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/test
# COMMENT "Generating source code from script")
# search by file pattern and include in library
# file(GLOB TEST_F_SOURCES test/*.f90)
# file(GLOB TEST_F_HEADERS test/*.fi)
# add_library(input_storage_test_f ${TEST_F_SOURCES} ${TEST_F_HEADERS})
# set_property(TARGET input_storage_test_f PROPERTY LINKER_LANGUAGE Fortran)

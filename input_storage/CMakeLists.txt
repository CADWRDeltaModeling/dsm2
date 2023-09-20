cmake_minimum_required(VERSION 3.17)
project(input_storage LANGUAGES C CXX)

#
string(TIMESTAMP CONFIG_DATE "%Y-%m-%d")

# -----------------------------------------------------------------------------
# Set the core names of all the libraries
# -----------------------------------------------------------------------------
set(DSM2_TOP_DIR "../")

if(DEFINED THIRD_PARTY_DIR)
    set(DSM2_THIRD_PARTY_DIR "${THIRD_PARTY_DIR}")
else()
    set(DSM2_THIRD_PARTY_DIR "${DSM2_TOP_DIR}/third_party")
endif()
set(CMAKE_MSVC_RUNTIME_LIBRARY "MultiThreaded$<$<CONFIG:Debug>:Debug>DLL")

# include the common macros for compiler flags and third party libraries
# include(${DSM2_TOP_DIR}CmakeLibraryMacro.txt)

# set_compiler_flags()

# ---------------LIBRARIES-------------------------------------------------
# find_libs()
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

list(APPEND BOOST_LIB_NAMES filesystem regex "system" unit_test_framework)

foreach(NAME IN LISTS BOOST_LIB_NAMES)
    message(STATUS "Looking for boost library ${NAME}, boost_${NAME}${BOOST_RELEASE_VERSION}.${STATIC_LIB_SUFFIX}")
    unset(LIB)
    find_library(LIB NAMES boost_${NAME}${BOOST_RELEASE_VERSION}.${STATIC_LIB_SUFFIX}
        PATHS ${BOOST_LIB_DIR} NO_CACHE NO_DEFAULT_PATH)
    message(STATUS "LIB: ${LIB}")

    if(LIB)
        message(STATUS "Found boost library ${NAME}: ${LIB}")
        list(APPEND BOOST_LIBRARIES ${LIB})
    else()
        message(FATAL_ERROR "Could not find boost library ${BOOST_LIBLRARY_NAME}")
    endif()
endforeach()

set(HDF5_INCLUDE_DIR ${DSM2_THIRD_PARTY_DIR}/HDF5-1.10.10-win64/include)

# -------------add linker flags and excluded and included libraries
# if(MSVC)
# # set(CMAKE_EXE_LINKER_FLAGS
# # "${CMAKE_EXE_LINKER_FLAGS} /NODEFAULTLIB:libc.lib /NODEFAULTLIB:msvcrt.lib /NODEFAULTLIB:libcmt.lib /NODEFAULTLIB:msvcrtd.lib /NODEFAULTLIB:libmmd.lib /MANIFEST:NO")
# # set (CMAKE_SHARED_LINKER_FLAGS "${CMAKE_SHARED_LINKER_FLAGS} /NODEFAULTLIB:libc.lib /NODEFAULTLIB:msvcrt.lib /NODEFAULTLIB:libcmt.lib /NODEFAULTLIB:msvcrtd.lib /NODEFAULTLIB:libcpmt.lib /NODEFAULTLIB:libmmd.lib /MANIFEST:NO")
# # set(VS_STARTUP_PROJECT hydro)
# else(MSVC)
# set(CMAKE_EXE_LINKER_FLAGS "-nodefaultlibs")
# set(CMAKE_SHARED_LINKER_FLAGS "-nodefaultlibs")
# endif()

# include_directories()
file(GLOB SOURCES src/*.cpp)
list(REMOVE_ITEM SOURCES "${CMAKE_SOURCE_DIR}/src/ItemInputState.cpp")
set(CMAKE_DEBUG_POSTFIX "d")
add_library(input_storage ${SOURCES})
target_include_directories(input_storage PUBLIC
    ${BOOST_INCLUDE_DIR}
    ${HDF5_INCLUDE_DIR})
# target_compile_options(input_storage PUBLIC
#     "/MD$<$<CONFIG:Debug>:d>")
# target_link_libraries(input_storage PUBLIC
#     ${BOOST_LIBRARIES})
set_target_properties(input_storage
    PROPERTIES POSITION_INDEPENDENT_CODE ON)

# if(WIN32)
# set_target_properties(input_storage PROPERTIES OUTPUT_NAME "input_storage")
# else(WIN32)
# set_target_properties(input_storage PROPERTIES OUTPUT_NAME "input_storage_intel")
# endif(WIN32)

# Post build command
add_custom_command(TARGET input_storage POST_BUILD
    COMMAND "${CMAKE_COMMAND}" -E copy
    "$<TARGET_FILE:input_storage>"
    "${CMAKE_SOURCE_DIR}/../lib/$<TARGET_FILE_NAME:input_storage>"
    COMMENT "Copying to output directory")

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

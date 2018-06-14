cmake_minimum_required (VERSION 3.2.2)
PROJECT(DSM2)
enable_language(C CXX Fortran)
# 
string(TIMESTAMP CONFIG_DATE "%Y-%m-%d")
#-----------------------------------------------------------------------------
# Set the core names of all the libraries
#-----------------------------------------------------------------------------
set (DSM2_COMMON_LIB_CORENAME   "dsm2common")
set (DSM2_IO_LIB_CORENAME       "dsm2io")
#-----------------------------------------------------------------------------
# Set the true names of all the libraries if customized by external project
#-----------------------------------------------------------------------------
set (DSM2_COMMON_LIB_NAME     "${DSM2_COMMON_LIB_CORENAME}")
set (DSM2_IO_LIB_NAME         "${DSM2_IO_LIB_CORENAME}")
#-----------------------------------------------------------------------------
# Define some CMake variables for use later in the project
#-----------------------------------------------------------------------------
set (DSM2_COMMONS_DIR       ${DSM2_SOURCE_DIR}/common)
set (DSM2_FIXED_DIR         ${DSM2_SOURCE_DIR}/fixed)
set (DSM2_HDF_TIDEFILE      ${DSM2_SOURCE_DIR}/hdf_tidefile)
set (DSM2_HYDRO             ${DSM2_SOURCE_DIR}/hydro)
set (DSM2_HYDROLIB          ${DSM2_SOURCE_DIR}/hydrolib)
set (DSM2_INPUT_STORAGE     ${DSM2_SOURCE_DIR}/input_storage)
set (DSM2_KLU_FORTRAN       ${DSM2_SOURCE_DIR}/klu_fortran)
set (DSM2_OPRULE_INTERFACE  ${DSM2_SOURCE_DIR}/oprule_interface)
set (DSM2_PATTERN_MATCH ${DSM2_SOURCE_DIR}/pattern_match)
set (DSM2_PTM      ${DSM2_SOURCE_DIR}/ptm)
set (DSM2_QUAL      ${DSM2_SOURCE_DIR}/qual)
set (DSM2_SPARSE      ${DSM2_SOURCE_DIR}/sparse)
set (DSM2_TIMEVAR      ${DSM2_SOURCE_DIR}/timevar)
#
set(DSM2_TOP_DIR "../../../")
set(DSM2_THIRD_PARTY_DIR "${DSM2_TOP_DIR}/third_party")

# make sure that the default is a RELEASE
if (NOT CMAKE_BUILD_TYPE)
  set (CMAKE_BUILD_TYPE Debug CACHE STRING
      "Choose the type of build, options are: None Debug Release."
      FORCE)
endif (NOT CMAKE_BUILD_TYPE)
# FFLAGS depend on the compiler
get_filename_component (Fortran_COMPILER_NAME ${CMAKE_Fortran_COMPILER} NAME)
#####################
### GENERAL FLAGS ###
#####################
#- To ensure static linking for C++ code
set(CMAKE_CXX_FLAGS_RELEASE "/MT")
set(CMAKE_CXX_FLAGS_DEBUG "/MTd")
#- To ensure static linking for C code
set(CMAKE_C_FLAGS_RELEASE "/MT")
set(CMAKE_C_FLAGS_DEBUG "/MTd")
# Fortran flags
set(CMAKE_Fortran_MODULE_DIRECTORY ${PROJECT_BINARY_DIR}/modules)
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
#----------------- EXECUTABLES --------------------------------------------
set (CMAKE_Fortran_FLAGS "/extend_source:132")
#FIXME: change top directory to one-less. There should be a better way
set(DSM2_TOP_DIR "../../")
set(DSM2_THIRD_PARTY_DIR "${DSM2_TOP_DIR}/third_party")
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
include_directories("${HDF5_DIR}/include" "${HDF5_DIR}/include/static")
find_library(HDF5 NAMES ${HDF5_LIB_PREFIX}hdf5.lib PATHS ${HDF5_LIB_PATH} NO_DEFAULT_PATH)
find_library(HDF5_HL NAMES ${HDF5_LIB_PREFIX}hdf5_hl.lib PATHS ${HDF5_LIB_PATH} NO_DEFAULT_PATH)
find_library(HDF5_CPP NAMES ${HDF5_LIB_PREFIX}hdf5_cpp.lib PATHS ${HDF5_LIB_PATH} NO_DEFAULT_PATH)
find_library(HDF5_HL_CPP NAMES ${HDF5_LIB_PREFIX}hdf5_hl_cpp.lib PATHS ${HDF5_LIB_PATH} NO_DEFAULT_PATH)
find_library(HDF5_FORTRAN NAMES ${HDF5_LIB_PREFIX}hdf5_fortran.lib PATHS ${HDF5_LIB_PATH} NO_DEFAULT_PATH)
find_library(HDF5_HL_FORTRAN NAMES ${HDF5_LIB_PREFIX}hdf5_hl_fortran.lib PATHS ${HDF5_LIB_PATH} NO_DEFAULT_PATH)
find_library(HDF5_F90CSTUB NAMES ${HDF5_LIB_PREFIX}hdf5_f90cstub.lib PATHS ${HDF5_LIB_PATH} NO_DEFAULT_PATH)
find_library(HDF5_HL_F90CSTUB NAMES ${HDF5_LIB_PREFIX}hdf5_hl_f90cstub.lib PATHS ${HDF5_LIB_PATH} NO_DEFAULT_PATH)
#szlib.lib zlib.lib 
find_library(SZLIB NAMES ${HDF5_LIB_PREFIX}szip.lib PATHS "${HDF5_LIB_PATH}" NO_DEFAULT_PATH)
find_library(ZLIB NAMES ${HDF5_LIB_PREFIX}zlib.lib PATHS "${HDF5_LIB_PATH}" NO_DEFAULT_PATH)
# heclib_vs14_debug.lib 
find_library(HECLIB_DEBUG NAMES heclib_vs14_debug.lib PATHS "${DSM2_TOP_DIR}/lib" NO_DEFAULT_PATH)
find_library(HECLIB_RELEASE NAMES heclib_vs14.lib PATHS "${DSM2_TOP_DIR}/lib" NO_DEFAULT_PATH)
# oprule_vs14_mtd.lib oprule_parser_vs14_mtd.lib
find_library(OPRULE_DEBUG NAMES_DEBUG oprule_vs14_mtd.lib PATHS "${DSM2_TOP_DIR}/lib" NO_DEFAULT_PATH)
find_library(OPRULE_PARSER_DEBUG NAMES oprule_parser_vs14_mtd.lib PATHS "${DSM2_TOP_DIR}/lib" NO_DEFAULT_PATH)
find_library(OPRULE_RELEASE NAMES oprule_vs14_mt.lib PATHS "${DSM2_TOP_DIR}/lib" NO_DEFAULT_PATH)
find_library(OPRULE_PARSER_RELEASE NAMES oprule_parser_vs14_mt.lib PATHS "${DSM2_TOP_DIR}/lib" NO_DEFAULT_PATH)
# input_storage_vs14.lib 
find_library(INPUT_STORAGE_DEBUG input_storage_vs14_mtd.lib PATHS "${DSM2_TOP_DIR}/lib" NO_DEFAULT_PATH)
find_library(INPUT_STORAGE_RELEASE input_storage_vs14_mt.lib PATHS "${DSM2_TOP_DIR}/lib" NO_DEFAULT_PATH)
# sparse klu library
find_library(KLULIB_DEBUG KLU.lib PATHS "${DSM2_THIRD_PARTY_DIR}/SuiteSparse/KLU/build/Debug" NO_DEFAULT_PATH)
find_library(KLULIB_RELEASE KLU.lib PATHS "${DSM2_THIRD_PARTY_DIR}/SuiteSparse/KLU/build/Release" NO_DEFAULT_PATH)
# mkl libraries
find_library(MKL_CORE_LIB NAMES mkl_core.lib HINTS "$ENV{IFORT_COMPILER13}/mkl/lib/ia32")
find_library(MKL_INTEL_LIB NAMES mkl_intel_c.lib HINTS "$ENV{IFORT_COMPILER13}/mkl/lib/ia32")
#standard libraries. no need to specify path as they are linked from default library paths FIXME: find_package
set (CMT_LIB_DEBUG "libcmtd.lib")
set (CMT_LIB_RELEASE "libcmt.lib")
#math library for Intel Composer 2013 set (MMT_LIB "libmmt.lib")
# math library for Intel VF 16  mkl_intel_c.lib mkl_sequential.lib mkl_core.lib
set(INTEL_LIB_PATH "C:/Program Files (x86)/IntelSWTools/compilers_and_libraries/windows/mkl/lib/ia32")
set(MMT_LIB "${INTEL_LIB_PATH}/mkl_intel_c.lib")
set(MMT_SQ_LIB "${INTEL_LIB_PATH}/mkl_sequential.lib")
set(MMT_CORE_LIB "${INTEL_LIB_PATH}/mkl_core.lib")
#-------------add linker flags and excluded and included libraries
if(MSVC)
  set(CMAKE_EXE_LINKER_FLAGS
      "${CMAKE_EXE_LINKER_FLAGS} /NODEFAULTLIB:libc.lib /NODEFAULTLIB:msvcrt.lib /NODEFAULTLIB:libcmt.lib /NODEFAULTLIB:msvcrtd.lib /NODEFAULTLIB:libmmd.lib /MANIFEST:NO")
  set (CMAKE_SHARED_LINKER_FLAGS "${CMAKE_SHARED_LINKER_FLAGS} /NODEFAULTLIB:libc.lib /NODEFAULTLIB:msvcrt.lib /NODEFAULTLIB:libcmt.lib /NODEFAULTLIB:msvcrtd.lib /NODEFAULTLIB:libcpmt.lib /NODEFAULTLIB:libmmd.lib /MANIFEST:NO")
  set (VS_STARTUP_PROJECT hydro)
endif()
include_directories("${DSM2_TOP_DIR}/input_storage/src")
include_directories("${CMAKE_Fortran_MODULE_DIRECTORY}" "../hdf_tidefile" "../fixed")
#------------------------- SUB PROJECTS --------------------------------
add_subdirectory(${DSM2_COMMONS_DIR})
# generate the files now
execute_process(COMMAND "python" "generate.py" WORKING_DIRECTORY ${DSM2_INPUT_STORAGE})
add_subdirectory(${DSM2_INPUT_STORAGE})
add_subdirectory(${DSM2_FIXED_DIR})
# FIXME: specify order of compile using add_dependencies(${DSM2_FIXED_DIR} ${DSM2_INPUT_STORAGE})
add_subdirectory(${DSM2_HDF_TIDEFILE})
add_subdirectory(${DSM2_HYDROLIB})
add_subdirectory(${DSM2_KLU_FORTRAN})
add_subdirectory(${DSM2_OPRULE_INTERFACE})
add_subdirectory(${DSM2_SPARSE})
add_subdirectory(${DSM2_PATTERN_MATCH})
add_subdirectory(${DSM2_TIMEVAR})
#------------------ DEPENDENCIES ---------------------
add_dependencies(oprule_interface_fortran common fixed timevar input_storage hydrolib oprule_interface_cpp pattern_match_cpp input_storage)
add_dependencies(hydrolib common fixed input_storage_cpp input_storage hdf_tidefile_cpp hdf_tidefile_fortran klu_fortran pattern_match_cpp sparse timevar)
add_dependencies(input_storage input_storage_cpp)
add_dependencies(fixed hdf_tidefile_fortran common input_storage timevar)
add_dependencies(timevar common)
add_dependencies(hdf_tidefile_fortran common timevar input_storage hdf_tidefile_cpp)
add_dependencies(klu_fortran common sparse)
add_executable(hydro hydro/fourpt.f)
add_dependencies(hydro common fixed hydrolib hdf_tidefile_cpp hdf_tidefile_fortran input_storage input_storage_cpp oprule_interface_cpp oprule_interface_fortran pattern_match_cpp sparse klu_fortran timevar)
set_property(TARGET hydro PROPERTY LINKER_LANGUAGE Fortran)
target_link_libraries(hydro debug libucrtd.lib optimized libucrt.lib libmmt.lib)
target_link_libraries(hydro common fixed hydrolib hdf_tidefile_cpp hdf_tidefile_fortran input_storage input_storage_cpp oprule_interface_cpp oprule_interface_fortran pattern_match_cpp sparse klu_fortran timevar)
target_link_libraries(hydro 
    debug ${BOOST_FILESYSTEM_DEBUG} optimized ${BOOST_FILESYSTEM_RELEASE}
    debug ${BOOST_REGEX_DEBUG} optimized ${BOOST_REGEX_RELEASE}
    debug ${BOOST_SYSTEM_DEBUG} optimized ${BOOST_SYSTEM_RELEASE}
    ${HDF5} ${HDF5_FORTRAN} ${HDF5_F90CSTUB} ${HDF5_HL} ${HDF5_HL_FORTRAN} ${HDF5_HL_F90CSTUB} ${SZLIB} ${ZLIB}
    debug ${HECLIB_DEBUG} optimized ${HECLIB_RELEASE}
    debug ${OPRULE_DEBUG} optimized ${OPRULE_RELEASE}
    debug ${OPRULE_PARSER_DEBUG} optimized ${OPRULE_PARSER_RELEASE}
    debug ${INPUT_STORAGE_DEBUG} optimized ${INPUT_STORAGE_RELEASE}
    debug ${KLULIB_DEBUG} optimized ${KLULIB_RELEASE}
    ${MMT_LIB} ${MMT_SQ_LIB} ${MMT_CORE_LIB}
    debug ${CMT_LIB_DEBUG} optimized ${CMT_LIB_RELEASE}
    )
# QUAL EXE
file(GLOB QUAL_SOURCES qual/*.f)
set (CMAKE_Fortran_FLAGS "/extend_source:132")
add_executable(qual ${QUAL_SOURCES})
set_property(TARGET qual PROPERTY LINKER_LANGUAGE Fortran)
target_link_libraries(qual debug libucrtd.lib optimized libucrt.lib libmmt.lib)
target_link_libraries(qual common fixed hydrolib hdf_tidefile_cpp hdf_tidefile_fortran input_storage input_storage_cpp oprule_interface_cpp oprule_interface_fortran pattern_match_cpp sparse timevar)
target_link_libraries(qual 
    debug ${BOOST_FILESYSTEM_DEBUG} optimized ${BOOST_FILESYSTEM_RELEASE}
    debug ${BOOST_REGEX_DEBUG} optimized ${BOOST_REGEX_RELEASE}
    debug ${BOOST_SYSTEM_DEBUG} optimized ${BOOST_SYSTEM_RELEASE}
    ${HDF5} ${HDF5_FORTRAN} ${HDF5_F90CSTUB} ${HDF5_HL} ${HDF5_HL_FORTRAN} ${HDF5_HL_F90CSTUB} ${SZLIB} ${ZLIB}
    debug ${HECLIB_DEBUG} optimized ${HECLIB_RELEASE}
    debug ${OPRULE_DEBUG} optimized ${OPRULE_RELEASE}
    debug ${OPRULE_PARSER_DEBUG} optimized ${OPRULE_PARSER_RELEASE}
    debug ${INPUT_STORAGE_DEBUG} optimized ${INPUT_STORAGE_RELEASE}
    ${MMT_LIB} ${MMT_SQ_LIB} ${MMT_CORE_LIB}
    debug ${CMT_LIB_DEBUG} optimized ${CMT_LIB_RELEASE}
    )
# PTM DLL
file(GLOB PTM_FORTRAN_SOURCES ptm/native/*.f)
set (CMAKE_Fortran_FLAGS "/extend_source:132")
add_library(ptm_fortran STATIC ${PTM_FORTRAN_SOURCES})
add_dependencies(ptm_fortran common timevar fixed hydrolib)
file(GLOB PTM_JAVA_CPP_SOURCES ptm/native/*.cpp)
add_library(ptm_dll STATIC ${PTM_JAVA_CPP_SOURCES})
include(FindJNI)
target_include_directories(ptm_dll PUBLIC ${JNI_INCLUDE_DIRS})
set (CMAKE_CXX_STANDARD_LIBRARIES "")
target_link_libraries(ptm_dll ptm_fortran common fixed timevar hydrolib input_storage input_storage_cpp hdf_tidefile_cpp hdf_tidefile_fortran pattern_match_cpp)
add_dependencies(ptm_dll common fixed timevar hydrolib)
target_link_libraries(ptm_dll 
    debug ${BOOST_FILESYSTEM_DEBUG} optimized ${BOOST_FILESYSTEM_RELEASE}
    debug ${BOOST_REGEX_DEBUG} optimized ${BOOST_REGEX_RELEASE}
    debug ${BOOST_SYSTEM_DEBUG} optimized ${BOOST_SYSTEM_RELEASE}
    ${HDF5} ${HDF5_FORTRAN} ${HDF5_F90CSTUB} ${HDF5_HL} ${HDF5_HL_FORTRAN} ${HDF5_HL_F90CSTUB} ${SZLIB} ${ZLIB}
    debug ${HECLIB_DEBUG} optimized ${HECLIB_RELEASE}
    debug ${OPRULE_DEBUG} optimized ${OPRULE_RELEASE}
    debug ${OPRULE_PARSER_DEBUG} optimized ${OPRULE_PARSER_RELEASE}
    debug ${INPUT_STORAGE_DEBUG} optimized ${INPUT_STORAGE_RELEASE}
    ${MMT_LIB} ${MMT_SQ_LIB} ${MMT_CORE_LIB}
    debug ${CMT_LIB_DEBUG} optimized ${CMT_LIB_RELEASE}
    )

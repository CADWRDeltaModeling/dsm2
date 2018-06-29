cmake_minimum_required (VERSION 3.2.2)
PROJECT(GTM)
enable_language(C CXX Fortran)
# 
string(TIMESTAMP CONFIG_DATE "%Y-%m-%d")
#-----------------------------------------------------------------------------
# Define some CMake variables for use later in the project
#-----------------------------------------------------------------------------
set(GTM_GTM       ${GTM_SOURCE_DIR}/gtm)
set(GTM_CORE    ${GTM_SOURCE_DIR}/gtm_core)
set(GTM_INPUT_STORAGE ${GTM_SOURCE_DIR}/input_storage)
set(GTM_PROCESS_IO ${GTM_SOURCE_DIR}/process_io)
set(GTM_TRANSPORT ${GTM_SOURCE_DIR}/transport)
set(GTM_DO_MODULE ${GTM_SOURCE_DIR}/do_module)
set(GTM_SEDIMENT ${GTM_SOURCE_DIR}/sediment)
set(GTM_SEDIMENT_BED ${GTM_SOURCE_DIR}/sediment_bed)
set(GTM_SEDIMENT_BED_SETUP ${GTM_SOURCE_DIR}/sediment_bed_setup)
set(GTM_MERCURY ${GTM_SOURCE_DIR}/mercury)
set(GTM_TOOLS ${GTM_SOURCE_DIR}/tools)
set(GTM_TOOLS_DRIVER ${GTM_SOURCE_DIR}/tools_driver)
#FIXME: change top directory to one-less. There should be a better way
set(DSM2_TOP_DIR "../../")
set(DSM2_THIRD_PARTY_DIR "${DSM2_TOP_DIR}/third_party")

# make sure that the default is a RELEASE
if (NOT CMAKE_BUILD_TYPE)
  set (CMAKE_BUILD_TYPE Debug CACHE STRING
      "Choose the type of build, options are: None Debug Release."
      FORCE)
endif (NOT CMAKE_BUILD_TYPE)
# FFLAGS depend on the compiler
get_filename_component (Fortran_COMPILER_NAME ${CMAKE_Fortran_COMPILER} NAME)
# include the common macros for compiler flags and third party libraries
include(${DSM2_TOP_DIR}CmakeLibraryMacro.txt)
set_compiler_flags()
#---------------LIBRARIES-------------------------------------------------
find_libs()
#----------------- EXECUTABLES --------------------------------------------
if(MSVC)
  set(CMAKE_EXE_LINKER_FLAGS
      "${CMAKE_EXE_LINKER_FLAGS} /NODEFAULTLIB:libc.lib /NODEFAULTLIB:msvcrt.lib /NODEFAULTLIB:libcmt.lib /NODEFAULTLIB:msvcrtd.lib /NODEFAULTLIB:libmmd.lib /MANIFEST:NO")
  set (CMAKE_SHARED_LINKER_FLAGS "${CMAKE_SHARED_LINKER_FLAGS} /NODEFAULTLIB:libc.lib /NODEFAULTLIB:msvcrt.lib /NODEFAULTLIB:libcmt.lib /NODEFAULTLIB:msvcrtd.lib /NODEFAULTLIB:libcpmt.lib /NODEFAULTLIB:libmmd.lib /MANIFEST:NO")
  set (VS_STARTUP_PROJECT gtm)
endif()
include_directories("${DSM2_TOP_DIR}/input_storage/src")
include_directories("${CMAKE_Fortran_MODULE_DIRECTORY}" "../hdf_tidefile" "../fixed")
#------------------------- SUB PROJECTS --------------------------------
add_subdirectory(${GTM_CORE})
# generate the files now
execute_process(COMMAND "python" "generate_gtm.py" WORKING_DIRECTORY ${GTM_INPUT_STORAGE})
add_subdirectory(${GTM_INPUT_STORAGE})
add_subdirectory(${GTM_PROCESS_IO})
add_subdirectory(${GTM_SEDIMENT})
add_subdirectory(${GTM_SEDIMENT_BED})
add_subdirectory(${GTM_SEDIMENT_BED_SETUP})
add_subdirectory(${GTM_DO_MODULE})
add_subdirectory(${GTM_MERCURY})
add_subdirectory(${GTM_TOOLS})
add_subdirectory(${GTM_TRANSPORT})
# Add gtm executable
file(GLOB GTM_SOURCES ${GTM_GTM}/*.f90)
add_executable(gtm ${GTM_SOURCES})
set_property(TARGET gtm PROPERTY LINKER_LANGUAGE Fortran)
#------------------ DEPENDENCIES ---------------------
add_dependencies(gtm gtm_core input_storage input_storage_cpp mercury process_io sediment sediment_bed sediment_bed_setup transport)
add_dependencies(do_module gtm_core process_io transport)
add_dependencies(input_storage input_storage_cpp)
add_dependencies(mercury gtm_core sediment_bed)
add_dependencies(process_io gtm_core input_storage input_storage_cpp)
add_dependencies(sediment gtm_core)
add_dependencies(sediment_bed gtm_core input_storage process_io)
add_dependencies(sediment_bed_setup gtm_core process_io)
add_dependencies(transport gtm_core)
set_target_properties(gtm PROPERTIES LINK_FLAGS "/STACK:8000000")
target_link_libraries(gtm debug libucrtd.lib optimized libucrt.lib libmmt.lib)
target_link_libraries(gtm gtm_core transport input_storage input_storage_cpp process_io sediment sediment_bed sediment_bed_setup do_module mercury)
target_link_libraries(gtm 
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
    debug ${CPMT_LIB_DEBUG} optimized ${CPMT_LIB_RELEASE}
    )

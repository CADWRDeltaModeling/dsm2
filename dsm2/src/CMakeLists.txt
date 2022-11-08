cmake_minimum_required (VERSION 3.2.2)
############ options
option(hydro_1000 "Enable running hydro at 1000 ft resolution: Default OFF" OFF)
#add_compile_options(-fpp)
############ GIT information
# in case Git is not available, we default to "unknown"
set(GIT_HASH "unknown")

# find Git and if available set GIT_HASH variable
find_package(Git QUIET)
if(GIT_FOUND)
  execute_process(
    COMMAND ${GIT_EXECUTABLE} log -1 --pretty=format:%h
    OUTPUT_VARIABLE GIT_HASH
    OUTPUT_STRIP_TRAILING_WHITESPACE
    ERROR_QUIET
    )
endif()

message(STATUS "Git hash is ${GIT_HASH}")
#############
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
# set (DSM2_TIMEVAR      ${DSM2_SOURCE_DIR}/timevar)
set (DSM2_PYTHON      ${DSM2_SOURCE_DIR}/python)
#
#FIXME: change top directory to one-less. There should be a better way
set(DSM2_TOP_DIR "../..")
if (DEFINED THIRD_PARTY_DIR)
  set(DSM2_THIRD_PARTY_DIR "${THIRD_PARTY_DIR}")
else()
  set(DSM2_THIRD_PARTY_DIR "${DSM2_TOP_DIR}/third_party")
endif()

# FFLAGS depend on the compiler
get_filename_component (Fortran_COMPILER_NAME ${CMAKE_Fortran_COMPILER} NAME)
# include the common macros for compiler flags and third party libraries
include(${DSM2_TOP_DIR}/CmakeLibraryMacro.txt)
set_compiler_flags()
#---------------LIBRARIES-------------------------------------------------
find_libs()
#----------------- EXECUTABLES --------------------------------------------
if(MSVC)
  #set(CMAKE_EXE_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS} /NODEFAULTLIB:msvcrt.lib /NODEFAULTLIB:libmmd.lib /NODEFAULTLIB:libifcoremd.lib /MANIFEST:NO")
  #set (CMAKE_SHARED_LINKER_FLAGS "${CMAKE_SHARED_LINKER_FLAGS} /NODEFAULTLIB:libifcoremd.lib /NODEFAULTLIB:msvcrt.lib /NODEFAULTLIB:libmmd.lib /NODEFAULTLIB:mscvcprt.lib /NODEFAULTLIB:libucrtd.lib /MANIFEST:NO")
  set(CMAKE_EXE_LINKER_FLAGS_DEBUG
      "${CMAKE_EXE_LINKER_FLAGS_DEBUG} /NODEFAULTLIB:libifcoremd.lib /NODEFAULTLIB:libc.lib /NODEFAULTLIB:msvcrt.lib /NODEFAULTLIB:libcmt.lib /NODEFAULTLIB:msvcrtd.lib /NODEFAULTLIB:libmmd.lib  /MANIFEST:NO")
  set(CMAKE_EXE_LINKER_FLAGS_RELEASE
      "${CMAKE_EXE_LINKER_FLAGS_RELEASE} /NODEFAULTLIB:libifcoremd.lib /NODEFAULTLIB:libc.lib /NODEFAULTLIB:msvcrt.lib /NODEFAULTLIB:libcmtd.lib /NODEFAULTLIB:msvcrtd.lib /NODEFAULTLIB:libmmd.lib /MANIFEST:NO")
  set(CMAKE_SHARED_LINKER_FLAGS_DEBUG "${CMAKE_SHARED_LINKER_FLAGS_DEBUG} /NODEFAULTLIB:libifcoremd.lib /NODEFAULTLIB:libc.lib /NODEFAULTLIB:msvcrt.lib /NODEFAULTLIB:libcmt.lib /NODEFAULTLIB:msvcrtd.lib /NODEFAULTLIB:libmmd.lib /MANIFEST:NO")
  set(CMAKE_SHARED_LINKER_FLAGS_RELEASE "${CMAKE_SHARED_LINKER_FLAGS_RELEASE} /NODEFAULTLIB:libifcoremd.lib /NODEFAULTLIB:libc.lib /NODEFAULTLIB:msvcrt.lib /NODEFAULTLIB:libcmtd.lib /NODEFAULTLIB:msvcrtd.lib  /NODEFAULTLIB:libmmd.lib /MANIFEST:NO")
  set (VS_STARTUP_PROJECT hydro)
else(MSVC)
  #set(CMAKE_EXE_LINKER_FLAGS "-nodefaultlibs")
  #set(CMAKE_SHARED_LINKER_FLAGS "-nodefaultlibs")
endif()
include_directories("${DSM2_TOP_DIR}/input_storage/src")
include_directories("${CMAKE_Fortran_MODULE_DIRECTORY}" "../hdf_tidefile" "../fixed")
#------------------------- SUB PROJECTS --------------------------------
add_subdirectory(${DSM2_COMMONS_DIR})
# generate the files now
execute_process(COMMAND "python" "generate.py" WORKING_DIRECTORY ${DSM2_INPUT_STORAGE})
add_subdirectory(${DSM2_INPUT_STORAGE})
add_subdirectory(${DSM2_FIXED_DIR})
add_subdirectory(${DSM2_HDF_TIDEFILE})
add_subdirectory(${DSM2_HYDRO})
add_subdirectory(${DSM2_HYDROLIB})
add_subdirectory(${DSM2_KLU_FORTRAN})
add_subdirectory(${DSM2_OPRULE_INTERFACE})
add_subdirectory(${DSM2_SPARSE})
add_subdirectory(${DSM2_PATTERN_MATCH})
# add_subdirectory(${DSM2_TIMEVAR})
add_subdirectory(${DSM2_QUAL})
add_subdirectory(${DSM2_PYTHON})
#--------------- HYDRO EXE -------------------------
add_executable(hydro_exe hydro/hydro_driver.f90)
include_directories("${CMAKE_Fortran_MODULE_DIRECTORY}" "../hdf_tidefile" "../fixed")
add_dependencies(hydro_exe fourpt)
if (WIN32)
else (WIN32)
set(CMAKE_Fortran_MODULE_DIRECTORY ${CMAKE_BINARY_DIR}/modules)
endif (WIN32)
set_property(TARGET hydro_exe PROPERTY LINKER_LANGUAGE Fortran)
if (WIN32)
set_target_properties(hydro_exe PROPERTIES COMPILE_FLAGS "/Qmkl:sequential") #linker in windows in plain vanilla and doesn't understand ifort flags
#set_target_properties(hydro_exe PROPERTIES LINK_FLAGS "/libs:static /threads")
target_link_libraries(hydro_exe DEBUG libucrtd.lib OPTIMIZED libucrt.lib libmmt.lib)
target_link_libraries(hydro_exe fourpt common fixed hydrolib hdf_tidefile_cpp hdf_tidefile_fortran input_storage input_storage_cpp oprule_interface_cpp oprule_interface_fortran pattern_match_cpp sparse klu_fortran)
else (WIN32)
# link statically with intel libraries - avoids intel library redistribution
# full static is not working on linux due to lack of standard .a libraries
set_target_properties(hydro_exe PROPERTIES LINK_FLAGS "-qmkl=sequential -static-intel")
target_include_directories(hydro_exe PUBLIC "${CMAKE_Fortran_MODULE_DIRECTORY}")
target_link_libraries(hydro_exe -Wl,-rpath='$ORIGIN/lib' -Wl,--start-group fourpt gtm_core common fixed hydrolib hdf_tidefile_cpp hdf_tidefile_fortran input_storage input_storage_cpp oprule_interface_cpp oprule_interface_fortran pattern_match_cpp sparse klu_fortran -Wl,--end-group)
endif (WIN32)
if (WIN32)
target_link_libraries(hydro_exe
    debug ${BOOST_FILESYSTEM_DEBUG} optimized ${BOOST_FILESYSTEM_RELEASE}
    debug ${BOOST_REGEX_DEBUG} optimized ${BOOST_REGEX_RELEASE}
    debug ${BOOST_SYSTEM_DEBUG} optimized ${BOOST_SYSTEM_RELEASE}
    ${HDF5} ${HDF5_FORTRAN} ${HDF5_F90CSTUB} ${HDF5_HL} ${HDF5_HL_FORTRAN} ${HDF5_HL_F90CSTUB} ${SZLIB} ${ZLIB}
    debug ${HECLIB_DEBUG} optimized ${HECLIB_RELEASE}
    debug ${OPRULE_DEBUG} optimized ${OPRULE_RELEASE}
    debug ${OPRULE_PARSER_DEBUG} optimized ${OPRULE_PARSER_RELEASE}
    debug ${INPUT_STORAGE_DEBUG} optimized ${INPUT_STORAGE_RELEASE}
    debug ${KLULIB_DEBUG} optimized ${KLULIB_RELEASE}
	  ${IFCORE_LIB}
    ${MMT_LIB} ${MMT_SQ_LIB} ${MMT_CORE_LIB}
    debug ${CMT_LIB_DEBUG} optimized ${CMT_LIB_RELEASE}
    )
else (WIN32)
target_link_libraries(hydro_exe
    debug ${BOOST_FILESYSTEM_DEBUG} optimized ${BOOST_FILESYSTEM_RELEASE}
    debug ${BOOST_REGEX_DEBUG} optimized ${BOOST_REGEX_RELEASE}
    debug ${BOOST_SYSTEM_DEBUG} optimized ${BOOST_SYSTEM_RELEASE}
    -Wl,--start-group ${HDF5} ${HDF5_FORTRAN} ${HDF5_F90CSTUB} ${HDF5_HL} ${HDF5_HL_FORTRAN} ${HDF5_HL_F90CSTUB} ${SZLIB} ${ZLIB} -Wl,--end-group
    debug ${HECLIB_DEBUG} optimized ${HECLIB_RELEASE}
    debug ${OPRULE_PARSER_DEBUG} optimized ${OPRULE_PARSER_RELEASE}
    debug ${OPRULE_DEBUG} optimized ${OPRULE_RELEASE}
    debug ${INPUT_STORAGE_DEBUG} optimized ${INPUT_STORAGE_RELEASE}
    debug ${KLULIB_DEBUG} optimized ${KLULIB_RELEASE}
    )
target_link_libraries(hydro_exe -lc -lintlc -lstdc++ -lm -lgcc_s -ldl -lcilkrts)
endif (WIN32)
set_target_properties(common PROPERTIES Fortran_PREPROCESS on)
set_target_properties(hydrolib PROPERTIES Fortran_PREPROCESS on)
if (hydro_1000)
    message("hydro_1000 is on: ${hydro_1000} --> hydro_exe will run at 1000 ft spatial resolution")
    target_compile_definitions(common PRIVATE hydro_1000=1)
    target_compile_definitions(hydrolib PRIVATE hydro_1000=1)
else (hydro_1000)
    message("hydro_1000 is on: ${hydro_1000} --> hydro_exe will run at 2000 ft spatial resolution")
endif (hydro_1000)
# QUAL EXE
# file(GLOB QUAL_SOURCES qual/*.f qual/*.f90)
# if (WIN32)
#    set (CMAKE_Fortran_FLAGS "/extend_source:132")
# else (WIN32)
#    set (CMAKE_Fortran_FLAGS "-extend-source 132")
# endif (WIN32)
# add_executable(qual ${QUAL_SOURCES})
add_executable(qual_exe qual/qual_driver.f90)
set_property(TARGET qual_exe PROPERTY LINKER_LANGUAGE Fortran)
add_dependencies(qual_exe qual_core)
if (WIN32)
target_link_libraries(qual_exe debug libucrtd.lib optimized libucrt.lib libmmt.lib)
target_link_libraries(qual_exe qual_core gtm_core common fixed hydrolib hdf_tidefile_cpp hdf_tidefile_fortran input_storage input_storage_cpp oprule_interface_cpp oprule_interface_fortran pattern_match_cpp sparse)
target_link_libraries(qual_exe
    debug ${BOOST_FILESYSTEM_DEBUG} optimized ${BOOST_FILESYSTEM_RELEASE}
    debug ${BOOST_REGEX_DEBUG} optimized ${BOOST_REGEX_RELEASE}
    debug ${BOOST_SYSTEM_DEBUG} optimized ${BOOST_SYSTEM_RELEASE}
    ${HDF5} ${HDF5_FORTRAN} ${HDF5_F90CSTUB} ${HDF5_HL} ${HDF5_HL_FORTRAN} ${HDF5_HL_F90CSTUB} ${SZLIB} ${ZLIB}
    debug ${HECLIB_DEBUG} optimized ${HECLIB_RELEASE}
    debug ${OPRULE_DEBUG} optimized ${OPRULE_RELEASE}
    debug ${OPRULE_PARSER_DEBUG} optimized ${OPRULE_PARSER_RELEASE}
    debug ${INPUT_STORAGE_DEBUG} optimized ${INPUT_STORAGE_RELEASE}
	  ${IFCORE_LIB}
    ${MMT_LIB} ${MMT_SQ_LIB} ${MMT_CORE_LIB}
    debug ${CMT_LIB_DEBUG} optimized ${CMT_LIB_RELEASE}
    )
else (WIN32)
set_target_properties(qual_exe PROPERTIES LINK_FLAGS "-qmkl=sequential -static-intel")
target_include_directories(qual_exe PUBLIC "${CMAKE_Fortran_MODULE_DIRECTORY}")
target_link_libraries(qual_exe -Wl,-rpath='$ORIGIN/lib' -Wl,--start-group qual_core gtm_core common fixed hydrolib hdf_tidefile_cpp hdf_tidefile_fortran input_storage input_storage_cpp oprule_interface_cpp oprule_interface_fortran pattern_match_cpp sparse -Wl,--end-group)
target_link_libraries(qual_exe
    debug ${BOOST_FILESYSTEM_DEBUG} optimized ${BOOST_FILESYSTEM_RELEASE}
    debug ${BOOST_REGEX_DEBUG} optimized ${BOOST_REGEX_RELEASE}
    debug ${BOOST_SYSTEM_DEBUG} optimized ${BOOST_SYSTEM_RELEASE}
    -Wl,--start-group ${HDF5} ${HDF5_FORTRAN} ${HDF5_F90CSTUB} ${HDF5_HL} ${HDF5_HL_FORTRAN} ${HDF5_HL_F90CSTUB} ${SZLIB} ${ZLIB} -Wl,--end-group
    debug ${HECLIB_DEBUG} optimized ${HECLIB_RELEASE}
    debug ${OPRULE_DEBUG} optimized ${OPRULE_RELEASE}
    debug ${OPRULE_PARSER_DEBUG} optimized ${OPRULE_PARSER_RELEASE}
    debug ${INPUT_STORAGE_DEBUG} optimized ${INPUT_STORAGE_RELEASE}
    )
target_link_libraries(qual_exe -lc -lintlc -lstdc++ -lm -lgcc_s -ldl -lcilkrts)
endif (WIN32)
# PTM DLL
file(GLOB PTM_FORTRAN_SOURCES ptm/native/*.f)
if (WIN32)
   set (CMAKE_Fortran_FLAGS "/extend_source:132")
else (WIN32)
   set (CMAKE_Fortran_FLAGS "-extend-source 132")
endif (WIN32)
add_library(ptm_fortran STATIC ${PTM_FORTRAN_SOURCES})
add_dependencies(ptm_fortran common fixed hydrolib)
file(GLOB PTM_JAVA_CPP_SOURCES ptm/native/*.cpp)
add_library(PTM SHARED ${PTM_JAVA_CPP_SOURCES})
include(FindJNI)
target_include_directories(PTM PUBLIC ${JNI_INCLUDE_DIRS})
if (WIN32)
target_link_libraries(PTM ptm_fortran common fixed hydrolib input_storage input_storage_cpp hdf_tidefile_cpp hdf_tidefile_fortran pattern_match_cpp)
add_dependencies(PTM common fixed hydrolib)
target_link_libraries(PTM
    debug ${BOOST_FILESYSTEM_DEBUG} optimized ${BOOST_FILESYSTEM_RELEASE}
    debug ${BOOST_REGEX_DEBUG} optimized ${BOOST_REGEX_RELEASE}
    debug ${BOOST_SYSTEM_DEBUG} optimized ${BOOST_SYSTEM_RELEASE}
    ${HDF5} ${HDF5_FORTRAN} ${HDF5_F90CSTUB} ${HDF5_HL} ${HDF5_HL_FORTRAN} ${HDF5_HL_F90CSTUB} ${SZLIB} ${ZLIB}
    debug ${HECLIB_DEBUG} optimized ${HECLIB_RELEASE}
    debug ${OPRULE_DEBUG} optimized ${OPRULE_RELEASE}
    debug ${OPRULE_PARSER_DEBUG} optimized ${OPRULE_PARSER_RELEASE}
    debug ${INPUT_STORAGE_DEBUG} optimized ${INPUT_STORAGE_RELEASE}
	  ${IFCORE_LIB}
    ${MMT_LIB} ${MMT_SQ_LIB} ${MMT_CORE_LIB}
    debug ${CMT_LIB_DEBUG} optimized ${CMT_LIB_RELEASE}
	  debug libcpmtd.lib optimized libcpmt.lib
    debug libucrtd.lib optimized libucrt.lib # explicit linking required to avoid conflict with _powf function in libmmt.lib (Intel)
    )
else (WIN32)
set_target_properties(PTM PROPERTIES LINK_FLAGS "-qmkl=sequential -static-intel")
target_link_libraries(PTM ptm_fortran -Wl,-rpath='$ORIGIN/lib' -Wl,--whole-archive common fixed input_storage input_storage_cpp hdf_tidefile_cpp hdf_tidefile_fortran pattern_match_cpp -Wl,--no-whole-archive hydrolib)
target_link_libraries(PTM
    debug ${BOOST_FILESYSTEM_DEBUG} optimized ${BOOST_FILESYSTEM_RELEASE}
    debug ${BOOST_REGEX_DEBUG} optimized ${BOOST_REGEX_RELEASE}
    debug ${BOOST_SYSTEM_DEBUG} optimized ${BOOST_SYSTEM_RELEASE}
    -Wl,--start-group ${HDF5} ${HDF5_FORTRAN} ${HDF5_F90CSTUB} ${HDF5_HL} ${HDF5_HL_FORTRAN} ${HDF5_HL_F90CSTUB} ${SZLIB} ${ZLIB} -Wl,--end-group
    debug ${HECLIB_DEBUG} optimized ${HECLIB_RELEASE}
    debug ${OPRULE_DEBUG} optimized ${OPRULE_RELEASE}
    debug ${OPRULE_PARSER_DEBUG} optimized ${OPRULE_PARSER_RELEASE}
    debug ${INPUT_STORAGE_DEBUG} optimized ${INPUT_STORAGE_RELEASE}
    )
endif (WIN32)
#PTM java
find_package(Java 1.6 REQUIRED)
message(STATUS "Found Java version ${Java_VERSION}")
get_filename_component(Java_JAVA_BIN_DIR ${Java_JAVA_EXECUTABLE} DIRECTORY)
file(TO_CMAKE_PATH $ENV{JAVA_HOME}/jre JAVA_JRE_HOME)
message(STATUS "Using Java env variable $ENV{JAVA_HOME} location to guess JAVA_JRE_HOME is -> ${JAVA_JRE_HOME}")
include(UseJava)
file(GLOB PTM_JAVA_SOURCES ptm/DWR/DMS/PTM/*.java ptm/DWR/DMS/PTM/behave/*.java)
file(GLOB PTM_JAVA_DEP_JARS ptm/lib/*.jar)
set(CMAKE_JAVA_COMPILE_FLAGS "-source" "1.6" "-target" "1.6")
add_jar(PTM_JAR
  SOURCES ${PTM_JAVA_SOURCES}
  INCLUDE_JARS ${PTM_JAVA_DEP_JARS}
  ENTRY_POINT "DWR/DMS/PTM/MainPTM"
  OUTPUT_NAME PTM)
install_jar(PTM_JAR ${CMAKE_BINARY_DIR})
#--------------- GTM EXE -------------------------
#-----------------------------------------------------------------------------
# Define some CMake variables for use later in the project
#-----------------------------------------------------------------------------
set(GTM_GTM       ${DSM2_SOURCE_DIR}/gtm)
set(GTM_GTM_CORE    ${DSM2_SOURCE_DIR}/gtm_core)
set(GTM_PROCESS_IO ${DSM2_SOURCE_DIR}/process_io)
set(GTM_TRANSPORT ${DSM2_SOURCE_DIR}/transport)
set(GTM_DO_MODULE ${DSM2_SOURCE_DIR}/do_module)
set(GTM_SEDIMENT ${DSM2_SOURCE_DIR}/sediment)
set(GTM_SEDIMENT_BED_CORE ${DSM2_SOURCE_DIR}/sediment_bed_core)
set(GTM_SEDIMENT_BED ${DSM2_SOURCE_DIR}/sediment_bed)
set(GTM_SEDIMENT_BED_SETUP ${DSM2_SOURCE_DIR}/sediment_bed_setup)
set(GTM_MERCURY ${DSM2_SOURCE_DIR}/mercury)
set(GTM_TOOLS ${DSM2_SOURCE_DIR}/tools)
set(GTM_TOOLS_DRIVER ${DSM2_SOURCE_DIR}/tools_driver)
#------------------------- SUB PROJECTS --------------------------------
add_subdirectory(${GTM_GTM})
add_subdirectory(${GTM_GTM_CORE})
add_subdirectory(${GTM_PROCESS_IO})
add_subdirectory(${GTM_SEDIMENT})
add_subdirectory(${GTM_SEDIMENT_BED_CORE})
add_subdirectory(${GTM_SEDIMENT_BED})
add_subdirectory(${GTM_DO_MODULE})
add_subdirectory(${GTM_MERCURY})
add_subdirectory(${GTM_TOOLS})
add_subdirectory(${GTM_TRANSPORT})
# Add gtm executable
file(GLOB GTM_SOURCES ${GTM_GTM}/*.f90)
add_executable(gtm_exe gtm/gtm_driver.f90)
set_property(TARGET gtm_exe PROPERTY LINKER_LANGUAGE Fortran)
add_dependencies(gtm_exe gtm_main)

# Add sediment_bed_setup executable
file(GLOB SEDIMENT_BED_SETUP_SOURCES ${GTM_SEDIMENT_BED_SETUP}/*.f90)
add_executable(sed_bed_init ${SEDIMENT_BED_SETUP_SOURCES})
set_property(TARGET sed_bed_init PROPERTY LINKER_LANGUAGE Fortran)
#------------------ DEPENDENCIES ---------------------
target_link_libraries(gtm_exe mercury gtm_main gtm_core common transport input_storage input_storage_cpp process_io sediment sediment_bed sediment_bed_core do_module)
if (WIN32)
set_target_properties(gtm_exe PROPERTIES LINK_FLAGS "/STACK:8000000")
target_link_libraries(gtm_exe
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
    debug libcpmtd.lib optimized libcpmt.lib
    debug libucrtd.lib optimized libucrt.lib # explicit linking required to avoid conflict with _powf function in libmmt.lib (Intel)
    )
else (WIN32)
message("Building on Linux with no stack size directive. Use ulimit -s to specify stack size")
set_target_properties(gtm_exe PROPERTIES LINK_FLAGS "-qmkl=sequential -static-intel")
target_include_directories(gtm_exe PUBLIC "${CMAKE_Fortran_MODULE_DIRECTORY}")
target_link_libraries(gtm_exe -Wl,-rpath='$ORIGIN/lib' -Wl,--start-group gtm_main gtm_core transport input_storage input_storage_cpp process_io sediment sediment_bed do_module mercury -Wl,--end-group)
target_link_libraries(gtm_exe
    debug ${OPRULE_DEBUG} optimized ${OPRULE_RELEASE}
    debug ${OPRULE_PARSER_DEBUG} optimized ${OPRULE_PARSER_RELEASE}
    debug ${INPUT_STORAGE_DEBUG} optimized ${INPUT_STORAGE_RELEASE}
    debug ${BOOST_FILESYSTEM_DEBUG} optimized ${BOOST_FILESYSTEM_RELEASE}
    debug ${BOOST_REGEX_DEBUG} optimized ${BOOST_REGEX_RELEASE}
    debug ${BOOST_SYSTEM_DEBUG} optimized ${BOOST_SYSTEM_RELEASE}
    -Wl,--start-group ${HDF5} ${HDF5_FORTRAN} ${HDF5_F90CSTUB} ${HDF5_HL} ${HDF5_HL_FORTRAN} ${HDF5_HL_F90CSTUB} ${SZLIB} ${ZLIB} -Wl,--end-group
    debug ${HECLIB_DEBUG} optimized ${HECLIB_RELEASE}
    )
target_link_libraries(gtm_exe -lc -lintlc -lstdc++ -lm -lgcc_s -ldl -lcilkrts)
endif (WIN32)
if (WIN32)
   set (CMAKE_Fortran_FLAGS "/extend_source:132")
else (WIN32)
   set (CMAKE_Fortran_FLAGS "-extend-source 132")
endif (WIN32)

target_link_libraries(sed_bed_init common process_io)
set_target_properties(sed_bed_init PROPERTIES LINK_FLAGS "/STACK:8000000")
if (WIN32)
target_link_libraries(sed_bed_init
    ${HDF5} ${HDF5_FORTRAN} ${HDF5_F90CSTUB} ${HDF5_HL} ${HDF5_HL_FORTRAN} ${HDF5_HL_F90CSTUB} ${SZLIB} ${ZLIB}
    debug ${HECLIB_DEBUG} optimized ${HECLIB_RELEASE}
    debug libcpmtd.lib optimized libcpmt.lib
    debug libucrtd.lib optimized libucrt.lib # explicit linking required to avoid conflict with _powf function in libmmt.lib (Intel)
    )
else (WIN32)
message("Building on Linux with no stack size directive. Use ulimit -s to specify stack size")
set_target_properties(sed_bed_init PROPERTIES LINK_FLAGS "-qmkl=sequential -static-intel")
target_include_directories(sed_bed_init PUBLIC "${CMAKE_Fortran_MODULE_DIRECTORY}")
target_link_libraries(sed_bed_init -Wl,-rpath='$ORIGIN/lib' -Wl,--start-group gtm_core process_io -Wl,--end-group)
target_link_libraries(sed_bed_init
    -Wl,--start-group ${HDF5} ${HDF5_FORTRAN} ${HDF5_F90CSTUB} ${HDF5_HL} ${HDF5_HL_FORTRAN} ${HDF5_HL_F90CSTUB} ${SZLIB} ${ZLIB} -Wl,--end-group
    debug ${HECLIB_DEBUG} optimized ${HECLIB_RELEASE}
    )
target_link_libraries(sed_bed_init -lc -lintlc -lstdc++ -lm -lgcc_s -ldl -lcilkrts)
endif (WIN32)
set_target_properties(gtm_core PROPERTIES Fortran_PREPROCESS on)
if (hydro_1000)
    message("hydro_1000 is on: ${hydro_1000} --> hydro_exe will run at 1000 ft spatial resolution")
    target_compile_definitions(gtm_core PRIVATE hydro_1000=1)
else (hydro_1000)
    message("hydro_1000 is on: ${hydro_1000} --> hydro_exe will run at 2000 ft spatial resolution")
endif (hydro_1000)
#-------------- TEST -------------------#

#-------------- INSTALL ----------------#
set(CMAKE_INSTALL_PREFIX "${CMAKE_BINARY_DIR}/install") # will setting it this way override command line directives?
install(TARGETS hydro_exe qual_exe gtm_exe PTM sed_bed_init
  RUNTIME DESTINATION bin
  LIBRARY DESTINATION bin)
install(FILES ${CMAKE_BINARY_DIR}/PTM.jar DESTINATION bin)
message("Copy jar files over to bin/lib")
install(FILES ${PTM_JAVA_DEP_JARS} DESTINATION bin/lib)
install(DIRECTORY ${JAVA_JRE_HOME} DESTINATION bin USE_SOURCE_PERMISSIONS)
if (WIN32)
  install(PROGRAMS ptm/bin/ptm.bat DESTINATION bin)
  message("Windows exes are static so Intel redistribution files are not needed.")
else (WIN32)
  install(PROGRAMS ptm/bin/ptm DESTINATION bin)
  message("Copy .so dependencies over to install directory. Currently for only 32 bit linux libraries!!!")
  message("compiler path: ${CMAKE_C_COMPILER}")
  get_filename_component(COMPILER_DIR ${CMAKE_C_COMPILER} DIRECTORY)
  set(INTEL_DIR "${COMPILER_DIR}/../../compiler/lib/ia32_lin")
  set(RUNTIME_DEPENDENCIES
    ${INTEL_DIR}/libintlc.so.5 ${INTEL_DIR}/libcilkrts.so.5 ${INTEL_DIR}/libirng.so
    ${INTEL_DIR}/libifport.so.5 ${INTEL_DIR}/libifcoremt.so.5 ${INTEL_DIR}/libsvml.so ${INTEL_DIR}/libimf.so)
  install(FILES ${RUNTIME_DEPENDENCIES} DESTINATION bin/lib)
endif (WIN32)
#------------- PACKAGE WINDOWS -----------------$
if (WIN32)
  set(CPACK_GENERATOR "ZIP")
else (WIN32)
  set(CPACK_GENERATOR "TGZ")
endif (WIN32)
#-------------- PACKAGE --------------------------#
set(CPACK_PACKAGE_NAME "DSM2")
set(CPACK_PACKAGE_VENDOR "Delta Modeling Section")
set(CPACK_PACKAGE_DESCRIPTION_SUMMARY "Delta Simulation Model 2")
set(CPACK_PACKAGE_VERSION_MAJOR "8")
set(CPACK_PACKAGE_VERSION_MINOR "2")
set(CPACK_PACKAGE_VERSION_PATCH "${GIT_HASH}")
set(CPACK_PACKAGE_VERSION "${CPACK_PACKAGE_VERSION_MAJOR}.${CPACK_PACKAGE_VERSION_MINOR}.${CPACK_PACKAGE_VERSION_PATCH}")
get_filename_component(LICENSE_FILE "${DSM2_TOP_DIR}/LICENSE.txt" ABSOLUTE)
set(CPACK_RESOURCE_FILE_LICENSE ${LICENSE_FILE})
set(CPACK_PACKAGE_INSTALL_DIRECTORY "CPack")
include(CPack)




#if(CMAKE_Fortran_COMPILER MATCHES ifort)
    #add_compile_options(-fpp)
#    target_compile_options(DSM2 PUBLIC -fpp)
#    target_compile_options(DSM2 PUBLIC -fpp)
#endif()
#set_source_files_properties(
#    PROPERTIES Fortran_PREPROCESS ON)

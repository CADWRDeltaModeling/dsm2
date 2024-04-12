include(${CMAKE_ROOT}/Modules/SelectLibraryConfigurations.cmake)
include(${CMAKE_ROOT}/Modules/FindPackageHandleStandardArgs.cmake)

set(INTELRUNTIME_FOUND OFF)

if(NOT INTELRUNTIME_LIBRARIES)

  if(CMAKE_C_COMPILER_ID STREQUAL "Intel"
     OR CMAKE_Fortran_COMPILER_ID STREQUAL "Intel"
     OR CMAKE_C_COMPILER_ID STREQUAL "IntelLLVM"
     OR CMAKE_Fortran_COMPILER_ID STREQUAL "IntelLLVM")
    set(INTEL_COMPILER ON)
  else()
    if(CMAKE_C_COMPILER_ID STREQUAL "GNU")
      set(GNU_C_COMPILER ON)
    endif()
    if(CMAKE_Fortran_COMPILER_ID STREQUAL "GNU")
      set(GNU_Fortran_COMPILER ON)
    endif()
  endif()

  # Extensions
  if(UNIX)
    set(LIB_PREFIX "lib")
    set(LIB_EXT ".a")
    set(DLL_EXT ".so")
    if(APPLE)
      set(DLL_EXT ".dylib")
    endif()
    set(LINK_PREFIX "-l")
    set(LINK_SUFFIX "")
  else()
    set(LIB_PREFIX "")
    set(LIB_EXT ".lib")
    set(DLL_EXT "_dll.lib")
    set(LINK_PREFIX "")
    set(LINK_SUFFIX ".lib")
  endif()

  if(NOT DEFINED CMPLR_ROOT)
    if(DEFINED ENV{CMPLR_ROOT})
      set(CMPLR_ROOT $ENV{CMPLR_ROOT})
    else()
      message(FATAL_ERROR "CMPLR_ROOT is not defined")
    endif()
  endif()

  list(APPEND INTELRUNTIME_LIBS "irng" "intlc")

  # Find the Intel runtime libraries
  foreach(lib ${INTELRUNTIME_LIBS})
    unset(${lib}_file CACHE)
    find_library(
      ${lib}_file
      NAMES ${LIB_PREFIX}${lib}${DLL_EXT} ${lib}
      HINTS ${CMPLR_ROOT}
      PATH_SUFFIXES
        "lib"
        "lib/${INTEL_ARCH}"
        "lib/${INTEL_ARCH}_lin"
        "lib/${INTEL_ARCH}_win"
        "linux/compiler/lib/${INTEL_ARCH}"
        "linux/compiler/lib/${INTEL_ARCH}_lin"
        "windows/compiler/lib/${INTEL_ARCH}"
        "windows/compiler/lib/${INTEL_ARCH}_win"
        "../compiler/lib/${INTEL_ARCH}_lin"
        "../compiler/lib/${INTEL_ARCH}_win"
        "../compiler/lib/${INTEL_ARCH}"
        "../compiler/lib"
        "../../compiler/latest/linux/compiler/lib/${INTEL_ARCH}"
        "../../compiler/latest/linux/compiler/lib/${INTEL_ARCH}_lin"
        "../../compiler/latest/windows/compiler/lib/${INTEL_ARCH}"
        "../../compiler/latest/windows/compiler/lib/${INTEL_ARCH}_win"
        "../../compiler/latest/mac/compiler/lib")
    list(APPEND INTELRUNTIME_LIBRARIES ${${lib}_file})
    get_filename_component(${lib}_dir "${${lib}_file}" DIRECTORY)
    set(INTELRUNTIME_DIR "${${lib}_dir}")
  endforeach()
  if(INTELRUNTIME_LIBRARIES)
    if(NOT TARGET INTELRUNTIME::INTELRUNTIME)
      add_library(INTELRUNTIME::INTELRUNTIME INTERFACE IMPORTED GLOBAL)
      target_link_libraries(INTELRUNTIME::INTELRUNTIME
                            INTERFACE ${INTELRUNTIME_LIBRARIES})
    endif()
  else()
    message(FATAL_ERROR "Intel runtime libraries not found")
  endif()

endif() # NOT INTELRUNTIME_LIBRARIES

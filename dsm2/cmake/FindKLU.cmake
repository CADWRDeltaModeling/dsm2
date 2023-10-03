include(${CMAKE_ROOT}/Modules/SelectLibraryConfigurations.cmake)
include(${CMAKE_ROOT}/Modules/FindPackageHandleStandardArgs.cmake)

set(KLU_FOUND OFF)

if(NOT KLU_ROOT)
  set(KLU_ROOT $ENV{KLU_ROOT})
endif()
if(KLU_ROOT)
  set(_KLU_SEARCH_OPTS NO_DEFAULT_PATH)
else()
  set(_KLU_SEARCH_OPTS)
endif()

if(WIN32)
  set(LIB_PREFIX "")
  set(LIB_SUFFIX ".lib")
else()
  set(LIB_PREFIX "lib")
  set(LIB_SUFFIX ".a")
endif()

if(NOT KLU_FOUND)
  set(THIS_LIBRARY_SEARCH_DEBUG ${LIB_PREFIX}KLUd${LIB_SUFFIX})
  find_library(
    KLU_LIBRARY_DEBUG
    NAMES ${THIS_LIBRARY_SEARCH_DEBUG}
    HINTS ${KLU_ROOT}
    PATH_SUFFIXES lib Lib ${_KLU_SEARCH_OPTS})
  set(THIS_LIBRARY_SEARCH_RELEASE ${LIB_PREFIX}KLU${LIB_SUFFIX})
  find_library(
    KLU_LIBRARY_RELEASE
    NAMES ${THIS_LIBRARY_SEARCH_RELEASE}
    HINTS ${KLU_ROOT}
    PATH_SUFFIXES lib Lib ${_KLU_SEARCH_OPTS})

  select_library_configurations(KLU)
  list(APPEND KLU_LIBRARIES ${KLU_LIBRARY})
  if(KLU_LIBRARIES)
    set(KLU_FOUND TRUE)
  endif()

  set(KLU_INCLUDE_FILENAME klu.h)
  find_path(
    KLU_INCLUDE_DIR ${KLU_INCLUDE_FILENAME}
    HINTS ${KLU_ROOT}
    PATH_SUFFIXES include)
  list(APPEND KLU_INCLUDE_DIRS ${KLU_INCLUDE_DIR})
endif()

if(KLU_FOUND)
  if(NOT TARGET KLU::KLU)
    add_library(KLU::KLU UNKNOWN IMPORTED)
    set_target_properties(KLU::KLU PROPERTIES INTERFACE_INCLUDE_DIRECTORIES
                                              ${KLU_INCLUDE_DIRS})
    target_link_libraries(KLU::KLU INTERFACE ${KLU_LIBRARIES})
  endif()
endif()

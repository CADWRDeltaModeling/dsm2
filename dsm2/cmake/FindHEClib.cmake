include(${CMAKE_ROOT}/Modules/SelectLibraryConfigurations.cmake)
include(${CMAKE_ROOT}/Modules/FindPackageHandleStandardArgs.cmake)

set(HEClib_FOUND OFF)

if(NOT HEClib_ROOT)
  set(HEClib_ROOT $ENV{HEClib_ROOT})
endif()
if(HEClib_ROOT)
  set(_HEClib_SEARCH_OPTS NO_DEFAULT_PATH)
else()
  set(_HEClib_SEARCH_OPTS)
endif()

if(WIN32)
  set(LIB_PREFIX "")
  set(LIB_SUFFIX ".lib")
else()
  set(LIB_PREFIX "lib")
  set(LIB_SUFFIX ".a")
endif()

if(NOT HEClib_FOUND)
  set(THIS_LIBRARY_SEARCH_DEBUG ${LIB_PREFIX}heclibd${LIB_SUFFIX})
  find_library(
    HEClib_LIBRARY_DEBUG
    NAMES ${THIS_LIBRARY_SEARCH_DEBUG}
    HINTS ${HEClib_ROOT}
    PATH_SUFFIXES lib Lib)
  set(THIS_LIBRARY_SEARCH_RELEASE ${LIB_PREFIX}heclib${LIB_SUFFIX})
  find_library(
    HEClib_LIBRARY_RELEASE
    NAMES ${THIS_LIBRARY_SEARCH_RELEASE}
    HINTS ${HEClib_ROOT}
    PATH_SUFFIXES lib Lib)

  select_library_configurations(HEClib)
  list(APPEND HEClib_LIBRARIES ${HEClib_LIBRARY})
  if(HEClib_LIBRARIES)
    set(HEClib_FOUND TRUE)
  endif()
endif()

if(HEClib_FOUND)
  if(NOT TARGET HEClib::HEClib)
    add_library(HEClib::HEClib UNKNOWN IMPORTED)
    # set_target_properties(HEClib::HEClib PROPERTIES
    # INTERFACE_INCLUDE_DIRECTORIES ${HEClib_ROOT}/include)
    target_link_libraries(HEClib::HEClib INTERFACE ${HEClib_LIBRARIES})
  endif()
endif()

include(${CMAKE_ROOT}/Modules/SelectLibraryConfigurations.cmake)

set(Loki_FOUND OFF)

if(NOT Loki_ROOT)
  set(Loki_ROOT $ENV{Loki_ROOT})
endif()
if(Loki_ROOT)
  set(_Loki_SEARCH_OPTS NO_DEFAULT_PATH)
else()
  set(_Loki_SEARCH_OPTS)
endif()

if(NOT Loki_FOUND)
  set(Loki_INCLUDE_FILENAME LokiTypeInfo.h)
  find_path(
    Loki_INCLUDE_DIR ${Loki_INCLUDE_FILENAME}
    HINTS ${Loki_ROOT}
    PATH_SUFFIXES include/loki)
  list(APPEND Loki_INCLUDE_DIRS ${Loki_INCLUDE_DIR})
  if(Loki_INCLUDE_DIRS)
    set(Loki_FOUND ON)
  endif()
endif()

if(Loki_FOUND)
  if(NOT TARGET "Loki::Loki")
    add_library("Loki::Loki" INTERFACE IMPORTED)
    set_target_properties("Loki::Loki" PROPERTIES INTERFACE_INCLUDE_DIRECTORIES
                                                  "${Loki_INCLUDE_DIRS}")
  endif()
endif()

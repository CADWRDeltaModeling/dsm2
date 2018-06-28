# Install script for directory: Z:/gtm_build/gtm_hydro/dsm2/src

# Set the install prefix
if(NOT DEFINED CMAKE_INSTALL_PREFIX)
  set(CMAKE_INSTALL_PREFIX "C:/Program Files (x86)/DSM2")
endif()
string(REGEX REPLACE "/$" "" CMAKE_INSTALL_PREFIX "${CMAKE_INSTALL_PREFIX}")

# Set the install configuration name.
if(NOT DEFINED CMAKE_INSTALL_CONFIG_NAME)
  if(BUILD_TYPE)
    string(REGEX REPLACE "^[^A-Za-z0-9_]+" ""
           CMAKE_INSTALL_CONFIG_NAME "${BUILD_TYPE}")
  else()
    set(CMAKE_INSTALL_CONFIG_NAME "Release")
  endif()
  message(STATUS "Install configuration: \"${CMAKE_INSTALL_CONFIG_NAME}\"")
endif()

# Set the component getting installed.
if(NOT CMAKE_INSTALL_COMPONENT)
  if(COMPONENT)
    message(STATUS "Install component: \"${COMPONENT}\"")
    set(CMAKE_INSTALL_COMPONENT "${COMPONENT}")
  else()
    set(CMAKE_INSTALL_COMPONENT)
  endif()
endif()

if(NOT CMAKE_INSTALL_LOCAL_ONLY)
  # Include the install script for each subdirectory.
  include("Z:/gtm_build/gtm_hydro/dsm2/BUILD/common/cmake_install.cmake")
  include("Z:/gtm_build/gtm_hydro/dsm2/BUILD/input_storage/cmake_install.cmake")
  include("Z:/gtm_build/gtm_hydro/dsm2/BUILD/fixed/cmake_install.cmake")
  include("Z:/gtm_build/gtm_hydro/dsm2/BUILD/hdf_tidefile/cmake_install.cmake")
  include("Z:/gtm_build/gtm_hydro/dsm2/BUILD/hydrolib/cmake_install.cmake")
  include("Z:/gtm_build/gtm_hydro/dsm2/BUILD/klu_fortran/cmake_install.cmake")
  include("Z:/gtm_build/gtm_hydro/dsm2/BUILD/oprule_interface/cmake_install.cmake")
  include("Z:/gtm_build/gtm_hydro/dsm2/BUILD/sparse/cmake_install.cmake")
  include("Z:/gtm_build/gtm_hydro/dsm2/BUILD/pattern_match/cmake_install.cmake")
  include("Z:/gtm_build/gtm_hydro/dsm2/BUILD/timevar/cmake_install.cmake")

endif()

if(CMAKE_INSTALL_COMPONENT)
  set(CMAKE_INSTALL_MANIFEST "install_manifest_${CMAKE_INSTALL_COMPONENT}.txt")
else()
  set(CMAKE_INSTALL_MANIFEST "install_manifest.txt")
endif()

string(REPLACE ";" "\n" CMAKE_INSTALL_MANIFEST_CONTENT
       "${CMAKE_INSTALL_MANIFEST_FILES}")
file(WRITE "Z:/gtm_build/gtm_hydro/dsm2/BUILD/${CMAKE_INSTALL_MANIFEST}"
     "${CMAKE_INSTALL_MANIFEST_CONTENT}")

# This options are tested for v1.14.2.
set (HDF_PACKAGE_NAMESPACE "hdf5::" CACHE STRING "Name for HDF package namespace (can be empty)" FORCE)
set(DEFAULT_API_VERSION v18 CACHE STRING "Default API version") # -DHDF5_PACKAGE_EXTLIBS:BOOL=ON \
set(HDF5_BUILD_FORTRAN ON CACHE BOOL "Build Fortran support")
set(CMAKE_Fortran_COMPILER ifort CACHE STRING "Fortran compiler")

set (ADD_BUILD_OPTIONS "${ADD_BUILD_OPTIONS} -DHDF5_BUILD_FORTRAN:BOOL=ON" CACHE STRING "Build with Fortran support")
set (ADD_BUILD_OPTIONS "${ADD_BUILD_OPTIONS} -DCMAKE_ANSI_CFLAGS:STRING=-fPIC" CACHE STRING "Build with PIC")

set(USE_LIBAEC ON CACHE BOOL "Use libaec")
set(USE_LIBAEC_STATIC ON CACHE BOOL "Use libaec static")

set(HDF5_ALLOW_EXTERNAL_SUPPORT TGZ CACHE STRING "External TGZ support")
set(ZLIB_PACKAGE_NAME "zlib" CACHE STRING "Name of ZLIB package" FORCE)
set(ZLIB_USE_LOCALCONTENT ON CACHE BOOL "Use local zlib")

set(HDF5_ENABLE_SZIP_SUPPORT ON CACHE BOOL "Enable SZIP support")
set(LIBAEC_PACKAGE_NAME "libaec" CACHE STRING "Name of AEC SZIP package" FORCE)
set(LIBAEC_USE_LOCALCONTENT ON CACHE BOOL "Use local libaec")

set(TGZPATH ${CMAKE_SOURCE_DIR}/.. CACHE STRING "Path to HDF5 tgz file")
set(ZLIB_TGZ_NAME ZLib.tar.gz CACHE STRING "Zlib tarball name")
set(SZAEC_TGZ_NAME LIBAEC.tar.gz CACHE STRING "libaec tarball name")

set(PLUGIN_TGZ_NAME hdf5_plugins-1_14_2.tar.gz CACHE STRING "HDF5 plugin tarball name")

# set(HDF5_NO_PACKAGE OFF CACHE BOOL "packaging")
set(HDF5_PACKAGE_EXTLIBS ON CACHE BOOL "Package external libs")

set(ZLIB_EXPORTED_TARGETS ON CACHE BOOL "Export extlibs cmake")
set(LIBAEC_EXPORTED_TARGETS ON CACHE BOOL "Export extlibs cmake")

#!/bin/bash
# build HDF5 using Intel Compilers.
# The default version is 1.14.2, and other versions are not tested.

# CLI parser
# The first on is the version of HDF5 to build
while getopts 'v:h' opt; do
    case "$opt" in
    v)
        HDF5_VERSION="$OPTARG"
        ;;

    ? | h)
        echo "Usage: $(basename $0) [-v arg]"
        echo "-v, version of HDF5 to build, e.g. 1.14.2"
        exit 1
        ;;
    esac
    shift "$(($OPTIND - 1))"
done

if [ -z "$HDF5_VERSION" ]; then
    HDF5_VERSION=1.14.2
fi

CUR_DIR=$(pwd)
if [ ! -d "build" ]; then
    mkdir build
fi
pushd build

# Download the HDF5 source package
echo "Downloading HDF5 source package... ${HDF5_VERSION}."
# Split the version string into tokens
TOKENS=(${HDF5_VERSION//./ })
MAJOR_VERSION="${TOKENS[0]}.${TOKENS[1]}"
# echo "HDF_MAJOR_VERSION: $MAJOR_VERSION"
HDF5_NAME="CMake-hdf5-${HDF5_VERSION}"
HDF5_ZIP="${HDF5_NAME}.tar.gz"
URL_HDF5="https://support.hdfgroup.org/ftp/HDF5/releases/hdf5-${MAJOR_VERSION}/hdf5-${HDF5_VERSION}/src/${HDF5_ZIP}"
echo "Downloading from $URL_HDF5"
# curl -L ${URL_HDF5} -o CMake-hdf5-${HDF5_VERSION}.tar.gz
# Unpack the HDF5 source package
# If the directory already exists, remove it
if [ -d "${HDF5_NAME}" ]; then
    echo "Removing existing ${HDF5_NAME} first."
    # rm -rf ${HDF5_NAME}
fi
echo "Unpacking HDF5 source package..."
# tar -xzf ${HDF5_ZIP}
cd CMake-hdf5-${HDF5_VERSION}

# Start building HDF5
echo "Building HDF5..."

# I think there is a bug in HDF5 CMake files v1.14.2.
# sed -i -e 's/@ZLIB_PACKAGE_NAME@/zlib/g' "hdf5-${HDF5_VERSION}/config/cmake/hdf5-config.cmake.in"
# sed -i -e 's/@LIBAEC_PACKAGE_NAME@/libaec/g' "hdf5-${HDF5_VERSION}/config/cmake/hdf5-config.cmake.in"

# set options to build fortran
CMAKE_OPTION_FILE="${CUR_DIR}/HDF5options.cmake"

# Build with CMake
BUILD_DIR=build
cmake -E remove_directory ${BUILD_DIR}
cmake -E make_directory ${BUILD_DIR}
cd ${BUILD_DIR}

# Configure the Release mode
cmake -C ${CMAKE_OPTION_FILE} -G "Unix Makefiles" \
-DCMAKE_BUILD_TYPE:STRING=Release \
-DCMAKE_INSTALL_PREFIX=${CUR_DIR}/hdf5-${HDF5_VERSION} ../hdf5-${HDF5_VERSION}
cmake --build . --target install -j $(nproc)

# These dependency directory names are for v1.14.2.
pushd HDF5_ZLIB-prefix/src/HDF5_ZLIB-build
cmake --build . --target install -j ${nproc}
popd

pushd SZIP-prefix/src/SZIP-build
cmake --build . --target install -j ${nproc}
popd

# Debug mode
# cmake -C ${CMAKE_OPTION_FILE} -G "Unix Makefiles" \
# -DCMAKE_BUILD_TYPE:STRING=Debug \
# -DCMAKE_INSTALL_PREFIX=${CUR_DIR}/hdf5-${HDF5_VERSION} ../CMake-hdf5-${HDF5_VERSION}/hdf5-${HDF5_VERSION}
# cmake --build . --target install -j $(nproc)

# cpack -G TGZ

popd
echo "Finished building HDF5"

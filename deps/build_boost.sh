#!/bin/bash
# Linux script to build Boost C++ library using Intel compiler in the release mode.
# It uses the version 1.83.0 by default. The other versions are not tested.

# CLI parser
# The first on is the version of Boost to build
while getopts 'v:h' opt; do
    case "$opt" in
    v)
        BOOST_VERSION="$OPTARG"
        ;;

    ? | h)
        echo "Usage: $(basename $0) [-v arg]"
        echo "-v, version of Boost to build, e.g. 1.83.0"
        exit 1
        ;;
    esac
    shift "$(($OPTIND - 1))"
done

# Download the Boost source package
# # Replace dots with underscores
if [ -z "$BOOST_VERSION" ]; then
    $BOOST_VERSION=1.83.0
fi
echo "Installing ${BOOST_VERSION}."

CUR_DIR=$(pwd)
if [ ! -d "build" ]; then
    mkdir build
fi
pushd build

BOOST_NAME="boost_${BOOST_VERSION//./_}"
BOOST_ZIP="${BOOST_NAME}.tar.bz2"
URL_BOOST="https://boostorg.jfrog.io/artifactory/main/release/${BOOST_VERSION}/source/${BOOST_ZIP}"
echo "Downloading from ${URL_BOOST}."
curl -L ${URL_BOOST} -o ${BOOST_ZIP}
# Unpack the Boost source package
# If the directory already exists, remove it
if [ -d "build/$BOOST_NAME" ]; then
    echo "Removing existing directory ${BOOST_NAME} first"
    rm -rf build/$BOOST_NAME
fi
echo "Unpacking Boost source package..."
tar -xjf ${BOOST_ZIP}

# Start building Boost
echo "Building Boost..."
pushd ${BOOST_NAME}

./bootstrap.sh
./b2 --clean
./b2 install --prefix=${CUR_DIR}/${BOOST_NAME} --with-filesystem --with-regex --with-test --with-system cxxstd=11 toolset=intel-linux

popd
popd
echo "Finished building Boost"

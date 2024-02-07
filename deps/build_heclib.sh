#!/bin/bash
# Build HEClib for DSM2.
set -e

CUR_DIR=$(pwd)
if [ ! -d "build" ]; then
    mkdir build
fi
pushd build

# NOTE: The repository is accessible only from within DWR network
if [ -d  "heclib_2016" ]; then
    echo "Removing existing heclib_2016 first"
    rm -rf heclib_2016
fi
echo "Cloning HEClib..."
git clone http://dwrrhapp0179.ad.water.ca.gov/gitea/nsandhu/heclib_2016.git

# setup compilers
echo "Building HEClib..."
cd heclib_2016/linux-sources
git checkout 64bit
cmake -E remove_directory build
cmake -E make_directory build

cd build
CMAKE_GENERATOR="Unix Makefiles"
cmake -G "${CMAKE_GENERATOR}" -DCMAKE_BUILD_TYPE:STRING=Release \
-DCMAKE_INSTALL_PREFIX=${CUR_DIR}/heclib-6 \
-DCMAKE_C_COMPILER=icc -DCMAKE_Fortran_COMPILER=ifort ..
cmake --build . --target install -j $(nproc)

# cmake -G "${CMAKE_GENERATOR}" -DCMAKE_BUILD_TYPE:STRING=Debug \
# -DCMAKE_INSTALL_PREFIX=${CUR_DIR}/heclib-6 \
# -DCMAKE_C_COMPILER=icc -DCMAKE_Fortran_COMPILER=ifort ..
# cmake --build . --target install -j $(nproc)

popd
echo "Finished building HEClib."

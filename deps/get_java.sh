#!/bin/bash
# Get Java 21
CUR_DIR=$(pwd)
if [ ! -d "build" ]; then
    mkdir build
fi
pushd build

echo "Downloading Java 21 package..."
curl -L https://download.oracle.com/java/21/archive/jdk-21.0.5_linux-x64_bin.tar.gz -o jdk-21_linux-x64_bin.tar.gz
echo "Expanding it..."
tar -xf jdk-21_linux-x64_bin.tar.gz -C ${CUR_DIR}

popd
echo "Done."

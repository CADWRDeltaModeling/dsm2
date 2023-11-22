#!/bin/bash
# Get Loki 0.1.7.
CUR_DIR=$(pwd)
if [ ! -d "build" ]; then
    mkdir build
fi
pushd build

echo "Downloading Loki 0.1.7 source package..."
curl -L http://sourceforge.net/projects/loki-lib/files/Loki/Loki%200.1.7/loki-0.1.7.tar.gz/download -o lock-0.1.7.tar.gz
tar -xf lock-0.1.7.tar.gz -C ${CUR_DIR}

popd
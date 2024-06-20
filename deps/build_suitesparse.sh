# Build SuiteSparse for DSM2. The default version is 4.0.2.

# CLI parser
# The first on is the version of SUITESPARSE to build
while getopts 'v:h' opt; do
    case "$opt" in
    v)
        SUITESPARSE_VERSION="$OPTARG"
        ;;

    ? | h)
        echo "Usage: $(basename $0) [-v arg]"
        echo "-v, version of SUITESPARSE to build, e.g. 4.0.2"
        exit 1
        ;;
    esac
    shift "$(($OPTIND - 1))"
done

if [ -z "$SUITESPARSE_VERSION" ]; then
    SUITESPARSE_VERSION=4.0.2
fi

CUR_DIR=$(pwd)
if [ ! -d "build" ]; then
    mkdir build
fi
pushd build

# Download the SuiteSparse source package
echo "Downloading SuiteSparse source package... $SUITESPARSE_VERSION"
SUITESPARSE_NAME="SuiteSparse-${SUITESPARSE_VERSION}"
SUITESPARSE_TAR="${SUITESPARSE_NAME}.tar.gz"
URL_SUITESPARSE="https://people.engr.tamu.edu/davis/SuiteSparse/${SUITESPARSE_TAR}"
echo "Downloading from $URL_SUITESPARSE"
curl -L ${URL_SUITESPARSE} -o ${SUITESPARSE_TAR}
# Unpack the SuiteSparse source package
echo "Unpacking SuiteSparse source package..."
# If the directory already exists, remove it
if [ -d SuiteSparse ]; then
    echo "Removing existing directory SuiteSparse first"
    rm -rf SuiteSparse
fi
tar -xf ${SUITESPARSE_TAR}

# Start building SuiteSparse
echo "Building SuiteSparse..."
cd "SuiteSparse/KLU"

# Clone the SuiteSparse KLU CMake setting
if [ -d suitesparse_klu_cmake ]; then
    echo "Removing existing directory suitesparse_klu_cmake"
    rm -rf suitesparse_klu_cmake
fi
# NOTE: The repository is not accessible publicly.
git clone -b 64bit http://dwrrhapp0179.ad.water.ca.gov/gitea/nsandhu/suitesparse_klu_cmake.git

# Move the CMakeLists.txt to the root directory
cp suitesparse_klu_cmake/CMakeLists.txt .
cmake -E remove_directory build
cmake -E make_directory build
cd build
CMAKE_GENERATOR="Unix Makefiles"

cmake -G "$CMAKE_GENERATOR" \
-DCMAKE_INSTALL_PREFIX=${CUR_DIR}/suitesparse-${SUITESPARSE_VERSION} \
-DCMAKE_C_COMPILER=icx \
-DCMAKE_BUILD_TYPE:STRING=Release ../
cmake --build . --target install -j $(nproc)

popd
echo "Finished building SuiteSparse"

## How to Build DSM2

### Prerequisites
We support Linux and Windows 10 in the 64-bit memory model for DSM2 build. The following software are required to build DSM2:

- CMake 3.25 or later.
- Intel oneAPI compilers with Fortran (ifort, classic Fortran). Supporting version 2022.1 to 2023.2.
- On Windows, Visual Studio 2022 (for C/C++ and build tools).
- Java SDK ver 1.6.
- Bison/Flex. (Available from Cygwin on Windows.)
- Python 3.9 and later.

### DSM2 dependencies
DSM2 depends on the following libraries and versions:

- [Boost](https://www.boost.org/) v1.83.0
- [HDF5](https://www.hdfgroup.org/solutions/hdf5/) v1.14.2
- HEClib v6
- [SuiteSparse](https://people.engr.tamu.edu/davis/suitesparse.html) v4.0.2
- [Loki](https://sourceforge.net/projects/loki-lib/) v0.1.7

We are using Intel Fortran compiler to build DSM2, and these dependencies - especially HDF5 - are better to build with the same compiler. Thus, we cannot use the pre-built libraries that you can download.

For convenience, there are scripts to build dependencies in a separate repository and pre-built libraries for Windows available. When using the pre-built libraries, copy it under the base of the source codes. (Or, copy anywhere you like and update the CMake option files accordingly.)

## DSM2 Build steps
First, build or copy/download the dependencies to deps under the top directory or to any location you want. If the dependencies are not under deps, modify CMake option files to point to the location of the dependencies. The option files are located in the top directory of DSM2 source code. The CMake option files are: `win_options.cmake` for Windows and `linux_options.cmake` for Linux.

On Linux, run `build_linux.sh`, assuming the prerequisites accessible. This will create a build directory, run CMake to generate Makefiles, run Linux Make to build DSM2 in the release mode, and package products into a `tar.gz` file under the build directory.

On Window,s run `build_win.bat`, assuming the prerequisites accessible, similarly to Linux. This will build and package DSM2 under the build directory in a zip file.

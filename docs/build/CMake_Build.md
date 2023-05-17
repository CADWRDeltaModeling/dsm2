# CMake Build

# Introduction

DSM2 consists of many projects and third party libraries. This has meant
running a number of scripts as documented in [CMake Build](CMake_Build).
It has also entailed opening multiple solution files and build libraries
by hand. Furthermore, the compiler and visual studio upgrades were slow
to adopt due to the cost of breaking changes when upgrading versions.
And finally and most importantly, the daunting task of building all
these files for a different OS such as linux.

# CMake

<a href="https://cmake.org/" rel="nofollow">cmake</a> is a system that
generates the build system. In other words cmake does not help in
building the libraries and projects but encompasses the high-level, OS
and build system independent instructions for generating those systems.
Its introduction and tutorial could be found at
<a href="https://cmake.org/" rel="nofollow">https://cmake.org/</a>

  

A first effort at a cmake generated build system is working for VS2015
with the latest intel compiler on Windows. The instructions for this and
the files needed have been checked into <a
href="https://github.com/CADWRDeltaModeling/dsm2/blob/master/dsm2/src/README.CMAKE.txt"
rel="nofollow">github master</a>

-   A CmakeLibraryMacro.txt is placed at DSM2 root path, with global
    macro and environment settings
-   A CmakeLists is placed under each project/sub-subject to govern its
    compilation.
-   build\*.bat is created for DSM2, input_storage, oprule,
    respectively, to contain the key cmake commands (listed in the
    following sections).
-   After compilation, the exe/dll are generated under subfolders
    BUILD\release or BUILD\debug.

# DSM2 core project

    CMake Instructions
    Create a build directory BUILD under dsm2


    mkdir BUILD
    cd BUILD

    First setup path 
    "C:\Program Files (x86)\IntelSWTools\compilers_and_libraries\windows\bin\compilevars.bat" ia32 vs2015

    Next execute for VS2015 the cmake command
    cmake -G "Visual Studio 14 2015" ..\src

    Finally open the DSM2.sln file in VS 2008 and compile

    or compile from command line with this command
    cmake --build . --target ALL_BUILD --config Debug
    cmake --build . --target ALL_BUILD --config Release

# Input Storage and Oprule

The libraries input_storage and oprule are built to support DSM2 core
project. (confirm their building success before compile core project)

``` python
cd input_storage
mkdir BUILD
cd BUILD
cmake -G "Visual Studio 14 2015" ..\
cmake --build . --target ALL_BUILD --config Debug
cmake --build . --target ALL_BUILD --config Release
```

``` python
cd oprule
mkdir BUILD
cd BUILD
cmake -G "Visual Studio 14 2015" ..\
cmake --build . --target ALL_BUILD --config Debug
cmake --build . --target ALL_BUILD --config Release
```

# Third Party

DSM2 relies on third parties and requires the required libraries
below. Usually DSM2 just uses the built-up libraries; however, sometimes
(when version/environment changes), the following libraries need
re-build. Note DSM2 only requires some specific subsets in these
libraries and these specification could be found in
CmakeLibraryMacro.txt

<img src="attachments/87228956/87228957.png"
data-image-src="attachments/87228956/87228957.png"
data-unresolved-comment-count="0" data-linked-resource-id="87228957"
data-linked-resource-version="1" data-linked-resource-type="attachment"
data-linked-resource-default-alias="sum.PNG"
data-base-url="http://msb-confluence"
data-linked-resource-content-type="image/png"
data-linked-resource-container-id="87228956"
data-linked-resource-container-version="1" height="217" />

  

## **boost**

Run bootstrap.bat to build b2.exe; then run b2.exe

Note b2.exe is required to run in complete mode (the default minimal
mode won't build the required library; run b2 --help for details)

**Linux sample for boost build**

``` python
#use this script to get started with new boost library
#cp this script into new boost library source directory
./bootstrap.sh --with-toolset=intel-linux
./b2 --clean
./b2 -a toolset=intel-linux link=static variant=release --with-filesystem --with-regex --with-system --with-test
```

**Windos sample for boost build**

``` python
./b2 -a runtime-link=static --with-filesystem --with-regex --with-system --with-test
```

## **HDF5**

Go to CMake-hdf5-1.8.20 folder to run batch of the relevant version,
e.g. build-VS2015-32.bat, which builds HDF5-1.8.20-win32.zip

Unzip it and place it under third_party folder.

To build the static libraries which is what is needed with DSM2 static
build see this [HDF5 CMake Static Build](HDF5_CMake_Static_Build)

  

## **heclib**

Build heclib\windows_sources\windows_build_MT_default_settings.bat

-   MT is for static version as we needed (MD for dynamic)
-   Make sure setting compiler path as the required version (as
    exemplified in core project)

  

**For using Visual Studio 2017**

Install Visual Studio 2017:

In addition to the standard Visual Studio 2017 installation, download
individual components from the VS2017 installer.  The individual
components needed are:

<img src="attachments/87228956/87228955.jpg"
data-image-src="attachments/87228956/87228955.jpg"
data-unresolved-comment-count="0" data-linked-resource-id="87228955"
data-linked-resource-version="1" data-linked-resource-type="attachment"
data-linked-resource-default-alias="VS2017_individaul_components.jpg"
data-base-url="http://msb-confluence"
data-linked-resource-content-type="image/jpeg"
data-linked-resource-container-id="87228956"
data-linked-resource-container-version="1" height="250" />

Run the installer, and click on Modify, then Individual components tab,
and check the components, finally click on Modify.

  

Change the lines in build_dsm2_vs2015_32b.bat:

call "C:\Program Files
(x86)\IntelSWTools\compilers_and_libraries\windows\bin\compilevars.bat"
ia32 vs2015

to

call "C:\Program Files
(x86)\IntelSWTools\compilers_and_libraries_2019\windows\bin\compilervars.bat"
ia32 vs2017

  

cmake -G "Visual Studio 14 2015" ..\src

to 

cmake -G "Visual Studio 15 2017" ..\src

  

Rerun the build scripts as instructed above.

  

  

## Attachments:

<img src="images/icons/bullet_blue.gif" width="8" height="8" />
[VS2017_individaul_components.jpg](attachments/87228956/87228955.jpg)
(image/jpeg)  
<img src="images/icons/bullet_blue.gif" width="8" height="8" />
[sum.PNG](attachments/87228956/87228957.png) (image/png)  

# HDF5 CMake Static Build

DSM2 binaries are built with static links (no DLLs are needed). However
HDF5 1.8.10+ does not support static builds as there are fundamental
problems if parallel support is enabled. However DSM2 does not need the
parallel support and static builds are very convenient for us.

The information here was documented in  DSM2-117 - Update HDF5 library
to 1.8.19 or later Done  and the kernel of information is included here
for future HDF5 builds for static linking.

  

# This blog entry explains how to build with /MT flag

<a href="https://blog.afach.de/?page_id=421"
rel="nofollow">https://blog.afach.de/?page_id=421</a>

# HDF5 Static (with /MT flag) compilation Auto compile script – Visual Studio

This is a compile script that compiles HDF5 libraries from source
statically with multithread support, i.e., “/MT” flag in Visual Studio.
automatically.

### Warning

After discussing with one of the programmers of HDF5, it was made clear
that linking statically works safely only in the condition HDF5
library wasn’t compiled with parallel support.

### The script

The script involves going to the file  
config\cmake\UserMacros\Windows_MT.cmake  
and copying the file’s contents to “UserMacros.cmake”. The same is also
done for ZLib and SZip after extracting them, and rezipping them again.

    @echo off
    ::The following is the name of the folder of HDF5 source
    set "hdffolder=hdf5-1.8.16"

    ::add a new line then add /MT compilation options
    call echo & echo. >> %hdffolder%\UserMacros.cmake
    cat %hdffolder%\config\cmake\UserMacros\Windows_MT.cmake >> %hdffolder%\UserMacros.cmake
    for %%i in (%hdffolder%\UserMacros.cmake) do sed -i "s/\"Build With Static CRT Libraries\" OFF/\"Build With Static CRT Libraries\" ON/g" %%i

    ::add a new line then add /MT to SZip after extracting it, and then recompress it
    gzip -dc SZip.tar.gz | tar -xf -
    mv SZip.tar.gz SZip-dynamic.tar.gz
    call echo & echo. >> UserMacros.cmake
    cat SZip\config\cmake\UserMacros\Windows_MT.cmake >>SZip\UserMacros.cmake
    for %%i in (SZip\UserMacros.cmake) do sed -i "s/\"Build With Static CRT Libraries\" OFF/\"Build With Static CRT Libraries\" ON/g" %%i
    tar cf SZip.tar SZip\
    gzip SZip.tar
    rm -r SZip

    ::do the same to ZLib
    gzip -dc ZLib.tar.gz | tar -xf -
    mv ZLib.tar.gz ZLib-dynamic.tar.gz
    call echo & echo. >> UserMacros.cmake
    cat ZLib\config\cmake\UserMacros\Windows_MT.cmake >>ZLib\UserMacros.cmake
    for %%i in (ZLib\UserMacros.cmake) do sed -i "s/\"Build With Static CRT Libraries\" OFF/\"Build With Static CRT Libraries\" ON/g" %%i
    tar cf ZLib.tar ZLib\
    gzip ZLib.tar
    rm -r ZLib

    build-VS2013-32.bat

### Requirements

1- <a href="http://www.cmake.org/download/" rel="nofollow">CMake</a> (add
its executable folder to path)  
2- \[GOW

|                                                                                                                                                               |
|---------------------------------------------------------------------------------------------------------------------------------------------------------------|
| <a href="https://github.com/bmatzelle/gow/downloads%5D3-"                                                                                                     
 rel="nofollow">https://github.com/bmatzelle/gow/downloads]3-</a> <a                                                                                            
 href="https://www.visualstudio.com/en-us/products/visual-studio-express-vs.aspx"                                                                               
 rel="nofollow">Visual Studio or C++ Express</a> (this you can get for free from Microsoft, but I assume you know enough about this already since you’re here)  |

  

Note: If CMake won’t show in path in command prompt, run prompt as
administrator, or use this command to add the path you want to the
environment variable %PATH%  
 set PATH=C:\Program Files (x86)\CMake\bin;%PATH%  
 

Gow is GNU tools for windows, like tar, gzip and sed. These are
important for the script.

Whether you’d like to have a 32-bit or 64-bit version of visual studio
used depends on the environment variables that are defined. The easiest
way is to run the run command prompt for the version you want. For
example, in Visual Studio 2013, if one goes to Start, then types in
quick search “Visual”, you’ll find a folder called “Visual Studio
Tools”. This folder will have both command prompts with the relevant
environment variables. The following shows this folder:

<a
href="https://blog.afach.de/wp-content/uploads/2015/07/VisualStudioCMDShortcuts.jpg"
rel="nofollow"><img
src="https://blog.afach.de/wp-content/uploads/2015/07/VisualStudioCMDShortcuts.jpg"
data-image-src="https://blog.afach.de/wp-content/uploads/2015/07/VisualStudioCMDShortcuts.jpg"
width="668" height="317" /></a>

### Prepare to run the script

Go
to <a href="https://support.hdfgroup.org/HDF5/release/" rel="nofollow">this
page</a>, and download the CMake source. Extract it; put the script in a
file there; if the version you want to compile is different than the one
in the script, modify the folder name; and finally run the script. After
the script is finished, you’ll have a compressed zip file with compiled
source and an installer executable.

<a
href="https://blog.afach.de/wp-content/uploads/2015/07/HDF5FolderLook.gif"
rel="nofollow"><img
src="https://blog.afach.de/wp-content/uploads/2015/07/HDF5FolderLook.gif"
data-image-src="https://blog.afach.de/wp-content/uploads/2015/07/HDF5FolderLook.gif"
width="640" height="306" /></a>The file HDF5CompileScript.bat is where I
copied the script of compile that I created. Just run this script
through the command prompt of visual studio and it’ll compile.

 

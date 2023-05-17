# Cloud Notes

This page documents important notes for running DSM2 in the cloud.

DSM2 has been compiled on Ubuntu Linux and Windows. Only 32 bit version
has been compiled, the path to 64 bit conversion is much longer due to
C/C++ code that needs cleaning up.

## Linux

DSM2 has been compiled and tested on Linux VMs running Red Hat 4.8 with
kernel version 3.10. Static linking does not seem to work as documented
and the dependencies on Intel fortran libraries are packaged in a lib/
subfolder.

AWS Linux version is Red Hat 7.3 running kernel 4.14.  The following
libraries are needed on top of the base image from AWS

``` python
#sudo yum upgrade #-- Do this to ensure installs of the below go through
sudo yum install glibc.i686
sudo yum install libgcc.i686
sudo yum install libstdc++.i686
```

  

## Windows

Windows version is statically compiled and so the executables should
work without any other dependencies. 

TODO: JRE version and installation 

# Developer and Build

# **Basic steps**

-   Install prerequisite softwares

-   Checkout DSM2 from github repository 

-   Compile and build input_storage and oprule libraries (may also need
    third party)

-   Compile and build DSM2 (That should result in the hydro.exe and
    qual.exe and ptm.dll in the build folders)

-   Test and validate the newly compiled

-   Copy and update DSM2 distribution package

-   Package for DSM2 new release

  

DSM2 version 82\* compilation and packaging assuming a minor (bug-fix)
release. 

-   DSM2 source codes and its relevant support party are placed on open
    source platform github
    (<a href="https://github.com/CADWRDeltaModeling/dsm2"
    rel="nofollow">https://github.com/CADWRDeltaModeling/dsm2</a>) for
    version control,

  

# **Required Software**

1.  Visual Studio 2015 (check its installation and management details
    at [Intel Compiler Installation for
    Windows](http://msb-confluence/display/DM/Intel+Compiler+Installation+for+Windows))
2.  Intel Composer for Fortran and C++ (Parallel Studio 2019)
3.  Cmake
    3.14 <a href="https://cmake.org/" rel="nofollow">https://cmake.org/</a> (better
    use a latest stable version, not \*rc)
4.  Git <a href="https://git-scm.com/downloads"
    rel="nofollow">https://git-scm.com/downloads</a>
5.  Source Tree (free git client, optional)
    <a href="https://www.sourcetreeapp.com/"
    rel="nofollow">https://www.sourcetreeapp.com/</a>
6.  Flex and Bison packages in Cygwin
    <a href="http://www.cygwin.com/setup.exe"
    rel="nofollow">http://www.cygwin.com/setup.exe</a>  (make
    sure {cywin}/bin in the environment path)
7.  Inno Setup Compiler v5.2.3
    <a href="http://files.jrsoftware.org/is/5/isetup-5.2.3.exe"
    rel="nofollow">http://files.jrsoftware.org/is/5/isetup-5.2.3.exe</a>
8.  Python 3 <a href="http://www.python.org/download/"
    rel="nofollow">http://www.python.org/download/</a>
9.  Java Development Kit (32-bit) 
    <a href="http://java.sun.com/javase/downloads/index.jsp"
    rel="nofollow">http://java.sun.com/javase/downloads/index.jsp</a>
10. Microsoft Office 2010
11. Apache ANT http://ant.apache.org/bindownload.cgi

  
Note: make sure all software have their binary, header, or library
folder set in the environment variables. (given that Department Virtual
Machines may prohibit editing, users can still edit in their own
account. Be aware of the software version which could fail some step of
compiling)

<img src="attachments/87228890/87228892.png"
data-image-src="attachments/87228890/87228892.png"
data-unresolved-comment-count="0" data-linked-resource-id="87228892"
data-linked-resource-version="1" data-linked-resource-type="attachment"
data-linked-resource-default-alias="image2019-11-15_10-53-40.png"
data-base-url="http://msb-confluence"
data-linked-resource-content-type="image/png"
data-linked-resource-container-id="87228890"
data-linked-resource-container-version="2" height="250" /><img src="attachments/87228890/87228891.png"
data-image-src="attachments/87228890/87228891.png"
data-unresolved-comment-count="0" data-linked-resource-id="87228891"
data-linked-resource-version="1" data-linked-resource-type="attachment"
data-linked-resource-default-alias="image2019-11-15_10-54-29.png"
data-base-url="http://msb-confluence"
data-linked-resource-content-type="image/png"
data-linked-resource-container-id="87228890"
data-linked-resource-container-version="2" height="250" /><img src="attachments/87228890/87228889.png"
data-image-src="attachments/87228890/87228889.png"
data-unresolved-comment-count="0" data-linked-resource-id="87228889"
data-linked-resource-version="1" data-linked-resource-type="attachment"
data-linked-resource-default-alias="image2019-11-15_10-58-45.png"
data-base-url="http://msb-confluence"
data-linked-resource-content-type="image/png"
data-linked-resource-container-id="87228890"
data-linked-resource-container-version="2" height="120" />

# **Validation**

To test new compiled DSM2 and see its difference from older version, the
following tools are often used:

-   DSM2-vista [Compare DSS Files
    Tool](http://msb-confluence/display/DM/Compare+DSS+Files+Tool) requires
    output settings (pathnames) exactly the same, but provides a quick
    summary of accumulated difference, especially useful when we want to
    confirm if two versions are the same. 
-   DSM2-vista [Compare DSS
    Tool](http://msb-confluence/display/DM/Compare+DSS+Tool) is flexible
    to compare between different pathnames, yet requires users set up
    configuration one-by-one.
-   HEC-DSSVue has a compare function in its 'Tools' menu

  

  

## Attachments:

<img src="images/icons/bullet_blue.gif" width="8" height="8" />
[image2019-11-15_10-58-45.png](attachments/87228890/87228889.png)
(image/png)  
<img src="images/icons/bullet_blue.gif" width="8" height="8" />
[image2019-11-15_10-54-29.png](attachments/87228890/87228891.png)
(image/png)  
<img src="images/icons/bullet_blue.gif" width="8" height="8" />
[image2019-11-15_10-53-40.png](attachments/87228890/87228892.png)
(image/png)  
<img src="images/icons/bullet_blue.gif" width="8" height="8" />
[DSM2_v8_0_PTM_Compiling_eclipse.docx](attachments/87228890/87228893.docx)
(application/vnd.openxmlformats-officedocument.wordprocessingml.document)  
<img src="images/icons/bullet_blue.gif" width="8" height="8" />
[DSM2_v8_0_Compiling_and_Packaging.docx](attachments/87228890/87228894.docx)
(application/vnd.openxmlformats-officedocument.wordprocessingml.document)  

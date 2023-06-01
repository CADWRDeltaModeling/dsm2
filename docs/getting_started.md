# Getting Started

Welcome to DSM2. This section of the documentation is intended to help
you acquire and install dsm2, test that it is working, and understand the
layout of the distribution. After you have taken these steps, you will
probably want to tackle the tutorials in the <B>/tutorials</B> folder of the
distribution or consult the documentation or grid map in the
<B>/documentation</B> folder -- a link has been provided on the start menu to
make the documentation easier to find.

## Getting DSM2

DSM2 is distributed by the California Department of Water Resources
Delta Modeling Section. You can find the model at the <a href="https://data.cnra.ca.gov/dataset/dsm2" target="_blank">CNRA Open Data web site</a>.
Currently we distribute Windows executables, tutorials,
source code, and a data package with templates for common studies.

## Installing DSM2

DSM2 has been tested on Windows 10. <BR>
DSM2 is distributed as a .zip file, which contains the model executables and input files. <BR>
You should not unzip it to a location with spaces. <BR>
We recommend D:\delta\dsm2 or C:\delta\dsm2.
Unzip it to a drive with a lot (gigabytes) of room. This will make it
easier to use in-place

## Recommended Third Party Extras

DSM2 comes with a numerical model and scripting capabilities. It is
easier to use the model if you also have a text editor with syntax
highlighting, a tool for differencing text files ("diff tool"), a DSS
viewer and an hdf5 viewer.


<B>Open command window here:</B> Follow the instructions <a href='https://www.windowscentral.com/add-open-command-window-here-back-context-menu-windows-10'>here</a> to 
add the option 'Open command window here' to the Windows Explorer context menu. You will need administrative privileges to do this, and you
should only do this if you are comfortable modifying the registry in Windows 10. This will allow you to open 
a command window by right clicking on a folder in Windows explorer. DSM2 models and Python
scripts can be run in the command window. The tool is essential for working
with DSM2 efficiently.  
<B>Notepad++</B> is a text editor that works well with DSM2 input data and
integrates nicely into the Windows file explorer. We support the editor
with syntax highlighting. Here are some instructions for configuring
Notepad++  
<B>DiffMerge</B> is a good free differencing tool for text files. Beyond
Compare is an inexpensive commercial product that is intuitive and also
compares Word files.  
<B>Vista</B>, one of the first graphical tools for examining data in HEC-DSS
format, comes with DSM2 in the /dsm2/vista/bin directory.  
<B>HEC-DSSVUE</B> is distributed by HEC and is actively maintained. Most people
use DSSVUE as their primary tool with Vista for specific tasks. An Excel
add-in for DSS data is also available on the HEC web page.  
<B>HDFView</B> and <B>HDF-Explorer</B> are two independent browsers for the HDF5 file
format. This lets you look inside a tidefile, one of the outputs of the
model. You only need one of them.

## Test Launching DSM2

The first step is to see whether the installation was successful. To do
this, get a DOS-style command prompt window and type from any location:

``` python
C:\>hydro -v
DSM2-Hydro 8.2.2  Git Version: 1468 Git GUI: 54a9cc3c
Usage: Hydro input-file
```

If you got a message like the one above, you are up and running! 

If instead you get this:

``` text
C:\>hydro -v
'hydro' is not recognized as an internal or external command,
operable program or batch file.
```

  
...you have a path problem and we need to straighten it out.

Your next stop should be to read the [Tutorials](tutorials/Tutorials.md). <BR>
The Basic Tutorials (Tutorial 1-6) feature most of the nuances of the model using a very simple grid
and are an excellent way to learn about the model -- including
subtleties that are new or have caused confusion in the past. The Delta
Tutorial series are more applied -- tasks on the Delta. Doing some of
Delta Tutorial #1 as a motivator, and then tackling the simple ones is a
quick way to get a sense of the model.


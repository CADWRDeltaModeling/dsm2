# Getting Started

Welcome to DSM2. This section of the documentation is intended to help
you acquire and install dsm2, test that it is working and understand the
layout of the distribution. After you have taken these steps, you will
probably want to tackle the tutorials in the /tutorials folder of the
distribution or consult the documentation or grid map in the
/documentation folder -- a link has been provided on the start menu to
make the documentation easier to find.

## Getting DSM2

DSM2 is distributed by the California Department of Water Resources
Delta Modeling Section. You can find the model at our delta evaluation
tools web site. Currently we distribute Windows executables, tutorials,
source code, and a data package with templates for common studies.

## Installing DSM2

DSM2 comes with a binary installer, and you should be able to set it up
easily with the default installation. There are two limitations:  
DSM2 is tested on Windows XP, Windows 7. It has a history of working on
NT, and we would love to hear from you if you can confirm it works on
other operating system.  
You need administrative permission to install the package.  
You should not set it up in a location with spaces. We recommend:
D:\delta\dsm2 or C:\delta\dsm2...  
Set it up on a drive with a lot (gigabytes) of room. This will make it
easier to use in-place

## Recommended Third Party Extras

DSM2 comes with a numerical model and scripting capabilities. It is
easier to use the model if you also have a text editor with syntax
highlighting, a tool for differencing text files ("diff tool"), a dss
viewer and an hdf5 viewer.

Open Command Window Here is a free "power tool" distributed by Microsoft
(the download links are on the right hand side next to the description
of the tool). It allows you to get a command window by right clicking a
location in Windows file explorer. The tool is essential for working
with DSM2 efficiently.  
Notepad++ is a text editor that works well with DSM2 input data and
integrates nicely into the Windows file explorer. We support the editor
with syntax highlighting. Here are some instructions for configuring
Notepad++  
DiffMerge is a good free differencing tool for text files. Beyond
Compare is an inexpensive commercial product that is intuitive and also
compares Word files.  
Vista, one of the first graphical tools for examining data in HEC-DSS
format, comes with DSM2 in the /dsm2/vista/bin directory.  
HEC-DSSVUE is distributed by HEC and is actively maintained. Most people
use DSSVUE as their primary tool with Vista for specific tasks. An Excel
add-in for DSS data is also available on the HEC web page.  
HDFView and HDF-Explorer are two independent browsers for the HDF5 file
format. This lets you look inside a tidefile, one of the outputs of the
model. You only need one of them.

## Test Launching DSM2

The first step is to see whether the installation was successful. To do
this, get a DOS-style command prompt window and type from any location:

``` python
C:\>hydro -v
DSM2-Hydro 8.1.2
Usage: Hydro input-file
```

If you got a message like the one above, you are up and running! Your
next stop should be Tutorial #1 or Delta Tutorial #1. Look in the
/tutorials folder of the distribution. The Basic Tutorials (Tutorial
1-6) feature most of the nuances of the model using a very simple grid
and are an excellent way to learn about the model -- including
subtleties that are new or caused confusion in the past. The Delta
Tutorial series are more applied -- tasks on the Delta. Doing some of
Delta Tutorial #1 as a motivator and then tackling the simple ones is a
quick way to get a sense of the model.

If instead you get this:

``` text
C:\>hydro -v
'hydro' is not recognized as an internal or external command,
operable program or batch file.
```

  
...you have a path problem and we need to straighten it out.

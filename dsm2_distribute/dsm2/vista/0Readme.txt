				README
	
		VISTA: VISualization Tool and Analyzer
			    VISTA (beta 3.xx)
-----------------------------------------------------------------------
			       CONTENTS


   Overview of VISTA
	- Introduction
	- Purpose
	- What this package contains
	- Where to find more information
	- Submitting Comments, Bug Reports and Feature Requests
	
   Installing and Running VISTA
	- Installation Notes
	- Windows
	- Solaris	

=======================================================================
			  OVERVIEW OF VISTA
=======================================================================

-----------------------------------------------------------------------
			     INTRODUCTION
-----------------------------------------------------------------------
This is beta version 3.xx of VISTA. VISTA is a tool for data retrieval
(primarily time-series) from a HEC-DSS data base. Furthermore
functionality is provided for the management, manipulation and
visualization of data.

This the second public beta release. The major feature of this release
is addition of scripting which could be used for end-user
customization as well as for helping with repititive tasks. All of
VISTA's components are available from the scripting environment
-----------------------------------------------------------------------
			       PURPOSE
-----------------------------------------------------------------------

VISTA allows you to:
	- treat a time-series as a type with math operations ( both
	vector and scalar) available amongst others such as period
	averaging, merging, etcetra.
	- Retrieve/store data from a HEC-DSS data base file on your
	local machine or from a machine on the internet.
	- Form virtual groups of data sets which may physically reside
	on a combination of local and internet accessible machines. 
	- View data in a table or a graph.
	- Manipulate on time-series using math functions.
	
-----------------------------------------------------------------------
		      WHAT THIS PACKAGE CONTAINS
-----------------------------------------------------------------------

  - README.txt
	This file you are currently reading

  - COPYRIGHT
	Copyright notice for VISTA

  - VISTA Code ( vista.jar )

  - a shared object or dll for accessing the time-series database ( HEC-DSS)

  - public domain packages (COM.jar, oro.jar, printing.jar, Acme.jar,
	jpython.jar) 

  - Wrapper to start VISTA 
	vista.bat on Windows NT/95 and vista.sh on Solaris 2.x

	The structure after installation looks like this
			vista
			|
	       |--------+----------------------|
	      bin/			      lib/
		vista(.ksh|.bat)               libdss.so, dss.dll, libdss-linux.so
		vscript(.ksh|.bat)             vista.jar, COM.jar, oro.jar
		vserver(.ksh|.bat)             Acme.jar, jpython.jar

-----------------------------------------------------------------------
		       WHAT THIS PACKAGE NEEDS
-----------------------------------------------------------------------

  - Any machine that has a Java virtual machine available. 

The two officially supported version are Windows NT/95 and Solaris
2.x. Other platform support may be available. For further information
see :

	http://sunsite.unc.edu/javafaq/javafaq.html#xtocid190293

  - Java Runtime Environment for Windows NT/95 and Solaris

JRE for Windows NT/95 and Solaris is available from :

	http://java.sun.com/products/jdk/1.1/jre/index.html


-----------------------------------------------------------------------
		    WHERE TO FIND MORE INFORMATION
-----------------------------------------------------------------------
	More information on VISTA and further updates are available
from :

	http://wwwdelmod.water.ca.gov/dsm2/vista/vista.html
	
	In addition to download documentation the latest documentation
is also available from that page.

-----------------------------------------------------------------------
	SUBMITTING COMMENTS, BUG REPORTS AND FEATURE REQUESTS
-----------------------------------------------------------------------

	Comments, bug reports and feature requests may be mailed to
nsandhu@water.ca.gov. 
	There is also a vista mailing list  on which announcements
pertaining to vista are announced. Subscription information to this
mailing list is also available from the above web site.
=======================================================================
		     INSTALLING AND RUNNING VISTA
=======================================================================

-----------------------------------------------------------------------
			  INSTALLATION NOTES
-----------------------------------------------------------------------

    IMPORTANT: Please make sure you understand the Copyright
    (in the file named COPYRIGHT) before installing this release.

	If you meet this package's requirments then 
1) Install JRE 1.1.x for Windows 95/NT or Solaris.
	Goto download page for JRE and install by following
instructions therein. The download page for JRE is : 

	http://java.sun.com/products/jdk/1.1/jre/index.html

2) Unzip the vista-bin.zip file where you would like it to be installed
	(Solaris)
	mv vista-bin.zip install-dir
	(MS-DOS)
	move vista-bin.zip install-dir


3)  Unzip vista-b2.zip
	unzip vista.zip
	You should get the following structure
	vista ------------|0Readme.txt,COPYRIGHT,*.txt
			  |
		|------------------------|
		bin/			lib/
		vista.ksh		*.jar
		vserver.ksh      	libDSS.so
		vscript.ksh
		vista.bat		DSS.dll
		vserver.bat
		vscript.bat
	Goto installation notes for WINDOWS or SOLARIS as needed below:

-----------------------------------------------------------------------
			       WINDOWS
-----------------------------------------------------------------------

	If you have unzipped the vista-bin.zip correctly you should see
a directory containing vista.bat. 
	
1) make sure jre is in your system path. check this by typing
c:\ > jre
If you do not see a help listing for the jre or it says jre command
not found then edit the path variable as follows:
c:\> path = %path%;(JRE_TOP_INSTALL_DIR)/bin
	Then type
c:\> jre 
	to see if it worked
2) set environment variable vista_home to the vista installation
directory. That is the name of the directory where this file is located.

Example: 
c:\> set vista_home=c:\vista

2) run the vista.bat file to run the client or vscript.bat to run the
script or vserver.bat to run the server

c:\> c:\vista\bin\vista.bat
c:\> c:\vista\bin\vserver.bat
c:\> c:\vista\bin\vscript.bat

-----------------------------------------------------------------------
			       SOLARIS/LINUX
-----------------------------------------------------------------------

	If you unzip the vista-sun.zip file you should see a
vista-beta2 directory containing vista.sh and other files

1) check to see if jre is in the path else add jre to your path

2) set environment variable VISTA_HOME to the vista installation
directory. That is the name of the directory where this file is located.
	(Solaris version sets this variable if you execute the
vista.ksh script directly)
	(Linux users should move the libdss.so to libdss-solaris.so
and libdss-linux.so to libdss.so)

Example: setenv VISTA_HOME /usr/vista

3) Execute vista.ksh to run the client or vscript.ksh to run the
script or vserver.ksh to run the server
	/usr/vista/vista.ksh
	/usr/vista/vserver.ksh
	/usr/vista/vscript.ksh

************************************************************************

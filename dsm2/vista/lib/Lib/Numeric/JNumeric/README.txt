JNumeric-0.1a9

WHAT IS JNUMERIC?  
----------------- 

JNumeric provides the functionality for JPython that Numeric does for
CPython. As the Numeric documentation states, Numeric is "...a
collection of extension modules to provide high-performance
multidimensional numeric arrays to the Python programming language."
As of JNumeric-0.1a4, JNumeric provides the same functionality as the
core of Numeric module, although it does not provide any of the
Standard extensions to Numeric module (FFT, LinearAlgebra,
RandomArray, Matrix, MLab, and UserArray).

OBTAINING JNUMERIC
------------------

The latest version of JNumeric can be obtained from
http://starship.skyport.net/~hochberg/jnumeric.tgz.  If you plan to
use JNumeric, please subscribe to the jnumeric mailing list
(jnumeric@egroups.com) by sending a blank message to
jnumeric-subscribe@egroups.com. Bug reports, installation, problems,
etc. should be directed to this address. I would also appreciate it if
you send a message to the list if you successfully install and use
JNumeric, so that I have some idea how may people are using JNumeric.

Also, please see the file DIFFERENCES.txt for a list of intentional
differences between JNumeric and CNumeric.

WHENCE FROM HERE?
-----------------

This is probably the last alpha release of JNumeric. Sometime after Christmas I 
plan to release 0.1b1. Somewhere in the future I see the following releases as 
well:

	* 0.1 (final)
	  * Full doc strings (Stolen from Numeric docs when they come out).
	* 0.2 (when JPython starts working with Java 1.2.)
	  * Standard Extensions (FFT, LinearAlgebra, and RandomArray)
	  * Sorting funtions -> java
	  * Cleaned up ufunc interface.

I would realy love FFT, LinearAlgebra, or RandomArray code -- (if you're
interested in working on LinearAlgebrea let me know I have some preliminary 
ideas for this). 

INSTALLATION 
------------
(1-3 Courtesy David Ascher)

(1) Unpack the distribution in a directory (e.g. /home/me/jpythonstuff,
  which after unpacking contains a Numeric directory).

(2) If that directory is not already there, add it to the
  'python.path' entry in the "registry" file in JPython's home directory.
  (e.g.: python.path=.;/home/me/jpythonstuff)

(3) Add the exact location of the Numeric.jar file to your Java CLASSPATH
  (e.g. CLASSPATH="$CLASSPATH:/home/me/jpythonstuff/Numeric/JNumeric-0.1a6/jnumeric-0.1a6.jar")

(4) Optionally, "cd Numeric/JNumeric-0.1a6; jpython test_all.py". 
    (this should complete without errors).

That's it. I did step (2) by editing the python.path in the jpython registry file 
and step (3) by modifying my jpython startup script to:
"jview  /cp:p C:/WINDOWS/JAVA/JPython-1.01/jpython.jar /cp:p C:/WINDOWS/JAVA/JPython-1.01/Numeric/JNumeric-0.1a6/jnumeric-0.1a6.jar org.python.core.jpython $1 $2 $3 $4 $5 $6 $7 $8 $9"
If you have a better way to do this let me know -- I'd like to be
able to install without mucking with the classpath.

If you wish to recompile the source files: (1) you may need to add
jpython.jar to your classpath when you compile, (2) the resulting
.class files should end up in .../Numeric/JNumeric-XXX/JNumeric.

COPYING
-------

All files in this distribution are Copyright (c) 1998 Timothy Hochberg
except where indicated otherwise in the source. Do not redistribute
without an unmodified copy of this file. This code provided in the
hope that it will be useful, however it comes with no warranty of any
kind.

Enjoy!

____ 
/im  (tim.hochberg@ieee.org)


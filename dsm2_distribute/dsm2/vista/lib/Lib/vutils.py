"""
A collection of utilities for vista
"""
import sys
from vista.set import TimeSeriesMath
from java.lang import System

TimeSeriesMath.DUMB_PATCH = 0 # disable dumb patches per Eli's recommendation
# check for jnios
vh = System.getProperty("vista.home")
try:
    if not vh:
	System.loadLibrary("errno")
	System.loadLibrary("posix")
    else:
	osname = System.getProperty("os.name")
	fs = System.getProperty("file.separator")
	if string.find(osname,"Sun") != -1:
	    System.load(vh+fs+"lib"+fs+"liberrno.so")
	    System.load(vh+fs+"lib"+fs+"libposix.so")
	elif string.find(osname,"Win") != -1:
	    System.load(vh+fs+"lib"+fs+"errno.dll")
	    System.load(vh+fs+"lib"+fs+"posix.dll")
	else:
	    System.loadLibrary("errno")
	    System.loadLibrary("posix")
except:
    pass
# check for display
from java.awt import Toolkit
display = 1
try :
    tk = Toolkit.getDefaultToolkit()
except:
    print 'Problem with display'
    display = 0
#
from vista.db.dss import DSSUtil
from vista.app import MainProperties
DSSUtil.setAccessProperties(MainProperties.getProperties())
#
from vdss import *
from vmath import *
from vtimeseries import *
from vdisplay import *
from vchecker import *
#
def exit():
    sys.exit()
#

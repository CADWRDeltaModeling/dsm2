## Convert an irregular CDEC timeseries to regular time series
# Import VTools time and numpy array creation function
from vtools import *
from vtools.data.api import *
from vtools.functions.api import *
from vtools.data.vtime import *
from vtools.data.constants import *
from vtools.data.timeseries import *   
from datetime import datetime         
from numpy import arange,sin,pi

# Import VTools dss utility function
from vtools.datastore.dss.api import *
# Import VTools Excel utility function
from vtools.datastore.excel.api import *
import os, sys, string, re
from numpy import zeros
from os import listdir
import types
import operator
import pdb

def striplist(l):
    return([x.strip() for x in l])

if __name__ == "__main__":
    input_dss_file = 'cdec-its.dss'
    output_dss_file = 'cdec-rts.dss'
    its_paths_file = 'cdec-its-paths.txt'
    in_tw='(01/01/1995 00:00, 07/01/2009 00:00)'
    its_paths_hdl=open(its_paths_file)
    
    ir_block={'IR-YEAR': '1DAY', 'IR-MON': '1HOUR', 'IR-DAY': '15MIN'}
    
    for its_dss_path in its_paths_hdl:
	if re.match('^#',its_dss_path): continue
	ir_intvl=re.split('/',its_dss_path)[5]
	interval=ir_block[ir_intvl]
	try:
	    its0=dss_retrieve_ts(input_dss_file, its_dss_path, in_tw)
	    print 'Retrieved',str(len(its0))+' '+its_dss_path
	except: 
	try:
	    rts0=interpolate_ts(its0,interval,method=LINEAR)
	    # method==SPLINE,MONOTONIC_SPLINE,LINEAR,PREVIOUS,NEXT,NEAREST)
	    rts_dss_path=re.sub(ir_intvl,interval,its_dss_path)
	    print 'Created',str(len(rts0))+' '+rts_dss_path
	    dss_store_ts(rts0,output_dss_file,rts_dss_path)
	except:
	    print 'Error converting this path:'
	    print str(len(its0))+' '+its_dss_path

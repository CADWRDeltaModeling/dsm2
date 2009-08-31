import string, re, os
from vista.set import Constants,CompositeFilter,DataReference
from vtimeseries import timewindow
from vdss import findpath,opendss,writedss
from vista.db.dss import DSSUtil
from vtimeseries import *
from vdss import *
from vista.set import *
from vista.db.dss import *
from vchecker import *

if __name__ == "__main__":

    infile=r'Y:\Observed Data\CDEC\CDEC-its.dss'
    outfile=r'Y:\Observed Data\CDEC\CDEC-its-fixed.dss'
    tw=timewindow("01JAN1990 0000 - 30JUN2009 2400")
    dss_handle=opendss(infile)
    try: os.remove('CheckRange.log')
    except: pass
    for tsref in dss_handle.getAllDataReferences():
        inpath=tsref.getPathname()
        if re.search('/EC',str(inpath)): minval=50.0; maxval=25000.0
        if re.search('/STAGE',str(inpath)): minval=-15.0; maxval=50.0
        if re.search('/TEMP',str(inpath)): minval=35.0; maxval=90.0
        if re.search('/FLOW',str(inpath)): minval=-100000.0; maxval=300000.
        if not re.search('IR-DAY',str(inpath)): continue
        
        outref=flagData('R',tsref,minval,maxval,None,'CheckRange.log')
        if outref:
            print outref
            writedss(outfile,str(inpath),outref)
            print 'Checked path', str(inpath)
        else:
            print 'No bad values for',str(inpath)

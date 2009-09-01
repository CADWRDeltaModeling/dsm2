import string, re, os
from vtimeseries import timewindow
from vdss import findpath,opendss,writedss
from vdss import *
from vista.set import *
from vista.db.dss import *
from vchecker import *

if __name__ == "__main__":

    infile=r'Y:\Observed Data\CDEC\CDEC-its.dss'
    outfile=r'Y:\Observed Data\CDEC\CDEC-its-fixed.dss'
    tw=timewindow("01JAN1990 0000 - 30JUN2009 2400")
    valArray=[None, None]
    try: os.remove('FlagData.log')
    except: pass
    dss_handle=opendss(infile)
    for tsref in dss_handle.getAllDataReferences():
        yUnits=tsref.getData().getAttributes().getYUnits()
        inpath=tsref.getPathname()
        if not re.search('IR-DAY',str(inpath)): continue

        if re.search('/EC',str(inpath)): 
            if re.search('mil',yUnits): valArray=[0.05,25.0]    # millmhos/cm
            else: valArray=[50.0,25000.0]   # micromhos/cm
        if re.search('/STAGE',str(inpath)): valArray=[-15.0,50.0]
        if re.search('/TEMP',str(inpath)): valArray=[35.0,90.0]
        if re.search('/FLOW',str(inpath)): valArray=[-100000.0,300000.]
        outref=flagData('R',tsref,valArray,'FlagData.log')
        if not outref: outref=tsref
        
        if re.search('/EC',str(inpath)): valArray=[-800.0,800.0]
        if re.search('/STAGE',str(inpath)): valArray=[-1.0,1.0]
        if re.search('/TEMP',str(inpath)): valArray=[-3.0,3.0]
        if re.search('/FLOW',str(inpath)): valArray=[-1000.0,1000.]
        outref=flagData('D',outref,valArray,'FlagData.log')
        if not outref: outref=tsref
        
        valArray=[99999.0]
        if re.search('/FLOW',str(inpath)): outref=flagData('M',outref,valArray,'FlagData.log')
        if outref:
            print outref
            writedss(outfile,str(inpath),outref.getData())
            print 'Checked path', str(inpath)
        else:
            print 'No bad values for',str(inpath)

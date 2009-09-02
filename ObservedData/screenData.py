import string, re, os, sys
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
    try: os.remove('FlagData.log')
    except: pass
    dss_group=opendss(infile)
    for dataref in dss_group.getAllDataReferences():
        dataset=dataref.getData();outds=None
        valArray=[None, None]
        yUnits=dataref.getData().getAttributes().getYUnits()
        inpath=dataref.getPathname()
        if not re.search('IR-DAY',str(inpath)): continue

        valArray=[-99999.0]
        if re.search('/FLOW',str(inpath)): outds=flagData('M',dataset,valArray,'FlagData.log')
        if not outds: outds=dataset

        if re.search('/EC/',str(inpath)): 
            if re.search('mil',yUnits): valArray=[0.05,25.0]    # millmhos/cm
            else: valArray=[50.0,25000.0]   # micromhos/cm
        if re.search('/CL/',str(inpath)): valArray=[5,3000.0]
        if re.search('/STAGE',str(inpath)): valArray=[-15.0,50.0]
        if re.search('/TEMP',str(inpath)): valArray=[35.0,90.0]
        if re.search('/FLOW',str(inpath)): valArray=[-100000.0,300000.]
        if re.search('/EXPORT',str(inpath)): valArray=[-100.0,130000.]
        outds=flagData('R',outds,valArray,'FlagData.log')
        if not outds: outds=dataset
        
        if re.search('/EC/',str(inpath)): valArray=[-800.0,800.0]
        if re.search('/CL/',str(inpath)): valArray=[-80.0,80.0]
        if re.search('/STAGE',str(inpath)): valArray=[-1.0,1.0]
        if re.search('/TEMP',str(inpath)): valArray=[-3.0,3.0]
        if re.search('/FLOW',str(inpath)): valArray=[-1000.0,1000.]
        if re.search('/EXPORT',str(inpath)): valArray=[-2000.0,2000.]
        outds=flagData('D',outds,valArray,'FlagData.log')
        
        if outds:
            writedss(outfile,str(inpath),outds)
            print 'Checked path', str(inpath)
        else:
            print 'No bad values for',str(inpath)
    sys.exit()

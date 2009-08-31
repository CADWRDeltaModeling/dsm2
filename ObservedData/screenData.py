import string, re, os
from vista.set import Constants,CompositeFilter,DataReference
from vtimeseries import timewindow
from vdss import findpath,opendss,writedss
from vista.db.dss import DSSUtil
from vtimeseries import *
from vdss import *
from vista.set import *
from vista.db.dss import *

__doc__="""
vchecker:
This module contains the functions for checking data based on its value, rate of
change, quality flag values etcetra.
"""
def checkRange(ref, min, max, log = 'checkrange.log'):
    """
    checkRange(ref, min, max, log = 'checkrange.log'):
    Checks the range of values to lie within min to max. For the values
    outside this range, they are marked as reject. All values marked
    as reject are cached in the log file. Flags added to timeseries
    if needed.
    """
    # a flag to check if any flag was changed
    changedFlag=0
    # get the filter for missing values
    filter = Constants.DEFAULT_FILTER
    # get the data
    if hasattr(ref,'getPathname'):
	ds = ref.getData()
    else:
	ds = ref
    # check if ref already has flags, if not, make them
    makeFlags=False
    if not ds[0].getFlag():
	# create copy of incoming ref but with flags
	makeFlags=True
	changedFlag = 1
	DSSUtil.updateData(ref,1)
	xa=jarray.zeros(len(ds),'d')
	ya=jarray.zeros(len(ds),'d')
	flUnscreened=make_flag_value('UNSCREENED|null')
	fa = jarray.zeros(len(ds),'i')
	for i in range(len(ds)):
	    xa[i]=ds[i].getX()
	    ya[i]=ds[i].getY()
	    #fa[i]=flUnscreened
	ref=IrregularTimeSeries(str(ref.getPathname()),xa,ya,fa,ds.getAttributes())
	ds=ref
    # get the iterator on the data
    dsi = ds.getIterator()
    # get user id for setting flags
    uId = DSSUtil.getUserId();
    # open log file
    logfile = open(log,'a')
    logfile.write('\n' + 'Name: ' + ds.getName())
    logfile.write('\n' + 'Acceptable range: ' + str(min) + ' - ' + str(max))
    # while not at the end of data do...
    dsi.resetIterator() 
    while not dsi.atEnd():
        # get the data element at the current position
        e = dsi.getElement()
        # if value is acceptable and exceeds range flag as reject
        if filter.isAcceptable(e) :
            if e.y < min or e.y > max :
                FlagUtils.setQualityFlag(e,FlagUtils.REJECT_FLAG,uId)
                changedFlag = 1
                dsi.putElement(e) # put the element back into the data set
                logfile.write('\n' + " Rejected data @: " +
                              e.getXString() + " : " + e.getYString())
        dsi.advance() # move to next value
    # end the while loop
    logfile.close()
    if changedFlag:
        return ref
    else:
        return None
#
def checkDiff(ref, min, max, log = 'checkslope.log'):
    """
    checkDiff(ref, min, max, log = 'checkdiff.log'):
    checks the range of 1st difference values to lie within min to max. For the values
    outside this range, they are marked as reject. All values marked
    as reject are cached in the log file
    """
    # get the filter for missing values
    filter = Constants.DEFAULT_FILTER
    # get the data
    if hasattr(ref,'getPathname'):
	ds = ref.getData()
    else:
	ds = ref
    # get the iterator on the data
    dsi = ds.getIterator()
    # a flag to check if any flag was cleared
    changedFlag=0
    # get user id for setting flags
    uId = DSSUtil.getUserId();
    # open log file
    logfile = open(log,'w')
    # previous value
    have_previous_value = 0
    previous_value=0.0
    diff = 0.0
    # while not at the end of data do...
    while not dsi.atEnd():
        # get the data element at the current position
        e = dsi.getElement()
        # if value is acceptable and flagged as missing then clear its flags
        if filter.isAcceptable(e) :
            if have_previous_value:
                diff = previous_value - e.y
                if diff < min or diff > max :
                    FlagUtils.setQualityFlag(e,FlagUtils.REJECT_FLAG,'CheckDiff')
                    changedFlag = 1
                    dsi.putElement(e) # put the element so cleared into the data set
                    logfile.write('\n' + " Rejected data @: " +
                                  e.getXString() + " : " + e.getYString())
            have_previous_value = 1
            previous_value = e.y
        else:
            have_previous_value = 0
        dsi.advance() # move to next value
    # end the while loop
    logfile.close()
    if changedFlag:
        return ref
    else:
        return None
#

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
	
	outref=checkRange(tsref,minval,maxval,'CheckRange.log')
        if outref:
	    print outref
	    writedss(outfile,str(inpath),outref)
	    print 'Checked path', str(inpath)
        else:
            print 'No bad values for',str(inpath)

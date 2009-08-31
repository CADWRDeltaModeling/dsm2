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
def flagData(ftype, ref, val1=None, val2=None, val3=None, log = 'flag.log'):
    """
    flagData(ftype, ref, val1, val2, val3, log = 'flag.log'):
    Flags a datastream's values for bad data:
    ftype='R': datavalue not within val1 to val2 marked as reject.
         ='D': datavalue difference from previous value not within
               val1 to val2 range.  If val3 given any value, use percentage
               for val1 and val2.
         ='M': datavalue equals or very close to val1, a Missing Value marker;
               DSS flag set to missing.
    All values marked are written to log file. Flags added to timeseries
    if needed.
    """
    if ftype == 'R':
        if val1==None or val2==None: raise 'Two values must be given for Range check.'
    elif ftype == 'D':
        if val1==None or val2==None: raise 'Two values must be given for Diff check; optional 3rd val=>use pct.'
    elif ftype == 'M':
        if val1==None: raise 'One value must be given for Missing check.'
    else: raise 'First arg must be a single character R, D or M.'
    # a flag to check if any flag was changed
    changedFlag=0
    # get the filter for missing values
    filter = Constants.DEFAULT_FILTER
    # get the data
    ds = ref.getData()
    # check if ref already has flags, if not, make them
    makeFlags=False
    if not ds[0].getFlag():
        # create copy of incoming ref but with flags
        makeFlags=True
        changedFlag = 1
        #DSSUtil.updateData(ref,1)
        xa=jarray.zeros(len(ds),'d')
        ya=jarray.zeros(len(ds),'d')
        flUnscreened=make_flag_value('UNSCREENED|null')
        fa = jarray.zeros(len(ds),'i')
        for i in range(len(ds)):
            xa[i]=ds[i].getX()
            ya[i]=ds[i].getY()
            #fa[i]=flUnscreened
        ds=IrregularTimeSeries(str(ref.getPathname()),xa,ya,fa,ds.getAttributes())
        #ds=ref
    # get the iterator on the data
    dsi = ds.getIterator()
    # get user id for setting flags
    uId = DSSUtil.getUserId();
    prevY=None
    diff = 0.0
# open log file
    logfile = open(log,'a')
    logfile.write('\n' + 'Name: ' + ds.getName())
    logfile.write('\n' + 'Acceptable range: ' + str(min) + ' - ' + str(max))
    # while not at the end of data do...
    dsi.resetIterator() 
    while not dsi.atEnd():
        # get the data element at the current position
        e = dsi.getElement()
        # if value not already marked check for bad value
        if filter.isAcceptable(e) :
            if ftype=='R':
                if e.y < val1 or e.y > val2 : FlagUtils.setQualityFlag(e,FlagUtils.REJECT_FLAG,uId)
                changedFlag = 1
            if ftype=='D':
                if prev_y:
                    diff=prev_y-e.y
                    if val3: diff=diff/prev_y*100.
                    if diff < val1 or diff > val2 : FlagUtils.setQualityFlag(e,FlagUtils.REJECT_FLAG,uId)
                    changedFlag = 1
            if ftype=='M':
                if val1*0.999 < e.y and val1*1.0001 > e.y : FlagUtils.setQualityFlag(e,FlagUtils.MISSING_FLAG,uId)
                changedFlag = 1
            if changedFlag:
                dsi.putElement(e) # put the element back into the data set
                logfile.write('\n' + " Rejected data @: " +
                              e.getXString() + " : " + e.getYString())
            prev_y=e.y
        dsi.advance() # move to next value
    # end the while loop
    logfile.close()
    if changedFlag:
        return ds
    else:
        return None
    
def display_missing(ds):
    """
    display_missing(ds)
    where
    ds is a data set or time series
    displays missing value ranges for given time series
    """
    dsi = ds.getIterator()
    while not dsi.atEnd():
	el = dsi.getElement()
	begin_date = None
	while not Constants.DEFAULT_FLAG_FILTER.isAcceptable(el):
	    if begin_date == None: begin_date = el.getXString()
	    end_date = el.getXString()
	    dsi.advance()
	    if dsi.atEnd():
		break
	    el = dsi.getElement()
	    #print el
	if begin_date != None:
	    print 'Missing for %s to %s'%(begin_date,end_date)
	if dsi.atEnd(): break
	dsi.advance()
#
def diff(rts1,rts2,outfile=None):
    '''
    diff(rts1,rts2,outfile=None):
    Prints to stdout the differences between rts1 and rts2 to
    outfile or if outfile is None to standard out. When writing
    to file it appends to existing outfile if any.
    '''
    if outfile ==None:
	fh=sys.stdout
    else:
	fh=open(outfile,'a+')
    if rts1.getTimeInterval().compare(rts2.getTimeInterval()) !=0 :
	raise "Incompatible time intervals for %s and %s"%(rts1.getName(),rts2.getName())
    tw = rts1.getTimeWindow()
    if not tw.isSameAs(rts2.getTimeWindow()):
	fh.write('TimeWindow for %s is %s & %s is %s\n'\
		 %(rts1.getName(),str(rts1.getTimeWindow()),\
		   rts2.getName(),str(rts2.getTimeWindow())))
	tw = tw.intersection(rts2.getTimeWindow())
    if tw == None:
	raise "No intersecting time window for %s and %s"%(rts1.getName(),rts2.getName())
    else:
	rts1 = rts1.createSlice(tw)
	rts2 = rts2.createSlice(tw)
    dsi1 = rts1.getIterator()
    dsi2 = rts2.getIterator()
    while not dsi1.atEnd():
	e1 = dsi1.getElement()
	e2 = dsi2.getElement()
	if e1.y != e2.y:
	    fh.write('Value difference @ %s , 1: %f , 2: %f\n'\
		     %(e1.getXString(),e1.y,e2.y))
	if e1.flag != e2.flag:
	    fh.write('Flag difference @ %s , 1: %s, 2: %s\n'\
		     %(e1.getXString(), e1.getFlagString(), e2.getFlagString()))
	dsi1.advance()
	dsi2.advance()
#

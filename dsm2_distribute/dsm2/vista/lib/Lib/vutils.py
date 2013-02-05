__doc__ ="""

"""
"""
A collection of utilities for vista
This module contains the functions for checking data based on its value, rate of
change, quality flag values etcetra.
"""
import sys, datetime, jarray
from vista.set import TimeSeriesMath
from vtimeseries import *
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
from vtimeseries import *
from vdisplay import *
#
def exit():
    sys.exit()
#
def flag_data(ftype, dataset, valArray, log = 'flag.log', \
              Special = False, ResetExist = False):
    """
    flag_data(ftype, dataset, valArray, log = 'flag.log', Special = False):
    Flags a datastream's values for bad data:
    ftype='R': datavalue not within valArray[0]:valArray[1] marked as reject.
         ='D': datavalue difference from previous value not within
               valArray[0] range marked as reject. If len(valArray) > 1, 
               valArray[0] is an exceedance factor for values, and
               valArray[1] is an exceedance factor for 1st diff of values,
               both compared to Trailing Moving Average
         ='M': datavalue equals or very close to val[0:], an array of Missing Value markers;
               DSS flag set to missing. Optional Special means marker value
               is within normal data range, check more carefully.
         ='+|*': datavalue within range valArray[0]:valArray[1] scaled by valArray[2]
               amount, that is, datavalue is added/multiplied to/by valArray[2]. Used mainly
               for CDEC EC data that switches between milli- and micro-mhos/cm and
               to add 100 to USGS stage data.
    All values marked are written to log file.
    Special=True: Special treatment for Missing values that are within the
    normal operating range of the parameter
    ResetExist=True: reset any existing flag values to Unscreened
    Flags added to timeseries if needed.
    """
    from jarray import zeros
    def nearVal (val, target, tol=.001):
        # return True if relative error is less than tol
        if abs(val-target) < tol:
            return True
        else:
            return False
    if ftype == 'R':
        if len(valArray) != 2: 
            # assume hi and lo range will be calculated from percentiles
            yVals = sorted(SetUtils.createYArray(dataset))
            # lo and hi: 1st and 99th percentiles
            valArray = [yVals[int(len(yVals)*0.01)], yVals[int(len(yVals)*0.99)]]
        rej_head = 'Check range ' + str(valArray[0]) + ' - ' + str(valArray[1])
        rej_note = 'Range reject @ '
    elif ftype == 'D':
        nMoveAve = 5   # number of good elements in moving average
        moveAve = 0.0
        moveAveDiff = 0.0
        xTA = zeros(nMoveAve,'d')        
        diffFactor = False
        rej_head = 'Check diff w/ moving average ' + str(valArray[0])
        rej_note = 'Diff reject @ '
        if len(valArray) > 1: # factor
            diffFactor = True
            rej_head = 'Check vals, 1st diffs w/ MA ' + str(valArray[0])+ ', ' + str(valArray[1]) + ' factors'
#            yVals = sorted(SetUtils.createYArray(dataset))
#            # difference between 10th and 90th percentiles
#            pdiff = abs(yVals[int(len(yVals)*0.10)] - yVals[int(len(yVals)*0.90)])
            rej_note = 'Diff reject % @ '
    elif ftype == 'M':
        if len(valArray) < 1: raise 'At least one value must be given for Missing check.'
        rej_head = 'Check Missing value marker ' + str(valArray)
        rej_note = 'Missing @ '
    elif ftype == '+' or ftype == '*':
        if len(valArray) != 3: raise 'Three values must be given for Shift.'
        rej_head = 'Check scale ' + str(valArray[0]) + ' - ' + str(valArray[1])
        rej_note = 'Value scaled @ '
    else: raise 'First arg must be a single character R, D or M.'
    # nominal time interval in minutes to detect gaps in data
    intvls = {'IR-DAY': 15, 'IR-MON': 60, 'IR-YEAR': 1440, \
              '2MIN': 2, '10MIN': 10, '15MIN': 15, '1HOUR': 60, \
              '1DAY': 1440, '1MON': 43920}
    ePart = dataset.getName().split('/')[5]
    nomIntvl = intvls[ePart]
    # a flag to check if any flag was changed
    changedFlag = False
    # get the filter for missing values
    filter = Constants.DEFAULT_FLAG_FILTER
    # check if ds already has flags, if not, make them
    # open log file
    logfile = open(log, 'a')
    logfile.write('\n\n' + 'Name: ' + dataset.getName())
    logfile.write('\n' + 'Units: ' + dataset.getAttributes().getYUnits())
    logfile.write('\n' + rej_head)
    if dataset.isFlagged(): ds = dataset
    else: ds = ds_add_flags(dataset)
    # get user id for setting flags
    uId = DSSUtil.getUserId('datachecker')
    # create a missing data element
    ex = dataset.getElementAt(0)
    ex.setY(Constants.MISSING_VALUE)
    ex.setFlag(FlagUtils.MISSING_FLAG)
    eBad = None
    intvl = None
    nGood = 0
    for i in range(dataset.size()):
        changedEl = False
        e1 = dataset.getElementAt(i)
        if ResetExist:
            FlagUtils.clearAllFlags(e1, uId)
            dataset.putElementAt(i, e1) # put the element back into the data set
        if not filter.isAcceptable(e1) or \
               FlagUtils.getQualityFlag(e1) == FlagUtils.MISSING_FLAG: continue
        # get the data elements at the i, i-1, and i+1 positions
        if i > 0:
            e0 = dataset.getElementAt(i - 1)
            intvl = int(e1.getX() - e0.getX() + .01)
        else: 
            e0 = ex
        if i < len(dataset) - 1: 
            e2 = dataset.getElementAt(i + 1)
        else: 
            e2 = ex
        if ftype == 'R':    # Range
            if e1.y < valArray[0] or e1.y > valArray[1] :
                FlagUtils.setQualityFlag(e1, FlagUtils.REJECT_FLAG, uId)
                changedEl = True
        elif ftype == 'D':  # Difference (abs or %) between this and moving-ave value
# if large data time gap noted, flag element as questionable and reset moving average
            if intvl > nomIntvl*6:
                FlagUtils.setQualityFlag(e1, FlagUtils.QUESTIONABLE_FLAG, uId)
                changedEl = True
                moveAve = 0
                xTA = zeros(nMoveAve,'d')
                nGood = 0        
            if not diffFactor: 
                diff1 = abs(e1.y - moveAve)
            else:
                if moveAve <> 0: 
                    diff1 = abs(e1.y / moveAve)
                else:
                    diff1 = 1
                if moveAveDiff <> 0:
                    diff2 = abs((e1.y - e0.y) / moveAveDiff)
                else:
                    diff2 = 0
                if diff2 == 0:
                    diff2 = .1
                if diff2 < 1:
                    diff2 = 1 / diff2
            # check for spikes and longer plateaus near a bad value
            if diff1 == 0:
                diff1 = .1
            if diff1 < 1:
                diff1 = 1 / diff1
            if (nGood >= nMoveAve) and \
                ( (diff1 > valArray[0] and (not diffFactor or (diffFactor and diff2 > valArray[1]))) or \
                (eBad and nearVal(e1.y, eBad.y, abs(eBad.y*.001)))):
                FlagUtils.setQualityFlag(e1, FlagUtils.REJECT_FLAG, uId)
                changedEl = True
                if not eBad:
                    eBad = e1
            else:
                eBad = None
            if filter.isAcceptable(e1) and \
                FlagUtils.getQualityFlag(e1) != FlagUtils.MISSING_FLAG: 
                nGood += 1
                xTA.pop(0)
                xTA.append(e1.y)
            # moving average of data values
            moveAve = sum(xTA) / nMoveAve
            if diffFactor: 
                # moving average of 1st difference of data values
                xTA1 = xTA[1:nMoveAve]
                xTA0 = xTA[0:nMoveAve-1]
                moveAveDiff = sum([abs(xTA1[j]-xTA0[j]) for j in range(nMoveAve-1)]) / (nMoveAve -1)
        elif ftype == 'M':  # Missing values
            for vA in valArray:
                if nearVal(vA, e1.y):
                    if not Special or (Special and \
                    # Special treatment for Missing values that are within the
                    # normal operating range of the parameter; check that the value
                    # before or after is also Missing or not acceptable before
                    # marking this value as Missing
                     (not filter.isAcceptable(e0) or not filter.isAcceptable(e2)) or \
                     (nearVal(vA, e0.y) or nearVal(vA, e2.y))):
                        FlagUtils.setQualityFlag(e1, FlagUtils.MISSING_FLAG, uId)
                        #e1.y=Constants.MISSING_VALUE
                        changedEl = True
        elif ftype == '+':  # re-scale
            if not filter.isAcceptable(e1): continue
            if e1.y >= valArray[0] and e1.y <= valArray[1] :
                e1.y += valArray[2]
                changedEl = True
        elif ftype == '*':  # re-scale
            if not filter.isAcceptable(e1): continue
            if e1.y >= valArray[0] and e1.y <= valArray[1] :
                e1.y *= valArray[2]
                changedEl = True
        if changedEl:
            changedFlag = True
            dataset.putElementAt(i, e1) # put the element back into the data set
            logfile.write('\n' + rej_note + e1.getXString() + " : " + e1.getYString())
    # end the for loop
    logfile.close()
    if changedFlag or ResetExist:
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
def diff(rts1, rts2, outfile=None):
    '''
    diff(rts1,rts2,outfile=None):
    Prints to stdout the differences between rts1 and rts2 to
    outfile or if outfile is None to standard out. When writing
    to file it appends to existing outfile if any.
    '''
    if outfile == None:
        fh = sys.stdout
    else:
        fh = open(outfile, 'a+')
    if rts1.getTimeInterval().compare(rts2.getTimeInterval()) != 0:
        raise "Incompatible time intervals for %s and %s" % (rts1.getName(), rts2.getName())
    tw = rts1.getTimeWindow()
    if not tw.isSameAs(rts2.getTimeWindow()):
        fh.write('TimeWindow for %s is %s & %s is %s\n'\
             % (rts1.getName(), str(rts1.getTimeWindow()), \
               rts2.getName(), str(rts2.getTimeWindow())))
    tw = tw.intersection(rts2.getTimeWindow())
    if tw == None:
        raise "No intersecting time window for %s and %s" % (rts1.getName(), rts2.getName())
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
                     % (e1.getXString(), e1.y, e2.y))
            if e1.flag != e2.flag:
                fh.write('Flag difference @ %s , 1: %s, 2: %s\n'\
                     % (e1.getXString(), e1.getFlagString(), e2.getFlagString()))
            dsi1.advance()
            dsi2.advance()
#
def flags_to_missing(ds,filter=Constants.DEFAULT_FLAG_FILTER):
    """
    flags_to_missing(ds,filter=Constants.DEFAULT_FLAG_FILTER):
     returns a dataset with missing values instead of flags
     where ds is an ITS or RTS and
          filter is of type vista.set.ElementFilter e.g. Constants.DEFAULT_FLAG_FILTER
    """
    dsi = ds.getIterator()
    while not dsi.atEnd():
        e = dsi.getElement()
        if not filter.isAcceptable(e):
            e.setY(Constants.MISSING_VALUE)
            dsi.putElement(e)
        dsi.advance()
    return ds
#

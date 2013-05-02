import string, jarray
from jarray import zeros
from datetime import *
from java.lang import Math
#from string import split, strip, lower
from vdss import wrap_data
from vista.time import *
from vista.set import Constants, DataReference, DataType, FlagUtils, \
     IrregularTimeSeries, MovingAverageProxy, Pathname, ProxyFactory, \
     RegularTimeSeries, Stats, TimeSeriesMath
from vista.db.dss import DSSUtil
#
def timewindow(twstr):
    """
    timewindow(tm)
    creates a time window object from a string
    """
    if ( isinstance(twstr,TimeWindow)):
        return TimeFactory.getInstance().createTimeWindow(twstr.getStartTime(), twstr.getEndTime())
    else:
        return TimeFactory.getInstance().createTimeWindow(twstr)
#
def time(tmstr,pattern=None):
    """
    time(tmstr,pattern):
    creates a time object for the given time string in ddMMMyyyy HHmm
    format.
    """
    if pattern == None:
               return TimeFactory.getInstance().createTime(tmstr)
    else:
               return TimeFactory.getInstance().createTime(tmstr,pattern)
#
def convert_to_date(time_val):
    from java.util import TimeZone,Date
    return Date(time_val.date.time-TimeZone.getDefault().getRawOffset())
def timeinterval(tmistr):
    """
    timeinterval(tmistr):
    creates a time interval object for the given time interval
    in the format of #interval where # is a an integer and
    interval is a string such as min, hour, day, month, year
    """
    return TimeFactory.getInstance().createTimeInterval(tmistr);
#
def interpolate(ref,tm_intvl='1day',flat=True):
    """
    interpolate(ref,tm_intvl='1day',flat=True):
    generates from given data which may be reference or regular time series
    either a reference or time series depending upon the input. The generated
    data interpolates using values of input creating a data set of interval
    smaller than given data.
    For right now only flat = True works but someday we'll have the linear
    interpolate working as well.

    Usage:
    ref1 = interpolate(ref_input,'1day')
    OR
    ds1 = interpolate(ref_input.getData(),'1day')
    """
    got_ref = False
    if isinstance(ref, DataReference):
        ds = ref.getData();
        got_ref = True
    else:
        ds = ref
        got_ref = False
    # check for regular time series
    if not isinstance(ds,RegularTimeSeries):
        print ref, " is not a regular time-series data set"
        return None
    tm = TimeFactory.getInstance().createTime(0);
    ti = ds.getTimeInterval();
    tiday = ti.create(tm_intvl)
    if ti.compare(tiday) < 0 :
               raise repr(ref.getName()) + " : has interval lesser than " + repr(tiday)
    elif ti.compare(tiday) == 0:
           if isinstance(ds,RegularTimeSeries):
               return ds
           else:
               return ref
    filter = Constants.DEFAULT_FLAG_FILTER
    # convert monthly data to daily data
    y_cfs = [] # initialize empty array
    nvals = 0
    for e in ds:
        tm = tm.create(Math.round(e.x))
        tm2 = tm.create(tm)
        tm2.incrementBy(ti,-1) # go back one month
        try:
            nvals = tm2.getExactNumberOfIntervalsTo(tm,tiday)
        except:
            raise "Don't have exact number of intervals from " + repr(tm2) + \
                  " to " + repr(tm) + " using " + repr(tiday)
        if filter.isAcceptable(e): 
            val = e.y
        else:
            val = Constants.MISSING_VALUE
        for i in range(nvals):
            y_cfs.append(val)
    #
    # create a regular time series to store this data
    stime = ds.getStartTime().create(ds.getStartTime())
    stime.incrementBy(ti,-1)
    stime.incrementBy(tiday)
    rts = RegularTimeSeries(ds.getName(), 
                            stime.toString(), repr(tiday), y_cfs)
    if got_ref:
        path= ref.getPathname()
        path = Pathname.createPathname(path)
        path.setPart(Pathname.F_PART,'(MATH_COMP)')
        path.setPart(Pathname.E_PART,repr(tiday))
        new_ref = wrap_data(rts,ref.getFilename(), 
                            ref.getServername(), repr(path));
        return new_ref
    else :
        return rts
##############################

def resample(ref,outint,offset=0):
    '''
    Resample a time series at regular intervals
    (e.g. turn 15 minute data into 1 hour data)
    
    Arguments:
    ref     reference to series to be resampled
    outint  (string) time interval for resampling. Must be greater than that of ref
    offset  shift in sampling time. The offset is measured in the
            units of the original series. For example, when 15min is data is
            sampled daily, the default is to sample at time 2400 hours. To
            sample at 1200 hours, use offset=48.
    Return value:    reference to resampled series
    '''

#    Resample a time series at regular intervals
#    (e.g. turn 15 minute data into 1 hour data)

#    Arguments:
#    ref     reference to series to be resampled
#    outint  (string) time interval for resampling. Must be courser than that of ref
#    offset  shift in sampling time. The offset is measured in the
#            units of the original series. For example, when 15min is data is
#            sampled daily, the default is to sample at time 2400 hours. To
#            sample at 1200 hours, use offset=48.
#    Return value:    reference to resampled series
#

    got_ref = False
    if isinstance(ref, DataReference):
        data = ref.getData()
        got_ref = True
    else:
        data = ref
        got_ref = False
    # check for regular time series
    if not isinstance(data,RegularTimeSeries):
        print ref, " is not a regular time-series data set"
        return None

    stime = data.getStartTime()
    interval=data.getTimeInterval()
    yt = data.getIterator()
    tf =  TimeFactory.getInstance()
    
    outint = tf.createTimeInterval(outint) ## force to TimeInterval (string incompatibilities)
    divunit= outint.getIntervalInMinutes(ref.getTimeWindow().getStartTime())
    origunit = data.getTimeInterval().getIntervalInMinutes( \
        ref.getTimeWindow().getStartTime())

    from jarray import zeros
    subunit = divunit/origunit

    while not yt.atEnd():
        if ((yt.getElement().getX()-offset*origunit) % divunit  < 1E-5):
            break   # Appropriate start time
        yt.advance()
        stime=stime+interval
    #
    if yt.atEnd(): raise "Resampling frequency too long. No points to be sampled"

    totalsize = data.size()
    orignewsize = totalsize - yt.getIndex()
    newsize = int((orignewsize + subunit - 1) / subunit)

    vals = zeros(newsize,'d')
    dflags = zeros(newsize,'i') ## changed to int[] from long[] for RTS compatibility
    i=0
    j=0
    while not yt.atEnd():
        el=yt.getElement()
        if i % subunit == 0:
            vals[j]=el.getY()
            dflags[j]=el.getFlag()
            j=j+1
        i=i+1
    #
        yt.advance()
    #
    if got_ref:
        refpath=ref.getPathname()
    else:
        refpath=Pathname.createPathname(data.getName())

    refpath.setPart(Pathname.E_PART,outint.toString())
    name=refpath.toString()
    rts = RegularTimeSeries(name,
                            stime,
                            outint,
                            vals,dflags,data.getAttributes())

    return rts
def apply(ts, function, ):
    """
    apply(ts, function)
        applies to this time series the function
         by evaluating the function at each point 
         the function takes DataSetElement and modifying it if necessary
         the conditional takes DataSetElement and returns 0/1 or False/True
        http://dsm2-vista.googlecode.com/svn/trunk/vista/doc/vista/set/DataSetElement.html
    """
    tsi = ts.getIterator()
    while not tsi.atEnd():
        el = tsi.getElement()
        function(el)
        tsi.advance()
def where(ts, conditional):
    """
    where(ts, conditional):
        returns a time series with a 1 where conditional evaluates to true and 0 otherwise
        conditional is a function that accepts a DataSetElement consisting of a x value, a y value and optionally a flag value
        it can return True/False or 0/1
    """
    xa = zeros(len(ts),'d')
    tsi = ts.getIterator()
    index=0
    while not tsi.atEnd():
        el = tsi.getElement()
        if conditional(el):
            xa[index]=0
        else:
            xa[index]=1
        tsi.advance()
        index=index+1
    return RegularTimeSeries(ts.getName(),str(ts.getStartTime()),str(ts.getTimeInterval()),xa)

def where_missing(rts,filter=Constants.DEFAULT_FLAG_FILTER):
    """
    where_missing(rts,filter=Constants.DEFAULT_FLAG_FILTER):
     returns a time series with a 1 where missing or not acceptable with filter
     and 0 otherwise
     where rts is a regular time series and
          filter is of type vista.set.ElementFilter e.g. Constants.DEFAULT_FLAG_FILTER
    """
    xa = zeros(len(rts),'d')
    rtsi = rts.getIterator()
    index=0
    while not rtsi.atEnd():
        el = rtsi.getElement()
        if filter.isAcceptable(el):
            xa[index]=0
        else:
            xa[index]=1
        rtsi.advance()
        index=index+1
    return RegularTimeSeries(rts.getName(),str(rts.getStartTime()),str(rts.getTimeInterval()),xa)
#

def taf2cfs(ref):
    """
    taf2cfs(ref):
    converts units of TAF to CFS for the given reference. Additionaly it
    also sets the units of the data set pointed to by the reference to CFS.
    This method does an exact conversion based on the number of days in
    the particular month
    """
    import java.lang; from java.lang import Math
    if hasattr(ref,'getData'):
        ds = ref.getData()
    else:
        ds = ref
    ds = ds.createSlice(ds.getTimeWindow())
    dsi = ds.getIterator()
    tifrom = ds.getTimeInterval()
    tiday = tifrom.create("1day")
    timonth = tifrom.create("1month")
    factor = (1000.0*43560)/(24*60*60.0)
    filter = Constants.DEFAULT_FLAG_FILTER
    tm = ds.getStartTime()
    tm2 = tm.create(tm)
    while not dsi.atEnd():
        e = dsi.getElement()
        tm = tm.create(Math.round(e.getX()));
        tm2 = tm.create(tm);
        tm2.incrementBy(tifrom,-1); # go back one interval
        nvals = tm2.getExactNumberOfIntervalsTo(tm,tiday);
        if filter.isAcceptable(e):
            e.setY(e.getY()*factor/nvals)
            dsi.putElement(e)
        dsi.advance()
    ds.getAttributes().setYUnits("CFS")
    return ds
#
def its2rts(its,tis=None):
    """
    its2rts(its,tw=None,tis=None):
    Converts irregular time series to regular time series with
    time interval.
    Input is irregular time series dataref or dataset,
    optional time interval as string. If not given, an
    estimate is made.
    """
    td_OK={'1MIN': 0, '5MIN': 1, '10MIN': 1, '15MIN': 2, '1HOUR': 5, '1DAY': 15}
    if not its:
        return None
    got_ref=False
    if isinstance(its,DataReference):
        its = its.getData()
        got_ref=True
    tw = its.getTimeWindow()
    sti = 0
    eti = len(its) - 1
    if not tis:
        # estimate time interval from average its intervals;
        # set an accepted discrepency beyond which a missing value
        # will be used
        aveIntvl = 0
        ctr = 0
        xArr = its.getXArray()
        for i in range(sti+1,eti):
            if xArr[i] - xArr[i-1] > 1500:
                # gap greater than 1 day, skip to improve estimate
                continue
            aveIntvl += xArr[i] - xArr[i-1]
            ctr += 1
        aveIntvl /= ctr
        if aveIntvl > 0.9 and aveIntvl < 1.1:
            tis = '1MIN'
        if aveIntvl > 4.9 and aveIntvl < 5.1:
            tis = '5MIN'
        elif aveIntvl > 9.5 and aveIntvl < 10.5:
            tis = '10MIN'
        elif aveIntvl > 13. and aveIntvl < 17.:
            tis = '15MIN'
        elif aveIntvl > 55. and aveIntvl < 65.:
            tis = '1HOUR'
        elif aveIntvl > 1420. and aveIntvl < 1460.:
            tis = '1DAY'
        else:
            print 'Unable to determine time interval in its2rts',
            return None
    ti = timeinterval(tis)
    st = time(tw.getStartTime().ceiling(ti))
    et = time(tw.getEndTime().floor(ti))
    # update dataset name with new E part
    parts = string.split(its.getName(), '/')
    if len(parts) == 8: parts[5] = tis.upper()
    name = string.join(parts, '/')
    nvals = st.getNumberOfIntervalsTo(et,ti)+1
    # make blank arrays of the y values and flags
    yvals = jarray.zeros(nvals,'d')
    flags = jarray.zeros(nvals,'i')
    # initialize loop values
    itrtr_its = its.getIterator()
    # get first time value in irregular time series
    curr_time_its = time(long(itrtr_its.getElement().getX()))
    # get starting time of regular time series
    curr_time_rts = time(st)
    # loop over rts to fill values in rts from either its
    # or missing values
    time_diff = 0
    index = 0
    while index < nvals:
        while curr_time_its < curr_time_rts \
            and abs(time_diff) > td_OK[tis] \
            and not itrtr_its.atEnd():
            itrtr_its.advance()
            curr_time_its = time(long(itrtr_its.getElement().getX()))
            time_diff = curr_time_rts.compare(curr_time_its)
        # if time of the its is "near" the expected time of rts,
        # use current data value of its; else, use missing value
        if abs(time_diff) > td_OK[tis]:
            use_val = Constants.MISSING_VALUE
            use_flag = FlagUtils.MISSING_FLAG
        else:
            use_val = itrtr_its.getElement().getY()
            use_flag = itrtr_its.getElement().getFlag()
        # set previous data value and flag value
        yvals[index] = use_val
        flags[index] = use_flag
        # increment current time value by regular interval
        curr_time_rts.incrementBy(ti)
        time_diff = curr_time_rts.compare(curr_time_its)
        index=index+1
    # create an attribute as clone of irregular time series
    attr = its.getAttributes().createClone()
    # change its type to regular
    attr.setType(DataType.REGULAR_TIME_SERIES)
    # create a new time series with values, flags and attr with
    # the correct start time and time interval
    rts = RegularTimeSeries(name,st,ti,yvals,flags,attr)
    if got_ref:
        return wrap_data(rts, "", "", name)
    else:
        return rts
#
def nearest_index(ds,timeinst,ndxHint=0):
    """
    nearest_index(ds, timeinst):
    Returns the nearest index of the dataset that includes timeinst,
    or None if the timeinst is not in the dataset. With optional ndxHint,
    start at that index.
    """
    if isinstance(ds, DataReference):
        ds = ds.getData()
    if isinstance(timeinst, str):
        timeinst = TimeFactory.getInstance().createTime(timeinst)
    tii = timeinst.getTimeInMinutes()
    if isinstance(ds,RegularTimeSeries):
        # get index by calculation, not search
        ndx = long((float(tii) - ds.getElementAt(0).getX()) / \
            float(ds.getTimeInterval().getIntervalInMinutes(ds.getStartTime())) + 0.5)
        #t = TimeFactory.getInstance().createTime(long(ds.getElementAt(ndx).getX()))
        return ndx
    if ndxHint >= 0 and ndxHint < len(ds):
        if long(ds.getElementAt(len(ds)-1).getX()) < tii: return None
        # check each time element for the first one equal to or greater than timeinst
        if long(ds.getElementAt(ndxHint).getX()) > tii: ndxStart = 0
        else: ndxStart = ndxHint
        ndxEnd = len(ds)
        for ndx in range(ndxStart,ndxEnd,1):
            if long(ds.getElementAt(ndx).getX()) >= tii:
                return ndx
        return None
    else:   # backwards
        if long(ds.getElementAt(0).getX()) > tii: return None
        ndxEnd = -1
        if long(ds.getElementAt(abs(ndxHint)).getX()) < tii or \
            abs(ndxHint) > len(ds): ndxStart = len(ds) - 1
        else: ndxStart = abs(ndxHint)
        for ndx in range(ndxStart,ndxEnd,-1):
            if long(ds.getElementAt(ndx).getX()) <= tii:
                return ndx
        return None
#
TF = TimeFactory.getInstance()
def merge_with_flags(ds1, ds2):
    """
    merge_with_flags(ds1,ds2)
    Merge dataset1 into dataset2 using flags.  Priority:
    FlagUtils.OK_FLAG, QUESTIONABLE_FLAG, MISSING_FLAG, REJECT_FLAG, UNSCREENED_FLAG
    If ds1Flag priority >= ds2Flag priority, use ds1 value;
    If ds1Flag priority < ds2Flag priority, use ds2 value.
    If a dataset does not have flags, UNSCREENED_FLAG is assumed.
    
    Return a list of merged elements from ds1 and ds2. 
    """
    # datasets should be similar, i.e. both RTS or both ITS
    ds1_rts = isinstance(ds1, RegularTimeSeries)
    ds2_rts = isinstance(ds2, RegularTimeSeries)
    if ds1_rts != ds2_rts:
        raise "Datasets must both be RTS or ITS timeseries for merge_with_flags."
    if ds1_rts and ds2_rts:
        if ds1.getTimeInterval().compare(ds2.getTimeInterval()) != 0 :
            raise "Incompatible time intervals for %s and %s" % (ds1.getName(), ds2.getName())
    # flag priorities
    pri_dict = {\
        FlagUtils.UNSCREENED_FLAG:1, \
        FlagUtils.REJECT_FLAG:2, \
        FlagUtils.MISSING_FLAG:3, \
        FlagUtils.QUESTIONABLE_FLAG:4, \
        FlagUtils.OK_FLAG:5\
        }
    if not ds1_rts:
        mY = []     # merged Y (value) list
        mX = []     # merged X (time) list
        mF = []     # merged flags list
        dsi1 = ds1.getIterator()
        dsi2 = ds2.getIterator()
        # perform merge
        while not (dsi1.atEnd() and dsi2.atEnd()):
            # get the data elements
            e1 = dsi1.getElement()
            e2 = dsi2.getElement()
            if not e2 or (e1 and e1.getX() < e2.getX()):
                # use e1 if e2 is at end, or e1 earliest
                mX.append(e1.getX())
                mY.append(e1.getY())
                mF.append(e1.getFlag())
                dsi1.advance()
            elif not e1 or (e2 and e2.getX() < e1.getX()):
                # use e2 if e1 is at end, or e2 earliest
                mX.append(e2.getX())
                mY.append(e2.getY())
                mF.append(e2.getFlag())
                dsi2.advance()
            else:
                # same time; use the better quality flag, or e1 if same quality
                e1fp = pri_dict[FlagUtils.getQualityFlag(e1)]
                e2fp = pri_dict[FlagUtils.getQualityFlag(e2)]
                if e1fp < e2fp:
                    mX.append(e2.getX())
                    mY.append(e2.getY())
                    mF.append(e2.getFlag())
                else:
                    mX.append(e1.getX())
                    mY.append(e1.getY())
                    mF.append(e1.getFlag())
                dsi1.advance()
                dsi2.advance()
#        print ds1.getName()
        return IrregularTimeSeries('', mX, mY, mF)
#
#
def isITS(dataset):
    """
    isITS(dataset)
    return True if dataset is Irregular Time Series,
    False if not
    """ 
    if dataset.getAttributes().getType() == DataType.IRREGULAR_TIME_SERIES:
        return True
    return False
##
def isRTS(dataset):
    """
    isRTS(dataset)
    return True if dataset is Regular Time Series,
    False if not
    """ 
    if dataset.getAttributes().getType() == DataType.REGULAR_TIME_SERIES:
        return True
    return False
##
def ds_add_flags(dataset):
    """
    ds_add_flags(dataset)
    Add UNSCREENED_FLAG to dataset that does not have any flags
    """
    if dataset.isFlagged(): return dataset
    # create copy of incoming dataset but with flags
    fa = jarray.zeros(len(dataset), 'i')
    if dataset.getAttributes().getType() == DataType.REGULAR_TIME_SERIES:    #RTS
        datasetFlagged = RegularTimeSeries(dataset.getName(), str(dataset.getStartTime()), \
                             str(dataset.getTimeInterval()), dataset.getYArray(), \
                             fa, dataset.getAttributes())
    else:   # ITS
        xa = jarray.zeros(len(dataset), 'd')
        ya = jarray.zeros(len(dataset), 'd')
        for i in range(len(dataset)):
            xa[i] = dataset[i].getX()
            ya[i] = dataset[i].getY()
        datasetFlagged = IrregularTimeSeries(
            dataset.getName(), xa, ya, fa, dataset.getAttributes())
    return datasetFlagged
#
#
def find_tidal_pt(dsref, dsrefSm=None):
    """
    find_tidal_pt(dataset-or-reference, [dsrefSm], [dsSmOffset]):
    Find tidal Peaks and Troughs within a dataset or reference (either RTS or ITS).
    Return a list of two irregular time series of the computed values;
    for each element the time is the time of the peak or trough;
    y is the value of the peak or trough. 
    Optional dsrefSm is a smoothed version of dsref, helpful for noisy
    timeseries such as observed EC.  The smoothed version should be padded
    and extended with Constants.MISSING_VALUE if needed.
    """
    if isinstance(dsref, DataReference):
        ds = dsref.getData()
#        ref = dsref
        isRef = True
    else:
        ds = dsref
#        ref = wrap_data(dsref)
        isRef = False
    # initialize
    # amount by which a potential peak or trough must be higher or lower than
    # values tiFB back and ahead
    FB_FACTOR = 1.03
    #tiFB = timeinterval('1HOUR_45MIN') # time to look forward/back for lesser/greater vals for P/T
    tiFB = timeinterval('2HOUR') # time to look forward/back for lesser/greater vals for P/T
    # use smoothed dataset if given
    if (dsrefSm):
        dsUse = dsrefSm
    else:
        dsUse = ds
    filter = Constants.DEFAULT_FLAG_FILTER
    BIG_NUMBER = 1.e8
    xpar = []   # x peak time array
    xtar = []   # x trough time array
    ypar = []   # y peak values array
    ytar = []   # y trough values array
    fpar = []   # peak flag quality array
    ftar = []   # trough flag quality array
    yp = -BIG_NUMBER
    yt = BIG_NUMBER
    dsi = dsUse.getIterator()
    e1 = dsi.getElement()
    x1 = e1.getX()
    t1 = TF.createTime(long(x1))
    xp = x1
    xt = x1
    dsi.resetIterator()
    ndx0 = 0; ndx2 = 0
#    now = datetime.now()
    while (t1 + tiFB).compare(dsUse.getEndTime()) < 0:
        #if dsi.getIndex() % (24*365) == 0: print t1, datetime.now() - now; now = datetime.now()
        # advance iterator to allow room for backward tiFB
        if (t1 - tiFB).compare(dsUse.getStartTime()) < 0:
            dsi.advance()
            e1 = dsi.getElement()
            x1 = e1.getX()
            t1 = TF.createTime(long(x1))
            continue
        # 1 or 2 hours behind
        #e0 = dsUse.getElementAt((t1 - tiFB).format())
        ndx0 = dsIndex(dsUse, (t1 - tiFB), ndx0)
        e0 = dsUse.getElementAt(ndx0)
        #  1 or 2 hours ahead
        #e2 = dsUse.getElementAt((t1 + tiFB).format())
        ndx2 = dsIndex(dsUse, (t1 + tiFB), ndx2)
        e2 = dsUse.getElementAt(ndx2)
        e0OK = filter.isAcceptable(e0)
        e1OK = filter.isAcceptable(e1)
        e2OK = filter.isAcceptable(e2)
        if e0OK and e1OK and e2OK:
            y0 = e0.getY()
            y1 = e1.getY()
            y2 = e2.getY()
            # Checks for Peak
            if y1 > yp and y1 > y0 * FB_FACTOR and y1 > y2 * FB_FACTOR:   # new peak
                    yp = y1
                    xp = x1
                    ndxp = dsi.getIndex()
            if yp != -BIG_NUMBER and x1 > xp and y0 > y1 > y2:
                # tiFB time after highest peak, on downslope: record highest peak
                if (dsrefSm):   # use y value from original dataset for peak
                    yp = ds.getElementAt(ndxp).getY()
                xpar.append(xp)
                ypar.append(yp)
#                fpar.append(FlagUtils.OK_FLAG)
                fpar.append(3)
                yp = -BIG_NUMBER
            # Checks for Trough
            if y1 < yt and y1 < y0 / FB_FACTOR and y1 < y2 / FB_FACTOR:   # new trough
                    yt = y1
                    xt = x1
                    ndxt = dsi.getIndex()
            if yt != BIG_NUMBER and x1 > xt and y0 < y1 < y2:
                # tiFB time after lowest trough, on upslope: record lowest trough
                if (dsrefSm):   # use y value from original dataset for trough
                    yt = ds.getElementAt(ndxt).getY()
                xtar.append(xt)
                ytar.append(yt)
#                ftar.append(FlagUtils.OK_FLAG)
                ftar.append(3)
                yt = BIG_NUMBER
        dsi.advance()
        e1 = dsi.getElement()
        x1 = e1.getX()
        t1 = TF.createTime(long(x1))
    #
    if len(ypar) == 0 or len(ytar) == 0: return None
    #
    pn = Pathname.createPathname(ds.getName())
    pn.setPart(Pathname.E_PART, 'IR-MON')
    pn = str(pn)
    attrds = ds.getAttributes().createClone()
    attrds.setType(DataType.IRREGULAR_TIME_SERIES)
    attrds.setYType('INST-VAL')
    dsp = IrregularTimeSeries(pn, xpar, ypar, fpar, attrds)
    dst = IrregularTimeSeries(pn, xtar, ytar, ftar, attrds)
    if isRef:
        return [wrap_data(dsp, filename=dsref.getFilename(), pathname=pn), \
                wrap_data(dst, filename=dsref.getFilename(), pathname=pn)]
    else:
        return [dsp, dst]
#
#
def calc_tidal_amplitude(dsAr):
    """
    calc_tidal_amplitude(dsAr)
    Calculate the tidal amplitude of the Peak and Trough datasets
    in the dsAr array.  Amplitude is defined as the difference between
    a Peak Y value and the immediate next Trough Y value.
    """
    if len(dsAr) != 2 or not isinstance(dsAr[0], IrregularTimeSeries) or \
        not isinstance(dsAr[0], IrregularTimeSeries):
        raise 'findTidalAmp: Argument must be array of two ITS datasets,\n' + \
        'containing Peak and Trough values.'
    # copy peak ds for amplitude ds
    dsAmp = IrregularTimeSeries(dsAr[0])
    dsAmp.setAttributes(dsAr[0].getAttributes())
    # max time forward to find trough corresponding to peak
    tiP2T = timeinterval('16HOUR')
    uId = DSSUtil.getUserId('datachecker')
    # loop through the Peaks; find the next Trough within several hours
    # difference is the Amplitude at the Peak time
    dsP = dsAr[0]
    dsT = dsAr[1]
    ndxT = 0
    for ndxP in range(dsAmp.size()):
        # peak
        elP = dsP.getElementAt(ndxP)
        elA = elP.createClone()
        yP = elP.getY()
        xP = elP.getX()
        tP = TF.createTime(long(xP))
        # now find nearest forward trough
        ndxT = dsIndex(dsT, tP, ndxT)
        if ndxT == None: break
        elT = dsT.getElementAt(ndxT)
        yT = elT.getY()
        xT = elT.getX()
        tT = TF.createTime(long(xT))
        if tT.compare(tP + tiP2T) <= 0:   # trough within time limit
            yA = yP - yT
            fA = FlagUtils.OK_FLAG
        else:
            yA = Constants.MISSING_VALUE
            fA = FlagUtils.MISSING_FLAG
        elA.setY(yA)
        FlagUtils.setQualityFlag(elA, fA, uId)
        dsAmp.putElementAt(ndxP, elA)
    pass
    for ndx in range(ndxP, len(dsP)):
        elA = dsAmp.getElementAt(ndx)
        elA.setY(Constants.MISSING_VALUE)
        FlagUtils.setQualityFlag(elA, FlagUtils.MISSING_FLAG, uId)
        dsAmp.putElementAt(ndx, elA)
    dsAmp.getAttributes().setYType('PER-AVER')
    return dsAmp
#
#
def tidal_avg(ref):
    """
    tidal_avg(ref):
    calculates the tidal average of the given reference and returns it in
    another reference. The returning reference can be treated just like any
    other reference.
    A tidal cycle is assumed to be 24hours_45 minutes but for 1hour time interval
    data it is rounded off to 25hours.
    """
    ti = ref.getTimeInterval();
    ti_tidal = TimeFactory.getInstance().createTimeInterval('24hour_45min')
    if ti_tidal.compare(ti) < 0:
        if hasattr(ref, 'getPathname'):
            path = ref.getPathname().toString()
        else:
            path = ref.getName()
            raise 'Time interval of ' + path + ' is greater than tidal cycle of ' + str(ti_tidal)
    num_intervals = (ti_tidal / ti - 1) / 2
    return mov_avg(ref, num_intervals, num_intervals)
#
def godin(ref):
    """
    Tidal average using a filter similar to the Godin 25-24-24 tidal average.
    Arguments:
    ref     reference to series to be averaged
    Return value:
    reference to averaged filter

    This is the Godin filter discussed in the G-model literature 
    and popular in the tidal literature (though
    not necessarily under the name Godin filter).

    For hourly data, the 25-24-24 filter is applied exactly 
   (25-hour moving average, then two 24-hour moving
    averages). When the input series uses time steps, 
    the lunar tidal cycle is assumed to be 24hours_45 minutes
    (e.g. 99 values for 15 min data).

    The mechanical difference between godin and tidal_avg, 
    is this filter removes 24-hour constituents more
    completely. A practical difference is this returned average 
    is period-centered. The shift operator can
    be easily used to adjust this to the statutory version 
    """
    isRef = False
    if isinstance(ref, DataReference):
        data = ref.getData()
        isRef = True
    else:
        data = ref
        ref = wrap_data(data)
        isRef = False
    
    # check for regular time series
    if not isinstance(data, RegularTimeSeries):
        print ref, " is not a regular time-series data set"
        return None  

    ti = ref.getTimeInterval();
    tf = TimeFactory.getInstance()
    ti_day = tf.createTimeInterval('24hours')
    ti_tidal = tf.createTimeInterval('24hour_45min')
    if ti_tidal.compare(ti) < 0 :
        raise 'Time interval of ' + ref.getPathname.toString() + ' is greater than tidal cycle of ' + ti_tidal
    #24.75 lunar constituents
    num_intervals = (ti_tidal / ti - 1) / 2;
    ma = MovingAverageProxy(ref, num_intervals, num_intervals)
    # 24 hour solar constituents
    num_intervals = (ti_day / ti) / 2
    ma = MovingAverageProxy(ma, num_intervals - 1, num_intervals)
    ma = MovingAverageProxy(ma, num_intervals, num_intervals - 1)
    if isRef:
        return ma
    else:
        return ma.getData()
def do_period_op(ds, ti, OPER):
    TF = TimeFactory.getInstance()
    dsi = ds.getIterator()
    if OPER == TimeSeriesMath.PERIOD_AVERAGE: perVal = 0.
    elif OPER == TimeSeriesMath.PERIOD_MAX: perVal = 1.e-10
    elif OPER == TimeSeriesMath.PERIOD_MIN: perVal = 1.e+10
    filter = Constants.DEFAULT_FLAG_FILTER
    e = dsi.getElement()
    xPrev = long(e.getX())
    nPers = 0
    nGood = 0
    xar = []
    yar = []
    flar = []
    tNext = TF.createTime(xPrev).ceiling(ti)
    xNext = tNext.getTimeInMinutes()
    while (not dsi.atEnd()):
        e = dsi.getElement()
        y = e.getY()
        x = long(e.getX())
        if x < xNext:
            if filter.isAcceptable(e):
                nGood += 1
                if OPER == TimeSeriesMath.PERIOD_AVERAGE: 
                    perVal += y
                elif OPER == TimeSeriesMath.PERIOD_MAX: 
                    if y > perVal: xMM = x; perVal = y
                elif OPER == TimeSeriesMath.PERIOD_MIN: 
                    if y < perVal: xMM = x; perVal = y
        else:
            nPers += 1
            if nGood == 0:
                # use "end of period" for time of Missing or Average Value 
                xar.append(float(xNext))
                perVal = Constants.MISSING_VALUE
#                flar.append(FlagUtils.MISSING_FLAG)
                flar.append(5)
            else:
                if OPER == TimeSeriesMath.PERIOD_AVERAGE:
                    xar.append(float(xNext))
                    perVal = perVal / nGood
                else:
                    # use its time instant for time of Max or Min Value
                    xar.append(float(xMM))
#                flar.append(FlagUtils.OK_FLAG)
                flar.append(3)
            yar.append(perVal)
            if OPER == TimeSeriesMath.PERIOD_AVERAGE:
                perVal = 0.
            elif OPER == TimeSeriesMath.PERIOD_MAX:
                perVal = 1.e-10
            elif OPER == TimeSeriesMath.PERIOD_MIN:
                perVal = 1.e+10
            tNext.incrementBy(ti)
            xPrev = xNext
            xNext = tNext.getTimeInMinutes()
            nGood = 0
        #
        dsi.advance()
    dsOp = IrregularTimeSeries(ds.getName(), xar, yar, flar, ds.getAttributes())
    if OPER == TimeSeriesMath.PERIOD_AVERAGE:
        dsOp.getAttributes().setYType('PER-AVER')
    elif OPER == TimeSeriesMath.PERIOD_MAX:
        dsOp.getAttributes().setYType('INST-VAL')
    elif OPER == TimeSeriesMath.PERIOD_MIN:
        dsOp.getAttributes().setYType('INST-VAL')
    return its2rts(dsOp,None,str(ti))
#
def per_oper(dsref, oper, interval):
    """
    per_oper(dataset-or-reference, operation, interval='1mon'):
    Period operations (average, max, min) given regular or irregular time series
    or data reference within each interval, over the entire TS. Always
    return a RTS.
    """
    ti = TimeFactory.getInstance().createTimeInterval(interval)
    #
    if isinstance(dsref, DataReference):
        ds = dsref.getData();
        isRef = True
    else:
        ds = dsref
        isRef = False
    # check for regular time series
    if isinstance(ds,RegularTimeSeries):
        isRTS = True
    else:
        isRTS = False
    #
    if oper[:2].lower() == 'av': 
        OPER = TimeSeriesMath.PERIOD_AVERAGE
    elif oper[:3].lower() == 'max':
        OPER = TimeSeriesMath.PERIOD_MAX
    elif oper[:3].lower() == 'min':            
        OPER = TimeSeriesMath.PERIOD_MIN
    else:
        raise 'Operation must be ave, max, or min.'
    if isRTS:
        if isRef:
            pn = Pathname.createPathname(dsref.getPathname())
            pn.setPart(Pathname.E_PART,interval)
            pn = str(pn)
            return wrap_data(TimeSeriesMath.doPeriodOperation(ds, ti, OPER), \
                             filename=dsref.getFilename(),pathname=pn)
        else:
            return TimeSeriesMath.doPeriodOperation(ds, ti, OPER)
    else:   # ITS
        if isRef:
            pn = Pathname.createPathname(dsref.getPathname())
            pn.setPart(Pathname.E_PART,interval)
            pn = str(pn)
            return wrap_data(do_period_op(ds, ti, OPER), \
                             filename=dsref.getFilename(),pathname=pn)
        else:
            return do_period_op(ds, ti, OPER)
#
def per_avg(ds, interval='1mon'):
    return per_oper(ds, 'ave', interval)
#
def per_max(ds, interval='1mon'):
    return per_oper(ds, 'max', interval)
#
def per_min(ds, interval='1mon'):
    return per_oper(ds, 'min', interval)

def per_max_time(ds, interval='1mon'):
    """
    Locates the maximum within an interval and returns an irregular time series with the max value
    and the time at which it occurs
    """
    return TimeSeriesMath.getPeriodMinMax(ds, TimeFactory.getInstance().createTimeInterval(interval), TimeSeriesMath.PERIOD_MAX)

def per_min_time(ds, interval='1mon'):
    """
    Locates the minimum within an interval and returns an irregular time series with the min value
    and the time at which it occurs
    """
    return TimeSeriesMath.getPeriodMinMax(ds, TimeFactory.getInstance().createTimeInterval(interval), TimeSeriesMath.PERIOD_MIN)
#
#def mov_avg(ts, backLength, forwardLength):
#    '''
#    mov_avg(ts,backLength,forwardLength):
#    Does a moving average of the time series (dataset or ref) with 
#    backLength previous points and forwardLength future points and
#    the present point. Returns the result as a new time series.
#    '''
#    if isinstance(ts, DataReference):
#        return ProxyFactory.createMovingAverageProxy(ts, backLength, forwardLength)
#    else:
#        return ProxyFactory.createMovingAverageProxy(wrap_data(ds), backLength, forwardLength).getData()
#  
def mov_avg(dsref, backLength, forwardLength):
    '''
    mov_avg(ts,backLength,forwardLength):
    Does a moving average of the time series (dataset or ref) with 
    backLength previous points and forwardLength future points and
    the present point. Returns the result as a new time series.
    '''
    filter = Constants.DEFAULT_FLAG_FILTER
    if isinstance(dsref, DataReference):
        ds = dsref.getData()
        ref = dsref
        isRef = True
    else:   # dataset
        if isinstance(dsref, RegularTimeSeries):    #RTS
            ds = dsref
        else:       # ITS
            ds = IrregularTimeSeries(dsref)
            ds.setAttributes(dsref.getAttributes())
        isRef = False
    # first fill a list with back and forward y values
    # then MAs are then computed by
    # adding the newest value, removing the oldest,
    # divide by the number of good values in the vector
    smallNumber = 1.e-10
    totLength = backLength + forwardLength + 1
    vecY = jarray.zeros(totLength, 'd')
    for ndx in range(totLength - 1):
        el1 = ds.getElementAt(ndx)
        if filter.isAcceptable(el1): vecY[ndx + 1] = el1.getY()
        else: vecY[ndx + 1] = smallNumber
    for ndx in range(ds.size()):
        el1 = ds.getElementAt(ndx)  # element now
        if ndx < backLength or ndx >= (ds.size() - forwardLength):
            el1.setY(Constants.MISSING_VALUE)
        else:   # compute the MA centered at ndx
            el2 = ds.getElementAt(ndx + forwardLength)    # farthest future element
            # update vector with new value
            if filter.isAcceptable(el2): vecY.append(el2.getY())
            else: vecY.append(smallNumber)
            vecY.pop(0)
            if vecY.count(smallNumber) < totLength:
                aveY = sum(vecY) / (totLength - vecY.count(smallNumber))
            else:
                aveY = Constants.MISSING_VALUE
            el1.setY(aveY)
        ds.putElementAt(ndx, el1)
    if isRef:
        return wrap_data(ds,filename=dsref.getFilename(),pathname=str(dsref.getPathname()))
    else:
        return ds
#  
def merge(args, filter=Constants.DEFAULT_FLAG_FILTER):
    """
    merge(args,filter=Constants.DEFAULT_FLAG_FILTER):
    where args is an array of data references or data sets
    & filter is the filter for accepting or rejecting values
    (type vista.set.ElementFilter)
    It returns a merged reference or a merged data set
    """
    if len(args) == 0: raise "Nothing to merge"
    if len(args) == 1: return args[0]
    refs = []
    any_ds = False # will become true if any array objects are data sets
    for arg in args:
        if hasattr(arg, 'getServername'): # arg is a data ref
            refs.append(arg)
        elif hasattr(arg, 'getPathname'): # arg is a data set
            any_ds = True
            refs.append(wrap_data(arg))
        else:
            raise "arg %s is neither a data reference or a data set" % str(arg)
        result = ProxyFactory.createMergingProxy(refs)
        result.setFilter(filter)
    if any_ds:
        return result.getData()
    else:
        return result
#
# a function to convert a given reference to monthly
def tsmax(ts):
    """
    The maximum of a time series
    """
    data = None
    if isinstance(ts, DataReference):
        data = ts.getData()
    else:   # dataset
        data = ts
    return Stats.max(data)
#
def tsmin(ts):
    """
    The minimum value of a time series
    """
    data = None
    if isinstance(ts, DataReference):
        data = ts.getData()
    else:   # dataset
        data = ts
    return Stats.min(data)
#
def avg(ts):
    """
    The average of a time series
    """
    data = None
    if isinstance(ts, DataReference):
        data = ts.getData()
    else:   # dataset
        data = ts
    return Stats.avg(data)
#
def sdev(ts):
    """
    The standard deviation of a time series
    """
    data = None
    if isinstance(ts, DataReference):
        data = ts.getData()
    else:   # dataset
        data = ts
    return Stats.sdev(data)
#
def total(ts):
    """
    The total of a time series
    """
    data = None
    if isinstance(ts, DataReference):
        data = ts.getData()
    else:   # dataset
        data = ts
    return Stats.total(data)
#
# Interpolating spline
# Eli Ateljevich 9/27/99
"""
  Interpolate a (reasonably) smooth time series
  into a smaller one covering the same time window.
  The spline is monotonicity-preserving
  and fourth order accurate except at boundaries

  Usage:          interpolate(ref,
                      outint = '15min',
                             offset = 0.0)
  where ref    is a time series or data reference (latter may not be supported in the future)
        outint is the interval of the interpolated output
    offset is the fraction of a time step ahead at which the series will be sampled.

  the reason for offset is to properly interpolate daily averages or other averages
  over an even number of values, which requires a 0.5 time step shift.

  For inst-val input, set offset =  0.0
  To interpolate input from a daily average, time stamped at the end of the day,
  use offset=0.5 and shift the output back one half day minus one time step

  eg. hourts=interpolate(ref,'1hour',0.5)<<11
      15minrts = interpolate(ref,'15min',0.5)<<47


   Reference: Huynh, HT <<Accurate Monotone Cubic Interpolation>>,
   SIAM J. Numer. Analysis V30 No. 1 pp 57-100 
   All equation numbers refer to this paper. The variable names are 
   also almost the same. Double letters like "ee" to indicate 
   that the subscript should have "+1/2" added to it.
"""
from vista.time import TimeFactory
from vista.set import DataReference, RegularTimeSeries, Constants
#
def minmod(a,b):
    if a*b > 0:
        if a>0:
            return min(a,b)
        else:
            return max(a,b);
    else:
        return 0.
#
def median3(a,b,c):
        l = list([a,b,c])
        l.sort()
        return l[1];
#
def spline(ref,outint,offset=0):
    '''
    Usage example:  interpolate(ref,outint = timeinterval("15min"),offset = 48)

    Interpolating spline
    Eli Ateljevich 9/27/99

    This functions is designed to map a coarser time series into a smaller one
    covering the same time window. The spline is monotonicity-preserving
    and fourth order accurate (except near boundaries)
    
    offset shifts the output as appropriate. Typically, offset will be
    zero for inst-val input. For per-ave input, offset will often be
    half of the output frequency. In the example above, NDO 
    input is treated as  "daily averaged". Output is in units of
    15minutes. Since there are are 96 15min samples per 24 hours
    offset = 0.5*96 = 48.
    
    Output is a regular time series (rts).
    
    Reference: Huynh, HT "Accurate Monotone Cubic Interpolation",
    SIAM J. Numer. Analysis V30 No. 1 pp 57-100 
    All equation numbers refer to this paper. The variable names are 
    also almost the same. Double letters like "ee" to indicate 
    that the subscript should have "+1/2" added to it.
    '''
    got_ref = 0
    if isinstance(ref, DataReference):
        data = ref.getData()
        got_ref = 1
    else:
        data = ref
        got_ref = 0
    # check for regular time series
    if not isinstance(data,RegularTimeSeries):
        print ref, " is not a regular time-series data set"
        return None
    
    yt = data.getIterator()
    div =  TimeFactory.getInstance().createTimeInterval(outint)
    nsub = data.getTimeInterval()/div
    from jarray import zeros
    vals = zeros(1+nsub*(data.size()-1),'d')
    vals=map(lambda x: -901.0, vals)
    vallength = len(vals)
    dflags = zeros(vallength,'l')
    lastone = vallength + 4*nsub  -1
    firstone = 4*nsub
 
    y4,y3,y2,y1,y0,ss3,ss2,ss1,ss0,s1,s0 = 0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.
    dd1,dd0,d3,d2,d1,d1,d0,e1,e0,ee2,ee1,ee0,eem1,df0,df1 = 0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.
    count = 0
    subcount = 0
    nextbad = 5
    atstart=1
    atend =0
    while not yt.atEnd() or nextbad>1: 
        if not (yt.atEnd() or atend==1):
                el = yt.getElement()     #Read new value, good or bad
                count = count + 1
                if Constants.DEFAULT_FLAG_FILTER.isAcceptable(el):           
                    y4 = el.getY();
                    #
                    ss3 = y4-y3
                    d3 = ss3 - ss2
                    ee2 = d3-d2
                    atstart = 0
                else:
                    if atstart == 0:
                        atend = 1
                        nextbad = nextbad - 1
                    #NoMissing = "Internal missing values not allowed."
                    #raise NoMissing
        else:
                nextbad = nextbad - 1;
        #
        # minmod-based estimates
        s2 = minmod(ss1,ss2)
        dd2 = minmod(d2,d3)
        e2 = minmod(ee1,ee2)

        #polynomial slopes
        dpp1adj1 = ss1 - dd1
        dpp0adj1 = ss0 + dd0

        t1 = minmod(dpp0adj1,dpp1adj1)
        dqq1adj1 = ss1 - minmod(d1+e1,d2-2*e2)   #Eq4.7a
        dqq0adj1 = ss0 + minmod(d0+2*e0,d1-e1)   #Eq4.7b
        ttilde1 = minmod(dqq0adj1,dqq1adj1)

        df1 = 0.5*(dqq0adj1 + dqq1adj1)          #First cut, Eq. 4.16
        tmin = min(0.,3*s1,1.5*t1,ttilde1)
        tmax = max(0.,3*s1,1.5*t1,ttilde1)

#    #If count == 3:         # have enough to make up boundary quantities
#    gamma = (ee1 - ee0)/4   #5.8, 
#        eex0 = ee0 - 4*gamma  #5.9 x is the boundary value
#        qqx = median3(ssm1,qqx,
        
        df1 = df1 + minmod(tmin-df1, tmax-df1)   #Revise,  Eq. 4.16
        
        for j in range(nsub):
            jfrac = (float(j)+offset)/ float(nsub)
            c0 = y0                  # from eq. 2 in the paper
            c1 = df0
            c2 = 3*ss0 - 2*df0 - df1
            c3 = df0 + df1 - 2*ss0
            if count > 4:
                if subcount <= lastone and subcount >= firstone:  
                    vals[subcount-4*nsub] = c0 + c1*jfrac +c2*jfrac**2 + c3*jfrac**3;
   
            subcount = subcount + 1

        # Now lag all data and estimates to make room for next time step
        y3,y2,y1,y0 = y4,y3,y2,y1
        ss2,ss1,ss0,ssm1 = ss3,ss2,ss1,ss0
        s1,s0 = s2,s1
        dd1,dd0 = dd2,dd1
        d2,d1,d0 = d3,d2,d1
        e1,e0 = e2,e1
        ee1,ee0,eem1 = ee2,ee1,ee0
        df0 = df1

        if not  yt.atEnd():
            yt.advance()
        #
    #
    #refpath=ref.getPathname()
    #refpath.setPart(Pathname.E_PART,outint)
    rts = RegularTimeSeries(data.getName(),
                data.getStartTime().toString(),
                outint,vals)
 
    return rts
#



# Simple linear interpolation
# Eli Ateljevich 9/27/99

# Linear interpolation of missing/bad values in a time series
# Usage example:  interpolate(ref,
#                     outint = timeinterval('15min'),
#                            )
# 

from vista.time import TimeFactory, Time, TimeInterval, TimeFormat
from vista.set import DataReference, Pathname, PathnamePredicate,\
     RegularTimeSeries,Constants
#

def linear(ref, myfilter=Constants.DEFAULT_FLAG_FILTER):
    '''
    Linearly interpolate missing data in a time series
    Eli Ateljevich 9/27/99

    '''
    if isinstance(ref, DataReference):
        data = ref.getData()
    else:
        data = ref
    # check for regular time series
    if not isinstance(data, RegularTimeSeries):
        print ref, " is not a regular time-series data set"
        return None

    yt = data.getIterator()
    st = data.getStartTime()
    et = data.getEndTime()
    ti = data.getTimeInterval()
    dsi = data.getIterator()
    n = st.getExactNumberOfIntervalsTo(et, ti) + 1

    from jarray import zeros
    vals = zeros(n, 'd')
    vals = map(lambda x:-901.0, vals)
    i = 0
    if myfilter.isAcceptable(dsi.getElement()):
        firstmissing = 0
    else:
        firstmissing = 1

    while not dsi.atEnd():
        el = dsi.getElement()
        if el:
            if myfilter.isAcceptable(el):
                vals[i] = el.getY()
                lasty = vals[i]
                lasti = i
            else:
                while not dsi.atEnd():
                    el = dsi.getElement()
                    if myfilter.isAcceptable(el):
                        nexty = el.getY()
                        nexti = i
                        if not firstmissing:    # no interpolation at begin or end of record
                            for ii in range(lasti, nexti):
                                vals[ii] = lasty + (ii - lasti) * (nexty - lasty) / (nexti - lasti)
                        vals[nexti] = nexty       # this one gets filled even at beginning of record.
                        firstmissing = 0
                        break
                    else:
                        i = i + 1
                        dsi.advance()
                #
        else:
            if not firstmissing: break
        #
        dsi.advance()
        i = i + 1
    # end while
    rts = RegularTimeSeries(data.getName(),
                data.getStartTime().toString(),
                data.getTimeInterval().toString(), vals)

    return rts

#
#def testlinear():
#    from vutils import opendss, findpath, timewindow, tabulate
#    infile='h:/data/dss/IEP/hydro.dss'
#    inpath='/RLTM\+CHAN/RSAC155/FLOW//1HOUR/CDEC/'
#    tw_str='16JAN1999 1200 - 21JAN1999 1600'
#    g=opendss(infile)
#    ref = findpath(g,inpath)[0]
#    ref = DataReference.create(ref,timewindow(tw_str))
#    rts=interplinear(ref)
#    tabulate(rts,ref.getData())



def testspline():
    from vutils import opendss, findpath, timewindow, tabulate
    infile='/delta/data/dss/dayflo.dss'
    inpath='/DELTA/OUT/FLOW//1day//'
    tw_str='01JAN1999 0000 - 30JUN1999 2400'
    g=opendss(infile)
    ref = findpath(g,inpath)[0]
    ref = DataReference.create(ref,timewindow(tw_str))
    rts=interpolate(ref,outint='15min',offset=48)
    tabulate(rts)
#
def dsIndex(ds,timeinst,ndxHint=0):
    """
    dsIndex(ds, timeinst):
    Returns the nearest index (long) of the dataset that includes timeinst (str),
    or None if the timeinst is not in the dataset. With optional ndxHint,
    start at that index.
    """
    if isinstance(ds, DataReference):
        ds = ds.getData()
    if isinstance(timeinst, str):
        timeinst = TimeFactory.getInstance().createTime(timeinst)
    tii = timeinst.getTimeInMinutes()
    if isinstance(ds,RegularTimeSeries):
        # get index by calculation, not search
        ndx = long((float(tii) - ds.getElementAt(0).getX()) / \
            float(ds.getTimeInterval().getIntervalInMinutes(ds.getStartTime())) + 0.5)
        #t = TimeFactory.getInstance().createTime(long(ds.getElementAt(ndx).getX()))
        return ndx
    if ndxHint >= 0 and ndxHint < len(ds):
        if long(ds.getElementAt(len(ds)-1).getX()) < tii: return None
        # check each time element for the first one equal to or greater than timeinst
        if long(ds.getElementAt(ndxHint).getX()) > tii: ndxStart = 0
        else: ndxStart = ndxHint
        ndxEnd = len(ds)
        for ndx in range(ndxStart,ndxEnd,1):
            if long(ds.getElementAt(ndx).getX()) >= tii:
                return ndx
        return None
    else:   # backwards
        if long(ds.getElementAt(0).getX()) > tii: return None
        ndxEnd = -1
        if long(ds.getElementAt(abs(ndxHint)).getX()) < tii or \
            abs(ndxHint) > len(ds): ndxStart = len(ds) - 1
        else: ndxStart = abs(ndxHint)
        for ndx in range(ndxStart,ndxEnd,-1):
            if long(ds.getElementAt(ndx).getX()) <= tii:
                return ndx
        return None
#
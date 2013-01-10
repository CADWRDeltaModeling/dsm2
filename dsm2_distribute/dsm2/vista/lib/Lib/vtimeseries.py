import jarray,string
from java.lang import Math
from jarray import zeros, array
from string import split, strip, lower
from vista.time import TimeFactory
from vista.set import ProxyFactory, MovingAverageProxy,\
     DataReference, RegularTimeSeries, Constants,\
     Pathname, DataType
from vdss import wrap_data
#
def timewindow(twstr):
    """
    timewindow(tm)
    creates a time window object from a string
    """
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
def timeinterval(tmistr):
    """
    timeinterval(tmistr):
    creates a time interval object for the given time interval
    in the format of #interval where # is a an integer and
    interval is a string such as min, hour, day, month, year
    """
    return TimeFactory.getInstance().createTimeInterval(tmistr);
#
def merge(args,filter=Constants.DEFAULT_FLAG_FILTER):
    """
    merge(args,filter=Constants.DEFAULT_FLAG_FILTER):
    where args is an array of data references or data set
    & filter is the filter for accepting or rejecting values (type vista.set.ElementFilter)
    It returns a merged reference if given an array of data references or
    a merged data set if given an array of data sets.
    
    """
    from vista.set import MovingAverageProxy, ProxyFactory
    if len(args) == 0: raise "Nothing to merge"
    if len(args) == 1: return args[0]
    refs = []
    all_ds = 1 # will remain true if all things are data sets
    for arg in args:
	if hasattr(arg,'getServername'):
	    all_ds = 0
	    refs.append(arg)
	elif hasattr(arg,'getTimeInterval'):
	    refs.append(wrap_data(arg))
	else:
	    raise "arg %s is neither a reference or a data set"%str(arg)
    result = ProxyFactory.createMergingProxy(refs)
    result.setFilter(filter)
    if all_ds == 1:
	return result.getData()
    else:
	return result
#
def interpolate(ref,tm_intvl='1day',flat=1):
    """
    interpolate(ref,tm_intvl='1day',flat=1):
    generates from given data which may be reference or regular time series
    either a referene or time series depending upon the input. The generated
    data interploates using values of input creating a data set of interval
    smaller than given data.
    For right now only flat = 1 works but someday we'll have the linear
    interpolate working as well.

    Usage:
    ref1 = interpolate(ref_input,'1day')
    OR
    ds1 = interpolate(ref_input.getData(),'1day')
    """
    got_ref = 0
    if isinstance(ref, DataReference):
        ds = ref.getData();
        got_ref = 1
    else:
        ds = ref
        got_ref = 0
    # check for regular time series
    if not isinstance(ds,RegularTimeSeries):
        print ref, " is not a regular time-series data set"
        return None
    tm = TimeFactory.getInstance().createTime(0);
    ti = ds.getTimeInterval();
    tiday = ti.create(tm_intvl)
    if ti.compare(tiday) < 0 :
	raise repr(ref.getName()) + " : has interval greater than " + repr(tiday)
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
            raise "Whoa! Don't have exact number of intervals from " + repr(tm2) + \
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
    outint  (string) time interval for resampling. Must be courser than that of ref
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
def toreg(irts,tw=None,ti='1hour'):
    """
    toreg(irts,tw=None,ti='1hour'):
    Converts irregular time series to regular time series with
    a time window and time interval.
    """
    ti = vutils.timeinterval(ti)
    if isinstance(irts,vutils.DataReference):
	irts = irts.getData() # initialize time window accurately
    if tw == None:
	tw = irts.getTimeWindow()
    else:
	tw = vutils.timewindow(tw)
    st = vutils.time(tw.getStartTime().ceiling(ti))
    et = vutils.time(tw.getEndTime().floor(ti))
    #print 'Start time: ',st
    #print 'End time: ',et
    nvals = st.getNumberOfIntervalsTo(et,ti)+1
    # set the pathname's F Part to -REG
    path = irts.getName()
    parts = string.split(path,'/')
    if len(parts) == 8: parts[6] = parts[6]+'-REG'
    name = string.join(parts,'/')
    #make arrays of the y values and flags
    yvals = jarray.zeros(nvals,'d')
    flags = jarray.zeros(nvals,'i')
    #initialize loop values
    index=0
    iterator = irts.getIterator()
    last_val = vutils.Constants.MISSING_VALUE
    last_flag = 0
    # get first time value in irregular time series
    next_time = vutils.time(long(iterator.getElement().getX()))
    # get starting time of regular time series
    time_val = vutils.time(st)
    # loop to fill values
    # loop takes care of filling the regular time series
    # with last values and flags.
    while index < nvals:
	# if time value of rts is >= irts then update values
	if not iterator.atEnd() and time_val.compare(next_time) >= 0:
	    # initialize last val and last flag value
	    last_val = iterator.getElement().getY()
	    last_flag = iterator.getElement().getFlag()
	    # advance by one & update next time value
	    iterator.advance()
	    # if iterator hasn't hit its end set the next_time value
	    if not iterator.atEnd():
		next_time = vutils.time(long(iterator.getElement().getX()))
	# keep filling with the last value and flag
	yvals[index] = last_val
	flags[index] = last_flag
	# increment current time value by regular interval
	time_val.incrementBy(ti)
	index=index+1
    # create an attribute as clone of irregular time series
    attr = irts.getAttributes().createClone()
    # change its type to regular
    attr.setType(vutils.DataType.REGULAR_TIME_SERIES)
    # create a new time series with values, flags and attr with
    # the correct start time and time interval
    rts = vutils.RegularTimeSeries(name,str(st),str(ti),yvals,flags,attr)
    return rts
#

import jarray,vutils,string
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
def mkirts():
    timea = ['01jan1990 0333','05jan1990 0733','06jan1990 0456']
    vala = [1,20,300]
    flaga = [0,0,0]
    for i in range(len(timea)):
	timea[i] = vutils.time(timea[i])
    
    irts = vutils.IrregularTimeSeries('/a/b/c/d/e/f/',timea,vala,flaga,None)
    return irts
def test():
    x = mkirts()
    rx0 = toreg(x,'01jan1990 0100 - 08jan1990 0100','15min')
    rx1 = toreg(x,'01jan1990 0100 - 08jan1990 0100','1hour')
    rx2 = toreg(x,'01jan1990 0100 - 08jan1990 0100','1day')
    rx3 = toreg(x,'01jan1990 0100 - 08jan1990 0100','10min')
    vutils.tabulate(rx0,rx1,rx2,rx3)

import jarray,vutils,string
def mkirts():
    timea = ['01jan1990 0333','05jan1990 0733','06jan1990 0456']
    vala = [1,20,300]
    flaga = [0,0,0]
    for i in range(len(timea)):
	timea[i] = vutils.time(timea[i])
    
    irts = vutils.IrregularTimeSeries('/a/b/c/d/e/f/',timea,vala,flaga,None)
    return irts
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
    path = irts.getName()
    parts = string.split(path,'/')
    if len(parts) == 8: parts[6] = parts[6]+'-REG'
    name = string.join(parts,'/')
    #print name
    yvals = jarray.zeros(nvals,'d')
    flags = jarray.zeros(nvals,'i')
    index=0
    iterator = irts.getIterator()
    last_val = vutils.Constants.MISSING_VALUE
    last_flag = 0
    # get first time value in irregular time series
    next_time = vutils.time(long(iterator.getElement().getX()))
    # get starting time of regular time series
    time_val = vutils.time(st)
    # loop to fill values
    while index < nvals:
	#print index,time_val
	#print next_time,last_val,last_flag
	# if time value of rts is >= irts then update values
	if not iterator.atEnd() and time_val.compare(next_time) >= 0:
	    last_val = iterator.getElement().getY()
	    last_flag = iterator.getElement().getFlag()
	    # advance by one & update next time value
	    iterator.advance()
	    if not iterator.atEnd():
		next_time = vutils.time(long(iterator.getElement().getX()))
	yvals[index] = last_val
	flags[index] = last_flag
	time_val.incrementBy(ti)
	index=index+1
    attr = irts.getAttributes().createClone()
    attr.setType(vutils.DataType.REGULAR_TIME_SERIES)
    rts = vutils.RegularTimeSeries(name,str(st),str(ti),yvals,flags,attr)
    return rts
#
def test():
    x = mkirts()
    rx0 = toreg(x,'01jan1990 0100 - 08jan1990 0100','15min')
    rx1 = toreg(x,'01jan1990 0100 - 08jan1990 0100','1hour')
    rx2 = toreg(x,'01jan1990 0100 - 08jan1990 0100','1day')
    rx3 = toreg(x,'01jan1990 0100 - 08jan1990 0100','10min')
    vutils.tabulate(rx0,rx1,rx2,rx3)

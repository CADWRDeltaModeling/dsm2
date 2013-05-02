import vutils
def dxcgen(ds,tm_intvl='1day'):
    tm = vutils.TimeFactory.getInstance().createTime(0);
    ti = ds.getTimeInterval();
    ti_final = ti.create(tm_intvl)
    if ti.compare(ti_final) < 0 :
	raise repr(ref.getName()) + " : has interval greater than " + repr(ti_final)
    elif ti.compare(ti_final) == 0:
	    return ds
    filter = vutils.Constants.DEFAULT_FLAG_FILTER
    yvals = [] # initialize empty array
    nvals = 0
    for e in ds:
        tm = tm.create(long(round(e.x)))
        tm2 = tm.create(tm)
        tm2.incrementBy(ti,-1) # go back one month
        try:
            nvals = tm2.getExactNumberOfIntervalsTo(tm,ti_final)
        except:
            raise "Whoa! Don't have exact number of intervals from " + repr(tm2) + \
                  " to " + repr(tm) + " using " + repr(ti_final)
        if filter.isAcceptable(e): 
            val = e.y
        else:
            val = vutils.Constants.MISSING_VALUE
	days_open = val
        for i in range(nvals):
	    if i < days_open:
		yvals.append(0)
	    else:
		yvals.append(1)
    #
    # create a regular time series to store this data
    stime = ds.getStartTime().create(ds.getStartTime())
    stime.incrementBy(ti,-1)
    stime.incrementBy(ti_final)
    rts = vutils.RegularTimeSeries(ds.getName(), str(stime), repr(ti_final), yvals)
    return rts



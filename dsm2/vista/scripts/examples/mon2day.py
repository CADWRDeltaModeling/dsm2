# a function to convert a given reference to monthly
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
    if ti.compare(tiday) <= 0 :
        raise repr(ref.getPathname()) + " : has interval greater than " + repr(tiday)
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

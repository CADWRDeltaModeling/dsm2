__doc__ ="""

"""
from vista.time import TimeFactory
from vista.set import ProxyFactory, TimeSeriesMath, \
     MovingAverageProxy, Constants, Stats, DataReference, \
     RegularTimeSeries
from vdss import wrap_data
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
    if ti_tidal.compare(ti) < 0 :
	if hasattr(ref,'getPathname'):
	    path = ref.getPathname().toString()
	else:
	    path = ref.getName()
        raise 'Time interval of ' + path + ' is greater than tidal cycle of ' + str(ti_tidal)
    num_intervals = (ti_tidal/ti-1)/2
    return mov_avg(ref,num_intervals,num_intervals)
#
def godin(ref):
    """
    Tidal average using a fileter similar to the Godin 25-24-24 tidal average.
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

    The mechanical difference between tide_avg and tidal_avg, 
    is that this filter removes 24-hour constituents more
    completely. A practical difference is that this returned average 
    is period-centered. The shift operator can
    be easily used to adjust this to the statutory version 
    """
    got_ref = 0
    if isinstance(ref, DataReference):
        data = ref.getData()
        got_ref = 1
    else:
        data = ref
	ref = wrap_data(data)
        got_ref = 0
    # check for regular time series
    if not isinstance(data,RegularTimeSeries):
        print ref, " is not a regular time-series data set"
        return None  

    ti = ref.getTimeInterval();
    tf = TimeFactory.getInstance()
    ti_day = tf.createTimeInterval('24hours')
    ti_tidal = tf.createTimeInterval('24hour_45min')
    if ti_tidal.compare(ti) < 0 :
        raise 'Time interval of ' + ref.getPathname.toString() + ' is greater than tidal cycle of ' + ti_tidal
    #24.75 lunar constituents
    num_intervals = (ti_tidal/ti - 1)/2;
    ma = MovingAverageProxy(ref,num_intervals,num_intervals)
    # 24 hour solar constituents
    num_intervals = (ti_day/ti)/2
    ma = MovingAverageProxy(ma,num_intervals-1,num_intervals)
    ma = MovingAverageProxy(ma,num_intervals,num_intervals-1)
    if got_ref:
	return ma
    else:
	return ma.getData()
#
def per_avg(ds,interval='1mon'):
    """
    per_avg(dataset, interval='1mon'):
    Period averages given regular time series or data reference to the given interval
    """
    if hasattr(ds,'getServername'):
        ti = TimeFactory.getInstance().createTimeInterval(interval)
        return ProxyFactory.createPeriodOperationProxy(ProxyFactory.PERIOD_AVERAGE, ds, ti);
    elif hasattr(ds,'getTimeInterval'):
        ti = TimeFactory.getInstance().createTimeInterval(interval)
        return TimeSeriesMath.doPeriodOperation(ds,ti,
                                                TimeSeriesMath.PERIOD_AVERAGE )
    else:
	return None
#
def per_max(ds,interval='1mon'):
    """
    per_max(dataset, interval='1mon'):
    Period maximums for a given regular time series or data reference for the given interval
    """
    if hasattr(ds,'getServername'):
        ti = TimeFactory.getInstance().createTimeInterval(interval)
        return ProxyFactory.createPeriodOperationProxy(ProxyFactory.PERIOD_MAX, ds, ti);
    elif hasattr(ds,'getTimeInterval'):
        ti = TimeFactory.getInstance().createTimeInterval(interval)
        return TimeSeriesMath.doPeriodOperation(ds,ti,
                                                TimeSeriesMath.PERIOD_MAX )
    else:
        return None
#
def per_min(ds,interval='1mon'):
    """
    per_min(dataset, interval='1mon'):
    Period maximums for a given regular time series or data reference for the given interval
    """
    if hasattr(ds,'getServername'):
        ti = TimeFactory.getInstance().createTimeInterval(interval)
        return ProxyFactory.createPeriodOperationProxy(ProxyFactory.PERIOD_MIN, ds, ti);
    elif hasattr(ds,'getTimeInterval'):
        ti = TimeFactory.getInstance().createTimeInterval(interval)
        return TimeSeriesMath.doPeriodOperation(ds,ti,
                                                TimeSeriesMath.PERIOD_MIN )
    else:
        return None
#
#
def mov_avg(ds, backLength, forwardLength):
    '''
    mov_avg(ds,backLength,forwardLength):
    Does a moving average of the time series with backLength previous points
    and forwardLength future points and the present point.
    Returns the result as a new time series.
    '''
    if hasattr(ds,'getServername'):
        return ProxyFactory.createMovingAverageProxy(ds, backLength, forwardLength)
    elif hasattr(ds,'getTimeInterval'):
        return ProxyFactory.createMovingAverageProxy(wrap_data(ds), backLength, forwardLength).getData()
    else:
        return None
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
# a function to convert a given reference to monthly
def tsmax(ts):
    """
    The maximum of a time series
    """
    data = None
    if hasattr(ts,'getServername'):
	data = ts.getData()
    elif hasattr(ts,'getTimeInterval'):
	data = ts
    else:
        return None
    return Stats.max(data)
#
def tsmin(ts):
    """
    The minimum value of a time series
    """
    data = None
    if hasattr(ts,'getServername'):
	data = ts.getData()
    elif hasattr(ts,'getTimeInterval'):
	data = ts
    else:
        return None
    return Stats.min(data)
#
def avg(ts):
    """
    The average of a time series
    """
    data = None
    if hasattr(ts,'getServername'):
	data = ts.getData()
    elif hasattr(ts,'getTimeInterval'):
	data = ts
    else:
        return avg(ts)
    return Stats.avg(data)
#
def sdev(ts):
    """
    The standard deviation of a time series
    """
    data = None
    if hasattr(ts,'getServername'):
	data = ts.getData()
    elif hasattr(ts,'getTimeInterval'):
	data = ts
    else:
        return avg(ts)
    return Stats.sdev(data)
#
def total(ts):
    """
    The total of a time series
    """
    data = None
    if hasattr(ts,'getServername'):
	data = ts.getData()
    elif hasattr(ts,'getTimeInterval'):
	data = ts
    else:
        return total(ts)
    return Stats.total(data)
#

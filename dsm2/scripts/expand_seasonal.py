
def expand_seasonal(seasonal, tw):
    """
    Given an input time series with seasonal (monthly) values,
    create a larger monthly time series
    recycling the seasonal values over and over.
    Arguments:
        seasonal: input time series with seasonal 
        tw: time window of output
    """
    from vtimeseries import time,timewindow,timeinterval
    import string
    from vista.set import Constants,RegularTimeSeries
    ti=timeinterval("1MON")
    seasonalVal={}
    filter=Constants.DEFAULT_FLAG_FILTER
    for el in seasonal:
        if el and filter.isAcceptable(el):
            xval = el.getX()
            xdate=time(long(xval))
#            xdate=time(long(xval)-1)
#            xdate=xdate.floor(ti)
            xstr=xdate.toString()
            yval = el.getY()
            month = string.lower(xstr[2:5])
            if seasonalVal.has_key(month):
                raise "Ambiguous seasonal values for month of: %s " % month
            seasonalVal[month]=yval
        #
    #
    if len(seasonalVal) != 12:
        raise "Not all seasonal values found"
    
    start=time(tw.getStartTime().getTimeInMinutes())
    end=tw.getEndTime()
    n=start.getNumberOfIntervalsTo(end,ti)
    from jarray import zeros
    y=zeros(n+1,'d')
    i=0

    while(start.getTimeInMinutes() <= end.getTimeInMinutes() ):
       xval=start.getTimeInMinutes()
       xdate=time(xval)
       xstr=xdate.toString()
       month=string.lower(xstr[2:5])
       y[i]=seasonalVal[month]
       start.incrementBy(ti)
       i=i+1
    out=RegularTimeSeries(seasonal.getName(),tw.getStartTime().toString(),
                          timeinterval("1MON").toString(),y,None,seasonal.getAttributes())
    return out

def prep_dicu(infile,outfile,tw):
    from vdss import opendss,writedss,findpath
    if (not infile): raise TypeError("infile was None")
    if (not outfile): raise TypeError("outfile was None")    
    if (not infile.endswith("dss") and outfile.endswith("dss")):
        raise "infile and outfile arguments must be a dss file"
    g=opendss(infile)
    all=findpath(g,"///DRAIN-EC////")
    for i in range(len(all)):
        ts=all[i].getData()
        s=expand_seasonal(ts,tw)
        writedss(outfile,all[i].getPathname().toString(),s)

def main():
    from vdisplay import tabulate
    from vdss import opendss,findpath,writedss
    from vtimeseries import timewindow
    import config
    infile=config.getAttr('DICUFILE-ECS')
    outfile=config.getAttr('DICUFILE-ECE')
    tws=config.getAttr('START_DATE')+ " 0000 - " + config.getAttr('END_DATE') + " 2400"
    tw=timewindow(tws)

    #tabulate(s)  # tabulate last one to check the conversion
    #tabulate(ts)

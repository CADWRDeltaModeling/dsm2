
import interpolator.ConservativeSpline
from vista.set import Constants,RegularTimeSeries
from vista.time import TimeInterval,Time
from vdisplay import tabulate
from vtimeseries import time,timeinterval

splineparam=20

def conserveSpline(avedata, interval):
    if not isinstance(interval,TimeInterval):
        interval=timeinterval(interval)
    start=avedata.getStartTime()
    try:
       nout=start.getExactNumberOfIntervalsTo(avedata.getEndTime(),interval)+1
    except:
       nout=start.getNumberOfIntervalsTo(avedata.getEndTime(), interval)

    n=avedata.size()
    from jarray import zeros
    x=zeros(n+1,'d')
    y=zeros(n+1,'d')
    i=0
    start2=time(start.toString())    # Pain in the neck way to clone
    rev=timeinterval("-"+interval.toString())
    start2.incrementBy(avedata.getTimeInterval(),-1)
    x[0]=start2.getTimeInMinutes()
    for el in avedata:
        x[i+1]=el.getX()
        y[i]=el.getY()
        i=i+1
        if not Constants.DEFAULT_FLAG_FILTER.isAcceptable(el):
            raise "Missing or bad data not allowed in conservative spline"
    p=map(lambda x: splineparam, y)
    spline=interpolator.ConservativeSpline(x,y,p,y[0],y[n-1])
    ynew=zeros(nout,'d')
    newseries=RegularTimeSeries('/Smooth/Data///'+interval.toString()+'//',start.toString(),interval.toString(),ynew)
    dsi=newseries.getIterator()
    while not dsi.atEnd():
        el=dsi.getElement()
        if spline.rh2val(el.getX()) < 0:
            # useY=y(el.getX())
            useY=spline.rh2val(el.getX())
        else:
            useY=spline.rh2val(el.getX())
        #
        el.setY(useY)
        dsi.putElement(el)
        dsi.advance()

    #tabulate(newseries,avedata)
    return newseries

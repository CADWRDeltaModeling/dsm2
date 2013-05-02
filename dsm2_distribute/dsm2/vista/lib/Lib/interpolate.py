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
from vista.time import TimeFactory, Time, TimeInterval, TimeFormat
from vista.set import DataReference, Pathname, PathnamePredicate,\
     RegularTimeSeries,Constants
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

#	#If count == 3:         # have enough to make up boundary quantities
#	gamma = (ee1 - ee0)/4   #5.8, 
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
#		             outint = timeinterval('15min'),
#                            )
# 

from vista.time import TimeFactory, Time, TimeInterval, TimeFormat
from vista.set import DataReference, Pathname, PathnamePredicate,\
     RegularTimeSeries,Constants
#

def linear(ref,myfilter=Constants.DEFAULT_FLAG_FILTER):
    '''
    Linearly interpolate missing data in a time series
    Eli Ateljevich 9/27/99

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
    st = data.getStartTime()
    et = data.getEndTime()
    ti = data.getTimeInterval()
    dsi = data.getIterator()
    n = st.getExactNumberOfIntervalsTo(et,ti) + 1

    from jarray import zeros
    vals = zeros(n,'d')
    vals=map(lambda x: -901.0, vals)
    i=0
    if myfilter.isAcceptable(dsi.getElement()):
        firstmissing=0
    else:
        firstmissing=1

    while not dsi.atEnd():
         el=dsi.getElement()
         if el:
             if myfilter.isAcceptable(el):
                 vals[i]=el.getY()
                 lasty=vals[i]
                 lasti=i
             else:
                 while not dsi.atEnd():
                     el=dsi.getElement()
                     if myfilter.isAcceptable(el):
                         nexty=el.getY()
                         nexti=i
                         if not firstmissing:    # no interpolation at begin or end of record
                             for ii in range(lasti,nexti):
                                 vals[ii]=lasty + (ii-lasti)*(nexty-lasty)/(nexti-lasti)
                         vals[nexti]=nexty       # this one gets filled even at beginning of record.
                         firstmissing=0
                         break
                     else:
                         i=i+1
                         dsi.advance()
                        
                 #
         else:
             if not firstmissing: break
             #
         #
         dsi.advance()
         i=i+1
    #

    rts = RegularTimeSeries(data.getName(),
			    data.getStartTime().toString(),
			    data.getTimeInterval().toString(),vals)
 
    return rts
#
def testlinear():
    from vutils import opendss, findpath, timewindow, tabulate
    infile='h:/data/dss/IEP/hydro.dss'
    inpath='/RLTM\+CHAN/RSAC155/FLOW//1HOUR/CDEC/'
    tw_str='16JAN1999 1200 - 21JAN1999 1600'
    g=opendss(infile)
    ref = findpath(g,inpath)[0]
    ref = DataReference.create(ref,timewindow(tw_str))
    rts=interplinear(ref)
    tabulate(rts,ref.getData())



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



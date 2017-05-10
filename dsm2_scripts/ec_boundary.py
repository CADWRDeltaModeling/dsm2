from vista.set import RegularTimeSeries, Constants
from vtimeseries import timewindow
from jarray import zeros
from math import sqrt,exp
def gCalc(ndo,beta,g0=None):
    """ Calculates antecedent outflow from a stream of ndo
        Arguments:
          ndo: a regular time series. Must be 15MIN, 1HOUR. Thus, NDO has been interpolated first.
          g0:  initial condition. If g0 is not given it is equal to ndo at the first time step.
          beta: g-model parameter.
        Output:
          g:  a regular time series, same sampling rate as input
              with the same start time as ndo
    """

    ti = ndo.getTimeInterval()
    if not ((ti.toString() == "15MIN") | (ti.toString() == "1HOUR")):
        raise "NDO time step must be 15MIN or 1HOUR."
    dt=1
    nstep=ndo.size()
    g=zeros(nstep,'d')
    
    if ti.toString() == "15MIN":
        dt=0.25
    g=zeros(nstep,'d')
    g=map(lambda x: -901.0, g)
    beta=beta*24*365
    div2dt=2*beta/dt
    dsi=ndo.getIterator()
    q0 = dsi.getElement().getY()
    # Set initial condition
    if g0==None:
        g[0]= q0        # ???
    else:
        g[0]=g0

    # Loop through and integrate gmodel using trapazoidal.
    atend=0
    i=1
    dsi.advance()

    while atend == 0:
        el=dsi.getElement()
        if el and Constants.DEFAULT_FLAG_FILTER.isAcceptable(el):
            q=el.getY()
            qterm=(q-div2dt)
            g[i]= 0.5*( qterm + sqrt(qterm*qterm + 4*g[i-1]*(q0-g[i-1]+div2dt)) )
            if dsi.atEnd():
                atend=1
            else:
                dsi.advance()
                q0=q
                i=i+1
        else:
            atend=1


    rts = RegularTimeSeries("/gcalc//////",ndo.getStartTime().toString(),
			    ndo.getTimeInterval().toString(),g)
    return rts


def getMTZCoef():
    return [0.00276159212914926,-6.070476764627e-005,0.000151597503356787,-1.0510895394871e-005,-2.8338337830349e-006,4.9586716960501e-005,-8.7552561972579e-005,7.2094495694158e-005,-5.1752915755519e-005]

#
def first_missing(rts,filter=Constants.DEFAULT_FLAG_FILTER):
    """
    first_missing(rts,filter=Constants.DEFAULT_FLAG_FILTER):
      returns index of the first piece of missing data or -1 if none are missing
    """
    rtsi = rts.getIterator()
    index=0
    while not rtsi.atEnd():
        el=rtsi.getElement()
        if not filter.isAcceptable(el):
            return index
        rtsi.advance()
        index=index+1
    return -1
#
def ECEst(stage, ndo, beta, npow1, npow2,g0=None, zrms=None, c=getMTZCoef()):
    """ Estimate 15min EC at the boundary.
        Inputs:
          stage   astronomical tide estimate. Only 15min data are acceptable
          ndo     ndo estimate -- e.g., from CALSIM
    """
    import interpolate
    from vista.set import Units
    if not isinstance(stage,RegularTimeSeries) or \
           not isinstance(ndo,RegularTimeSeries):
        raise "stage and ndo must be RegularTimeSeries"

    if ndo.getTimeInterval().toString() == "1DAY":
        ndo=interpolate.spline(ndo,"15MIN",0.5) << 95
    elif ndo.getTimeInterval().toString()!= "15MIN":
        raise "ndo must be a one day or 15 minute series"

    if not stage.getTimeInterval().toString() == "15MIN":
        raise "stage must be an hourly or 15 minute series"
    #
    if ndo.getTimeInterval().toString() != stage.getTimeInterval().toString():
        raise "stage and ndo must have the same window"
    #
    #if not len(c) ==9:
    #    raise "Wrong number (%s) of coefficients in the array c" % len(c)

    if (first_missing(ndo)) >= 0:
        raise "missing data not allowed in ndo. First missing data at index: %s" % first_missing(ndo)
    if (first_missing(stage)) >= 0:
        raise "missing data not allowed in stage. First missing data at index: %s" % first_missing(stage)
    
    newstart=ndo.getStartTime() - "21HOURS"
    newend  =ndo.getEndTime() - "3HOURS"
    if (stage.getStartTime().getTimeInMinutes() - newstart.getTimeInMinutes() > 0):
        print "Stage record starts %s and NDO starts %s" % (stage.getStartTime().toString(),ndo.getStartTime().toString())
        raise "stage record must begin at least 21 hours before ndo"
    if (newend.getTimeInMinutes() - stage.getEndTime().getTimeInMinutes() > 0):
        raise "stage record must end no more than 3 hours before end of ndo"
    ztw=timewindow(newstart.toString()+' - '+newend.toString())
    z=stage.createSlice(ztw)
    g=gCalc(ndo,beta,g0)
    zarr=z.getYArray()
    giter=g.getIterator()
    ec=zeros(g.size(),'d')
    ec=map(lambda x: -901.0, ec)
    so=32797       #ocean salinity parameter for gmodel
    sb=200         #river salinity parameter for gmodel

    zrmsiter=zrms.getIterator()
  
    i=0
    while (not giter.atEnd()):
        gval=giter.getElement().getY()
        zrmsval=zrmsiter.getElement().getY()
        ecfrac = c[0] +   gval*c[1] + 1.1*gval**npow1*(c[2]*zarr[i+72] + c[3]*zarr[i+60] 
                      +   c[4]*zarr[i+48] + c[5]*zarr[i+36]
                      +   c[6]*+zarr[i+24] + c[7]*zarr[i+12] + c[8]*zarr[i])-0.1
        #+  c[9]*zrmsval
                      
        ec[i]=max(200,exp(ecfrac)*(so-sb) + sb)
        #ec[i]=max((1+ecfrac)*(so-sb)+sb,200)
        zrmsiter.advance()
        giter.advance()
        i=i+1
    # ec needs to be set umhos/cm
    rts = RegularTimeSeries("/ECest//////",g.getStartTime().toString(),g.getTimeInterval().toString(),ec)
    rts.getAttributes().setYUnits(Units.UMHOS_CM)
    return [rts,gval]
        


def gCalcFlatQ(ndo, beta, g0,out = "inst"):
    """ Calculates antecedent outflow from ndo based on the flat ndo 
        assumption in the g documentation. In this case, the integration of g is exact
        rather than numeric, but the approximation to ndo is a series of flat lines. In the
        case of daily data this is probably acceptable. In the case of monthly data it leads
        to large errors, though it is common
          Arguments:
           ndo: a regular time series. Must be 1DAY, 1MONTH
           g0:  initial condition. If g0 is not given it is equal to ndo at the first time step.
           beta: g-model parameter.
           out: must be "inst" to calculate instantaneous values of g or "ave" to calculate averages over
               the time step. 
          Output:
           g:  a regular time series, same sampling rate as input
               with the same start time as ndo, ending at the end of ndo or the first piece of bad
                 data in ndo.

    """

    if ndo.getTimeInterval().toString() != "1DAY" | ndo.getTimeInterval().toString() != "1MONTH":
        raise "Time step for input must be 1DAY or 1MONTH"

    dsi=ndo.getIterator()
    nstep=ndo.size()
    g=zeros(nstep,'d')
    g=map(lambda x: -901.0, g)
    bdt=beta/dt
    if g0==None:
        g[0]=q[0]
    else:
        g[0]=g0

    atend=0
    i=1
    if out[:4] == "inst":
        while atend == 0:
            el=dsi.getElement()
            if Constants.DEFAULT_FLAG_FILTER.isAcceptable(el):
                q=el.getY()
                g[i] = q/(1+(q/g[i-1] -1)*exp(-q/bdt))
                i=i+1
                if not dsi.atEnd():
                     dsi.advance()
                else:
                     atend=1
            else:
                atend=1
            
    elif out[:3]=="ave":
        while atend == 0:
            el=dsi.getElement()
            if Constants.DEFAULT_FLAG_FILTER.isAcceptable(el):
                q=el.getY()
                g[i] = q/(1+(q/g[i-1] -1)*exp(-q/bdt))
                i=i+1
                if not dsi.atEnd():
                     dsi.advance()
                else:
                     atend=1
            else:
                atend=1
    else:
        raise "Argument out must be either \"inst\" or \"ave\")"
    rts = RegularTimeSeries("gcalc",
		    ndo.getStartTime().toString(),
		    ndo.getTimeInterval(),g)
    if out[:4] == "inst":
        raise "dummy exception"
    if out[:3]=="ave":
        rts.getAttributes().setXType("PER-VAL")
    return rts

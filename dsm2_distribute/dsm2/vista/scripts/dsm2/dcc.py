"""
Preprocessor script for Delta Cross Channel output from CALSIM.
Routines:
   daysPerMonthToITS Computational routine converts a time series of
                    'days operating per month' to a time series of
                     opening and closing operations. The input
                     and output to this routine are time series,
                     which allows the script to be called
                     from VPlotter or the driver routine dxcOp
                     
   dxcOp             Driver routine takes input and output path and
                     file names as arguments, loads time series,
                     calls daysPerMonthToITS and writes output
                     to dss.
   test              testing routine
"""
import string 


def daysPerMonthToITS(dxc,value,allThirty): 
  """
  Computational routine converts a monthly time series of
  days per month to an irregular time series of opening
  and closing operations. Converts Delta Cross Channel gate operation
  from a CALSIM file containing fraction of days open
  Args:
     dxc      input time series with days per month
     value    the value the output time series should take when 'on'
     allThirty set true if all months are assumed 30 day length.
  """
   
  from vutils import timewindow, timeinterval, time
  from vista.time import TimeWindow
  import vista.time.Time
  from vista.set import IrregularTimeSeries
  import config

  debug=0
  daysinmonth = {'jan': 31, 'feb': 28, 'mar': 31, 'apr': 30, 'may': 31, 'jun': 30,
            'jul': 31, 'aug': 31, 'sep': 30, 'oct': 31, 'nov': 30, 'dec': 31}

  x=[]
  y=[]
  lastopen=0                            # indicator that gate ended previous month open
  first=1  
  i=0
  maxYVal=0
  ti=timeinterval("1MON")               # to subtract 1 month from CALSIM DSS time stamp
  for el in dxc:
    if el:
      xval = el.getX()
      xstr=time(long(xval)).toString()
      xdate=time(long(xval)-1)
      xdate=xdate.floor(ti)
      if debug:
        print "Processing date %s" % time(long(xval))
      yval = el.getY()
      maxYVal=max(maxYVal,yval)
      month = string.lower(xstr[2:5])
      year = int(string.lower(xstr[5:9])) # for year we need int val
      if allThirty and yval > 30:
        raise "Argument allThirty appears to be wrong, element found with value>30"
      ndays_month=daysinmonth[month]
      if allThirty: ndays_month=30
      if month=='feb' and year%4 == 0:
        ndays_month = 29

      if debug: print "processesing date: %s  %s / %s" % (xdate,yval,ndays_month)
      if yval == 0.0:
        if lastopen or first:       # must close the gate
          x.append(time(xdate.getTimeInMinutes()))
          y.append(0.0)
        lastopen=0
      else:
        if (not lastopen) or first:  # closed, must open it
          if debug:
            print "opening on: %s ndays=%s days/mo=%s" % (xdate,yval,ndays_month)
          x.append(time(xdate.getTimeInMinutes()))
          y.append(value)
        if (ndays_month-yval)>1E-5:     # some days open in month
          ti_sub = timeinterval('%sday' % int(yval))
          xdate2=time(xdate.getTimeInMinutes())
          xdate2=xdate2+ti_sub
          x.append(xdate2)
          y.append(0.0)
          lastopen=0
        else:
          lastopen=1
      first=0
  
  import jarray
  xarr=jarray.array(x,vista.time.Time)
  yarr=jarray.array(y,'d')
  if debug: print x
  if (maxYVal==30 and not allThirty==1):
    print "Maximum open days in all months was 30. Argument allThirty=0 is probably wrong"
  dxcITS=IrregularTimeSeries("/dxc/pos/",xarr,yarr)
  return dxcITS

# Preprocessor script for Delta Cross Channel 
def dccOp(infile,outfile,inpath,outpath,allThirty=1,value=1.0,
          tw="01OCT1974 0000 - 01OCT1991 0000"):
  """
  Converts Delta Cross Channel gate operation
  from a CALSIM file containing fraction of days open
  Args:
    infile        CALSIM dss file specifying # days operating
    outfile       output dss file readable by DSM2
    inpath        input path, e.g. /CALSIM/DXC/GATE-DAYS-OPEN//1MON//
    outpath       output path, e.g. /CALSIM/DXC/GATE//IR-YEAR/fpart/ 
    value         time series value when gate is opened (must be 1.0 or 2.0),
                  where 1.0 is used for gate ops and 2.0 is number gates operating.
    tw            time window of output
    allThirty     true if CALSIM input is hardwired to thirty day months
  """  
  from vutils import timewindow
  from vdss import opendss,findpath,writedss
  from vista.time import TimeWindow
  from vista.set import DataReference
  import types
  g=opendss(infile)
  if not (type(outfile) == types.StringType):
    raise TypeError("Argument outfile should be name of a dss file")
  if not isinstance(tw,TimeWindow):
    tw = timewindow(tw)
  if not (value==1.0 or value==2.0):
    raise "Output time series 'on' value should be 1.0 (gate op) or 2.0 (gates operating)"
  x=findpath(g,inpath)[0]
  dxcref = DataReference.create(findpath(g,inpath)[0],tw)
  dxc=dxcref.getData()
  if not dxc:
    raise "Cross channel data not found"
  dxcITS=daysPerMonthToITS(dxc,value,allThirty)
  writedss(outfile,outpath,dxcITS)
  return dxcITS
  
def test():
  # test routine for this module
  from vtimeseries import timewindow
  from vdss import opendss,findpath
  from vista.set import DataReference
  from vdisplay import tabulate  
  inpath='/CALSIM/DXC/GATE-DAYS-OPEN//1MON//'
  outpath='/CALSIM_PROCESSED/DCC/GATE//IR-YEAR/TEST/'
  infile="../timeseries/2001d10adv.dss"
  outfile = "d:/delta/gates/dxc.dss"
  val=1.0
  allThirty=1.
  tw=timewindow("01oct1974 0000 - 30SEP1991 2400")
  dccITS=dccOp(infile,outfile,inpath,outpath,allThirty,val,tw)
  tabulate(dxcITS)

# Preprocessor, converting Delta Cross Channel gate operation from a
# Args:
#     infile CALSIM dss file specifying # days operating
#     outfile script output -- gates usable by DSM2
#     tw     interval of output
#     allThiry  -- set true if CALSIM input is hardwired to thirty day months
def dxcOp(infile,outfile,fPart,allThirty=0,tw="01OCT1974 0000 - 01OCT1991 0000"): 
  from vutils import timewindow, timeinterval, time
  from vdss import opendss,findpath,writedss
  from vista.time import TimeWindow
  import vista.time.Time
  from vista.set import IrregularTimeSeries, DataReference
  import string 
  import config

  if (not fPart or fPart==""):
    raise "fPart argument to dxcOp may not be blank or null"
  
  daysinmonth = {'jan': 31, 'feb': 28, 'mar': 31, 'apr': 30, 'may': 31, 'jun': 30,
            'jul': 31, 'aug': 31, 'sep': 30, 'oct': 31, 'nov': 30, 'dec': 31}

  if not isinstance(tw,TimeWindow):
    tw = timewindow(tw)
  g=opendss(infile)
  dxcref = DataReference.create(findpath(g,'/CALSIM/DXC/GATE-DAYS-OPEN//1MON//','b')[0],tw)
  dxc=dxcref.getData()
  if not dxc:
    raise "Cross channel data not found"
  x=[]
  y=[]
  lastopen=0                          # indicator that gate ended previous month open
  first=1  
  i=0
  for el in dxc:
    if el:
      xval = el.getX()
      xdate=time(long(xval))

      xdate2=time(xdate.getTimeInMinutes()+1)    # because of post-dating
      xstr=xdate2.toString()
      yval = el.getY()
      month = string.lower(xstr[2:5])
      year = int(string.lower(xstr[5:9])) # for year we need int val
      
      ndays_month=daysinmonth[month]
      if allThirty: ndays_month=30
      if month=='feb' and year%4 == 0 and year%400 == 0:
        ndays_month = 29
      #print "processesing date: %s  %s / %s" % (xdate,yval,ndays_month)
      xdate2=time(xdate.getTimeInMinutes())      
      
      if yval == 0.0:
        if lastopen or first:       # must close the gate
          x.append(xdate)
          y.append(0.0)
        lastopen=0
      else:
        if (not lastopen) or first:  # closed, must open it
          #print "xdate: %s yval=%s nday=%s" % (xdate,yval,ndays_month)
          x.append(xdate)
          y.append(1.0)
        if (yval-ndays_month-yval)>-1E-5:
          ti = timeinterval('%sday' % int(yval))
          xdate2.incrementBy(ti)
          #print "xdate2: %s" % (xdate2)
          x.append(xdate2)
          y.append(0.0)
          lastopen=0
        else:
          lastopen=1
      first=0
  
  import jarray
  xarr=jarray.array(x,vista.time.Time)
  yarr=jarray.array(y,'d')
  path='/CALSIM/DXC/GATE//IR-YEAR/'+fPart+"/"
  dxcdaily=IrregularTimeSeries(path,xarr,yarr)
#  for el in dxcdaily:
#    print el
  writedss(outfile,path,dxcdaily)


def test():
  # test purpose
  infile="2001d10adv.dss"
  outfile = "d:/delta/gates/dxc.dss"
  tw="01oct1974 0000 - 30SEP1991 2400"
  dxcOp(infile,outfile,tw,1)
  





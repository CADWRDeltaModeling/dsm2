from vutils import *
tm = TimeFactory.getInstance().createTime(0)
g=opendss('../testdata/sim1.dss')
ref = g[0];
ds = ref.getData();
ti = ds.getTimeInterval();
tiday = ti.create('1day')
filter = Constants.DEFAULT_FLAG_FILTER
# convert taf monthly data to cfs
y_cfs = [] # initialize empty arra
factor = 10000000.0/(24*60.0*60.0)
for e in ds:
 tm = tm.create(Math.round(e.x))
 tm2 = tm.create(tm)
 tm2.incrementBy(ti,-1) # go back one month
 nvals = tm2.getExactNumberOfIntervalsTo(tm,tiday)
 if filter.isAcceptable(e): 
  val = (e.y*factor)/nvals
 else:
  val = Constants.MISSING_VALUE
 for i in range(nvals):
  y_cfs.append(val)
# create a regular time series to store this data
stime = ds.getStartTime().create(ds.getStartTime())
stime.incrementBy(ti,-1)
stime.incrementBy(tiday)
rts = RegularTimeSeries(ds.getName() + ' in cfs', 
			stime.toString(), '1day', y_cfs)
# convert cfs daily data to taf monthly data

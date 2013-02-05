# Screen data (set Reject flag for bogus values)
#
# the sources of observed data;
# remote and local, you choose
file_obs = '/data0/dss/hydro.dss'; url_obs = 'iep.water.ca.gov'
#file_obs = '/delta4/data/dss/IEP/hydro.dss'; url_obs = 'local'
#file_obs = 'd:/dsm2/hydro.dss'; url_obs = 'local'
#
# The pathname to check
path='/RLTM\+CHAN/RCSM008/FLOW/.*/1HOUR/DWR-CDEC/'
# Get the data
group_obs = opendss(file_obs, url_obs)
group_obs.filterBy(path)
if len(group_obs) != 1:
  print "Should have one reference in group: ",len(group_obs)
  sys.exit()
#
# set the time window
tw = timeWindow('01JAN1987 0100 - 31DEC1997 2400')
tw = group_obs[0].getTimeWindow()
# create the data reference
ref_obs=DataReference.create(group_obs[0],tw)
if ref_obs==None:
  print "No data for ",path,tw
  sys.exit()
# get the data set from the reference
ds_obs = ref_obs.getData()
# get the iterator on the data set
dsi_obs = ds_obs.getIterator()
# get user id for setting flags
userId = DSSUtil.getUserId();
nreject=0
# loop over till not iterator at end 
while not dsi_obs.atEnd():
  element = dsi_obs.getElement() # gets the current element at iterator position
  if (element.getY() < 0.0) and \
     Constants.DEFAULT_FLAG_FILTER.isAcceptable(element):
    FlagUtils.setQualityFlag(element,FlagUtils.REJECT_FLAG,userId)
    dsi_obs.putElement(element)
    nreject=nreject+1
  #
  dsi_obs.advance() # advance to next element
#
# write back the reference to its server
if nreject > 0:
  print nreject," values rejected."
  DSSUtil.updateData(ref_obs,1)         # the 1 means store flags, 0 ignore flags
else:
  print "No values rejected."
#

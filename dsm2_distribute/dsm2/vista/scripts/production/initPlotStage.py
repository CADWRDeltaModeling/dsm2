# Initialize plotting for stage data
# set the time window
tw = timewindow('01JAN1997 0100 - 31DEC1997 2400')
tw = timewindow('01MAY1997 0100 - 30MAY1997 2400')
# the sources of model and observed data;
# remote and local, you choose
#file_mdl = '/delta5/calibration/realtide/hydro/output-files/hydro.dss'; url_mdl = 'grand.water.ca.gov'
file_mdl = '../testdata/ex3-pump.dss'; url_mdl = 'local'
#file_mdl = 'd:/dsm2/io/realtide/output-files/hydro-my.dss'; url_mdl = 'local'

#file_obs = '/data0/dss/hydro.dss'; url_obs = 'iep.water.ca.gov'
file_obs = '../testdata/ex3-base.dss'; url_obs = 'local'
#file_obs = 'd:/dsm2/hydro.dss'; url_obs = 'local'
#
# Read the model and observed path string regexps to plot,
# The file has model path string in field 1,
# observed in field 2.
# These should be unique enough to return one or two
# data refs (paths) (may include both HIST and RLTM)
# 
#file_paths='d:/dsm2/local_scripts/stage_stas.dat'
file_paths='stage_sample.data'
file_obj=open(file_paths)
lines=file_obj.readlines()
labels=[]; paths_mdl=[]; paths_obs=[]
for line in lines:
  parts=re.split('[ \t\n]+',line)
  label=re.sub('_',' Chan ',parts[0])
  label=re.sub('[()]','',label)
  labels.append(label)
  paths_mdl.append(parts[1])
  paths_obs.append(parts[2])
#
# Get data for model, observed
type='stage'
group_mdl = opendss(file_mdl, url_mdl)
group_mdl.filterBy(type)
group_obs = opendss(file_obs, url_obs)
group_obs.filterBy(type)

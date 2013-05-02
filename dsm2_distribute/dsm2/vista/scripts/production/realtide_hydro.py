# Compare realtide Hydro run with observed
#
# import additional vista objects
from vista.set import Stats
# set the time window
tw = timeWindow('01JAN1994 0100 - 31DEC1997 2400')
tw = timeWindow('01MAY1997 0100 - 30MAY1997 2400')
# the sources of model and observed data;
# remote and local, you choose
file_mdl = '/delta5/calibration/realtide/hydro/output-files/hydro.dss'
#file_mdl = 'd:/dsm2/io/calibration/hydro-short.dss'
url_mdl = 'grand.water.ca.gov'
url_mdl = 'local'
file_obs = '/data0/dss/hydro.dss'
#file_obs = 'd:/dsm2/hydro.dss'
url_obs = 'iep.water.ca.gov'
#url_obs = 'local'
# the stations to plot
# need the B part list for model, B part list for observed,
# and F part list for observed; the latter for
# additional filtering of observed data
# Furthermore, if you have multiple model runs in the model DS file,
# you will need to further filter the model paths
# 
BList_mdl = ['CHGRL009', 'CHGRL012', 'RMID009', 'RMID015-DWR-1', 'RMID015-USGS-1', 'RMID023-DWR', 'RMID027', 'RMID037', 'RMID040', 'RMKL005', 'ROLD024-DWR', 'ROLD024-USGS', 'ROLD034', 'ROLD040-DWR', 'ROLD046', 'ROLD050', 'ROLD059', 'ROLD074', 'RSAC054', 'RSAC075', 'RSAC081-DWR', 'RSAC081-DWR', 'RSAC121', 'RSAC124', 'RSAC128', 'RSAN007-DWR', 'RSAN007-DWR', 'RSAN018-USGS', 'RSAN032', 'RSAN058', 'RSAN063', 'RSAN072', 'RSMKL020', 'SLCBN1.0', 'SLCBN1.2', 'SLCRD000', 'SLCRD003', 'SLCRD006', 'SLCRD009', 'SLDUT007', 'SLGYR003', 'SLMZU003', 'SLMZU011', 'SLMZU025', 'SLMZU029', 'SLRAR000', 'SLSUS012', 'SLTRM004']
BList_obs = ['CHGRL009', 'CHGRL012', 'RMID009', 'RMID015',       'RMID015',        'RMID023',     'RMID027', 'RMID037', 'RMID040', 'RMKL005', 'ROLD024',     'ROLD024',      'ROLD034', 'ROLD040',     'ROLD046', 'ROLD050', 'ROLD059', 'ROLD074', 'RSAC054', 'RSAC075', 'RSAC081',     'RSAC081',     'RSAC121', 'RSAC124', 'RSAC128', 'RSAN007',     'RSAN007',     'RSAN018',      'RSAN032', 'RSAN058', 'RSAN063', 'RSAN072', 'RSMKL020', 'SLCBN1.0', 'SLCBN1.2', 'SLCRD000', 'SLCRD003', 'SLCRD006', 'SLCRD009', 'SLDUT007', 'SLGYR003', 'SLMZU003', 'SLMZU011', 'SLMZU025', 'SLMZU029', 'SLRAR000', 'SLSUS012', 'SLTRM004']
FList_obs = ['',         '',         '',        'DWR-CD-SURFWATER','USGS',         '',            '',        '',        '',        '',        'DWR-CD-SURFWATER','USGS',     '',        '',            '',        '',        '',        '',        '',        '',        'DWR-CD-SURFWATER','DWR-ESO-MARSH','',   '',        '',        'DWR-CD-SURFWATER', 'DWR-ESO-D1485C','',      '',        '',        '',        '',        '',         '',         '',         '',         '',         '',         '',         '',         '',         '',         '',         '',         '',         '',         '',         '']
#
BList_mdl = ['CHGRL009']
BList_obs = ['CHGRL009']
FList_obs = ['']
# check that the same number of filter strings exists
if len(BList_mdl) != len(BList_obs):
  print "Different number of stations in model and observed lists."
  sys.exit()
if len(FList_obs) != len(BList_obs):
  print "Different number of elements in observed B and F lists."
  sys.exit()
# some color constants
Green=Color(0,255,0)
DGreen=Color(0,100,0)
Blue=Color(0,0,255)
DBlue=Color(0,0,100)
# Get stage data for model, observed
group_mdl = opendss(file_mdl, url_mdl)
group_mdl.filterBy('STAGE')
group_obs = opendss(file_obs, url_obs)
group_obs.filterBy('stage')
# get MTZ stage for use as run quality indicator
group_mtz=group_obs.clone()
group_mtz.filterBy('RSAC054/STAGE.*DWR-ESO-D1485C')
if len(group_mtz) == 0:
  print "Couldn't find observed data for Martinez stage."
  sys.exit()
ref_mtz = DataReference.create(group_mtz[0],tw)
if ref_mtz==None:
  print "No MTZ Observed data for time window ",tw
  sys.exit()
# change good and bad MTZ values to constants
ds_mtz=ref_mtz.getData()                # get the data set
dsi_mtz=ds_mtz.getIterator()            # data set iterator
flag_filter=Constants.DEFAULT_FLAG_FILTER
good_iterator = ElementFilterIterator(dsi_mtz,flag_filter)
# create a new array which contains 0 for acceptable
# and Missing for not acceptable
good_y = []  # an empty array
dsi_mtz.resetIterator() # make sure it's reset to beginning
while not dsi_mtz.atEnd():
  element = dsi_mtz.getElement() # get the element
  if flag_filter.isAcceptable(element): 	
    good_y.append(0.)
  else :
    good_y.append(-901.)
  dsi_mtz.advance() 	# advance by one
#
# now loop over all the stations
nsta=len(BList_mdl)
for standx in range(nsta) :
  good_plot=None
  sta_mdl=BList_mdl[standx]
  sta_obs=BList_obs[standx]
  f_obs=FList_obs[standx]
  plotList = [] 
  # loop over instantaneous and residual plots
  for data_type in ['inst', 'resid']:
    print "Station", sta_obs, data_type
    #
    # for both model and observed data, each curve will actually
    # be two curves: one for actual data, the other a different
    # color and thickness for when MTZ historic stage is good
    #
    # model data
    gsm=group_mdl.clone()               # start with a copy of model stage data
    gsm.filterBy(sta_mdl)               # reduce to just this station
    if len(gsm) == 0:                   # check there is data for this station
      print "Couldn't find model data for " + sta_mdl
      break
    if len(gsm) > 1:
      # if true, you'll have to filter by the A or F part
      print "Too many model paths, add a filter for A and/or F part", gsm
      break
    ref_mdl = DataReference.create(gsm[0],tw)
    if ref_mdl==None:                   # check for data for this tw
      print "No Model data for time window ",tw
      break
    if data_type=='resid':              # convert to 1 day interval for resid plot
      ref_mdl=per_avg(ref_mdl,interval='1DAY')
    path_mdl=ref_mdl.getPathname()
    apart_mdl = path_mdl.getPart(path_mdl.A_PART)
    epart_mdl = path_mdl.getPart(path_mdl.E_PART)
    fpart_mdl = path_mdl.getPart(path_mdl.F_PART)
    # model data set for use when MTZ stage is good
    #
    # put MTZ arrays into data sets, using data set
    # attributes from model data
    ds_mtz_good = RegularTimeSeries('MTZ Valid Stage',\
                                    ds_mtz.getStartTime(), \
                                    ds_mtz.getTimeInterval(), \
                                    good_y, None, \
                                    ref_mdl.getData().getAttributes())
    # add mtz vector to model
    ds_mdl_good=TimeSeriesMath.doBinaryOperation(\
      ref_mdl.getData(),ds_mtz_good,TimeSeriesMath.ADD)
    #
    # observed data; similar to model data
    gso=group_obs.clone()
    gso.filterBy(sta_obs)
    if len(gso) == 0:
      print "Couldn't find observed data for " + sta_obs
      break
    if f_obs != '':
      gso.filterBy(f_obs)
      if len(gso) == 0:
        print "Couldn't find observed data for " + sta_obs + f_obs
        break
    if len(gso) > 1:
      print "Too many observed paths", gso
      break
    ref_obs = DataReference.create(gso[0],tw)
    if ref_obs==None:
      print "No Observed data for time window ",tw
      break
    if data_type=='resid':
      ref_obs=per_avg(ref_obs,interval='1DAY')
    # observed data set for use when MTZ stage is good
    # put arrays into data sets
    ds_mtz_good = RegularTimeSeries('MTZ Valid Stage',\
                                    ds_mtz.getStartTime(), \
                                    ds_mtz.getTimeInterval(), \
                                    good_y, None, \
                                    ref_obs.getData().getAttributes())
    #
    ds_obs_good=TimeSeriesMath.doBinaryOperation(\
      ref_obs.getData(),ds_mtz_good,TimeSeriesMath.ADD)
    path_obs=ref_obs.getPathname()
    apart_obs = path_obs.getPart(path_obs.A_PART)
    epart_obs = path_obs.getPart(path_obs.E_PART)
    fpart_obs = path_obs.getPart(path_obs.F_PART)
    #
    # now generate the plots
    plot = Plot()                       # start with blank canvas
    text_mdl= apart_mdl + ' ' + epart_mdl + ' ' + fpart_mdl
    text_obs= apart_obs + ' ' + epart_obs + ' ' + fpart_obs
    # curves for all data...
    crv_mdl = CurveFactory.createCurve(ref_mdl, AxisAttr.BOTTOM, 
                                       AxisAttr.LEFT, text_mdl)
    crv_obs = CurveFactory.createCurve(ref_obs, AxisAttr.BOTTOM, 
                                       AxisAttr.LEFT, text_obs)
    plot.add(crv_mdl); plot.add(crv_obs)
    # ...and curves for when MTZ stage is good-
    # these will be overlayed onto first curves
    crv_mdl_good = CurveFactory.createCurve(wrap_data(ds_mdl_good), AxisAttr.BOTTOM, 
                                            AxisAttr.LEFT, text_mdl)
    crv_obs_good = CurveFactory.createCurve(wrap_data(ds_obs_good), AxisAttr.BOTTOM, 
                                            AxisAttr.LEFT, text_obs)
    plot.add(crv_mdl_good)
    plot.add(crv_obs_good)
    #
    if data_type=='inst':
      plot.addTitle('Instantaneous Stage ' + sta_obs)
    else:
      plot.addTitle('Residual Stage ' + sta_obs)
    #
    # model data: dark blue line for all, then blue for good MTZ
    crv_mdl.getAttributes()._foregroundColor = DBlue
    crv_mdl_good.getAttributes()._foregroundColor = Blue
    crv_mdl_good.getAttributes()._thickness = 2
    # obs data: dark green line for all, then green; symbols always
    attr_obs = crv_obs.getAttributes()
    attr_obs._foregroundColor = DGreen
    attr_obs._drawSymbol = 1
    attr_obs_good = crv_obs_good.getAttributes()
    attr_obs_good._drawSymbol = 1
    attr_obs_good._thickness = 5
    crv_obs_good.setForegroundColor(Green)
    # try to get about 1 symbol every 2 days, for inst;
    # for residual, about 3-5 times that spacing
    if epart_obs=='10MIN':
      attr_obs._dataPerSymbol=250
      attr_obs_good._dataPerSymbol=250
    elif epart_obs=='15MIN':
      attr_obs._dataPerSymbol=175
      attr_obs_good._dataPerSymbol=175
    elif epart_obs=='1HOUR':
      attr_obs._dataPerSymbol=50
      attr_obs_good._dataPerSymbol=50
    elif epart_obs=='1DAY':
      attr_obs._dataPerSymbol=2
      attr_obs_good._dataPerSymbol=2
    if data_type=='resid':
      attr_obs._dataPerSymbol=attr_obs._dataPerSymbol*3
      attr_obs_good._dataPerSymbol=attr_obs._dataPerSymbol*3
    #
    crv_obs.setSymbol(SymbolFactory.createCircle(\
      1,attr_obs._foregroundColor,2))
    crv_obs_good.setSymbol(SymbolFactory.createCircle(\
      1,attr_obs_good._foregroundColor,2))
    # create legend and add
    legend = Legend()
    lgn_mdl = LegendItem(crv_mdl_good) 
    lgn_mdl.getAttributes().setOriginalFontSize(10)
    lgn_obs = LegendItem(crv_obs_good) 
    lgn_obs.getAttributes().setOriginalFontSize(10)
    legend.add(lgn_mdl); legend.add(lgn_obs)
    plot.add(legend)
    #
    plotList.append(plot)
    good_plot=1
    # end of inst/resid loop
  if good_plot:
    # put both plots on one page and display
    mp = MultiPlot(2,1) # 2 rows, 1 column
    for plot in plotList: mp.add(plot)
    graph = Graph()                     # a blank frame
    graph.add(mp)                       # add plots to frame
    graph_frame = DataGraphFrame(graph,'DSM2-Hydro ' + sta_obs)
    graph_frame.setLocation(10,10)      # upper left hand corner of desktop 
    graph_frame.setSize(600,500)        # pixels wide by pixels high 

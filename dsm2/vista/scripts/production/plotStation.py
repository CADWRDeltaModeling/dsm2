# Compare realtide Hydro run with observed
#
# import additional vista objects
from vista.set import Stats
from java.awt import Toolkit
from vista.time import TimeWindow
# some color constants
Green=Color(0,255,0)
DGreen=Color(0,125,0)
Blue=Color(0,0,255)
DBlue=Color(0,0,125)
Red=Color(255,0,0)
# screen size
tk = Toolkit.getDefaultToolkit()
screen=tk.getScreenSize()
# calc plot frame size: 1/2 of screen dims
plot_width=screen.width/2
plot_height=screen.height/2
#
# main plot function
#
from javax.swing import JOptionPane
from java.lang import Exception
def plotStation(standx,tw_all):
  good_plot=None
  xdelta=None
  # model group
  path_mdl=paths_mdl[standx]
  label=labels[standx]
  if label=='X-Delta':                  # special cross-delta flow case
    xdelta=1
  #
  gsm=group_mdl.clone()                 # start with a copy of model data
  gsm.filterBy(path_mdl)                # reduce to just this station
  if len(gsm) == 0:                     # check there is data for this station
    JOptionPane.showMessageDialog(None, "Couldn't find model data for " + path_mdl)
    return
  # observed group
  path_obs=paths_obs[standx]
  if path_obs=='X-Delta':               # special cross-delta flow case
    path_obs='rsac128|rsac121'
  #
  gso=group_obs.clone()
  gso.filterBy(path_obs)
  if len(gso) == 0:
    JOptionPane.showMessageDialog(None, "Couldn't find observed data for " + path_obs)
    return
  # check with time window field to see if it has a ok time window string
  # or says to use all intersecting data
  if tw_all:               # use intersection of obs and model data
    tw=TimeWindow.intersection(gsm[0].getTimeWindow(),gso[0].getTimeWindow())
    if tw==None:
      JOptionPane.showMessageDialog(None,"No overlapping data.")
      return
    else:
      twf.setText(repr(tw))
      return
    #
  #
  else:                                 # not all data, use time window
    try :
      tw = timeWindow(twf.getText());
    except Exception, e:
      JOptionPane.showMessageDialog(None,e.getMessage())
      twf.setText(repr(tw))
  #
  plotList = [] 
  # get MTZ stage for use as run quality indicator
  file_mtz = file_obs; url_mtz = url_obs
#  file_mtz = '/delta4/data/dss/IEP/hydro.dss'; url_mtz = 'local'
#  file_mtz = 'd:/dsm2/hydro.dss'; url_mtz = 'local'
  try:
    group_mtz = opendss(file_mtz, url_mtz)
  except:
    JOptionPane.showMessageDialog(None,'Invalid DSS filename.')
    return
  group_mtz.filterBy('RSAC054/STAGE.*DWR-ESO-D1485C')
  if len(group_mtz) == 0:
    JOptionPane.showMessageDialog(None, \
                                  "Couldn't find observed data for Martinez stage")
    return
  ref_mtz = DataReference.create(group_mtz[0],tw)
  if ref_mtz==None:
    JOptionPane.showMessageDialog(None, \
                                  "No MTZ Observed data for time window " + repr(tw))
    return
  # change good and bad MTZ values to constants
  flag_filter=Constants.DEFAULT_FLAG_FILTER
  ds_mtz=ref_mtz.getData()                # get the data set
  dsi_mtz=ds_mtz.getIterator()            # data set iterator
  # create a new array which contains 0 for acceptable
  # and Missing for not acceptable
  good_y = []  # an empty array
  dsi_mtz.resetIterator() # make sure it's reset to beginning
  while not dsi_mtz.atEnd():
    element = dsi_mtz.getElement() # get the element
    if flag_filter.isAcceptable(element): 	
      good_y.append(0.)
    else :
      good_y.append(Constants.MISSING_VALUE)
    dsi_mtz.advance() 	# advance by one
  #
  # loop over instantaneous and residual plots
  for data_type in ['inst', 'resid']:
    print "Station", path_mdl, path_obs, data_type
    #
    # for model data, the curve will actually
    # be two curves: one for actual data, the other a different
    # color and thickness for when MTZ historic stage is good
    #
    # model data
    if len(gsm) > 1:
      if re.match('RMID015',path_mdl):  # special case, subtract 2 paths
        ref_mdl=DataReference.create(gsm[0],tw)-gsm[1]
      elif re.match('RMID003',path_mdl): # special case, add 2 paths
        ref_mdl=DataReference.create(gsm[0],tw)+gsm[1]
      elif xdelta:                      # special case for cross delta flow
        ref_mdl=DataReference.create(gsm[1],tw)-gsm[0]
      else:                             # if true, you'll have to filter by the A or F part
        JOptionPane.showMessageDialog(None, \
                                      "Too many model paths, add a filter for A and/or F part "+ 
                                      repr(gsm))
        break
    else:
      ref_mdl = DataReference.create(gsm[0],tw)
    #
    if ref_mdl==None:                   # check for data for this tw
      JOptionPane.showMessageDialog(None, \
                                    "No Model data for " + path_mdl + " " + repr(tw))
      break
    if data_type=='resid':              # convert to 1 day interval for resid plot
      ref_mdl=per_avg(ref_mdl,interval='1DAY')
    full_path_mdl=ref_mdl.getPathname()
    apart_mdl = full_path_mdl.getPart(full_path_mdl.A_PART)
    epart_mdl = full_path_mdl.getPart(full_path_mdl.E_PART)
    fpart_mdl = full_path_mdl.getPart(full_path_mdl.F_PART)
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
    ds_mdl_good=ref_mdl.getData()+ds_mtz_good
    #
    # observed data; similar to model data
    if len(gso) > 1:
      if xdelta:                        # special case for cross delta flow
        ref_obs=DataReference.create(gso[1],tw)-gso[0]
      else:
        # merge the paths together (probably HIST & RLTM)
        print "Merging " + repr(len(gso)) + " observed paths for " + path_obs
        ref_obs_arr = []
        for ref in gso:
          ref_obs_arr.append(DataReference.create(ref,tw))
          #
          ref_obs=ProxyFactory.createMergingProxy(ref_obs_arr)
      #
    else:
      ref_obs = DataReference.create(gso[0],tw)
    #
    # last check for no data: check for at least one good data point
    if oneGood(ref_obs)==None:
      JOptionPane.showMessageDialog(None, \
                                    "No Observed data for " + path_obs + \
                                    " " + repr(tw))
      break
    #
    # if xdelta plot, also show DXC position
    if xdelta:
      gdxc=opendss(file_obs, url_obs)
      gdxc.filterBy('RSAC128/POS')
      if gdxc != None:
        if len(gdxc) > 1:
           ref_dxc_arr = []
           for ref in gdxc:
             if oneGood(DataReference.create(ref,tw)):
               ref_dxc_arr.append(DataReference.create(ref,tw))
           #
           ref_dxc=ProxyFactory.createMergingProxy(ref_dxc_arr)
        #
        else:                           # only 1 dxc path
          ref_dxc=DataReference.create(gdxc[0],tw)
        print "tw", tw
        tabulate(ref_dxc)
        # create two curves for dxc (open and close)
        ds_dxc=ref_dxc.getData()                # get the data set
        dsi_dxc=ds_dxc.getIterator()            # data set iterator
        # create two new arrays which contains 0 for open, missing for closed
        # and vice-versa
        open_y = []; close_y=[]
        dsi_dxc.resetIterator() # make sure it's reset to beginning
        while not dsi_dxc.atEnd():
          element = dsi_dxc.getElement() # get the element
          if element>0:             # open
            open_y.append(0.)
            close_y.append(Constants.MISSING_VALUE)
          else:                    # closed
            open_y.append(Constants.MISSING_VALUE)
            close_y.append(0.)
          #
          dsi_dxc.advance() 	# advance by one
        #
        print "ds_dxc", ds_dxc.getStartTime(),ds_dxc.getEndTime()
        ds_dxc_open=IrregularTimeSeries('DXC Open Gate',
                                        SetUtils.createXArray(ds_dxc),
                                        open_y)
        ds_dxc_close=IrregularTimeSeries('DXC Close Gate',
                                         SetUtils.createXArray(ds_dxc),
                                         close_y)
      else:
        ref_dxc=None
    else:
      ref_dxc=None
    #
    if data_type=='resid':
      ref_obs=per_avg(ref_obs,interval='1DAY')
    #
    full_path_obs=ref_obs.getPathname()
    apart_obs = full_path_obs.getPart(full_path_obs.A_PART)
    cpart_obs = full_path_obs.getPart(full_path_obs.C_PART)
    epart_obs = full_path_obs.getPart(full_path_obs.E_PART)
    fpart_obs = full_path_obs.getPart(full_path_obs.F_PART)
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
    #
    if ref_dxc != None:
      crv_dxc_open = CurveFactory.createCurve(wrap_data(ds_dxc_open), AxisAttr.BOTTOM, 
                                              AxisAttr.LEFT, 'DXC Open Gate')
      crv_dxc_close = CurveFactory.createCurve(wrap_data(ds_dxc_close), AxisAttr.BOTTOM, 
                                               AxisAttr.LEFT, 'DXC Close Gate')
      plot.add(crv_dxc_open); plot.add(crv_dxc_close)
    # ...and curves for when MTZ stage is good-
    # these will be overlayed onto first curves
    crv_mdl_good = CurveFactory.createCurve(wrap_data(ds_mdl_good), AxisAttr.BOTTOM, 
                                            AxisAttr.LEFT, text_mdl)
    plot.add(crv_mdl_good)
    #
    if data_type=='inst':
      plot.addTitle('Instantaneous ' + cpart_obs + ' ' + path_obs)
    else:
      plot.addTitle('Residual ' + cpart_obs + ' ' + path_obs)
    #
    # model data: dark blue line for all, then blue for good MTZ
    crv_mdl.getAttributes()._foregroundColor = DBlue
    crv_mdl_good.getAttributes()._foregroundColor = Blue
    crv_mdl_good.getAttributes()._thickness = 1
    # obs data: green line for all; symbols always
    attr_obs = crv_obs.getAttributes()
    attr_obs._foregroundColor = Green
    attr_obs._drawSymbol = 1
    if ref_dxc != None:
      crv_dxc_open.getAttributes()._foregroundColor=Green
      crv_dxc_close.getAttributes()._foregroundColor=Red
    #
    # try to get about 1 symbol every 2 days, for inst;
    # for residual, about 3-5 times that spacing
    attr_obs._dataPerSymbol=max(24*60 * 2 / \
                            TimeInterval.getIntervalInMinutes(\
                            ref_obs.getTimeInterval(), \
                            ref_obs.getTimeWindow().getStartTime()), 1)
    if data_type=='resid':
      attr_obs._dataPerSymbol=attr_obs._dataPerSymbol*3
    #
    crv_obs.setSymbol(SymbolFactory.createCircle(\
      1,attr_obs._foregroundColor,2))
    # create legend and add
    legend = Legend()
    lgn_mdl = LegendItem(crv_mdl_good) 
    lgn_mdl.getAttributes().setOriginalFontSize(10)
    lgn_obs = LegendItem(crv_obs)
    lgn_obs.getAttributes().setOriginalFontSize(10)
    legend.add(lgn_mdl); legend.add(lgn_obs)
    plot.add(legend)
    #
    plotList.append(plot)
    good_plot=1
    plot.getAxis(AxisAttr.BOTTOM).setDCRange(float(tw.getStartTime().getTimeInMinutes()),
                                             float(tw.getEndTime().getTimeInMinutes()))
    # end of inst/resid loop
  if good_plot:
    # put both plots on one page and display
    mp = MultiPlot(2,1) # 2 rows, 1 column
    for plot in plotList: mp.add(plot)
    graph = Graph()                     # a blank frame
    graph.add(mp)                       # add plots to frame
    graph_frame = DataGraphFrame(graph,'DSM2-Hydro ' + label,0)
    graph_frame.setLocation(screen.width/3,10) # upper left hand corner of desktop 
    graph_frame.setSize(plot_width,plot_height) # pixels wide by pixels high
    graph_frame.show()
#
def oneGood(ref):
  # if at least one value in the data ref is ok, return true;
  # else return null
  if ref==None:
    return None
  #
  try:
    dsi = ElementFilterIterator(ref.getData().getIterator(),
                                Constants.DEFAULT_FLAG_FILTER) # iterate over only good values
  except:
    return None                         # couldn't get any data
  #
  dsi.resetIterator(); dsi.advance()    # reset and try to advance to a good value
  if dsi.atEnd():
    return None                         # jumped to end, no good value
  else:
    return 1                            # at least one good value
#

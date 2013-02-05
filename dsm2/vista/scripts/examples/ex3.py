# A sample script to compare two pathnames residing in different
# dss files 
#
# select the two files to which to output the data
from vutils import *
file1 = '../testdata/ex3-base.dss'
file2 = '../testdata/ex3-pump.dss'
stationList = ['CLFCT','085_3500']
plotList = []
from vista.app import CurveFactory
# for each station in the station list array....
for station in stationList :
	print "Creating plots for " , station
	# set a time window
	tw = timewindow('08DEC1987 0700 - 09DEC1987 0800')
	# open the first file , filter for station+stage and get the first reference
	g1 = opendss(file1)
	g1.filterBy(station+'/STAGE'); 
	ref1 = DataReference.create(g1[0],tw)
	# get the second reference from second file. Again filter by station+stage and get the
	# first reference. Not clean but that's not the context of the example here...
	g2 = opendss(file2)
	g2.filterBy(station+'/stage');
	ref2 = DataReference.create(g2[0],tw)
	# initialize an empty plot
	plot0 = Plot();
	# create a legend for both curves
	l1text= 'STAGE at ' +station + ' (DWR-OSP-DMS+1C) (Base Case)'
	l2text= 'STAGE at ' +station+ ' (DWR-OSP-DMS+1C) (Pump Case)'
	# create a curve for the first data reference
	c1 = CurveFactory.createCurve(ref1, AxisAttr.BOTTOM, 
				      AxisAttr.LEFT, l1text);
	# create a curve for the second data reference
	c2 = CurveFactory.createCurve(ref2, AxisAttr.BOTTOM, 
				      AxisAttr.LEFT, l2text);
	# add the curves to the plot
	plot0.add(c1); plot0.add(c2);
	# add the title to the plot
	plot0.addTitle('DSM2: Water Level at ' + station)
	# for b/w plots endorn one curve with symbols
	# get the attribute for the first curve
	attr1 = c1.getAttributes();
	# set it to draw the symbol
	attr1._drawSymbol = 1
	# draw a symbol once every 5 data points
	attr1._dataPerSymbol=5
	# set the symbol to be drawn a circle with fill, same foreground color and size 2
	c1.setSymbol(SymbolFactory.createCircle(1,attr1._foregroundColor,2))
	# create legend and add
	legend = Legend();
	# create a legend item for the first curve
	li1 = LegendItem(c1); 
	li1.getAttributes().setOriginalFontSize(10);
	# create a legend item for the second curve
	li2 = LegendItem(c2); 
	li2.getAttributes().setOriginalFontSize(10);
	# add to the legend the legend items
	legend.add(li1); legend.add(li2);
	# add to the plot the legend
	plot0.add(legend)
	# set axis range
	axis = plot0.getAxis(AxisAttr.LEFT)
	# ask the tick label generator to use the data min/max instead of rounding off...
	axis.getTickGenerator().useDataMinMax(1);
	# set the range
	axis.setDCRange(-2.0,3.5)
	# add this plot to the reference list
	plotList.append(plot0)
#
# finally put 2 plots on a page and display
#put both plots on one page and display
#create a multiplot with 2 rows and 1 column
mp = MultiPlot(2,1) # 2 rows, 1 column
# for each plot in plot list add to the multiplot object
for plot0 in plotList: mp.add(plot0)
# create a graph object.
graph = Graph()
# add the multiplot object to the graph
graph.add(mp)
# display the graph in a frame...
dg = DataGraphFrame(graph,'Test Graph')
#

from vmath import *
from vista.graph import CurveAttr
crvAttr1 = (Color.blue,None,4,1)
crvAttr2 = (Color.red,SymbolFactory.createCross(0,Color.red,2),8,1)
crvAttr3 = (Color.green,None,4,1)
attrs = [crvAttr1,crvAttr2, crvAttr3]
crvAttrs = []
for attr in attrs:
    ca = CurveAttr()
    ca._foregroundColor = attr[0]
    if attr[1]:
	ca._drawSymbol = 1
	ca._symbol = attr[1]
	ca._dataPerSymbol = attr[2]
    ca._thickness = attr[3]
    crvAttrs.append(ca)
#
#graphTitle='EC Comparisions between DSM2 run vs ANN calibrated on it'
graphTitle=''
def setCurveAttr(crv,crvNumber):
    crvAttr = crvAttrs[crvNumber-1]
    crv.setAttributes(crvAttr)
# A script file to compare two dsm2 runs
# get's a reference given its b,c and e parts and an optional f and a part
# returns None if nothing found
def getRef(file,b_part,c_part,e_part,f_part=None,a_part=None):
    g=opendss(file)
    if b_part:
	g.filterBy(PathPartPredicate("^"+b_part+"$", Pathname.B_PART),1)
    if c_part:
	g.filterBy(PathPartPredicate("^"+c_part+"$", Pathname.C_PART),1)
    if e_part:
	g.filterBy(PathPartPredicate("^"+e_part+"$", Pathname.E_PART),1)
    if f_part:
	g.filterBy(PathPartPredicate(f_part, Pathname.F_PART),1)
    if a_part:
	g.filterBy(PathPartPredicate(a_part, Pathname.A_PART),1)
    if len(g) == 0:
	return None
    else:
	return g[0]
#
def getRefFromPath(file,path):
    return getRef(file,
                  path.getPart(Pathname.B_PART),
                  path.getPart(Pathname.C_PART),
                  path.getPart(Pathname.E_PART),
                  path.getPart(Pathname.F_PART),
                  path.getPart(Pathname.A_PART)
                  )
#
def c2plot(file1,path1,file2,path2,l1,l2,axisLabel,title,tw=None):
    return doPlot(file1,path1,file2,path2,None,None,l1,l2,None,axisLabel,title,tw,0)
#
def c2diffplot(file1,path1,file2,path2,l1,l2,axisLabel,title,tw=None):
    return doPlot(file1,path1,file2,path2,None,None,l1,l2,None,axisLabel,title,tw,1)
#
def c3diffplot(file1,path1,file2,path2,file3,path3,l1,l2,l3,axisLabel,title,tw):
    return doPlot(file1,path1,file2,path2,file3,path3,l1,l2,l3,axisLabel,title,tw,1)
#
def c3plot(file1,path1,file2,path2,file3,path3,l1,l2,l3,axisLabel,title,tw):
    return doPlot(file1,path1,file2,path2,file3,path3,l1,l2,l3,axisLabel,title,tw,0)
# comparitive plot of data at given b, c and e parts
# also file[1|2] is the dss filename 
def doPlot(file1,path1,file2,path2,file3,path3,l1,l2,l3,axisLabel,title,tw=None,diff=0):
    from java.lang import Class
    # get ref1 and ref2 data
    ref1 = getRefFromPath(file1,Pathname.createPathname(path1))
    ref2 = getRefFromPath(file2,Pathname.createPathname(path2))
    plot3 = (file3 != None and path3 != None)
    if plot3:
	plot3 = (file2 != file3) or (path2 != path3)
    if plot3:
	ref3 = getRefFromPath(file3,Pathname.createPathname(path3))
    #  check ref1 and ref2
    if ref1 == None:
	raise "No data reference for " + file1 + " & " + path1
    if ref2 == None:
	raise "No data reference for " + file2 + " & " + path2
    if plot3 and (ref3 == None):
	raise "No data reference for " + file3 + " & " + path3
    # trim to common time window
    if not tw:
	tw1 = ref1.getTimeWindow()
	tw2 = ref2.getTimeWindow()
	tw = tw1.intersection(tw2)
	if not tw:
	    raise "No common time window : " + path1 + " & " + path2
	if plot3:
	    tw3 = ref2.getTimeWindow()
	    tw = tw.intersection(tw3)
	if not tw:
	    raise "No common time window : " + path3 + " & " + path2 + " & " + path1
    #
    ref1 = DataReference.create(ref1,tw)
    ref2 = DataReference.create(ref2,tw)
    if not ref1:
	raise "No data for " + file1 + " & " + path1 + " & " + repr(tw)
    if not ref2:
	raise "No data for " + file2 + " & " + path2 + " & " + repr(tw)
    if plot3:
	ref3 = DataReference.create(ref3,tw)
	if not ref3:
	    raise "No data for " + file3 + " & " + path3 + " & " + repr(tw)
    # check for data 
    if ref1.getData() == None:
	raise ref1.getPathname().toString() + " is empty?"
    if ref2.getData() == None:
	raise ref2.getPathname().toString() + " is empty?"
    if plot3 and (not ref3.getData()):
	raise ref3.getPathname().toString() + " is empty?"
    # check if same data
    samedata=0
    if plot3:
	if (path2 == path3) and (file2 == file3):
	    samedata=1
	else:
	    samedata=0
    # construct legend text
    l1txt = l1
    l2txt = l2
    crv1,crv2,crv3,li2,li3=(None,None,None,None,None)
    if plot3:
	l3txt = l3
    if diff:
	crv1 = CurveFactory.createCurve(ref2-ref1,AxisAttr.BOTTOM, AxisAttr.LEFT,l2txt + " - " + l1txt) 
	if plot3:
	    crv2 = CurveFactory.createCurve(ref3-ref1,AxisAttr.BOTTOM, AxisAttr.LEFT, l3txt + " - " + l1txt)
    else:
	crv1 = CurveFactory.createCurve(ref1,AxisAttr.BOTTOM, AxisAttr.LEFT,l1txt) 
	crv2 = CurveFactory.createCurve(ref2,AxisAttr.BOTTOM, AxisAttr.LEFT, l2txt)
	if plot3:
	    crv3 = CurveFactory.createCurve(ref3,AxisAttr.BOTTOM, AxisAttr.LEFT, l3txt)
    # set curve characteristics
    setCurveAttr(crv1,1)
    if crv2:
	setCurveAttr(crv2,2)
    if crv3:
	setCurveAttr(crv3,3)
    # create legend
    leg = Legend()
    li1 = LegendItem(crv1)
    from vista.graph import TextLine
    tl = TextLine("")
    tl = li1.getElements(tl.getClass())[0];
    from java.awt import Font
    tl.originalFontSize=6
    if crv2:
	li2 = LegendItem(crv2)
	tl = li2.getElements(tl.getClass())[0];
	tl.originalFontSize=8
    if crv3:
	li3 = LegendItem(crv3)
	tl = li3.getElements(tl.getClass())[0];
	tl.originalFontSize=8
    leg.add(li1)
    if li2:
	leg.add(li2)
    if li3:
	leg.add(li3)
    # create plot and add curves and add legend
    pl = Plot() 
    pl.add(crv1)
    if crv2: pl.add(crv2)
    if crv3: pl.add(crv3)
    # set axis labels and font for it
    laxis = pl.getAxis(AxisAttr.LEFT);
    laxis.setAxisLabel(axisLabel)
    laxis.percentMajorTickLength=0.4
    laxis.percentMinorTickLength=0.2
    ttc = Class.forName("vista.graph.TickText");
    tt = laxis.getElements(ttc) [0]
    tt.attributes._originalFontSize=10
    alc = Class.forName("vista.graph.TextLine");
    axisLabel = laxis.getElements(alc)[0]
    axisLabel.attributes._originalFontSize=10
    #
    baxis = pl.getAxis(AxisAttr.LEFT);
    baxis.percentMajorTickLength=0.4
    baxis.percentMinorTickLength=0.2
    ttc = Class.forName("vista.graph.TickText");
    tt = baxis.getElements(ttc) [0]
    tt.attributes._originalFontSize=8
    alc = Class.forName("vista.graph.TextLine");
    axisLabel = baxis.getElements(alc)[0]
    axisLabel.attributes._originalFontSize=8
    #
    pl.add(leg)
    pl.addTitle(title)
    pl.addGrid(AxisAttr.LEFT)
    #pl.addLabel('An anchored label',0.05,0.2);
    return pl
# 
import string
def parseLine(line):
    """ parses line to extract
    file1,path1,file2,path2,file3,path3,legend1,legend2,legend3,title,timewindow
    If parse is successfull it returns them in an array in the above order
    else it returns a None
    """
    line = string.strip(line[:-1]) # take off the eol marker and extra spaces
    splits=string.split(line,",") # split line at the comma delimiters
    if len(splits) == 12 or len(splits) == 9:
	for i in range(len(splits)):
	    str = splits[i]
	    splits[i] = string.strip(str)
    else :
	print 'Incorrect number of fields in line ' , line
	return None # too few, return None
    return splits
# by default it will read from plots.data, plot 3 graphs per page and not do diff plots
# For difference plots : do3plots('xyz.dat',4,1)
# The last together parameter if 1 will plot diff and compare plots
# together and diffplot parameter will be ignored
def do3plots(plotfile='plots.data',ppg=3,diffplot=0,together=0):
    import string
    plots = []
    f=open(plotfile)
    for line in f.readlines()[1:]:
	splits=parseLine(line)
	if not splits: continue
	try:
	    if ( diffplot == 0 ) or together:
		if ( len(splits) == 12 ):
		    plots.append(c3plot(splits[0],splits[1],\
					splits[2],splits[3],\
					splits[4],splits[5],\
					splits[6],splits[7],\
					splits[8],splits[9],splits[10],\
					timewindow(splits[11])))
		else:
		    plots.append(c3plot(splits[0],splits[1],\
					splits[2],splits[3],\
					None, None,\
					splits[4],splits[5],\
					None,splits[6],splits[7],\
					timewindow(splits[8])))
	    if diffplot or together:
		if ( len(splits) == 12 ):
		    plots.append(c3diffplot(splits[0],splits[1],\
					    splits[2],splits[3],\
					    splits[4],splits[5],\
					    splits[6],splits[7],\
					    splits[8],splits[9],splits[10],\
					    timewindow(splits[11])))
		else:
		    plots.append(c3plot(splits[0],splits[1],\
					splits[2],splits[3],\
					None, None,\
					splits[4],splits[5],\
					None,splits[6],splits[7],\
					timewindow(splits[8])))
	except:
	    print sys.exc_info()[0]
	    print "Could not plot line: " + line
	    #print sys.last_traceback.dumpStack()
    # close the file
    f.close();
    # create Graph
    count = 0
    width=750
    height = 650
    from java.util import Date
    from vista.graph import TextLine, GEBorderLayout
    if len(plots) == 0 : return
    cpg = 1 # Columns of plots per page
    if together: cpg = 2
    for pl in plots:
	if not pl: continue
	count = count + 1
	# create a graph if ppg so dictates
	if (count%(ppg*cpg) == 1) or ppg == 1:
	    graph = Graph()
	    mp = MultiPlot(ppg,cpg)
	    graph.setInsets(Insets(20,0,0,0))
	    graph.add(mp)
	    graph.setTitle(graphTitle)
	# add the plot to the multiplot
	mp.add(pl);
	#pl.getLayout().setScaleComponents(0)
	# finally if we have enough plots lets make a frame
	if (count%(ppg*cpg) == 0 ) :
	    dg=DataGraphFrame(graph,'plot',0)
	    dg.setLocation(100,100);
	    dg.setSize(width,height);
	    dg.setVisible(1)
    #do the last one after loop
    if ( count%(ppg*cpg) != 0 ) and ppg != 1:
	dg=DataGraphFrame(graph,'plot',0)
	dg.setLocation(100,100)
	dg.setSize(width,height)
	dg.setVisible(1)
    # set the left axis range to desired
    #for pl in plots:
    #    laxis = pl.getAxis(AxisAttr.LEFT);
    #    laxis.setDCRange(0.0,20000.0);
>>>>>>> 1.6
#
if __name__ == 'main' :
    do3plots('plots.data',4,0)
    do3plots('plots.data',4,1)
    do3plots('plots.data',4,1,1)

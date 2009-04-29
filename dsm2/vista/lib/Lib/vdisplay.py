from vista.app import DataGraph, DataTable, MultiDataTable, GroupFrame, \
     SessionFrame, DefaultGraphBuilder
from vista.graph import GraphAttr, Plot, PlotAttr, \
     Axis, AxisAttr, GELineLayout, \
     GEAttr, GEContainer, FontResizeInteractor, \
     Legend, LegendAttr, LegendItem, \
     LegendItemAttr, MultiPlot, CurveAttr
from vista.graph import Graph, GECanvas, GraphUtils, SymbolFactory \

from vista.set import ProxyFactory, DefaultDataSet
from java.awt import Color
from vdss import gen_ref
#
cmap = {"red":Color.red,
        "green":Color.green,
        "blue":Color.blue,
        "cyan":Color.cyan,
        "orange":Color.orange,
        "magenta":Color.magenta,
        "yellow":Color.yellow,
        "black":Color.black
        }
#
symMap = {
    "circle"    : SymbolFactory.createCircle(0,Color.black,2),
    "triangle"  : SymbolFactory.createTriangle(0,Color.black,2),
    "square"    : SymbolFactory.createSquare(0,Color.black,2),
    "cross"     : SymbolFactory.createCross(0,Color.black,2),
    "hour-glass": SymbolFactory.createHourGlass(0,Color.black,2),
    "circle-filled"    : SymbolFactory.createCircle(1,Color.black,2),
    "triangle-filled"  : SymbolFactory.createTriangle(2,Color.black,2),
    "square-filled"    : SymbolFactory.createSquare(1,Color.black,2),
    "hour-glass-filled": SymbolFactory.createHourGlass(1,Color.black,2)
    }
#
def tabulate(*ref) :
    """
    tabulate(ref1,ref2,...)
    tabulates a given data reference or data set or a group of such items
    separated by commas
    """
    if ( ref == None ) : print 'Empty reference list'; return;
    ref=map(gen_ref,ref)
    if len(ref) == 1:
	if hasattr(ref[0],'__len__'):
	    MultiDataTable(ref[0])
	else: 
	    DataTable(ref[0])
    else:
	MultiDataTable(ref)
# define function to graph a given set of data references.
def plot(*reflist) :
    """
    plot(ref1,ref2,...)
    plots a given data reference or data set or a group of such items
    separated by commas
    """
    if ( reflist == None ) : print 'Empty reference list'; return;
    reflist=map(gen_ref,reflist)
    gb = DefaultGraphBuilder()
    for ref in reflist:
	if hasattr(ref,'__len__'):
	    for iref in ref :
		gb.addData(iref)
	else:
	    gb.addData(ref)
    graphs=gb.createGraphs()
    for graph in graphs: DataGraph(graph,"Graph")
#
#
# define function to graph a given set of data references.
def scatterplot(refx, refy) :
    """
    scatterplot(refx,refy):
    creates a scatter plot with refx along the x axis and refy along the y axis
    """
    refx = gen_ref(refx)
    refy = gen_ref(refy)
    refxy = ProxyFactory.createPairedTimeSeriesProxy(refx,refy)
    gb = DefaultGraphBuilder(); gb.addData(refxy);
    graphs = gb.createGraphs();
    DataGraph(graphs[0],'Scatter Plot')
#
def xyplot(x,y,
           xlabel="x axis", ylabel="y axis", title = "title",
           legend = "legend",
           color = "red", symbol = None, gridy = 1, gridx = 0):
    """
    xyplot(x,y,xlabel="x axis", ylabel="y axis", title = "title",
    legend = "legend",
    color = "red", symbol = None, gridy = 1, gridx = 0):
    uses vista's graphing capabilities to draw a simple line
    plot between x and y
    """
    ds = DefaultDataSet('',x,y)
    crv1 = CurveFactory.createCurve(ds,AxisAttr.BOTTOM, AxisAttr.LEFT, legend)
    crv1.attributes._drawLines = 1
    cc = cmap[color]
    if not cc:
        crv1.foregroundColor = Color.red
    else:
        crv1.foregroundColor = cc
    if symbol:
        crv1.drawSymbol = 1
        crv1.symbol = symMap[symbol]
    leg = Legend()
    li1 = LegendItem(crv1)
    pl = Plot()
    pl.add(crv1)
    laxis = pl.getAxis(AxisAttr.LEFT)
    if laxis:
        laxis.setAxisLabel(ylabel)
    baxis = pl.getAxis(AxisAttr.BOTTOM)
    if baxis:
        baxis.setAxisLabel(xlabel)
    pl.add(leg)
    pl.addTitle(title)
    if gridy:
        pl.addGrid(AxisAttr.LEFT)
    if gridx:
        pl.addGrid(AxisAttr.BOTTOM)
    return pl
#
def simple_plot(x,y,
                xlabel="x axis", ylabel="y axis", title = "title",
                legend = "legend",
                color = "red", symbol = None, gridy = 1, gridx = 0):
    """
    uses vista's graphing capabilities to draw a simple line
    plot between x and y 
    """
    pl = xyplot(x,y,xlabel,ylabel,title,legend,color,symbol,gridy,gridx)
    graph = Graph()
    graph.add(pl)
    graph.setTitle("")
    dg = DataGraph(graph,'',0)
    dg.setLocation(100,100)
    dg.setVisible(1)
    dg.setSize(600,400)
    return pl
#

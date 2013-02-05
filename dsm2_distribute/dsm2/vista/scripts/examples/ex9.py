from vutils import *
def simple_plot(x,y,
                xlabel="x axis", ylabel="y axis", title = "title",
                legend = "legend",
                color = "red", symbol = 0, gridy = 1, gridx = 0):
    """
    uses vista's graphing capabilities to draw a simple line
    plot between x an y 
    """
    cmap = {"red":Color.red,"green":Color.green,"blue":Color.blue}
    ds = DefaultDataSet('',x,y)
    crv1 = CurveFactory.createCurve(ds,AxisAttr.BOTTOM, AxisAttr.LEFT, legend)
    crv1.attributes._drawLines = 1
    cc = cmap[color]
    if not cc:
        crv1.foregroundColor = Color.red
    else:
        crv1.foregroundColor = cc
    crv1.drawSymbol = symbol
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
    graph = Graph()
    graph.add(pl)
    graph.setTitle("")
    dg = DataGraphFrame(graph,'',0)
    dg.setLocation(100,100)
    dg.setVisible(1)
    dg.setSize(600,400)
    return pl
#

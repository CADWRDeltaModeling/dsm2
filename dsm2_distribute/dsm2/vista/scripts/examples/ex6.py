from vutils import *
from vista.set import Stats, ElementFilterIterator
from vista.app import CurveFactory
#
g=opendss('../testdata/ex6.dss')
g.filterBy('stage')
#
def getLast25hours(ref):
 dsi = ref.getData().getIterator()
 esi = ElementFilterIterator(dsi, Constants.DEFAULT_FLAG_FILTER)
 while not esi.atEnd():
  esi.advance()
 esi.retreat()
 el = esi.getElement()
 endTime = TimeFactory.getInstance().createTime(el.getXString())
 tidalDay = TimeFactory.getInstance().createTimeInterval('25hours')
 startTime = endTime.create(endTime)
 startTime.incrementBy(tidalDay,-1)
 tw = TimeFactory.getInstance().createTimeWindow(startTime,endTime)
 return DataReference.create(ref,tw)
#
def getAmplitude(sname, gr):
 grc = gr.clone()
 grc.filterBy(sname)
 if len(grc) > 1 : 
  print "Too many pathnames for station: " , sname
  return 0
 elif len(grc) == 0 :
  print "No pathnames for station: " , sname
  return 0
 else:
  ref = getLast25hours(grc[0])
  return Stats.max(ref.getData()) - Stats.min(ref.getData())
# do alternative "BOTH" 
g2=g.clone()
g2.filterBy('BOTH')
amplsb = []
for i in range(3):
 chan = repr(i+1)
 amplsb.append(getAmplitude("00"+chan+"_0", g2));
# do alternative CHAN
g2 = g.clone()
g2.filterBy('DMS\+CHAN') # escape + character in reg exp
amplsc = []
for i in range(3):
 chan = repr(i+1)
 amplsc.append(getAmplitude("00"+chan+"_0", g2));
#
ds_both = DefaultDataSet('stage profiles (BOTH)',[1,2,3],amplsb);
ds_chan = DefaultDataSet('stage profiles (CHAN)',[1,2,3],amplsc);
crv_both = CurveFactory.createCurve(DefaultReference(ds_both),AxisAttr.BOTTOM, AxisAttr.LEFT, 'stage profile (BOTH)')
crv_chan = CurveFactory.createCurve(DefaultReference(ds_chan),AxisAttr.BOTTOM, AxisAttr.LEFT, 'stage profile (CHAN)')
crv_both.lineThickness=2; crv_both.foregroundColor=Color.blue;
crv_both.drawLines=1;
crv_chan.lineThickness=2; crv_chan.foregroundColor=Color.green;
crv_chan.drawLines=1;
crv_both.symbol.foregroundColor=Color.blue
crv_chan.symbol.foregroundColor=Color.green
legend = Legend(); 
legend.add(LegendItem(crv_both)); legend.add(LegendItem(crv_chan));
plot = Plot(); plot.add(crv_both); plot.add(crv_chan);

plot.addLegend(legend, AxisAttr.LEFT)
plot.addTitle("Stage Profiles BOTH vs CHAN");
plot.getAxis(AxisAttr.BOTTOM).setDCRange(0,4);
plot.getAxis(AxisAttr.LEFT).setDCRange(5.,6.5);
plot.getAxis(AxisAttr.BOTTOM).setAxisLabel("Node Numbers");
plot.getAxis(AxisAttr.LEFT).setAxisLabel("Stage Amplitude");
graph = Graph(); graph.add(plot);

dg=DataGraphFrame(graph,0);
dg.setTitle('STAGE PROFILES');
dg.setLocation(0,0); dg.setSize(600,400);
dg.setVisible(1);

import jarray
from hec.script import *
from hec.heclib.dss import *
from hec.hecmath import TimeSeriesMath, HecMath
from hec.gfx2d import G2dDialog, G2dLine, Symbol
from hec.io import PairedDataContainer,TimeSeriesContainer
from hec.gfx2d import G2dPanelProp
from java.util import Vector

def get_matching(dss,pattern):
    matches=dss.getCatalogedPathnames(pattern)
    if (len(matches) >= 1):
        return dss.get(matches[0])
    else:
        print 'No match for: %s, %s'%(pattern,matches)
        return None
def do_mb():
    dir='Z:/calibration/'
    type='FLOW'
    stime='01JAN2008 0000'
    etime='31DEC2008 2400'
    fileobs=dir+'observed/observed_flow_stage_for_compare_plots.dss'
    stations=['RSAC128','SSS','SUT','RSAC155']
    direction=[-1,-1,-1,1]
    dss=HecDss.open(fileobs,True)
    dss.setTimeWindow(stime,etime)
    
    plot=newPlot('Mass Balance Components (SAC)')
    mass_balance=None
    for i in range(0,len(stations)):
        sta=stations[i]
        dir=direction[i]
        data=get_matching(dss,'A=%s C=%s E=15MIN'%(sta,type))
        if data==None:
            data=get_matching(dss,'A=%s C=%s E=1HOUR'%(sta,type))
        data=TimeSeriesMath(data).transformTimeSeries("1HOUR", "", "AVE", 0)
        data=TimeSeriesMath(data.data)
        if dir==-1:
            data=data.negative();
        plot.addData(data.data)
        if mass_balance==None:
            mass_balance=data
        else:
            mass_balance=mass_balance.add(data)
    plot.showPlot();
    return mass_balance
def newPlot(title):
    plotProp = G2dPanelProp()
    plotProp.hasToolbar=False
    return G2dDialog(None,title,False,Vector(),plotProp)
if __name__=='__main__':
    d=do_mb()
    plot=newPlot('Mass Balance')
    plot.addData(d.data)
    d_daily=d.transformTimeSeries("1DAY", "", "AVE", 0)
    plot.addData(d_daily.data)
    plot.showPlot()
import jarray
from hec.script import *
from hec.hecmath import *
from hec.heclib.dss import *
from hec.hecmath import TimeSeriesMath, HecMath
from hec.gfx2d import G2dDialog, G2dLine, Symbol
from hec.io import PairedDataContainer,TimeSeriesContainer
from hec.gfx2d import G2dPanelProp
from java.util import Vector

def get_matching(dss,pattern):
    matches=dss.getCatalogedPathnames(pattern)
    if (len(matches) >= 1):
        return dss.read(matches[0])
    else:
        print 'No match for: %s, %s'%(pattern,matches)
        return None
def do_mb(dss,stations,direction,type,model=False):
    mass_balance=None
    loc=''
    for i in range(0,len(stations)):
        sta=stations[i]
        dir=direction[i]
        if model:
            match_str="B=%s C=%s"
        else:
            match_str="A=%s C=%s"
        data=get_matching(dss,match_str%(sta,type))
        if data==None:
            print 'No match for '+match_str%(sta,type)
        if dir==-1:
            data=data.multiply(-1);
            loc=loc+'-'
        else:
            loc=loc+'+'
        loc=loc+str(data.data.location)
        if mass_balance==None:
            mass_balance=godin_filter(data)
        else:
            mass_balance=mass_balance.add(godin_filter(data))
    mass_balance.location='MASS-BALANCE '+loc
    return mass_balance
def newPlot(title):
    plotProp = G2dPanelProp()
    plotProp.hasToolbar=False
    return G2dDialog(None,title,False,Vector(),plotProp)
def plot_with_daily_avg(d):
    plot=newPlot('Mass Balance')
    #plot.addData(d.data)
    d_daily=d.transformTimeSeries("1DAY", "", "AVE", 0)
    plot.addData(d_daily.data)
    d_monthly=d.transformTimeSeries("1MON","","AVE", 0)
    plot.addData(d_monthly.data)
    plot.showPlot()
def godin_filter(d):
    return d.forwardMovingAverage(24).forwardMovingAverage(24).forwardMovingAverage(25)
#
def plot_monthlys(dobs,dmod):
    plot=newPlot('Mass Balance (Observed vs Model)')
    dobs_monthly=dobs.transformTimeSeries("1DAY","","AVE", 0)
    dobs_monthly.version='OBS'
    dmod_monthly=dmod.transformTimeSeries("1DAY","","AVE", 0)
    dmod_monthly.version='MOD'
    plot.addData(dobs_monthly.data)
    plot.addData(dmod_monthly.data)
    plot.showPlot()
    plot.maximize()
    plot.repaint()
    import time; time.sleep(5)
    plot.saveToPng('z:/temp/mb_'+dobs.data.location+'.png')
#
if __name__=='__main__':
    type='FLOW'
    stime='01JAN2008 0000'
    etime='31DEC2008 2400'
    filemod='Z:/calibration/V810_recalibration/run26/text/output_test/hist_mini_calib_v811.dss'
    fileobs='Z:/calibration/observed/observed_flow_for_compare_plots.dss'
    dssobs=DSS.open(fileobs,stime,etime)
    dssmod=DSS.open(filemod,stime,etime)
    #
    stations=['RSAC128','RSAC123','GSS', 'DLC']
    direction=[1,-1,-1,-1]
    dobs=do_mb(dssobs,stations,direction,type,False)
    stations=['RSAC128','RSAC123','GSS', 'DLC']
    direction=[1,-1,-1,-1]
    dmod=do_mb(dssmod,stations,direction,type,True)
    #plot_with_daily_avg(d)
    plot_monthlys(dobs,dmod)
    #
    stations=['RSAC155','RSAC128','SUT','SSS']
    direction=[1,-1,-1,-1]
    dobs=do_mb(dssobs,stations,direction,type,False)
    dmod=do_mb(dssmod,stations,direction,type,True)
    #plot_with_daily_avg(d)
    plot_monthlys(dobs,dmod)
    #
    stations=['RYI','SSS','HWB','RSAC123','RSAC101']
    direction=[1,1,1,1,-1]
    dobs=do_mb(dssobs,stations,direction,type,False)
    dmod=do_mb(dssmod,stations,direction,type,True)
    #plot_with_daily_avg(d)
    plot_monthlys(dobs,dmod)
    #
    stations=['DLC','GSS','MOK','LPS','RMKL070']
    direction=[1,1,-1,-1,1]
    dobs=do_mb(dssobs,stations,direction,type,False)
    dmod=do_mb(dssmod,stations,direction,type,True)
    #plot_with_daily_avg(d)
    plot_monthlys(dobs,dmod)
    #
    dssobs.close()
    dssmod.close()
#

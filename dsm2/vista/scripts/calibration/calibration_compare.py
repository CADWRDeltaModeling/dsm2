import jarray
from hec.script import *
from hec.heclib.dss import *
from hec.hecmath import TimeSeriesMath, HecMath
from hec.gfx2d import G2dDialog, G2dLine, Symbol
from hec.io import PairedDataContainer
from hec.gfx2d import G2dPanelProp
from java.util import Vector

from org.w3c.dom import Document
import java
import math
from java.awt import Color
from javax.swing import *


class InputPanel(JPanel):
    def __init__(self):
        from java.util.prefs import Preferences
    
        prefs=Preferences.userRoot().node("gov.ca.dsm2.calibration.compare");
        
        self.type=JTextField(prefs.get("type","EC"),15)
        self.stime=JTextField(prefs.get("stime","02JUL2001 0000"),15)
        self.etime=JTextField(prefs.get("etime","31DEC2001 0000"),15)
        self.fileobs=JTextField(prefs.get("file.obs","Z:/condor_jobs_cmaes/output_obs/Observed_data_all_forViz.dss"),25)
        self.filerun1=JTextField(prefs.get("file.run1","Z:/condor_jobs_cmaes/output_obs/previous_run_results/hist_mini_calib_v811_317.dss"),25)
        self.filerun2=JTextField(prefs.get("file.run2","Z:/condor_jobs_cmaes/output_test/hist_mini_calib_v811_0.dss"),25)
        self.image_dir=JTextField(prefs.get("image.dir","d:/temp"),25)
        self.locations=JTextField(prefs.get("locations","RSAC077,RSAN007,RSAC081,RSAC092,RSAN018,RSAN032,ROLD024,CLIFTON_COURT"),35)
        
        self.runButton=JButton("Run",actionPerformed=self.dorun)
        from net.miginfocom.swing import MigLayout
        self.setLayout(MigLayout())
        self.add(JLabel("Output Type"))
        self.add(self.type,"wrap")
        self.add(JLabel("Start Time"))
        self.add(self.stime,"gap unrelated")
        self.add(JLabel("End Time"))
        self.add(self.etime,"wrap")
        self.add(JLabel("Observed File:"))
        self.add(self.fileobs,"spanx, growx")
        self.add(JLabel("Run1 File:"))
        self.add(self.filerun1,"spanx, growx")
        self.add(JLabel("Run2 File:"))
        self.add(self.filerun2,"spanx, growx")
        self.add(JLabel("Image Directory"))
        self.add(self.image_dir,"spanx, growx")
        self.add(JLabel("Locations"))
        self.add(self.locations,"spanx, growx")
        self.add(self.runButton)
        
    def dorun(self, event):
        doall(str.split(str(self.locations.text),","),str(self.fileobs.text),str(self.filerun1.text),str(self.filerun2.text),str(self.stime.text),str(self.etime.text),str(self.image_dir.text),None)

        from java.util.prefs import Preferences
        prefs=Preferences.userRoot().node("gov.ca.dsm2.calibration.compare");
        prefs.put("type",self.type.text)
        prefs.put("stime",self.stime.text)
        prefs.put("etime",self.etime.text)
        prefs.put("file.obs",self.fileobs.text)
        prefs.put("file.run1",self.filerun1.text)
        prefs.put("file.run2",self.filerun2.text)
        prefs.put("image.dir",self.image_dir.text)
        prefs.put("locations",self.locations.text)


def get_matching(dss,pattern):
    matches=dss.getCatalogedPathnames(pattern)
    if (len(matches) >= 1):
        return dss.get(matches[0])
    else:
        print 'No match for: %s, %s'%(pattern,matches)
        return None
def removeToolbar(panel):
    components=panel.getComponents();
    panel.remove(components[-1])
def createRegressionLine(drunm,dobsm,path):
    paired=dobsm.generateDataPairs(drunm,False)
    pairedData = paired.data
    pairedData.fullName=path
    reg=dobsm.multipleRegression([drunm],HecMath.UNDEFINED, HecMath.UNDEFINED)
    regData=reg.data
    a=regData.yOrdinates[0][1]
    b=regData.yOrdinates[0][0]
    regData.fullName="//REGRESSION LINE////GENERATED/"
    maxVal=drunm.max()
    minVal=drunm.min()
    regData.xOrdinates[0]=a*minVal+b
    regData.xOrdinates[1]=a*maxVal+b
    regData.yOrdinates[0][0]=minVal
    regData.yOrdinates[0][1]=maxVal
    regData.yunits = pairedData.yunits
    regData.xunits = pairedData.xunits
    regData.xtype=pairedData.xtype
    regData.ytype=pairedData.ytype
    regData.xparameter=pairedData.xparameter
    regData.yparameter=pairedData.yparameter
    regData.location=pairedData.location
    regData.version='LINEAR REGRESSION'
    return regData,pairedData
def do_regression_plots(dobsm, drun1m, drun2m):
    if dobsm == None:
        return None
    from hec.lang import DSSPathString
    obspath=DSSPathString(dobsm.path)
    path=DSSPathString(drun1m.path)
    path.setFPart(path.getFPart()+":1:"+obspath.getFPart())
    try:
        regData1, pairedData1 = createRegressionLine(drun1m,dobsm,path.getPathname())
    except:
        return
    if drun2m != None:
        path=DSSPathString(drun2m.path)
        path.setFPart(path.getFPart()+":2:"+obspath.getFPart())
        regData2, pairedData2 = createRegressionLine(drun2m,dobsm,path.getPathname())
    plots = newPlot("");
    plots.addData(regData1)
    plots.addData(pairedData1)
    if drun2m != None:
        plots.addData(regData2)
        plots.addData(pairedData2)
    plots.showPlot()
    #plots.getViewport(regData1).getAxis("x1").setViewLimits(0,10000)
    #plots.getViewport(regData1).getAxis("y1").setViewLimits(0,10000)
    pline = plots.getCurve(regData1)
    pline.setLineColor("red")
    pline = plots.getCurve(pairedData1)
    pline.setLineVisible(0)
    pline.setSymbolType(Symbol.SYMBOL_CIRCLE)
    pline.setSymbolsVisible(1)
    pline.setSymbolSize(3)
    pline.setSymbolFillColor(pline.getLineColorString())
    pline.setSymbolLineColor(pline.getLineColorString())
    if drun2m != None:
        pline = plots.getCurve(pairedData2)
        pline.setLineColor("blue")
        pline.setLineVisible(0)
        pline.setSymbolType(Symbol.SYMBOL_SQUARE)
        pline.setSymbolsVisible(1)
        pline.setSymbolSize(3)
        pline.setSymbolFillColor(pline.getLineColorString())
        pline.setSymbolLineColor(pline.getLineColorString())
    g2dPanel = plots.getPlotpanel()
    g2dPanel.revalidate()
    g2dPanel.paintGfx()
    plots.setVisible(False)
    return plots
def newPlot(title):
    plotProp = G2dPanelProp()
    plotProp.hasToolbar=False
    return G2dDialog(None,title,False,Vector(),plotProp)
def saveToPNG(p,filename):
    from java.awt.image import BufferedImage
    from javax.imageio import ImageIO
    from java.io import File
    bi = BufferedImage(p.size.width, p.size.height, BufferedImage.TYPE_INT_ARGB) 
    g = bi.createGraphics()
    p.invalidate()
    p.validate()
    p.paint(g)
    g.dispose();
    ImageIO.write(bi,"png", File(filename));
def calculateRMS(run,obs):
    runt = TimeSeriesMath(run)
    obst = TimeSeriesMath(obs)
    tavg = obst.abs().sum()/obst.numberValidValues()
    diff=runt.subtract(obst)
    return math.fabs(math.sqrt(diff.multiply(diff).sum()/diff.numberValidValues())/tavg)*math.log(tavg)
    #return Stats.sdev(run-obs)/Stats.avg(obs)
def doall(locations, fileobs,filerun1,filerun2,stime,etime,imageDir='d:/temp',weights=None,filter_type="AVE",normalize=False):
    obs=HecDss.open(fileobs,True)
    obs.setTimeWindow(stime,etime)
    run1=HecDss.open(filerun1,True)
    run1.setTimeWindow(stime,etime)
    if filerun2 != None:
        run2=HecDss.open(filerun2,True)
        run2.setTimeWindow(stime,etime)
    else:
        run2=None
    rms1=0
    rms1_min,rms1_max=0,0
    rms2=0
    rms2_min,rms2_max=0,0
    rmsmap={}
    #run2=None
    sumwts=0
    average_interval=None;
    for l in locations:
        dobs=get_matching(obs,'A=%s C=%s E=15MIN'%(l,type))
        if dobs == None:
            dobs=get_matching(obs,'A=%s C=%s E=1DAY'%(l,type))
        if dobs == None:
            dobs=get_matching(obs,'A=%s C=%s E=IR-DAY'%(l,type))
        if dobs == None:
            dobs=get_matching(obs,'A=%s C=%s E=1HOUR'%(l,type))
        drun1=get_matching(run1,'B=%s C=%s'%(l,type))
        if run2 != None:
            drun2=get_matching(run2, 'B=%s C=%s'%(l,type))
        else:
            drun2=None
        avg_intvl="1DAY"
        if dobs != None:
            if average_interval != None:
                dobsd=TimeSeriesMath(dobs).transformTimeSeries(average_interval, None, filter_type, 0)
            else:
                dobsd=TimeSeriesMath(dobs)
            if normalize:
                dobsd=dobsd.divide(TimeSeriesMath(dobs).mean())
            dobsm=TimeSeriesMath(dobs).transformTimeSeries(avg_intvl, None, filter_type, 0)
            dobsm_max=TimeSeriesMath(dobs).transformTimeSeries(avg_intvl, None, "MAX", 0)
            dobsm_max.data.fullName=dobsm_max.data.fullName+"MAX"
            dobsm_min=TimeSeriesMath(dobs).transformTimeSeries(avg_intvl, None, "MIN", 0)
            dobsm_min.data.fullName=dobsm_min.data.fullName+"MIN"
            if normalize:
                dobsm=dobsm.divide(TimeSeriesMath(dobs).mean())
        if drun1==None:
            continue;
        else:
            if average_interval != None:
                drun1d=TimeSeriesMath(drun1).transformTimeSeries(average_interval, None, filter_type, 0)
            else:
                drun1d=TimeSeriesMath(drun1)
            if normalize:
                drun1d=drun1d.divide(TimeSeriesMath(drun1).mean())
            if drun2 != None:
                if average_interval != None:
                    drun2d=TimeSeriesMath(drun2).transformTimeSeries(average_interval, None, filter_type, 0)
                else:
                    drun2d=TimeSeriesMath(drun2)
                if normalize:
                    drun2d=drun2d.divide(TimeSeriesMath(drun2).mean())
            drun1m=TimeSeriesMath(drun1).transformTimeSeries(avg_intvl, None, filter_type, 0)
            drun1m_max=TimeSeriesMath(drun1).transformTimeSeries(avg_intvl, None, "MAX", 0)
            drun1m_min=TimeSeriesMath(drun1).transformTimeSeries(avg_intvl, None, "MIN", 0)
            if normalize:
                drun1m=drun1m.divide(TimeSeriesMath(drun1).mean())
            if drun2 != None:
                drun2m=TimeSeriesMath(drun2).transformTimeSeries(avg_intvl, None, filter_type, 0)
                drun2m_max=TimeSeriesMath(drun2).transformTimeSeries(avg_intvl, None, "MAX", 0)
                drun2m_min=TimeSeriesMath(drun2).transformTimeSeries(avg_intvl, None, "MIN", 0)
                if normalize:
                    drun2m=drun2m.divide(TimeSeriesMath(drun2).mean())
            else:
                drun2m=None
        if weights != None:
            sumwts=sumwts+weights[l]
            lrms1 = calculateRMS(drun1m.data, dobsm.data)*weights[l]
            lrms1_min=calculateRMS(drun1m_min.data,dobsm_min.data)*weights[l]
            lrms1_max=calculateRMS(drun1m_max.data,dobsm_max.data)*weights[l]
            rms1=rms1+lrms1
            rms1_min=rms1_min+lrms1_min
            rms1_max=rms1_max+lrms1_max
            lrms2 = calculateRMS(drun2m.data,dobsm.data)*weights[l]
            lrms2_min=calculateRMS(drun2m_min.data,dobsm_min.data)*weights[l]
            lrms2_max=calculateRMS(drun2m_max.data,dobsm_max.data)*weights[l]
            rmsmap[l] = lrms1,lrms2,lrms1_min,lrms2_min,lrms1_max,lrms2_max
            rms2=rms2+lrms2
            rms2_min=rms2_min+lrms2_min
            rms2_max=rms2_max+lrms2_max
        plotd = newPlot("Hist vs New Geom [%s]"%l)
        if dobs != None:
            plotd.addData(dobsd.data)
        plotd.addData(drun1d.data)
        if drun2 != None:
            plotd.addData(drun2d.data)
        plotd.showPlot()
        legend_label = plotd.getLegendLabel(drun1d.data)
        legend_label.setText(legend_label.getText()+" ["+str(int(lrms1*100)/100.)+","+str(int(lrms1_min*100)/100.)+","+str(int(lrms1_max*100)/100.)+"]")
        legend_label = plotd.getLegendLabel(drun2d.data)
        legend_label.setText(legend_label.getText()+" ["+str(int(lrms2*100)/100.)+","+str(int(lrms2_min*100)/100.)+","+str(int(lrms2_max*100)/100.)+"]")
        plotd.setVisible(False)
        xaxis=plotd.getViewport(0).getAxis("x1")
        vmin =xaxis.getViewMin()+261500. # hardwired to around july 1, 2008
        xaxis.setViewLimits(vmin,vmin+10000.)
        if dobs != None:
            pline = plotd.getCurve(dobsd.data)
            pline.setLineVisible(1)
            pline.setLineColor("blue")
            pline.setSymbolType(Symbol.SYMBOL_CIRCLE)
            pline.setSymbolsVisible(0)
            pline.setSymbolSize(3)
            pline.setSymbolSkipCount(0)
            pline.setSymbolFillColor(pline.getLineColorString())
            pline.setSymbolLineColor(pline.getLineColorString())
            g2dPanel = plotd.getPlotpanel()
            g2dPanel.revalidate();
            g2dPanel.paintGfx();
        plotm = newPlot("Hist vs New Geom Monthly [%s]"%l)
        plotm.setSize(1800,1200)
        if dobs != None:
            plotm.addData(dobsm.data)
           #plotm.addData(dobsm_max.data)
            #plotm.addData(dobsm_min.data)
        plotm.addData(drun1m.data)
        #plotm.addData(drun1m_max.data)
        #plotm.addData(drun1m_min.data)
        if drun2 != None:
            plotm.addData(drun2m.data)
            #plotm.addData(drun2m_max.data)
            #plotm.addData(drun2m_min.data)
        plotm.showPlot()
        if dobs != None:
            pline = plotm.getCurve(dobsm.data)
            pline.setLineVisible(1)
            pline.setLineColor("blue")
            pline.setSymbolType(Symbol.SYMBOL_CIRCLE)
            pline.setSymbolsVisible(0)
            pline.setSymbolSize(3)
            pline.setSymbolSkipCount(0)
            pline.setSymbolFillColor(pline.getLineColorString())
            pline.setSymbolLineColor(pline.getLineColorString())
        plotm.setVisible(False)
        if dobs != None:
            plots=do_regression_plots(dobsm,drun1m,drun2m)
            if plots != None:
                spanel = plots.getPlotpanel()
                removeToolbar(spanel)
        mpanel = plotm.getPlotpanel()
        removeToolbar(mpanel)
        dpanel = plotd.getPlotpanel()
        removeToolbar(dpanel)
        from javax.swing import JPanel,JFrame
        from java.awt import GridBagLayout, GridBagConstraints
        mainPanel = JPanel()
        mainPanel.setLayout(GridBagLayout())
        c=GridBagConstraints()
        c.fill=c.BOTH
        c.weightx,c.weighty=0.5,1
        c.gridx,c.gridy,c.gridwidth,c.gridheight=0,0,10,4
        if dobs != None:
            if plots != None:
                pass
                #mainPanel.add(spanel,c)
        c.gridx,c.gridy,c.gridwidth,c.gridheight=0,0,10,4
        c.weightx,c.weighty=1,1
        mainPanel.add(mpanel,c)
        c.gridx,c.gridy,c.gridwidth,c.gridheight=0,4,10,6
        mainPanel.add(dpanel,c)
        fr=JFrame()
        fr.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)
        fr.getContentPane().add(mainPanel)
        fr.setSize(1100,850);
        fr.show();
        mainPanel.setSize(1100,850);
        mainPanel.setBackground(Color.WHITE);
        #import time; time.sleep(5)
        saveToPNG(mainPanel,imageDir+l+".png")
    if weights != None:
        rms1=(rms1+rms1_min+rms1_max)/sumwts
        rms2=(rms2+rms2_min+rms2_max)/sumwts
        print 'RMS Run 1: %f'%rms1
        print 'RMS Run 2: %f'%rms2
        for loc in rmsmap.keys():
            print loc, rmsmap[loc] 
def gui():
    fr = JFrame('Calibration Compare Tool',defaultCloseOperation=JFrame.EXIT_ON_CLOSE)
    fr.add(InputPanel())
    fr.pack()
    fr.setVisible(True)
if __name__=='__main__':
    #gui()
    #dir='D:/models/DSM2v8.1.x/Historical_MiniCalibration_811_MTZ_ts_corrected/'
    dir='Z:/calibration/'
    #dir='Z:/calibration/V810_recalibration/'
    type='FLOW'
    stime='01MAY2008 0000'
    etime='30JUN2008 2400'
    fileobs=dir+'observed/observed_flow_for_compare_plots.dss'
    #fileobs=dir+'V810_recalibration/observeddata/observed_flow.dss'
    #filerun1=dir+'output_test/hist_mini_calib_v811_303.dss'
    filerun1=dir+'V810_recalibration/run26/text/output_test/hist_mini_calib_v811.dss'
    #filerun2=dir+'archives/hist_mini_calib_v811_1755.dss'
    filerun1=dir+'output_test/hist_mini_calib_v811_0.dss'
    filerun2=dir+'output_test/hist_mini_calib_v811_3958.dss'
    #filerun1=dir+'run_template_copy/output_test/hist_mini_calib_v811_BASE_RUN.dss'
    #filerun2=dir+'run_template_sac_bndry/output_test/hist_mini_calib_v811_SAC_BNDRY_335.dss'
    imageDir='z:\\temp\\'
    locations=["RSAC155","RSAC128","RSAC123","RSAC101","SSS", "SUT", "DLC", "SLTRM004","GSS"]
    #locations+=["GLC","FAL","HOL","HWB","LPS","MOK","ORQ","OSJ"]
    #locations=["RMID005","RMID015"]
    #locations+=["ROLD024","ROLD034","ROLD047","ROLD074"]
    #locations+=["RSAN018","RSAN037","RSAN058","RSAN063","RSAN072","RSAN087"]
    #locations+=["RYI","SJL","SLDUT007","SLMZU025","TRN"]
    #locations=["RSAC123"]
    weights={}
    for l in locations: weights[l]=1.0
    doall(locations,fileobs,filerun1,filerun2,stime,etime,imageDir,weights,"AVE")
    print 'END'
    
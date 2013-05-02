import string, re, os, posixpath, sys, math
import glob
from vtimeseries import *
#from vista.time import *
from vdss import *
from vista.set import *
#from vista.set import Group
from vista.set import PathnamePredicate
from vista.db.dss import *
from vutils import *
from vista.time import TimeFactory

def GetElement (ds, t):
    """
    Given a dataset ds and time object or string t, return the element
    (time and value) from the dataset, which time spans or is at the
    time.
    """
    indx = dsIndex(ds,t)
    return ds.getElementAt(indx)
#
if __name__ == '__main__':
    TF = TimeFactory.getInstance()
    # Generate flow ratios (Delta Cross Channel / Sacramento R @ Freeport) but only when
    # DCC is open 
    DSSDir = 'Z:/Cortopossi/'
    DSSFileQ = 'hist_mini_calib_NHC_baserun_201108.dss'
    DSSFileGate = 'DCCGate.dss'
    FPTBPart = '414_0'
    DCCBPart = '365_0'
    GSlBPart = '366_0'
    NMokeBPart = '330_0'
    startWY = 1991
    endWY = 2010
    
    dss_groupDCCGate = opendss(DSSDir + DSSFileGate)
    data_setDCCGate = dss_groupDCCGate.getDataReference(0).getData()
    
    dss_groupQ=opendss(DSSDir + DSSFileQ)
    data_refFPT = findparts(dss_groupQ,b=FPTBPart,c='FLOW')
    data_refDCC = findparts(dss_groupQ,b=DCCBPart,c='FLOW')
    data_refGSl = findparts(dss_groupQ,b=GSlBPart,c='FLOW')
    data_refNMoke = findparts(dss_groupQ,b=NMokeBPart,c='FLOW')
    data_setFPT = data_refFPT[0].getData()
    data_setDCC = data_refDCC[0].getData()
    data_setGSl = data_refGSl[0].getData()
    data_setNMoke = data_refNMoke[0].getData()
    # Calculate yearly flow averages when DCC is open,
    # yearly flow ratios, write to ascii file
    WYResultsOut = DSSDir + 'Results.txt'
    fid_WYResults = open(WYResultsOut, 'w')
    fid_WYResults.write('DSM2 Sac R/DCC/Georgiana Sl Flow Ratios with DCC fully open\n\n')
    fid_WYResults.write('WY\tFreeport Ave Q\tDCC Ave Q\tRatio\tGSl Ave Q\tRatio\tNMoke Ave Q\n')
    # write monthly results to file for plotting: SacQ vs DCCQ, NMokeQ vs DCCQ
#    PlotResultsOut = DSSDir + 'Plot.txt'
#    fid_PlotResults = open(PlotResultsOut, 'w')
#    fid_PlotResults.write('WY\tFreeport Ave Q\tDCC Ave Q\tNMoke Ave Qn')
    # get run time window
    tw = data_refFPT[0].getTimeWindow()
    tStart = tw.getStartTime()
    tEnd = tw.getEndTime
    # loop over all water years
    nPtsTot = 0
    TOTQFPT = 0.
    TOTQDCC = 0.
    TOTQGSl = 0.
    for wy in range(startWY, endWY+1):
        totQFPT = 0.
        totQDCC = 0.
        totQGSl = 0.
        totQNMoke = 0.
        nPts = 0
        # get the Water Year start date
        startTime = TF.createTime('01OCT' + str(wy-1) + ' 0000')
        endTime = TF.createTime('30SEP'+ str(wy) + ' 0000')
        twWY = TF.createTimeWindow(startTime, startTime + timeinterval('1YEAR'))
        sti = dsIndex(data_setFPT, startTime)
        eti = dsIndex(data_setFPT, endTime) + 1
        for ndx in range(sti, eti):
            t = TF.createTime(long(data_setDCC.getElementAt(ndx).x))
#            print t
            ElPosDCC = GetElement(data_setDCCGate,t)
#            QDCC = data_setDCC.getElementAt(ndx).y
#            tDCC = TF.createTime(long(ElPosDCC.x))
#            print t,tDCC,ElPosDCC.y,ElPosDCC.y > 1.5,QDCC,abs(QDCC) > 5.0
            if ElPosDCC.y >= 1.9:  # indicates DCC is open
                nPts += 1
                nPtsTot += 1
                QDCC = data_setDCC.getElementAt(ndx).y
                QFPT = data_setFPT.getElementAt(ndx).y
                QGSl = data_setGSl.getElementAt(ndx).y
                QNMoke = data_setNMoke.getElementAt(ndx).y
                totQFPT += QFPT
                totQDCC += QDCC
                totQGSl += QGSl
                totQNMoke += QNMoke
            #
        # end of loop within WY
        TOTQFPT += totQFPT
        TOTQDCC += totQDCC
        TOTQGSl += totQGSl
        outStr = '%d\t%8.1f\t%7.1f\t\t%5.2f\t%7.1f\t\t%5.2f\t%7.1f\n' % \
            (wy,totQFPT/nPts,totQDCC/nPts,totQDCC/totQFPT,totQGSl/nPts, \
             totQGSl/totQFPT,totQNMoke/nPts)
        fid_WYResults.write(outStr)
    # end of loop for all WYs
    outStr = '\nAve\t%8.1f\t%7.1f\t\t%5.2f\t%7.1f\t\t%5.2f\n' % \
        (TOTQFPT/nPtsTot,TOTQDCC/nPtsTot,TOTQDCC/TOTQFPT,TOTQGSl/nPtsTot,TOTQGSl/TOTQFPT)
    fid_WYResults.write(outStr)
    fid_WYResults.close()
    print 'End processing.'
    #

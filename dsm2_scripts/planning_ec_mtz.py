''' Eli Ateljevich July 12, 2001
    Purpose: This script estimates Martinez EC for a multi-year planning run. 
    modified by Nicky Sandhu, Yu Zhou 2017/10
'''
# model requirement: dsm2-vista
# model Input:
#     ndo: Net Delta Outflow, 15MIN
#     astro: bandlimited estimate of astronomical stage (NGVD), 15min
# model Output:
#     EC: 15min

import os, string
import config, ec_boundary
from calsim_study_fpart import calsim_study_fpart
from vdss import opendss,findpath,writedss
from vtimeseries import timewindow   # for monthly add ,interpolate
from vmath import godin
from vista.set import DataReference
from planning_time_window import prepro_window,grow_window  

def planning_ec_mtz(): # MTZ = RSAC054 BC for the qual
    DEBUG = 0
    OUTPUT=config.getAttr('QUALBOUNDARYFILE')
    calsimfile = config.getAttr('CALSIMFILE')
    CALSIM=opendss(calsimfile)
    PLANNINGTIDE=opendss(config.getAttr('STAGE_SOURCE_FILE'))
    outputpath="/FILL+CHAN/RSAC054/EC//15MIN/"+config.getAttr("DSM2MODIFIER")+"/"
    if not(OUTPUT and os.path.exists(OUTPUT)):
        raise "Envvar QUALBOUNDARYFILE must exist as destination for EC"
        
    startyr=int(config.getAttr('START_DATE')[5:])
    endyr=int(config.getAttr('END_DATE')[5:])
    
    if (startyr < 1974 and endyr > 1991):
        blocks= [ "01NOV1921 0000 - 01OCT1940 0000",
             "01OCT1940 0000 - 01OCT1960 0000",
             "01OCT1960 0000 - 01OCT1974 0000",
             "01OCT1974 0000 - 01OCT1991 0000",
             "01OCT1991 0000 - 01OCT2003 0000"
                ]                                     # for memory reasons (year 2001).
    else: 
        blocks = [ "01OCT1974 0000 - 01OCT1991 0000" ]

    g0=5000.                                          # initial value of g (antecedent outflow) for the beginning
                                                      # of the first year. This is pretty arbitrary and makes little difference

    for twstr in blocks:    
        TWIND=timewindow(twstr)        # Actual period to be estimated
        print "Calculating boundary salinity for the period "+TWIND.toString()
        TWINDBUF=grow_window(TWIND,"1MON","1MON")     # Conservative buffered period for retrieval
                                                      # so that after prelimiary operations (e.g. time average)
                                                      # time series will still span at least TWIND
        fpart=calsim_study_fpart(modify=0)

        ndo15=DataReference.create(findpath(CALSIM,"/CALSIM/NDO/FLOW-NDO//15MIN/"
                                +fpart+"/")[0],TWIND).getData()
        
        astro_stage_version = config.getAttr("ASTRO_STAGE_VERSION")
        mtzastro=DataReference.create(findpath(PLANNINGTIDE,"/FILL\+CHAN/RSAC054/STAGE//15MIN/"+astro_stage_version + "/")[0],TWINDBUF).getData()
        if 'NAVD' in astro_stage_version: 
          mtzastro=mtzastro-2.68
          writedss(CALSIM,"/FILL\+CHAN/RSAC054/STAGE//15MIN/"+astro_stage_version.replace('NAVD','NGVD')+"/",mtzastro)
        
        astrorms=godin((mtzastro*mtzastro)**0.5)           # RMS energy of tide (used to predict filling and draining)
        dastrorms=(  (astrorms-(astrorms>>1))*96. ).createSlice(TWIND)    
        fifteenflo2=ndo15 - 53411.1*(dastrorms)

        # call to ec estimator. all parameters are included.
        so,sb=37196,2328.1
        c=[-6.00E-05,7.30E-05,-1.00E-05,-3.00E-05,1.70E-06,-1.00E-04,4.50E-05,-1.00E-04]
        [mtzecest, g1]=ec_boundary.ECEst(mtzastro,fifteenflo2,so,sb,beta=420.5205,npow1=0.7750588,npow2=1,g0=g0,zrms=astrorms,c=c)
        
        writedss(OUTPUT,outputpath,mtzecest)
            
        g0=g1
    return 0


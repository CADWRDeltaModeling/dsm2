# Eli Ateljevich July 12, 2001
# Purpose: This script estimates EC for a multi-year planning run. 

# Input
#     ndo: ndo time series
#     astro: bandlimited estimate of astronomical stage     

import config


def planning_ec_mtz(): # MTZ = RSAC054 BC for the qual
    from vdss import opendss,findpath,writedss
    from vtimeseries import timewindow   # for monthly add ,interpolate
    from vmath import godin
    from vista.set import DataReference
    import interpolate   # daily only
    import ec_boundary
    from jnios import os
    import config
    import string
    import conserve
    from calsim_study_fpart import calsim_study_fpart
    from vtimeseries import interpolate
    import vamp_ndo
    from planning_time_window import prepro_window,grow_window    
    DEBUG = 0
    OUTPUT=config.getAttr('QUALBOUNDARYFILE')
    calsimfile = config.getAttr('CALSIMFILE')
    vamp_corrected_dss = config.getAttr('CALSIM_VAMP')
    CALSIM=opendss(calsimfile)
    PLANNINGTIDE=opendss(config.getAttr('STAGE_SOURCE_FILE'))
    STEP=string.lower(config.getAttr('CALSIMSTEP'))
    SJR_PROCESS=config.getAttr("SJR_PROCESS")    
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
                ]
    else: 
        blocks = [ "01OCT1974 0000 - 01OCT1991 0000" ]
                                                      # for memory reasons (year 2001).

    g0=5000.                                          # initial value of g (antecedent outflow) for the beginning
                                                      # of the first year. This is pretty arbitrary and makes little difference
    if DEBUG:
        g0_no_vamp = 5000.

    for twstr in blocks:    
        TWIND=timewindow(twstr)        # Actual period to be estimated
        print "Calculating boundary salinity for the period "+TWIND.toString()
        TWINDBUF=grow_window(TWIND,"1MON","1MON")     # Conservative buffered period for retrieval
                                                      # so that after prelimiary operations (e.g. time average)
                                                      # time series will still span at least TWIND
        fpart=calsim_study_fpart(modify=0)
        ndo=DataReference.create(findpath(CALSIM,"/CALSIM/NDO/FLOW-NDO//"+STEP+"/"
                                  +fpart+"/")[0],TWIND).getData()
        ndo15=conserve.conserveSpline(ndo,"15MIN")
        ndo15_no_vamp = 0
        if DEBUG:
            ndo15_no_vamp = ndo15
        # calc  vamp caused ndo change
        if (SJR_PROCESS.upper()=="SINGLE_STEP") or (SJR_PROCESS.upper()=="MULTI_STEP"):
            fpart_modified=calsim_study_fpart(modify=1)
            delta_ndo = vamp_ndo.calc_vamp_delta_ndo(calsimfile,vamp_corrected_dss,fpart,fpart_modified,SJR_PROCESS)
            ndo15 = ndo15 + interpolate(delta_ndo, "15MIN")
		
        astro_stage_version = config.getAttr("ASTRO_STAGE_VERSION")
        mtzastro=DataReference.create(findpath(PLANNINGTIDE,"/FILL\+CHAN/RSAC054/STAGE//15MIN/"+astro_stage_version + "/")[0],TWINDBUF).getData()

        astrorms=godin((mtzastro*mtzastro)**0.5)           # RMS energy of tide (used to predict filling and draining)
        dastrorms=(  (astrorms-(astrorms>>1))*96. ).createSlice(TWIND)    
        fifteenflo2=ndo15  - 40000*(dastrorms)

        # call to ec estimator. all parameters are included. g0 is an
        [mtzecest, g1]=ec_boundary.ECEst(mtzastro,fifteenflo2,beta=600,npow1=0.75,npow2=1,g0=g0,zrms=astrorms)
        
        if DEBUG:
            fifteenflo2_no_vamp = ndo15_no_vamp  - 40000*(dastrorms)
            [mtzecest_no_vamp, g1_no_vamp]=ec_boundary.ECEst(mtzastro,fifteenflo2_no_vamp,beta=600,npow1=0.75,npow2=1,g0=g0_no_vamp,zrms=astrorms)
            g0_no_vamp = g1_no_vamp
            writedss("out_ec_check","/CALC/ndo_no_vamp/ndo////", ndo15_no_vamp)
            writedss("out_ec_check","/CALC/ndo_with_vamp/ndo////", ndo15)
            writedss("out_ec_check","/CALC/ndo_no_vamp/ec////", mtzecest_no_vamp)
            writedss("out_ec_check","/CALC/ndo_with_vamp/ec////", mtzecest)
        writedss(OUTPUT,outputpath,mtzecest)
            
        g0=g1
    return 0


# Eli Ateljevich July 12, 2001
# Purpose: This script estimates EC for a multi-year planning run. Planning runs
# are usually scheduled in terms of water years (e.g. 01OCT1974 - 30SEP1991).
# However, this script prepares EC estimates
# for the overlapping calander year (01JAN1974 - 30SEP1974) 

# Input
#     ndo: ndo time series
#     astro: bandlimited estimate of astronomical stage     


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
    
    OUTPUT=config.getAttr('QUALBOUNDARYFILE')
    CALSIM=opendss(config.getAttr('CALSIMFILE'))
    PLANNINGTIDE=opendss(config.getAttr('STAGEFILE'))
    STEP=string.lower(config.getAttr('CALSIMSTEP'))
    outputpath="/FILL+CHAN/RSAC054/EC//15MIN/"+config.getAttr("DSM2MODIFIER")+"/"
    if not(OUTPUT and os.path.exists(OUTPUT)):
        raise "Envvar QUALBOUNDARYFILE must exist as destination for EC"

    years= [ [1974,1979],[1980,1985],[1986,1992] ]    # the multi year block should be broken up into 5-6 year blocks
                                                      # for memory reasons (year 2001).

    g0=5000.                                          # initial value of g (antecedent outflow) for the beginning
                                                      # of the first year. This is pretty arbitrary and makes little difference

    for yearpair in years:
        syear = yearpair[0]
        eyear = yearpair[1]    
    
        TWIND=timewindow("01JAN%s 0000 - 01JAN%s 0000" % (syear, eyear+1))        # Actual period to be estimated
        print "Calculating boundary salinity for the period "+TWIND.toString()
        TWINDBUF=timewindow("26DEC%s 0000 - 04JAN%s 0000" % (syear-1, eyear+1) )     # Conservative buffered period for retrieval
                                                                                   # so that after prelimiary operations (e.g. time average)
                                                                                   # time series will still span at least TWIND
        ndo=DataReference.create(findpath(CALSIM,"/CALSIM/NDO/FLOW-NDO//"+STEP+"/"
                                  +os.environ['CALSIMSTUDY']+"/")[0],TWINDBUF).getData()
        ndo15=conserve.conserveSpline(ndo,"15MIN")
        mtzastro=DataReference.create(findpath(PLANNINGTIDE,"/FILL\+CHAN/RSAC054/STAGE//15MIN/"+config.getAttr("STAGE_VERSION") + "/")[0],TWINDBUF).getData()

        astrorms=godin((mtzastro*mtzastro)**0.5)           # RMS energy of tide (used to predict filling and draining)
        dastrorms=(  (astrorms-(astrorms>>1))*96. ).createSlice(TWIND)    
        fifteenflo2=ndo15  - 40000*(dastrorms)

        # call to ec estimator. all parameters are included. g0 is an
        [mtzecest, g1]=ec_boundary.ECEst(mtzastro,fifteenflo2,beta=600,npow1=0.75,npow2=1,g0=g0,zrms=astrorms)
        print "writing: %s::%s" % (OUTPUT,outputpath)
        writedss(OUTPUT,outputpath,mtzecest)
        print "done writing"
        g0=g1
    return 0


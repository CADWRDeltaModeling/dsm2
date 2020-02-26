''' NDO preparation any time-step to 15MIN for Martinez EC generation
    modified from CALSIM-DSM2 preprocess scripts 2001
    dsm2-vista is required to run
    by Yu Zhou 2017/10
'''

import sys,string
import config
from calsim_study_fpart import calsim_study_fpart
import conserve
from vista.set import DataReference
from vdss import opendss,findpath,writedss
from vtimeseries import timewindow

def prep_ndo(calsimf,dss_step,dss_fpart,twstr):
    ndofile = opendss(calsimf)
    TWIND=timewindow(twstr)
    ndo=DataReference.create(findpath(ndofile,"/CALSIM/NDO/FLOW-NDO//"+dss_step+"/"+dss_fpart+"/")[0],TWIND).getData()
    ndo15=conserve.conserveSpline(ndo,"15MIN")
    ndo15.getAttributes().setYUnits("CFS")
    ndo15.getAttributes().setYType("PER-AVER")
    writedss(calsimf,"/CALSIM/NDO/FLOW-NDO//15MIN/"+dss_fpart+"/",ndo15)
    return 0


def main():
    if len(sys.argv) != 2:
        raise SystemExit("""
        Usage: vscript prep_ec.py configfile
        where configfile is the input file for configuration variables
        (give full path if not in current running shell)
        """)
    else:
        print "prep NDO for Martinez EC"
        infile = sys.argv[1]
        config.setConfigVars(infile)
        calsimdss=config.getAttr("CALSIMFILE")
        fpart=calsim_study_fpart(modify=0)

    startyr=int(config.getAttr('START_DATE')[5:])
    endyr=int(config.getAttr('END_DATE')[5:])
    if (startyr < 1974 and endyr > 1991):
        twstr = "01NOV1921 0000 - 01OCT2003 0000"
    else: 
        twstr = "01OCT1974 0000 - 01OCT1991 0000"
    STEP=string.lower(config.getAttr('CALSIMSTEP'))
    
    prep_ndo(calsimdss,STEP,fpart,twstr)
    sys.exit()
    
        
if __name__ == '__main__':
    main()
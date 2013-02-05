"""Copy DICU flows.
   This script transfers stage from a DSS store to the boundary input file
"""
import sys
from vista.set import DataReference, Units
from vdss import opendss,findpath,writedss
from vtimeseries import timewindow
from config import getAttr,setConfigVars
from planning_time_window import prepro_window

def copy_stage():
    """ Unsmoothed transfer from DICU file to model input file.
    """
    sourcefile=getAttr("STAGE_SOURCE_FILE") 
    f=opendss(sourcefile)
    outfile=getAttr("STAGEFILE")
    stageversion=getAttr("STAGE_VERSION")
    dsspath="/FILL\+CHAN/RSAC054/STAGE//15MIN/%s/" % stageversion
    if not outfile or outfile == "":
        raise "Config variable BOUNDARYFILE not set and needed for prepro output"    
    tw=prepro_window()
    ref = findpath(f,dsspath)
    if len(ref) != 1:
        raise "Stage path not found or not unique. Found refs: %s" % ref
    ref=DataReference.create(ref[0],tw)
    data=ref.getData()
    writedss(outfile,ref.getPathname().toString(), data)

#
if __name__ == '__main__':
    if len(sys.argv) != 2:
        raise SystemExit("""
        Usage: vscript planning_dicu_flow.py configfile
        where configfile is the input file for configuration variables
        (give full path if not in current running shell)
        """)
    else:
        infile = sys.argv[1]
        setConfigVars(infile)
        print "Copying boundary stage to the planning input file"
        copy_stage()
        sys.exit()


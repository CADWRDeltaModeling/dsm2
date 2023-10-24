"""Copy DICU flows.
   This script transfers flows from the DICU file representing the correct
   DICI scenario (level of development)
"""
import sys
from vista.set import DataReference, Units
from vdss import opendss,findpath,writedss
from vtimeseries import timewindow
from config import getAttr,setConfigVars
from planning_time_window import prepro_window

def copy_dicu_flow():
    """ Unsmoothed transfer from DICU file to model input file.
    """
    dicufile=getAttr("DICUFLOWFILE")
    f=opendss(dicufile)           # open CALSIM file
    outfile=getAttr("DICUFILE")
    if not outfile or outfile == "":
        raise "Config variable BOUNDARYFILE not set and needed for prepro output"
    tw=prepro_window()

    for item in f :
        ref=DataReference.create(item,tw)
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
        print "Copying DICU flows to the planning input file"
        copy_dicu_flow()
        sys.exit()


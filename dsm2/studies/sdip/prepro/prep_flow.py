"""Prep boundary flows.
   This script transfers flows from the raw CALSIM file
   (CALSIMFILE in the config file) to the processed model
   input file (BOUNDARYINPUT in the config). CALSIM nodes that appear
   on the nodes_to_smooth list will be smoothed using a tension
   spline, which is recommended for any large flow with no tendency
   to go to zero. Calsim nodes on the nodes_to_transfer list
   will be moved unaltered.
"""

nodes_to_smooth=["C169","C644","C639"]
nodes_to_transfer=["C508","C501","C503",
                   "C508","C157",
                   "D408","D418","D419",
                   "D403A","D403B"
                   ]

import sys
import config
import conserve
from vista.set import DataReference, Units
from vdss import opendss,findpath,writedss
from vtimeseries import timewindow
from config import getAttr,setConfigVars

def calsim_path(calsimname):
    if calsimname.startswith("C"):
        return "/CALSIM/"+calsimname+"/FLOW-CHANNEL//1MON/" \
               + getAttr("CALSIMSTUDY") + "/"
    elif calsimname.startswith("D"):
        return "/CALSIM/"+calsimname+"/FLOW-DELIVERY//1MON/" \
               + getAttr("CALSIMSTUDY") + "/"
    else:
        raise "Unknown CALSIM prefix"
    
def smooth_flow():
    """ A slightly smoothed version of monthly flows to avoid sharp transitions
        between months. Uses a tension spline.
    """
    f=opendss(getAttr("CALSIMFILE"))           # open CALSIM file
    outfile=getAttr("BOUNDARYFILE")
    if not outfile or outfile == "":
        raise "Config variable BOUNDARYFILE not set and needed for prepro output"
    fpart=getAttr("DSM2MODIFIER")
    fpart = getAttr("CALSIMSTUDY")
    if not fpart:
        print "Config variable DSM2MODIFIER not set. Assuming blank, may cause unwanted behavior"
        fpart = ""
    
    tw=timewindow(getAttr("START_DATE")+ " 0000 - " + getAttr("END_DATE") + " 2400")

    for calsimname in nodes_to_smooth:      # Extend the list as needed, but please keep in mind the
                                            # limitations of the conservative spline, at least at present.
                                            # Mainly, input flows should be substantially greater than
                                            # zero at all times (yolo would be inappropriate, for instance)
        dsspath = calsim_path(calsimname)
        ref=DataReference.create(findpath(f,dsspath)[0],tw)
        monthly=ref.getData()
        if monthly:
            daily=conserve.conserveSpline(monthly,"1DAY")
            daily.getAttributes().setYUnits(Units.CFS)
            writedss(outfile,
                     "/CALSIM-SMOOTH/"+calsimname+"/FLOW/1DAY//" \
                     +fpart+"/",
                     daily)
        else:
            raise "Failure to find CALSIM input data for: " + calsimname 

def transfer_flow():
    """ Unsmoothed transfer from CALSIM file to model input file.
    """
    f=opendss(getAttr("CALSIMFILE"))           # open CALSIM file
    outfile=getAttr("BOUNDARYFILE")
    if not outfile or outfile == "":
        raise "Config variable BOUNDARYFILE not set and needed for prepro output"    
    tw=timewindow(getAttr("START_DATE")+ " 0000 - " + getAttr("END_DATE") + " 2400")

    for calsimname in nodes_to_transfer:    # Extend the list as needed, but please keep in mind the
                                            # limitations of the conservative spline, at least at present.
                                            # Mainly, input flows should be substantially greater than
                                            # zero at all times (yolo would be inappropriate, for instance)

        dsspath = calsim_path(calsimname)
        ref=DataReference.create(findpath(f,dsspath)[0],tw)
        monthly=ref.getData()
        if monthly:
            writedss(outfile,dsspath, monthly)

        else:
            raise "Failure to find CALSIM input data for: " + calsimname 

#
if __name__ == '__main__':
    if len(sys.argv) != 2:
        raise SystemExit("""
        Usage: vscript prepFlow.py configfile
        where configfile is the input file for configuration variables
        (give full path if not in current running shell)
        """)
    else:
        infile = sys.argv[1]
        setConfigVars(infile)
        print "Smoothing Boundary Flows..."
        smooth_flow()
        print "Transfering unsmoothed flows"
        transfer_flow()
        sys.exit()


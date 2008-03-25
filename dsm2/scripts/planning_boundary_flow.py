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
nodes_to_transfer=["C501","C504","C639",
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
from calsim_study_fpart import calsim_study_fpart

def calsim_path(calsimname,modified_fpart=None):
    if calsimname.startswith("C"):
        datalabel="FLOW-CHANNEL"
    elif calsimname.startswith("D"):
        datalabel="FLOW-DELIVERY"
    elif calsimname.startswith("I"):
        datalabel="FLOW-INFLOW"
    else:
        return 
        raise "Unknown CALSIM prefix"
    if modified_fpart:
        fpart=modified_fpart
    else:
        fpart=calsim_study_fpart(modify=0)
    return "/CALSIM/"+calsimname+"/"+datalabel+"//1MON/" \
           + fpart + "/"

    
def smooth_flow():
    """ A slightly smoothed version of monthly flows to avoid sharp transitions
        between months. Uses a tension spline.
    """
    calsimfile=getAttr("CALSIMFILE")
    f=opendss(calsimfile)           # open CALSIM file
    outfile=getAttr("BOUNDARYFILE")
    if not outfile or outfile == "":
        raise "Config variable BOUNDARYFILE not set and needed for prepro output"
    fpart_mod=calsim_study_fpart(modify=1)
    
    tw=timewindow(getAttr("START_DATE")+ " 0000 - " + getAttr("END_DATE") + " 2400")


    for calsimname in nodes_to_smooth:      # Extend the list as needed, but please keep in mind the
                                            # limitations of the conservative spline, at least at present.
                                            # Mainly, input flows should be substantially greater than
                                            # zero at all times (yolo would be inappropriate, for instance)
        dsspath = calsim_path(calsimname)
        paths=findpath(f,dsspath)
        if not paths or len(paths)>1:
            print "File: %s" % calsimfile
            raise "Path %s not found or not unique" % dsspath
        ref=DataReference.create(paths[0],tw)
        monthly=ref.getData()
        if monthly:		
            if len(monthly) < 4:
			    raise "Length of monthly data too short for smoothing. Wrong time window?"
            daily=conserve.conserveSpline(monthly,"1DAY")
            daily.getAttributes().setYUnits(Units.CFS)
            writedss(outfile,
                     "/CALSIM-SMOOTH/"+calsimname+"/FLOW/1DAY//" \
                     +fpart_mod+"/",
                     daily)
        else:
            raise "Failure to find CALSIM input data for: " + calsimname 

def transfer_flow():
    """ Unsmoothed transfer from CALSIM file to model input file.
    """
    calsimfile=getAttr("CALSIMFILE") 
    f=opendss(calsimfile)           # open CALSIM file
    outfile=getAttr("BOUNDARYFILE")
    if not outfile or outfile == "":
        raise "Config variable BOUNDARYFILE not set and needed for prepro output"    
    tw=timewindow(getAttr("START_DATE")+ " 0000 - " + getAttr("END_DATE") + " 2400")

    for calsimname in nodes_to_transfer:    # Extend the list as needed, but please keep in mind the
                                            # limitations of the conservative spline, at least at present.
                                            # Mainly, input flows should be substantially greater than
                                            # zero at all times (yolo would be inappropriate, for instance)
        dsspath = calsim_path(calsimname)
        paths = findpath(f,dsspath)
        if not paths or len(paths)>1:
            print "File: %s" % calsimfile
            raise "Path %s not found or not unique" % dsspath
        ref=DataReference.create(paths[0],tw)
        monthly=ref.getData()
        mf=calsim_study_fpart(modify=1)
        dsspath = calsim_path(calsimname,modified_fpart=mf)        
        if monthly:
            writedss(outfile,dsspath, monthly)

        else:
            raise "Failure to find CALSIM input data for: " + calsimname 

def moke_consumnes():
    calsimfile=getAttr("CALSIMFILE") 
    f=opendss(calsimfile)           # open CALSIM file
    outfile=getAttr("BOUNDARYFILE")
    if not outfile or outfile == "":
        raise "Config variable BOUNDARYFILE not set and needed for prepro output"    
    tw=timewindow(getAttr("START_DATE")+ " 0000 - " + getAttr("END_DATE") + " 2400")

    moke_us_path=calsim_path("I504")
    moke_ref=findpath(f,moke_us_path)
    if(moke_ref and len(moke_ref)>0):
        print "Upstream mokelumne flow found, not calculated"
    
    consumnes_path=calsim_path("C501")
    moke_ds_path=calsim_path("C504")
    consumnes_ref=findpath(f,consumnes_path)
    if not consumnes_ref:
        raise "Consumnes path %s not found" % consumnes_path
    moke_ds_ref=findpath(f,moke_ds_path)
    if not moke_ds_ref:
        raise "Mokulemne downstream path %s not found" % moke_ds_path
    consumnes=DataReference.create(consumnes_ref[0],tw).getData()
    moke_ds=DataReference.create(moke_ds_ref[0],tw).getData()
    mf=calsim_study_fpart(modify=1)
    moke_us_path=calsim_path("I504",mf)
    moke_us=moke_ds-consumnes
    
    writedss(outfile,moke_us_path,moke_us)
    return


#
if __name__ == '__main__':
    if len(sys.argv) != 2:
        raise SystemExit("""
        Usage: vscript planning_boundary_flow.py configfile
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
        print "Allocating Moke and Consumnes"
        moke_consumnes()
        sys.exit()


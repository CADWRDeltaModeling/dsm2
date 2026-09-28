# Chandra Chilmakuri Oct 1, 2009
# Modified to compute calaveras and san joaquin river inflows by including respective return flows
# Computed Sacramento river inflow by including Freeport diversion
# Smooths the modified san joaquin and sac inflows
# Included several CALSIM variables in the transfer without smoothing list.

"""Prep boundary flows.
   This script transfers flows from the raw CALSIM file
   (CALSIMFILE in the config file) to the processed model
   input file (BOUNDARYINPUT in the config). CALSIM nodes that appear
   on the nodes_to_smooth list will be smoothed using a tension
   spline, which is recommended for any large flow with no tendency
   to go to zero. Calsim nodes on the nodes_to_transfer list
   will be moved unaltered.
"""

import sys
import config
import conserve
from vista.set import DataReference, Units
from vdss import opendss,findpath,writedss
from vtimeseries import timewindow,interpolate
from config import getAttr,setConfigVars
from calsim_study_fpart import calsim_study_fpart
from planning_time_window import prepro_window

def calsim_path(calsimname,modified_fpart=None):
    if calsimname.startswith("C"):
        datalabel="FLOW-CHANNEL"
    elif calsimname.startswith("D"):
        datalabel="FLOW-DELIVERY"
    elif calsimname.startswith("I"):
        datalabel="FLOW-INFLOW"
    elif calsimname.startswith("R"):
        datalabel="FLOW-RETURN"
    else:
        return 
        raise "Unknown CALSIM prefix"
    if modified_fpart:
        fpart=modified_fpart
    else:
        fpart=calsim_study_fpart(modify=0)
    return "/CALSIM/"+calsimname+"/"+datalabel+"//1MON/" \
           + fpart + "/"

    
def smooth_flow(nodes_to_smooth):
    """ A slightly smoothed version of monthly flows to avoid sharp transitions
        between months. Uses a tension spline.
    """
    calsimfile=getAttr("CALSIMFILE")
    f=opendss(calsimfile)           # open CALSIM file
    outfile=getAttr("BOUNDARYFILE")
    if not outfile or outfile == "":
        raise "Config variable BOUNDARYFILE not set and needed for prepro output"
    fpart_mod=calsim_study_fpart(modify=1)
    
    tw=prepro_window()


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
            try:
                daily=conserve.conserveSpline(monthly,"1DAY")
            except:
                print "Failure to smooth path: %s over time window: %s" % (paths[0], tw)
                raise 
                
            daily.getAttributes().setYUnits(Units.CFS)
            writedss(outfile,
                     "/CALSIM-SMOOTH/"+calsimname+"/FLOW/1DAY//" \
                     +fpart_mod+"/",
                     daily)
        else:
            raise "Failure to find CALSIM input data for: " + calsimname 

def smooth_flow2():
    """ A slightly smoothed version of monthly flows to avoid sharp transitions
        between months. Uses a tension spline.
    """
    calsimfile=getAttr("CALSIMFILE") 
    f=opendss(calsimfile)           # open CALSIM file

    outfile=getAttr("BOUNDARYFILE")
    if not outfile or outfile == "":
        raise "Config variable BOUNDARYFILE not set and needed for prepro output"
    fpart_mod=calsim_study_fpart(modify=1)
    
    tw=prepro_window()


    print "Preparing SAC and SJR Inflows"

    sjr_path=calsim_path("C639")
    sjr_ret_path=calsim_path("R644")
    sjr_ref=findpath(f,sjr_path)
    if not sjr_ref:
        raise "San Joaquin path %s not found" % sjr_path
    sjr_ret_ref=findpath(f,sjr_ret_path)
    if not sjr_ret_ref:
        raise "San Joaquin return flow path %s not found" % sjr_ret_path
    sjr=DataReference.create(sjr_ref[0],tw) #.getData()
    sjr_ret=DataReference.create(sjr_ret_ref[0],tw) #.getData()
    sjr_in=sjr+sjr_ret

    sac_path=calsim_path("C169")
    frwa_div1_path=calsim_path("D168B")
    frwa_div2_path=calsim_path("D168C")
    sac_ref=findpath(f,sac_path)
    if not sac_ref:
        raise "Sacramento path %s not found" % sac_path
    frwa_div1_ref=findpath(f,frwa_div1_path)
    if not frwa_div1_ref:
        raise "Freeport Regional Water Authority Diversion path %s not found" % frwa_div1_path
    frwa_div2_ref=findpath(f,frwa_div2_path)
    if not frwa_div2_ref:
        raise "Freeport Regional Water Authority Diversion path %s not found" % frwa_div2_path
    sac=DataReference.create(sac_ref[0],tw) #.getData()
    frwa_div1=DataReference.create(frwa_div1_ref[0],tw) #.getData()
    frwa_div2=DataReference.create(frwa_div2_ref[0],tw) #.getData()
    sac_in=sac+frwa_div1+frwa_div2
    
#    sjr_sac(sjr_inf,sac_inf)

    print "smoothing SAC"
    sac_mon = sac_in.getData()
    if sac_mon :     
        if len(sac_mon) < 4:
            raise "Length of monthly data too short for smoothing. Wrong time window?"
        try:
            sac_day=conserve.conserveSpline(sac_mon,"1DAY")
        except:
            print "Failure to smooth path: %s over time window: %s" % ("C169_D168B_D168C", tw)
            raise 
            
        sac_day.getAttributes().setYUnits(Units.CFS)
        writedss(outfile,
                 "/CALSIM-SMOOTH/C169_D168B_D168C/FLOW/1DAY//" \
                     +fpart_mod+"/",sac_day)
    else:
        raise "Failure to find monthly data for C169+D168B+D168C" 

    print "smoothing SJR"
    sjr_mon = sjr_in.getData()
    if sjr_mon :     
        if len(sjr_mon) < 4:
            raise "Length of monthly data too short for smoothing. Wrong time window?"
        try:
            sjr_day=conserve.conserveSpline(sjr_mon,"1DAY")
        except:
            print "Failure to smooth path: %s over time window: %s" % ("C639_R644", tw)
            raise 
            
        sjr_day.getAttributes().setYUnits(Units.CFS)
        writedss(outfile,
             "/CALSIM-SMOOTH/C639_R644/FLOW/1DAY//" \
                 +fpart_mod+"/",sjr_day)
    else:
        raise "Failure to find monthly data for C639+R644" 


def transfer_flow(nodes_to_transfer):
    """ Unsmoothed transfer from CALSIM file to model input file.
    """
    calsimfile=getAttr("CALSIMFILE") 
    f=opendss(calsimfile)           # open CALSIM file
    outfile=getAttr("BOUNDARYFILE")
    if not outfile or outfile == "":
        raise "Config variable BOUNDARYFILE not set and needed for prepro output"    
    tw=prepro_window()

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
    tw=prepro_window()

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
    
def calaveras():
    calsimfile=getAttr("CALSIMFILE") 
    f=opendss(calsimfile)           # open CALSIM file
    outfile=getAttr("BOUNDARYFILE")
    if not outfile or outfile == "":
        raise "Config variable BOUNDARYFILE not set and needed for prepro output"    
    tw=prepro_window()

    cal_path=calsim_path("C508")
    cal_ret_path=calsim_path("R514")
    cal_ref=findpath(f,cal_path)
    if not cal_ref:
        raise "Calaveras path %s not found" % cal_path
    cal_ret_ref=findpath(f,cal_ret_path)
    if not cal_ret_ref:
        raise "Calaveras return flow path %s not found" % cal_ret_path
    cal=DataReference.create(cal_ref[0],tw).getData()
    cal_ret=DataReference.create(cal_ret_ref[0],tw).getData()
    mf=calsim_study_fpart(modify=1)
    cal_in_path=calsim_path("C508_R514",mf)
    cal_in=cal+cal_ret
    writedss(outfile,cal_in_path,cal_in)
    return

def sjr_sac(sjr_in,sac_in):
    calsimfile=getAttr("CALSIMFILE") 
    f=opendss(calsimfile)           # open CALSIM file
    tw=prepro_window()

    sjr_path=calsim_path("C639")
    sjr_ret_path=calsim_path("R644")
    sjr_ref=findpath(f,sjr_path)
    if not sjr_ref:
        raise "San Joaquin path %s not found" % sjr_path
    sjr_ret_ref=findpath(f,sjr_ret_path)
    if not sjr_ret_ref:
        raise "San Joaquin return flow path %s not found" % sjr_ret_path
    sjr=DataReference.create(sjr_ref[0],tw).getData()
    sjr_ret=DataReference.create(sjr_ret_ref[0],tw).getData()
    sjr_in=sjr+sjr_ret

    sac_path=calsim_path("C169")
    frwa_div1_path=calsim_path("D168B")
    frwa_div2_path=calsim_path("D168C")
    sac_ref=findpath(f,sac_path)
    if not sac_ref:
        raise "Sacramento path %s not found" % sac_path
    frwa_div1_ref=findpath(f,frwa_div1_path)
    if not frwa_div1_ref:
        raise "Freeport Regional Water Authority Diversion path %s not found" % frwa_div1_path
    frwa_div2_ref=findpath(f,frwa_div2_path)
    if not frwa_div2_ref:
        raise "Freeport Regional Water Authority Diversion path %s not found" % frwa_div2_path
    sac=DataReference.create(sac_ref[0],tw).getData()
    frwa_div1=DataReference.create(frwa_div1_ref[0],tw).getData()
    frwa_div2=DataReference.create(frwa_div2_ref[0],tw).getData()
    sac_in=sac+frwa_div1+frwa_div2
    return

def txfr_flow_day(nodes_to_txfr_day):
    """ Unsmoothed transfer from CALSIM file to model input file.
    """
    calsimfile=getAttr("CALSIMFILE") 
    f=opendss(calsimfile)           # open CALSIM file
    outfile=getAttr("BOUNDARYFILE")
    if not outfile or outfile == "":
        raise "Config variable BOUNDARYFILE not set and needed for prepro output"    
    tw=prepro_window()

    for calsimname in nodes_to_txfr_day:    # Extend the list as needed
        mf=calsim_study_fpart(modify=1)
        dsspath = calsim_path(calsimname)        
        dsspath1 = calsim_path(calsimname,modified_fpart=mf)        
        processedpath=dsspath1.replace("1MON","1DAY")
        print dsspath
        print processedpath
        paths = findpath(f,dsspath)
        if not paths or len(paths)>1:
            print "File: %s" % calsimfile
            raise "Path %s not found or not unique" % dsspath
        ref=DataReference.create(paths[0],tw)
        monthly=ref.getData()
        daily=interpolate(monthly,"1DAY")
        if daily:
            writedss(outfile,processedpath, daily)
        else:
            raise "Failure to find CALSIM input data for: " + calsimname 

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
#        nodes_to_smooth_list=["C169","C644","C639"]
        nodes_to_smooth_list=["C644"]
        # todo: once the use of 402B for Northbay Aq. is established, get rid of C403A
        nodes_to_transfer_list=["C501","C504","C508","C157","D408","D418","D419","C402B","D406B", \
                                "D403A","D403B","D403C","D403D","D418_TD","D419_TD","D418_IF", \
                                "D419_IF","D408_OR","D408_RS","D408_VC","D168B","D168C","D514A","D514B"]        
        nodes_to_txfr_day_list=["D400"]
        print "Smoothing Boundary Flows..."
        smooth_flow(nodes_to_smooth_list)
        smooth_flow2()
        print "Transfering unsmoothed flows"
        transfer_flow(nodes_to_transfer_list)
        txfr_flow_day(nodes_to_txfr_day_list)
        print "Allocating Moke and Consumnes"
        moke_consumnes()
        print "Computing Calaveras"
        calaveras()
        sys.exit()


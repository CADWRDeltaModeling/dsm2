"""Copy boundary EC at vernalis.
   This script transfers copies ec from the raw CALSIM file
   (CALSIMFILE in the config file) to the processed model
   input file (BOUNDARYINPUT in the config). No vamp
"""

import sys
import config
import conserve
from vista.set import DataReference, Units
from vdss import opendss,findpath,writedss
from vtimeseries import timewindow,interpolate
from config import getAttr,setConfigVars
from calsim_study_fpart import calsim_study_fpart

def calsim_path(calsimname):
    if calsimname.startswith("C"):
        return "/CALSIM/"+calsimname+"/FLOW-CHANNEL//1MON/" \
               + getAttr("CALSIMSTUDY") + "/"
    elif calsimname.startswith("D"):
        return "/CALSIM/"+calsimname+"/FLOW-DELIVERY//1MON/" \
               + getAttr("CALSIMSTUDY") + "/"
    else:
        raise "Unknown CALSIM prefix"



def transfer_ec():
    """ Unsmoothed transfer from CALSIM file to model input file.
    """
    
    f=opendss(getAttr("CALSIMFILE"))           # open CALSIM file
    outfile=getAttr("BOUNDARYFILE")
    process=getAttr("SJR_PROCESS")
    if not outfile or outfile == "":
        raise "Config variable BOUNDARYFILE not set and needed for prepro output"    
    tw=timewindow(getAttr("START_DATE")+ " 0000 - " + getAttr("END_DATE") + " 2400")
    calsimstudy=calsim_study_fpart(modify=0)
    calsimstudyout=calsim_study_fpart(modify=1)
    if not calsimstudy or calsimstudy=="":
        print "CALSIMSTUDY envvar not set"
    dsspath="/CALSIM.*/VERNWQFINAL/SALINITY-EC//1MON/%s/" % calsimstudy
    processedpath=dsspath.replace(".*","-"+process).replace(
        "1MON","1DAY").replace(calsimstudy,calsimstudyout)
    print processedpath
    refs=findpath(f,dsspath)
    if not refs or len(refs)> 1:
        raise "Vernalis EC path %s not found or not unique" % dsspath
    ref=DataReference.create(refs[0],tw)
    monthly=ref.getData()
    daily=interpolate(monthly,"1DAY")
    
    if daily:
        writedss(outfile,processedpath, daily)
    else:
        raise "Failure to find CALSIM input data for: " + calsimname 
    return
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
        print "Transfering unsmoothed ec"
        transfer_ec()
        sys.exit()



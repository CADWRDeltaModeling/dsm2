import sys
import config
import conserve
from vista.set import DataReference, Units
from vdss import opendss,findpath,writedss
from vtimeseries import timewindow,timeinterval
from config import getAttr,setConfigVars
from calsim_study_fpart import calsim_study_fpart
from planning_time_window import prepro_window
from jarray import zeros,array
from vista.set import RegularTimeSeries,DataSetAttr,DataType,Constants
#from vtimeseries import time,timewindow,timeinterval

def calsim_path(calsimname,modified_fpart=None):
    if calsimname.startswith("C"):
        datalabel="FLOW-CHANNEL"
    elif calsimname == "DXC":
        datalabel="GATE-DAYS-OPEN"
    elif calsimname == "NDO":
        datalabel="FLOW-NDO"
    elif calsimname.startswith("D"):
        datalabel="FLOW-DELIVERY"
    elif calsimname.startswith("I"):
        datalabel="FLOW-INFLOW"
    elif calsimname.startswith("R"):
        datalabel="FLOW-RETURN"
    elif calsimname.startswith("VERNWQ"):
        datalabel="SALINITY-EC"
    else:
        return 
        raise "Unknown CALSIM prefix"
    if modified_fpart:
        fpart=modified_fpart
    else:
        fpart=calsim_study_fpart(modify=0)
    return "/CALSIM/"+calsimname+"/"+datalabel+"//1MON/" \
           + fpart + "/"

def extend_flow(nodes_to_extend):
    """ Copying WY1922 data to WY1921 for allowing to preprocessing and running DSM2
        from 01Jan1921.
    """
    calsimfile=getAttr("CALSIMFILE") 
    f=opendss(calsimfile)           # open CALSIM file
    outfile=getAttr("BOUNDARYFILE")
    if not outfile or outfile == "":
        raise "Config variable BOUNDARYFILE not set and needed for prepro output"    
    tw=timewindow("01OCT1921 0000 - 01OCT1922 0000")

    for calsimname in nodes_to_extend:    
        print calsimname
        dsspath = calsim_path(calsimname)
        paths = findpath(f,dsspath)
        if not paths or len(paths)>1:
            print "File: %s" % calsimfile
            raise "Path %s not found or not unique" % dsspath
        ref=DataReference.create(paths[0],tw)
        monthly=ref.getData()

        itr = monthly.getIterator()
        d=zeros(len(monthly),'d')
        count=0
        while not itr.atEnd():
           el = itr.getElement()
           d[count] = el.getY()
           count = count + 1
           itr.advance()
        stime = "01OCT1920 0000" 
        rts = RegularTimeSeries(monthly.getName(),stime, \
              timeinterval("1MON").toString(), d, None, monthly.getAttributes())
        writedss(calsimfile,ref.getPathname().toString(),rts)

if __name__ == '__main__':
    if len(sys.argv) != 2:
        raise SystemExit("""
        Usage: vscript extend_calsim_output.py configfile
        where configfile is the input file for configuration variables
        (give full path if not in current running shell)
        """)
    else:
        infile = sys.argv[1]
        setConfigVars(infile)   
        nodes_to_extend_list=["C501","C504","C508","C157","D408","D418","D419","C402B","D406B", \
                                "D403A","D403B","D403C","D403D","D418_TD","D419_TD","D418_IF", \
                                "D419_IF","D408_OR","D408_RS","D408_VC","D168B","D168C","D514A",\
                                "D514B","C644","C644","C169","C639","R644","C508","R514",\
#                                "C639_SJRWQ1","C639_VAMPDO","D400","D418","D419","NDO",\
                                "D400","D418","D419","NDO",\
                                "VERNWQFINAL","VERNWQNONPULSEDV","VERNWQPULSEDV","DXC"]
# Need to include D418_TD_P, D419_TD_NP,"D406A"
        print "Extending flows"
        extend_flow(nodes_to_extend_list)
        sys.exit()
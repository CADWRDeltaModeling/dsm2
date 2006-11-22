import sys
import config
from vista.set import Units
# function to lightly smooth monthly flows to avoid sharp transitions between months.
def smoothFlow():
    import conserve
    from vista.set import DataReference
    from vdss import opendss,findpath,writedss
    from vtimeseries import timewindow
    from config import getAttr
    f=opendss(getAttr("CALSIMFILE"))           # open CALSIM file
    tw=timewindow(getAttr("START_DATE")+ " 0000 - " + getAttr("END_DATE") + " 2400")

    for calsimname in ["C169","C644" ]:     # Extend the list as needed, but please keep in mind the
                                            # limitations of the conservative spline, at least at present.
                                            # Mainly, input flows should be substantially greater than
                                            # zero at all times (yolo would be inappropriate, for instance)
        dsspath = "/CALSIM/" + calsimname + "/FLOW-CHANNEL//1MON/" + getAttr("CALSIMSTUDY") + "/"
        ref=DataReference.create(findpath(f,dsspath)[0],tw)
        monthly=ref.getData()
        if monthly:
            daily=conserve.conserveSpline(monthly,"1DAY")
            daily.getAttributes().setYUnits(Units.CFS)
            writedss(getAttr("BOUNDARY"),
                     "/CALSIM-SMOOTH/"+calsimname+"/FLOW/1DAY//"+getAttr("DSM2MODIFIER")+"/",
                     daily)
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
        config.setConfigVars(infile)
        print "Smoothing Boundary Flows..."
        smoothFlow()


##!d:/vista/bin/vscript.bat 
import os
import sys,string,time

from config import setConfigVars, getAttr
import dcc
import vdss
from vista.set import DataReference
from planning_time_window import prepro_window
#
if __name__ == '__main__':
    if len(sys.argv) != 2:
        raise SystemExit("""
        Usage: vscript dsm2.py infile-for-envvars
        where infile-for-envvars is the input file for env variables
        (give full path if not in current running shell)
        """)
    else:
        infile = sys.argv[1]

        c=setConfigVars(infile)
        print 'Creating Delta Cross Channel daily ops from monthly...'
        sdate = getAttr('START_DATE')
        if int(sdate[5:]) < 1974:
            tw = prepro_window("82yr")
        else:
            tw = prepro_window("16yr")
        tws = str(tw)
        print "Using time window: %s (dcc processing may exceed your run dates)" % tws
        
        dcc.dccOp(
            getAttr('CALSIMFILE'),              # CALSIM DSS file (input for DSM2)
            getAttr('GATEFILE'),                # processed gate DSS file (will be input for DSM2)
            '/CALSIM/DXC/GATE-DAYS-OPEN//1MON//' + getAttr('CALSIMSTUDY') + '/', # CALSIM DXC pathname
            '/CALSIM-PROCESSED/DCC/OP//IR-YEAR/' +   \
            getAttr('CALSIMSTUDY') + '/', # processed cross channel pathname
            0,                                  # 0: CALSIM input is hardwired to 30-day months
            1,                                  # operate gate between 0 & 1
            tws                                 # time window
            )

        print 'Copying gate ops for Clifton Court'
        path='/PLANNING\+GATE/CHWST000/OP-FROM-NODE//IR-YEAR/%s/' \
              % getAttr("CLIFTONCT_GATEOP")
        f=vdss.opendss(getAttr('CLIFTONCT_GATEFILE')) 
        g=vdss.findpath(f,path)
        if ( not g or len(g) != 1):
            raise "Path not found or not unique: %s" % (path)
        ts=DataReference.create(g[0]).getData()
        vdss.writedss(getAttr('GATEFILE'),path.replace("\\",""),ts)
        print "Finished with clifton court transfer"
        sys.exit(0)
#



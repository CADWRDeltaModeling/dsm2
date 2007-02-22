##!d:/vista/bin/vscript.bat 

from jnios import os
import sys,string,time

from config import setConfigVars, getAttr
import dcc
import vdss
from vista.set import DataReference
from calsim_study_fpart import calsim_study_fpart

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
        print infile
        print 'Creating Delta Cross Channel daily ops from monthly...'

        fpart_mod=calsim_study_fpart(modify=1)
        fpart=calsim_study_fpart(modify=0)        
        print getAttr('CALSIMFILE')
        
        dcc.dccOp(
            getAttr('CALSIMFILE'),              # CALSIM DSS file (input for DSM2)
            getAttr('GATEFILE'),                # processed gate DSS file (will be input for DSM2)
            '/CALSIM/DXC/GATE-DAYS-OPEN//1MON//'
            + fpart        + '/',               # CALSIM DCC pathname
            '/CALSIM-PROCESSED/DCC/OP//IR-YEAR/' +   \
            fpart_mod + '/', # processed cross channel pathname
            0,                                  # 0: CALSIM input is hardwired to 30-day months
            1,                                  # operate gate between 0 & 1
            '01JAN1974 0000 - 31DEC1991 2400'   # time window
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


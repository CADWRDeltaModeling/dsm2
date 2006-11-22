##!d:/vista/bin/vscript.bat 

from jnios import os
import sys,string,time

from config import setConfigVars, getAttr
import dxc

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
print 'Creating DXC daily ops from monthly...'
#
dxc.dxcOp(
    getAttr('CALSIMFILE'),              # CALSIM DSS file (input for DSM2)
    getAttr('GATEFILE'),                # processed gate DSS file (will be input for DSM2)
    '/CALSIM/DXC/GATE-DAYS-OPEN//1MON//' + getAttr('CALSIMSTUDY') + '/', # CALSIM DXC pathname
    '/CALSIM/DXC/GATE-POS//IR-YEAR/' + getAttr('GATE-ALT') + '/', # processed DXC pathname
    1,                                  # operate gate between 0 & 1
    1,                                  # 1: CALSIM input is hardwired to 30-day months
    '01JAN1974 0000 - 31DEC1991 2400'   # time window
    )

sys.exit()

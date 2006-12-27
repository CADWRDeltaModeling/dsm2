from jnios import os
import expand_seasonal
import sys,string
import config
import interpolate
from vtimeseries import timewindow
from vdss import findpath,opendss,writedss
from vista.set import DataReference
import config
#
if __name__ == '__main__':
    if len(sys.argv) != 2:
        raise SystemExit("""
        Usage: vscript prepEC.py configfile
        where configfile is the input file for configuration variables
        (give full path if not in current running shell)
        """)
    else:
        infile = sys.argv[1]
        config.setConfigVars(infile)
        print "Expanding seasonal DICU WQ drainage values"
        expand_seasonal.mainExpandSeasonal()
    sys.exit()

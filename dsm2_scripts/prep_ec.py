import os
import expand_seasonal
import planning_ec_mtz
import planning_ec_vernalis
import sys,string
import config
import interpolate
from vtimeseries import timewindow
from vista.set import DataReference
import config
from planning_time_window import prepro_window

#
if __name__ == '__main__':
    if len(sys.argv) != 2:
        raise SystemExit("""
        Usage: vscript prep_ec.py configfile
        where configfile is the input file for configuration variables
        (give full path if not in current running shell)
        """)
    else:
        print "IN prep EC"
        infile = sys.argv[1]
        config.setConfigVars(infile)
        tw=prepro_window()
        print "Expanding seasonal DICU WQ drainage values"
        expand_seasonal.prep_dicu(
            config.getAttr('DICUFILE_EC'),        # original DICU DSS file for EC
            config.getAttr('DICUFILE_ECE'),       # processed DICU DSS file (will be input for DSM2)
            tw)
        planning_ec_mtz.planning_ec_mtz()
        planning_ec_vernalis.transfer_ec()  #direct copy of planning ec, no vamp
        sys.exit()



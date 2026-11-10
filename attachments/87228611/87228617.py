import os
import expand_seasonal_bst
import sys,string
import config
import vutils
from vtimeseries import timewindow
from vista.set import DataReference, Units, Pathname
import config
from planning_time_window import prepro_window
from vdss import opendss,find,writedss

#
if __name__ == '__main__':
    if len(sys.argv) != 2:
        raise SystemExit("""
        Usage: vscript prep_doc.py configfile
        where configfile is the input file for configuration variables
        (give full path if not in current running shell)
        """)
    else:
        print "IN prep DOC"
        infile = sys.argv[1]
        config.setConfigVars(infile)
        tw=prepro_window()
        print "Expanding seasonal DICU DOC drainage values"
        expand_seasonal_bst.prep_dicu(
            config.getAttr('DICUFILE_DOC'),        # original DICU DSS file for EC
            config.getAttr('DICUFILE_DOCE'),       # processed DICU DSS file (will be input for DSM2)
            "DRAIN-DOC",tw)
        print "Expanding seasonal boundary DOC values"
#        expand_seasonal_bst.prep_dicu(
#            config.getAttr('TSFILE_DOC'),        # original DICU DSS file for EC
#            config.getAttr('DICUFILE_DOCE'),       # processed DICU DSS file (will be input for DSM2)
#            "DOC",tw)
    
    f=opendss(config.getAttr("TSFILE_DOC"))           # open DOC boundary file
    outfile=config.getAttr("BOUNDARYFILE")
    f2=find(f,'DOC','c')
    for ref in f2:
        path = ref.getPathname()
        daily = vutils.interpolate(ref.getData(),"1DAY")
        path.setPart(Pathname.E_PART, '1DAY')
        path.setPart(Pathname.F_PART, config.getAttr('DSM2MODIFIER'))
        mod_path = path.toString()
        writedss(outfile, mod_path, daily)

    sys.exit()



'''
Created on Sep 15, 2011
Convert DSS files' timeseries; change data elements to MISSING
for any flag other than isAcceptable, write the new
new paths to a DSS file with the same root name as 
the original, with "_MISS" appended to the root name.
Irregular Time Series is converted to Regular.
Argument[s] required: either DSS file name[s] and/or directory name[s].
@author: rfinch
'''
import os
import glob
from vtimeseries import *
from vdss import *
from vista.set import *
from vista.db.dss import *
from vutils import *

its2rts_Epart={'IR-YEAR': '1DAY', 'IR-MON': '1HOUR', 'IR-DAY': '15MIN'}
if __name__ == '__main__':
    if len(sys.argv) <= 1:
        print 'List DSS file(s) and/or directory name(s) on command line.'
        print 'For current directory, use a dot .'
        sys.exit(2)
    else:
        for fileDir in sys.argv[1:]:
            if os.path.isdir(fileDir):
                # it's a directory, generate DSS filelist
                filelist = glob.glob(fileDir+'*.dss')
            else:
                if os.path.splitext(fileDir)[1].lower() == '.dss':
                    filelist = [fileDir]
                else:
                    # not a dss file
                    continue
            for DSSFile in filelist:
                rootName = os.path.splitext(DSSFile)[0]
                outDSSFile = rootName + '_MISS.dss'
                try:
                    os.remove(outDSSFile)
                except:
                    pass
                dss_group=opendss(DSSFile)
                print 'Writing ' + outDSSFile
                ctr = 0
                pct = 10
                #dss_group.filterBy('/SJL/FLOW/.*/IR-DAY/')
                nrefs = len(dss_group)
                for dataref in dss_group.getAllDataReferences():
                    ctr += 1
                    pn = dataref.getPathname()
                    pathname = str(pn).upper()
                    dataSet = flags_to_missing(dataref.getData())
                    if isITS(dataSet):
                        ePart = pn.getPart(pn.E_PART)
                        dataSet = its2rts(dataSet,its2rts_Epart[ePart])
                        if not dataSet:
                            print pathname
                            continue
                    DSSUtil.writeData(outDSSFile, pathname, dataSet, True)
                    if int(ctr*100./nrefs + .5) >= pct: 
                        print '%2d%%' % int(ctr*100./nrefs + .5),
                        pct += 10
    #
    print '\nFinished all files.'
    sys.exit()

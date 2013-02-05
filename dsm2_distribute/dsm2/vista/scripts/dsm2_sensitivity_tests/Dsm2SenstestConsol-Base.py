import string, re, os, posixpath, sys, math
import glob
from vtimeseries import *
#from vista.time import *
from vdss import *
from vista.set import *
#from vista.set import Group
from vista.set import PathnamePredicate
from vista.db.dss import *
from vutils import *
from tideUtils import *
from vista.time import TimeFactory

if __name__ == '__main__':
    if len(sys.argv) <= 1:
        PFilter = None
    else:
        PFilter = sys.argv[1]

    TF = TimeFactory.getInstance()

    # Trim Base output DSS files and consolidate into
    # a single file
    # Then calculate the 4 desired averaged metrics:
    # stage amplitude
    # flow amplitude
    # flow net tidal
    # EC
    #
    STDir = 'D:/delta/models/studies/2010-Calibration/SensitivityTests/'
    # outPattern = 'HIST-CLB2K-ManN_Ch'
    ConsolDir = STDir + 'Consolidated/'
    RDBDir = STDir + 'RDB/'
    intervalAnalysis = timeinterval('28DAY')    # interval to analyze at end of run
    dss_group = opendss(STDir + 'BaseRun-1/HIST-CLB2K-BASE-1991-v81_1.dss')
    tw = False
    for dataref in dss_group.getAllDataReferences():
        if not tw:      # first path
            tw = dataref.getTimeWindow()
            # assume that all paths are same time window
            if (tw.getEndTime() - intervalAnalysis).compare(tw.getStartTime()) < 0:
                raise 'Error; DSM2 run length shorter than desired analysis interval.'
            twAnalysis = TF.createTimeWindow(tw.getEndTime() - intervalAnalysis,
                                           tw.getEndTime())
        ipsu = str(dataref.getPathname()).upper()
        newDataref = DataReference.create(dataref, twAnalysis)
        #print ipsu
        DSSConsolFile = ConsolDir + \
            dataref.getPathname().getPart(Pathname.C_PART) + '.dss'
        #print DSSConsolFile
        writedss(DSSConsolFile, ipsu, newDataref.getData())
    print 'Trimmed', os.path.basename('HIST-CLB2K-BASE.dss')
    ##
    # Now calculate amplitudes and averages,
    # average to a single number, and write to ascii file
    BOld = ''
    # reject these stations
    rejStas = '((rsan112|bnd_sacr).*ec)|(bnd_sacr|bnd_yolo|calaveras|CHCCC006|cosumnes|/cvp/|/mtz/|/sac/|rsan112|slbar002|tompn_sl|vernalis|sjr_mossdale|yolo).*(flow|stage)'
    for DSSConsolFile in glob.glob(ConsolDir + '*.dss'):
        print DSSConsolFile
        dss_group = opendss(DSSConsolFile)
        if PFilter:
            dss_group.filterBy(PFilter)
        dss_group.filterBy('Base')
        dss_group.filterBy(PathnamePredicate(rejStas), False)
        for dataref in dss_group.getAllDataReferences():
            pn = dataref.getPathname()
            B = pn.getPart(Pathname.B_PART)
            if B != BOld:
                print 'Processing', B
                BOld = B
            C = pn.getPart(Pathname.C_PART)
            tsA = calcAmplPhaseAve(dataref, True, False)
            # Average the time series to return a single value
            if tsA != None:
                avgVal = avg(tsA)
                ResultsOut = RDBDir + 'Base-' + C + '.txt'
                fp = open(ResultsOut, 'a')
                fp.write(str(avgVal) + \
                         '\t' + B + \
                         '\t' + C + \
                         '\t' + 'BASE' + \
                         '\n')
                fp.close()
    print 'End processing all files'
#

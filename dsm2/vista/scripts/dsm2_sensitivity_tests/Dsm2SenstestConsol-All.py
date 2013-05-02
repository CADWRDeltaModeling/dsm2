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
from calcAmplPhaseAve import calcAmplPhaseAve
from vista.time import TimeFactory

if __name__ == '__main__':
    if len(sys.argv) <= 1:
        PFilter = None
    else:
        PFilter = sys.argv[1]
 
    TF = TimeFactory.getInstance()
    
    # Trim perturbation test output DSS file for All (combined) perturbed parameters
    # Then calculate the 4 desired averaged metrics:
    # stage amplitude
    # flow amplitude
    # flow net tidal
    # EC
    #
    STDir = 'd:/delta/dsm2_v8/studies/2010-Calibration/SensitivityTests/All/'
    dssPattern = 'HIST-CLB2K-*-ALL.dss'
    DSSConsolRoot = STDir + 'Consolidated-All'
    intervalAnalysis = timeinterval('28DAY')    # interval to analyze at end of run
    # create DSS output filename from previously generated runnames,
    # checking existence of DSS output file (condor occasionally drops a run)
    tw=False
    for DSSFile in glob.glob(STDir+dssPattern):
        dss_group=opendss(DSSFile)
        if PFilter:
            dss_group.filterBy(PFilter)
        for dataref in dss_group.getAllDataReferences():
            if not tw:      # first path of first file
                tw=dataref.getTimeWindow()
                # assume that all paths are same time window
                if (tw.getEndTime() - intervalAnalysis).compare(tw.getStartTime()) < 0:
                    raise 'Error; DSM2 run length shorter than desired analysis interval.'
                twAnalysis=TF.createTimeWindow(tw.getEndTime() - intervalAnalysis,
                                               tw.getEndTime())
            ipsu=str(dataref.getPathname()).upper()
            newDataref=DataReference.create(dataref,twAnalysis)
            #print ipsu
            DSSConsolFile = DSSConsolRoot + '-' + \
                dataref.getPathname().getPart(Pathname.C_PART) + '.dss'
            #print DSSConsolFile
            writedss(DSSConsolFile, ipsu, newDataref.getData())
        print 'Trimmed',os.path.basename(DSSFile)
    ##
    # Now calculate amplitudes and averages,
    # average to a single number, and write
    # to ascii file
    ResultsOut = STDir + 'Results.txt'
    fid_Results = open(ResultsOut, 'w')
    fid_Results.write('Value\tOutput Location\tParameter\tPerturbed\n')
    BOld = ''
    # reject Base and these stations
    rejStas = '((base)|(rsan112|bnd_sacr).*ec)|(bnd_sacr|bnd_yolo|calaveras|CHCCC006|cosumnes|/cvp/|/mtz/|/sac/|rsan112|slbar002|tompn_sl|vernalis|yolo).*(flow|stage)'
    now = datetime.now()        
    for DSSConsolFile in glob.glob(DSSConsolRoot + '-*.dss'):
        print DSSConsolFile
        dss_group = opendss(DSSConsolFile)
        if PFilter:
            dss_group.filterBy(PFilter)
        dss_group.filterBy(False, PathnamePredicate(rejStas))
        for dataref in dss_group.getAllDataReferences():
            pn = dataref.getPathname()
            B = pn.getPart(Pathname.B_PART)
            if B != BOld:
                print 'Processing', B
                #print str(pn)
                BOld = B
            C = pn.getPart(Pathname.C_PART)
            # typical F part:
            # HIST-CLB2K-MANN+FROM-ALL
            # extract parameter, object, and ID
            FPartUse = pn.getPart(Pathname.F_PART).replace('HIST-CLB2K-','')
            FSplit = re.split('-',FPartUse)
            FSplit = re.split('[-_+]',FPartUse)
            if FPartUse.find('DICU') < 0: 
                param = FSplit[0]
                objType = FSplit[1][0:2]
            else: 
                param = FSplit[0] + '-' + FSplit[1]
                objType = FSplit[2][0:2]
            objID = 'ALL'
            #print B, C, param, objType, objID
            tsA = calcAmplPhaseAve(dataref,True,False)
            # Average the time series to return a single value
            if tsA != None: 
                avgVal = avg(tsA)
                fid_Results.write(str(avgVal) + \
                         '\t' + B + \
                         '\t' + C + \
                         '\t' + param + \
                         '\n')
    #print datetime.now() - now
    fid_Results.close()
    print 'End processing all files'
#

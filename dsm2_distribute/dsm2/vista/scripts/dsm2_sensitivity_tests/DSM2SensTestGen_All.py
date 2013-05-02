import string, re, os, sys, math
from vtimeseries import *
from vista.set import *
from vista.db.dss import *
from vutils import *
from gov.ca.dsm2.input.parser import Parser
from gov.ca.dsm2.input.parser import Tables
from gov.ca.dsm2.input.model import *

if __name__ == "__main__":

    # percent change for perturbed values
    pctChange = 5.
    ## just one of the below should be True, the others False
    # channel input parameters
    MANN = False
    DISP = False
    XELEV = False
    XTOPW = False
    LENGTH = True
    # node input parameters
    DICU = False
    # reservoir input parameters
    RESDEPTH = False

    infile = 'd:/delta/models/studies/2000-Calibration/historical/output/hydro_echo_hist-calib2000.inp'
    DICUdir = 'd:/delta/models/timeseries/'
    outdir = 'd:/delta/models/studies/2010-Calibration/SensitivityTests/PerturbedInputFiles/'
    outfilenm = 'PerturbedInp'

    p = Parser()
    tables = p.parseModel(infile)
    if MANN or DISP or XELEV or XTOPW or LENGTH:
        if MANN: PTBID = 'MANN'
        elif DISP: PTBID = 'DISP'
        elif LENGTH: PTBID = 'LEN'
        elif XELEV: PTBID = 'XELEV'
        elif XTOPW: PTBID = 'XTOPW'
        PTBID += '-ALL'
        # fid_EnvLabel is the study label/name
        fid_EnvLabel = open(outdir + 'ALL', 'w')
        fid_EnvLabel.write('ENVVAR\n')
        fid_EnvLabel.write('NAME\tVALUE\n'.expandtabs())
        fid_EnvLabel.write(('PTB\t' + PTBID).expandtabs())
        fid_EnvLabel.write('\nEND\n')
        fid_EnvLabel.close()
        channels = tables.toChannels()
        nChan = 0
        newValFNs = []
        fid_NewVal = None
        for chan in channels.getChannels():
            nChan+=1
            # have to split All output into multiple files to get around DSM2v8 limitation
            if nChan % 100 == 1:
                if len(newValFNs) > 0: fid_NewVal.close()
                DSM2InpLayer = outdir + outfilenm + PTBID + '-' + str(len(newValFNs)+1) + '.inp'
                newValFNs.append(DSM2InpLayer)
                # fid_NewVal is the replacement input layer for part of Hydro input
                fid_NewVal = open(DSM2InpLayer, 'w')
            chan3 = "%03d" % int(chan.getId())
            if MANN:    # mannings N...
                Val = chan.getMannings()
                newVal = int(10000. * (Val * (1. + pctChange / 100.)) + 0.5) / 10000.
                chan.setMannings(newVal)
            elif DISP:  # dispersion coefficient
                Val = chan.getDispersion()
                newVal = int(10000. * (Val * (1. + pctChange / 100.)) + 0.5) / 10000.
                chan.setDispersion(newVal)
            if LENGTH:    # nominal channel length...
                Val = chan.getLength()
                newVal = int(10000. * (Val * (1. + pctChange / 100.)) + 0.5) / 10000.
                chan.setLength(int(newVal))
            elif XELEV or XTOPW: # cross sections in channel
                if XELEV: XE = True       # increase elevations by % or...
                if XTOPW: XTW = True     # ...increase top width by %
                for xs in chan.getXsections():
                    for lyr in xs.getLayers():
                        area = lyr.getArea()
                        elev = lyr.getElevation()
                        TW = lyr.getTopWidth()
                        WP = lyr.getWettedPerimeter()
                        if XE: lyr.setElevation(int(100. * (elev * (1. + pctChange / 100.) + 0.5)) / 100.)
                        if area == 0:
                            continue
                        if XTW: lyr.setTopWidth(int(1000. * (TW * (1. + pctChange / 100.) + 0.5)) / 1000.)
                        lyr.setArea(int(1000. * (area * (1. + pctChange / 100.) + 0.5)) / 1000.)
                        lyr.setWettedPerimeter(int(1000. * (WP * (1. + pctChange / 100.)) + 0.5) / 1000.)
            newChans = Channels()
            newChans.addChannel(chan)
            newTables = Tables().fromChannels(newChans)
            for i in range(2):
                fid_NewVal.write(newTables[i].toStringRepresentation().expandtabs())
        # end channel loop
        fid = open(outdir + outfilenm + PTBID + '.lst', 'w')
        fid.write('GRID\n')
        for fn in newValFNs:
            fid.write(os.path.basename(fn)+'\n')
        fid.write('END\n')
        fid.close()
        print 'Prepared', len(channels.getChannels()), 'Channels'
    elif RESDEPTH:
        PTBID = 'RES-ALL'
        # fid_EnvLabel is the study label/name
        fid_EnvLabel = open(outdir + 'ALL', 'w')
        fid_EnvLabel.write('ENVVAR\n')
        fid_EnvLabel.write('NAME\tVALUE\n'.expandtabs())
        fid_EnvLabel.write(('PTB\t' + PTBID).expandtabs())
        fid_EnvLabel.write('\nEND\n')
        fid_EnvLabel.close()
        DSM2InpLayer = outdir + outfilenm + PTBID + '.inp'
        # fid_NewVal is the replacement input layer for part of Hydro input
        fid_NewVal = open(DSM2InpLayer, 'w')
        reservoirs = tables.toReservoirs()
        for res in reservoirs.getReservoirs():
            resname = res.getName()
            Val = res.getArea()
            newVal = int(10000. * (Val * (1. + pctChange / 100.)) + 0.5) / 10000.
            res.setArea(newVal)
            newRes = Reservoirs()
            newRes.addReservoir(res)
            newTables = Tables().fromReservoirs(newRes)
            outfile = outdir + outfilenm + PTBID + '.inp'
            fid_NewVal = open(outfile, 'w')
            for i in range(2):  
                fid_NewVal.write(newTables[i].toStringRepresentation().expandtabs())
        # end reservoir loop
        fid_NewVal.close()
        print 'Prepared', len(reservoirs.getReservoirs()), 'Reservoir files'
    elif DICU:
        # Set one to True, the others to False (Diversions, Return Flows, EC of return flows)
        QDIV = False
        QRET = True
        ECRET = False
        # Perturbed values for source/sink flows could be either 
        # a single replacement value, or an incremental (additional) value.
        # Use replacement values, because DSM2 arrays are not sized
        # to handle double the number of node inputs.
        # For ECs of return flows, always use a replacement value
        if QDIV: QType = 'QDIV'
        if QRET: QType = 'QRET'
        if ECRET: QType = 'ECRET'
        if QDIV or QRET: DICUfile = DICUdir + 'dicu_200705.dss'
        if ECRET: DICUfile = DICUdir + 'dicuwq_200611_expand.dss'
        fid_EnvLabel = open(outdir + 'ALL', 'w')
        PTBID = 'DICU-' + QType + '-ALL'
        dicufile = outdir + 'PerturbedInp' + PTBID + '.dss'
        try: os.remove(dicufile)
        except: pass
        fid_EnvLabel.write('ENVVAR\n')
        fid_EnvLabel.write('NAME\tVALUE\n'.expandtabs())
        fid_EnvLabel.write(('PTB\t' + PTBID).expandtabs())
        fid_EnvLabel.write('\nEND\n')
        fid_EnvLabel.close()
        DSM2InpLayer = outdir + 'PerturbedInp' + PTBID + '.inp'
        try: os.remove(DSM2InpLayer)
        except: pass
        fid_NewVal = open(DSM2InpLayer, 'w')
        if QDIV or QRET:
            fid_NewVal.write('SOURCE_FLOW\n')
            fid_NewVal.write('NAME\t\tNODE\tSIGN\tFILLIN\tFILE\t\t\tPATH\n'.expandtabs())
        if ECRET:
            fid_NewVal.write('NODE_CONCENTRATION\n')
            fid_NewVal.write('NAME\t\tNODE_NO\tVARIABLE\tFILLIN\tFILE\t\t\tPATH\n'.expandtabs())
        dss_group = opendss(DICUfile)
        if ECRET: dss_group.filterBy('/DRAIN-EC/')
        count = 0
        for dataref in dss_group.getAllDataReferences():
            dataset = dataref.getData()
            inpath = dataref.getPathname()
            B = inpath.getPart(inpath.B_PART)
            C = inpath.getPart(inpath.C_PART)
            if not re.search('^[0-9]+$', B):    # nodes only
                continue
            node3 = "%03d" % int(B)
            update = False  # flag that this path is updated
            if (QDIV and inpath.getPart(inpath.C_PART) == 'DIV-FLOW') or \
               (QRET and inpath.getPart(inpath.C_PART) == 'DRAIN-FLOW') or \
               (ECRET and inpath.getPart(inpath.C_PART) == 'DRAIN-EC'):
                update = True
                dataref = dataref * (1. + pctChange / 100.)
            if update:
                dataset = dataref.getData()
                inpath.setPart(inpath.F_PART, 'PRTB-' + QType)
                PTBID = 'DICU_' + QType + '-' + B
            else:
                PTBID = C + '-' + B
            if QDIV or QRET:
                if inpath.getPart(inpath.C_PART) == 'DIV-FLOW':
                    name = 'dicu_div_'
                    Sign = '-1'
                if inpath.getPart(inpath.C_PART) == 'DRAIN-FLOW':
                    name = 'dicu_drain_'
                    Sign = '+1'
                if inpath.getPart(inpath.C_PART) == 'SEEP-FLOW':
                    name = 'dicu_seep_'
                    Sign = '-1'
                fid_NewVal.write(name + B + '\t' + B + '\t' + Sign + '\tlast\t' + \
                         os.path.basename(dicufile) + '\t' + \
                         inpath.getFullPath() + '\n'.expandtabs())
            if ECRET:
                fid_NewVal.write('dicu_drain_' + B + '\t' + B + '\tEC\t\tlast\t' + \
                         os.path.basename(dicufile) + '\t' + \
                         inpath.getFullPath() + '\n'.expandtabs())
            writedss(dicufile, inpath.getFullPath(), dataset)
            count += 1
            if count % 100 == 1:
                print 'Wrote', inpath.getFullPath(),'...'
        # end of DICU loop
        fid_NewVal.write('END\n')
        fid_NewVal.close()
        # write include file
        fid = open(DSM2InpLayer.replace('.inp','.lst'), 'w')
        fid.write('GRID\n')
        fid.write(os.path.basename(DSM2InpLayer))
        fid.write('\nEND\n')
        fid.close()
        
        print 'Prepared', count, 'DICU DSS paths'
        #
    else:
        raise 'Set either MANN or DISP or XSECT or DICU True.'
    #
    sys.exit()

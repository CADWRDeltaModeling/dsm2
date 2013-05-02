import string, re, os, sys
from vtimeseries import *
from vdss import *
from vista.set import *
from vista.db.dss import *
from vutils import *

if __name__ == "__main__":

    infile=r'Y:\Observed Data\IEP\hydro_iep_14dec2009.dss'
    outfile = r'Y:\Observed Data\IEP\hydro_iep_auto.dss'
#    infile = r'Y:\Observed Data\IEP\quality_iep_14dec2009.dss'
#    outfile=r'Y:\Observed Data\IEP\quality_iep_auto.dss'
    logfile = 'flag_data.log'
    try: os.remove(logfile)
    except: pass
    dss_group = opendss(infile)
    units = {'STAGE': 'FEET', 'FLOW': 'CFS', 'EXPORT': 'CFS', 'EC': 'UMHOS/CM', 'CL': 'PPM'}
    uId = DSSUtil.getUserId('datachecker')

#    dss_group.filterBy('/EC/') # EC stations only
    dss_group.filterBy('/RSAN007/STAGE/') # misc
#    dss_group.filterBy('DWR') # USGS stations only
#   dss_group.filterBy('/(ANC|ANH|BDL|FPT|JER|MRZ|RIV|SAL)/(EC|STAGE)') # CDEC
#   dss_group.filterBy('/(RMID023|ROLD024|ROLD034|ROLD047|RSAC054|RSAC101'+\
#                       '|RSAC123|RSAN018|RSAN032|RSAN063|SLDUT007)/STAGE/')    # IEP

    for dataref in dss_group.getAllDataReferences():
        tw = dataref.getTimeWindow()
        inpath = dataref.getPathname()
        ipsu = str(inpath).upper()
        Flagged = False

        # fix units
        if re.search('/ANC/EC/.*/IR-MON/|/JER/EC/.*/IR-MON/|/RIV/EC/|/HLL/EC/|/SAL/EC/', ipsu):
            yUnits = 'US/CM'
            dataref.getData().getAttributes().setYUnits(yUnits)
            Flagged = True

        dataset = dataref.getData()
        dataset = dsAddFlags(dataset)

        # fix individual paths and their problems
        if re.search('/BAC/EC.*/IR-DAY/DWR-OM', ipsu):
            Flagged = True
            sti = dsIndex(dataset, '08MAR2000 0530')
            eti = dsIndex(dataset, '15MAR2000 1515') + 1
            for ndx in range(sti, eti):
                newEl = dataset.getElementAt(ndx)
                newEl.y *= 0.5 # scale change
                dataset.putElementAt(ndx, newEl) # put the element back into the data set
            sti = dsIndex(dataset, '03JAN2001 0945')
            eti = dsIndex(dataset, '09JAN2001 1045') + 1
            for ndx in range(sti, eti):
                newEl = dataset.getElementAt(ndx)
                FlagUtils.setQualityFlag(newEl, FlagUtils.REJECT_FLAG, uId)
                dataset.putElementAt(ndx, newEl) # put the element back into the data set
        # end of BAC

        if re.search('/BDL/EC.*/IR-MON/', ipsu):
            Flagged = True
            sti = dsIndex(dataset, '28AUG1992 1200')
            eti = dsIndex(dataset, '31AUG1992 1000') + 1
            for ndx in range(sti, eti):
                newEl = dataset.getElementAt(ndx)
                newEl.y *= 2.5 # scale change
                #newEl.setFlag(FlagUtils.OK_FLAG)
                FlagUtils.setQualityFlag(newEl, FlagUtils.OK_FLAG, uId)
                dataset.putElementAt(ndx, newEl) # put the element back into the data set

            sti = dsIndex(dataset, '22JAN1993 1500')
            eti = dsIndex(dataset, '27JAN1993 0900') + 1
            for ndx in range(sti, eti):
                newEl = dataset.getElementAt(ndx)
                newEl.y *= 0.5 # scale change
                FlagUtils.setQualityFlag(newEl, FlagUtils.OK_FLAG, uId)
                dataset.putElementAt(ndx, newEl) # put the element back into the data set

            sti = dsIndex(dataset, '31OCT1994 1300')
            eti = dsIndex(dataset, '31OCT1994 2300') + 1
            for ndx in range(sti, eti):
                newEl = dataset.getElementAt(ndx)
                newEl.y *= 2.5 # scale change 
                FlagUtils.setQualityFlag(newEl, FlagUtils.OK_FLAG, uId)
                dataset.putElementAt(ndx, newEl) # put the element back into the data set

            sti = dsIndex(dataset, '22FEB2008 1400')
            eti = dsIndex(dataset, '27FEB2008 0100') + 1
            for ndx in range(sti, eti):
                newEl = dataset.getElementAt(ndx)
                newEl.y *= 2.0 # scale change 
                FlagUtils.setQualityFlag(newEl, FlagUtils.OK_FLAG, uId)
                dataset.putElementAt(ndx, newEl) # put the element back into the data set
        # end of BDL
        if re.search('/BET/EC/.*/IR-MON/DWR/', ipsu):
            Flagged = True
            sti = dsIndex(dataset, '22MAY1992 0800')
            eti = dsIndex(dataset, '05JUN1992 1300') + 1
            for ndx in range(sti, eti):
                newEl = dataset.getElementAt(ndx)
                newEl.y /= 2.5 # scale change
                dataset.putElementAt(ndx, newEl) # put the element back into the data set
        # end of BET
        if re.search('/RSAN007/STAGE/.*/1HOUR/CDEC/', ipsu):
            Flagged = True
            sti = 0
            eti = len(dataset) - 1
            for ndx in range(sti, eti):
                newEl = dataset.getElementAt(ndx)
                if newEl.y > 80:
                    newEl.y -= 100.0 # offset change
                dataset.putElementAt(ndx, newEl) # put the element back into the data set
        # end of RSAN007
        if re.search('/SRV/FLOW/.*/IR-DAY/', ipsu):
            Flagged = True
            sti = dsIndex(dataset, '04OCT2003 2400')
            eti = dsIndex(dataset, '14NOV2004 2400') + 1
            for ndx in range(sti, eti):
                newEl = dataset.getElementAt(ndx)
                newEl.y *= 10.0 # ampl change
                dataset.putElementAt(ndx, newEl) # put the element back into the data set
        # end of SRV
        if re.search('/TSL/STAGE/.*/IR-DAY/', ipsu):
            Flagged = True
            sti = dsIndex(dataset, '24OCT2006 0845')
            eti = dsIndex(dataset, '28MAR2007 0830') + 1
            for ndx in range(sti, eti):
                newEl = dataset.getElementAt(ndx)
                newEl.y += 3.28 # scale change
                dataset.putElementAt(ndx, newEl) # put the element back into the data set
        # end of TSL
        if re.search('/PRI/EC/.*/IR-DAY/', ipsu):
            Flagged = True
            sti = dsIndex(dataset, '24OCT2006 0845')
            eti = dsIndex(dataset, '29DEC2008 1515') + 1
            for ndx in range(sti, eti):
                newEl = dataset.getElementAt(ndx)
                newEl.y /= 16.0 # scale change
                dataset.putElementAt(ndx, newEl) # put the element back into the data set
        # end of PRI
        if re.search('/ROLD024/STAGE/.*/15MIN/DWR-CD-SURFWATER/', ipsu):
            Flagged = True
            sti = dsIndex(dataset, '09MAR1988 2400')
            eti = dsIndex(dataset, '30SEP1991 2400') + 1
            for ndx in range(sti, eti):
                newEl = dataset.getElementAt(ndx)
                newEl.y += 3.0 # scale change
                dataset.putElementAt(ndx, newEl) # put the element back into the data set
            sti = dsIndex(dataset, '30SEP1992 2400')
            eti = dsIndex(dataset, '30SEP1995 2345') + 1
            for ndx in range(sti, eti):
                newEl = dataset.getElementAt(ndx)
                newEl.y += 3.0 # scale change
                dataset.putElementAt(ndx, newEl) # put the element back into the data set
        # end of ROLD024
        valArray = [None, None]

        yUnits = dataref.getData().getAttributes().getYUnits().upper()
        if yUnits == '' or yUnits == None:
            Flagged = True
            yUnits = [inpath.getPart(inpath.C_PART).upper()]
            dataref.getData().getAttributes().setYUnits(yUnits)

        # Missing
        Special = False
        ResetExist = True
        valArray = [99999.0, -99999.0, -999999.0, -9999.0, -7999., 7999]
        if re.search('/FPT/', ipsu): valArray += [0.0]
        if re.search('/(BDL|RMID023|ROLD034|RSAC123|RSAN032)/STAGE/.*/(IR-MON|15MIN)/', ipsu):
            valArray = [-3.0];Special = True
        if re.search('/MRZ/STAGE/.*/IR-MON/|/RSAC123/STAGE/.*/15MIN/USGS/', ipsu):
            valArray = [-10.0];Special = False
        if re.search('/RLTM\+CHAN/RSAC101/EC/.*/1HOUR/CDEC/', ipsu):
            valArray = [2400, 1200]; Special = False
        outds = flag_data('M', dataset, valArray, logfile, Special, ResetExist)
        if outds: dataset = outds;Flagged = True

        # Math; re-scale millimhos/cm values to umhos/cm, or vice-versa
        if re.search('/EC', ipsu):
            if re.search('MIL|MS|MMHO', yUnits):
                valArray = [50.0, 25000.0, .001]    # want millimhos/cm scale from micro
            else: valArray = [0.05, 25.0, 1000.]   # want micromhos/cm, scale from milli
            outds = flag_data('*', dataset, valArray, logfile)
            if outds: dataset = outds;Flagged = True
        # Math; + or - 100 USGS stage data
        if re.search('/(SJJ|ODM)/STAGE/.*/USGS/', ipsu):
            valArray = [-110.0, -90.0, 100.]   # want feet MSL, scale from -100
            outds = flag_data('+', dataset, valArray, logfile)
            if outds: dataset = outds;Flagged = True
        if re.search('/FPT/STAGE/.*/USGS/', ipsu):
            valArray = [98.0, 120.0, -100.]   # want feet MSL, scale from 100
            outds = flag_data('+', dataset, valArray, logfile)
            if outds: dataset = outds;Flagged = True

        # Range: defined range
        if re.search('/EC', ipsu):
            if re.search('MIL|MS|MMHO', yUnits): valArray = [0.05, 25.0]    # millimhos/cm
            else: valArray = [50.0, 25000.0]   # micromhos/cm
            if re.search('/MRZ/|/RSAC0', ipsu): valArray = [10.0, 50000.0]
            if re.search('/RSAC075/.*/DWR-CD-SURFWATER/', ipsu): valArray = [10.0, 9999.0]
            if re.search('/ROR/', ipsu): valArray = [10.0, 30000.0]
            if re.search('/CHWST', ipsu): valArray = [20.0, 3000.0]
        if re.search('/CL/', ipsu): valArray = [5, 3000.0]
        if re.search('/TDS/', ipsu): valArray = [30, 1000.0]
        if re.search('/MCC/STAGE/', ipsu):
            valArray = [20.0, 50.0]
        elif re.search('/STAGE/', ipsu):
            valArray = [-15.0, 25.0]
            if re.search('/MRZ/', ipsu): valArray = [-4.5, 7.5]
            if re.search('/RSAN007/', ipsu): valArray = [-7, 10.0]
        if re.search('/TEMP', ipsu): valArray = [35.0, 90.0]
        if re.search('/FLOW', ipsu):
            if re.search('/(RSAC|CSE|SRV|BYOLO|DOM|RSAN0[1-3]|PRI|RYI|SJJ)', ipsu):
                valArray = [-250000., 500000.]
            elif re.search('/(FPT)', ipsu):
                valArray = [-6000., 300000.]
            else:
                valArray = [ -50000., 75000.]
        if re.search('/EXPORT', ipsu): valArray = [-100.0, 130000.]
        outds = flag_data('R', dataset, valArray, logfile)
        if outds: dataset = outds;Flagged = True
        
        # Difference (spikes and plateaus)
        #=======================================================================
        # # absolute values first...
        # if re.search('/[0-9]+MIN|[1-6]HOUR|IR-DAY|IR-MON', ipsu):  # intra-tidal
        #    if re.search('/EC/', ipsu):
        #        valArray = [1500.0]
        #        if re.search('/MRZ/', ipsu): valArray = [7000.0]
        #        if re.search('/ROR|BDL|ANH|ANC|CLC|NSL/', ipsu): valArray = [3000.0]
        #    if re.search('/CL/', ipsu): valArray = [80.0]
        #    if re.search('/STAGE', ipsu):
        #        valArray = [2.0]
        #        if re.search('/RSAC0[5-7]|TSL|RSAN0[0-3]|MDM|FPT', ipsu): valArray = [3.0]
        #    if re.search('/TEMP', ipsu): valArray = [3.0]
        #    if re.search('/FLOW', ipsu):
        #        valArray = [5000.]
        #        if re.search('/(CSE|BYOLO.*|DOM|TSL|MDM)/', ipsu): valArray = [20000.]
        #        if re.search('/(PRI|FPT)/', ipsu): valArray = [8000.]
        #        if re.search('/RSAC0[5-7]|TSL|RSAN0[0-3]|PRI|RYI|SJJ|SRV', ipsu): valArray = [60000.]
        #    if re.search('/EXPORT', ipsu): valArray = [5000.]
        # else:   # averaged over at least one tidal day
        #    if re.search('/EC/', ipsu):
        #        valArray = [1500.0]
        #        if re.search('/(MRZ)/', ipsu): valArray = [6000.0]
        #        if re.search('/(BDL)/', ipsu): valArray = [3000.0]
        #    if re.search('/CL/', ipsu): valArray = [150.0]
        #    if re.search('/STAGE', ipsu): valArray = [2.0]
        #    if re.search('/TEMP', ipsu): valArray = [6.0]
        #    if re.search('/FLOW', ipsu):
        #        if re.search('/(RSAC|CSE|SRV|BYOLO|DOM)', ipsu): valArray = [100000.]
        #        elif re.search('/(PRI|FPT)', ipsu): valArray = [20000.]
        #        else: valArray = [5000.]
        #    if re.search('/EXPORT', ipsu): valArray = [8000.]
        # outds = flag_data('D', dataset, valArray, logfile)
        # if outds: dataset = outds;Flagged = True
        # # ... then factor of moving average, good only for EC/temp 
        # # and some stage locations (always well above zero)
        # if re.search('/(TEMP|EC)/', ipsu) or \
        #   re.search('/fpt/stage/', ipsu):
        #    if re.search('/[0-9]+MIN|[1-6]HOUR|IR-DAY|IR-MON', ipsu):  # intra-tidal
        #        valArray = [2.0, True]
        #    else:   # averaged over at least one tidal day
        #        valArray = [2.0, True]
        #    if re.search('/CHWST', ipsu):   # allow greater EC range near CCF intake
        #        valArray = [5.0, True]
        #    outds = flag_data('D', dataset, valArray, logfile)
        # if outds: dataset = outds;Flagged = True
        #=======================================================================

        # Difference (spikes and plateaus)
        # factor of moving average of values and MA of 1st diffs
        # good only for EC/temp and some stage locations (always well above zero)
        if re.search('/(TEMP|EC)/', ipsu) or \
           re.search('/fpt/stage/', ipsu):
            if re.search('/[0-9]+MIN|[1-6]HOUR|IR-DAY|IR-MON', ipsu):  # intra-tidal
                valArray = [3.0, 3.0]
                if re.search('/RSAC(0|10)', ipsu): # Lower Sacramento R.
                    valArray = [5.0, 7.0]
                if re.search('/RLTM\+CHAN/RSAC142/EC/.*/1HOUR/CDEC/', ipsu):
                    valArray = [1.5, 1.7]
            else:   # averaged over at least one tidal day
                valArray = [3.5, 3.5]
            if re.search('/CHWST.*/EC/', ipsu):   # allow greater EC range near CCF intake
                valArray = [3.0, 5.0]
            outds = flag_data('D', dataset, valArray, logfile)
            if outds: dataset = outds;Flagged = True

        writedss(outfile, ipsu, dataset)
        if Flagged:
            print 'Flagged path', ipsu, yUnits
        else:
            print 'No bad values found for', ipsu, yUnits
    print 'Finished auto-checking', infile
    print 'All paths written to', outfile
    sys.exit()


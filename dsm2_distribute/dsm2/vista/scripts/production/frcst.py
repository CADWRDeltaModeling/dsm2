# gets all data references for the matching b and c parts and
# optional e,f and a parts
def getRefs(dssfile,bp,cp,ap=None,ep=None,fp=None):
    g=opendss(dssfile)
    if g == None:
        raise "DSS file" + dssfile + " is empty"
    g.filterBy(PathPartPredicate("^"+bp+"$",Pathname.B_PART),1)
    if g == None:
        raise "DSS file" + dssfile + " has no data for b part = " + bp
    g.filterBy(PathPartPredicate("^"+cp+"$",Pathname.C_PART),1)
    if g == None:
        raise "DSS file" + dssfile + " has no data for c part = " + cp
    if ap != None:
        g.filterBy(PathPartPredicate("^"+ap+"$",Pathname.A_PART),1)
    if g == None:
        raise "DSS file" + dssfile + " has no data for a part = " + ap
    if fp != None:
        g.filterBy(PathPartPredicate("^"+fp+"$",Pathname.F_PART),1)
    if g == None:
        raise "DSS file" + dssfile + " has no data for f part = " + fp
    if ep != None:
        g.filterBy(PathPartPredicate("^"+ep+"$",Pathname.E_PART),1)
    if g == None:
        raise "DSS file" + dssfile + " has no data for e part = " + ep
    return g.getAllDataReferences()
#Use IEP hydro.dss file and
iep_hydro ='/delta4/data/dss/IEP/hydro.dss'
mtz_astro = '/delta5/calibration/realtide/hydro/mtz-fillin/mtz-astro.dss'
#
refs = getRefs(mtz_astro,'RSAC054','STAGE','astro\+chan','1hour','dwr-joc')
if not refs: raise ' No matching reference found for historical stage'
if ( len(refs) > 1 ):
    print 'More than one matching reference for astro'
#
ts_astro = refs[0].getData()
astro_path = refs[0].getPathname()
#
refs = getRefs(iep_hydro,'rsac054','stage','hist\+chan','15min','dwr-eso-d1485c')
if ( refs == None ) : raise 'No matching reference found for historical stage'
if ( len(refs) > 1 ):
    print 'More than one matching reference for historical stage'
ts_hist = refs[0].getData()
hist_path = refs[0].getPathname()
#
ctw = timeWindow('01jan1997 0000 - 30sep1997 2400')
st = repr(ctw.getStartTime())
et = repr(ctw.getEndTime())
ts_hist = ts_hist[st:et]
ts_astro = ts_astro[st:et]
# lets period average to 1hour
ts_hist = per_avg(ts_hist,'1hour')
ts_astro = per_avg(ts_astro,'1hour')
#
la = 3                                  # look-ahead in tidal days
stime = ctw.getStartTime()
etime = ctw.getEndTime()
fr_astro=ts_astro[st:et]
ts_diff = ts_hist-ts_astro
stime = stime + (repr(la*25)+'hours')
while 1:
    ts_diff2 = ts_diff[repr(stime+'-25hours'):repr(stime)]
    for i in range(la):
        for j in range(25):
            time_str = repr(stime+(repr(j+25*i)+'hour'))
            try :
                fr_astro[time_str]=fr_astro[time_str]+ts_diff2[j]
            except :
                break
    stime = stime + (repr(la*25)+'hours')
    print repr(stime)
    if stime.compare(etime) >= 0: break
#
# write out to dss file
print repr(hist_path)
DSSUtil.writeData('astro-frcst.dss',repr(hist_path),ts_hist)
#
print repr(astro_path)
DSSUtil.writeData('astro-frcst.dss',repr(astro_path),ts_astro)
#
astro_path.setPart(Pathname.A_PART,'FRCST+CHAN')
astro_path.setPart(Pathname.F_PART,'DWR-OSP-DMS')
print repr(astro_path)
DSSUtil.writeData('astro-frcst.dss',repr(astro_path),fr_astro)
#
#g3 = opendss('astro-frcst.dss')
#plot(g3[0:2])
#tabulate(g3[0:2])
#

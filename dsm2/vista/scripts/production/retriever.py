#
def getReferences(g,line):
    path = Pathname.createPathname(line)
    bp = path.getPart(Pathname.B_PART)
    cp = path.getPart(Pathname.C_PART)
    ep = path.getPart(Pathname.E_PART)
    gc = g.clone()
    gc.filterBy(PathPartPredicate("^"+bp+"$",Pathname.B_PART),1)
    gc.filterBy(PathPartPredicate("^"+cp+"$",Pathname.C_PART),1)
    gc.filterBy(PathPartPredicate("^"+ep+"$",Pathname.E_PART),1)
    return gc.getAllDataReferences()
#
from java.util import Date
from java.text import SimpleDateFormat
tf = TimeFactory.getInstance()
today_str = SimpleDateFormat("ddMMMyyyy HHmm").format(Date())
today = tf.createTime(today_str)
ti = tf.createTimeInterval('1year')
tw = tf.createTimeWindow(today-ti,today)
print 'Retrieving data for time window ' + repr(tw)
#
iep='iep.water.ca.gov'
iep_dir='/export/home/www/htdocs/dss/db/'
iep_hydro='hydro.dss'
iep_qual ='quality.dss'
local_hydro='iep-hydro.dss'
local_qual='iep-qual.dss'
#
g=opendss(iep_dir+iep_hydro,iep)
f=open('hydro.retrieve')
for line in f.readlines():
    if not g: break
    line=line[:-1]
    refs=getReferences(g,line)
    if not refs: continue
    print 'Working on line ' +line
    for ref in refs:
        if not ref: continue
        ref2 = DataReference.create(ref,tw)
        try:
            if not ref2: continue
            if not ref2.getData(): continue
            DSSUtil.writeData(local_hydro,line,ref2.getData())
            print 'updated ' + local_hydro + ' with ' + repr(ref2.getPathname())
        except:
            print 'Unsuccessful retrieving data for ' + repr(ref2.getPathname())
#
#
g=opendss(iep_dir+iep_qual,iep)
f=open('qual.retrieve')
for line in f.readlines():
    if not g: break
    line=line[:-1]
    refs=getReferences(g,line)
    if not refs: continue
    print 'Working on line ' +line
    for ref in refs:
        if not ref: continue
        ref2 = DataReference.create(ref,tw)
        try:
            if not ref2: continue
            if not ref2.getData(): continue
            DSSUtil.writeData(local_qual,line,ref2.getData())
            print 'updated ' + local_qual + ' with ' +  repr(ref2.getPathname())
        except:
            print 'Unsuccessful retrieving data for ' + repr(ref2.getPathname())
#

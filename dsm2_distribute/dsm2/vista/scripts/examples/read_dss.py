from vutils import *
from vtimeseries import *
def writecsv(filename,ts):
    h=open(filename,'w')
    h.write(ts.name+'\n')
    for e in ts:
        h.write("%s,%s\n"%(e.getXString(),e.getYString()))
    h.close()
#
g=opendss('../testdata/ex3-base.dss')
bpart='CLFCT'
cpart='STAGE'
epart='15MIN'
refs=findparts(g,b=bpart,c=cpart,e=epart)
if refs == None or len(refs)==0:
    print 'No data found for %s/%s//%s/'%(bpart,cpart,epart)
ref=refs[0]
from vista.set import DataReference
twstr='01DEC1987 0100-05DEC1987 2400'
ref=DataReference.create(ref,timewindow(twstr))
ts=ref.data
filename='%s_%s.txt'%(bpart,cpart)
#writeascii(filename,ts)
writecsv(filename,ts)
print 'Done'
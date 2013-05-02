from vtidefile import opentidefile
from vdss import writedss
from vutils import timewindow
import sys
import string
def get_flows(tidefile, chan, twstr):
    tf=opentidefile(tidefile)
    flows=[]
    if twstr != None:
        print 'Timewindow: %s'%twstr
        tw=timewindow(twstr)
    else:
        tw=None
    refs=tf.find(['','^%s$'%chan,'FLOW'])
    if refs and len(refs)==1:
        print "Getting data %s"%(str(chan))
        if tw!=None:
            ref=DataReference.create(refs[0],tw)
        else:
            ref=refs[0]
        flows.append(ref.data)
    return flows

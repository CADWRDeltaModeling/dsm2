# Uses tidefile to calculate volume of channels and reservoirs 
# The channels and reservoirs can be specified in an input file
from vtidefile import opentidefile
from vdss import writedss
from vutils import timewindow
import sys
import string
from vista.set import DataReference
def get_volumes_data(tidefile,channel_ranges, twstr):
    tf=opentidefile(tidefile)
    volumes=[]
    if twstr != None:
        print 'Timewindow: %s'%twstr
        tw=timewindow(twstr)
    else:
        tw=None
    for chan_range in channel_ranges:
        lo,hi=chan_range
        print lo,hi
        for chan in range(int(lo),int(hi+1)):
            refs=tf.find(['','^%s$'%chan,'VOLUME'])
            if refs and len(refs)==1:
                print "Getting data %s"%(str(chan))
                if tw!=None:
                    ref=DataReference.create(refs[0],tw)
                else:
                    ref=refs[0]
                volumes.append(ref.data)
    return volumes
def get_reservoir_volumes_data(tidefile,reservoir_names, twstr):
    tf=opentidefile(tidefile)
    volumes=[]
    if twstr != None:
        print 'Timewindow: %s'%twstr
        tw=timewindow(twstr)
    else:
        tw=None
    for name in reservoir_names:
        print 'Reservoir: %s'%name
        refs=tf.find(['','^%s$'%name,'VOLUME'])
        if refs and len(refs)==1:
            print "Getting data %s"%(str(name))
            if tw!=None:
                ref=DataReference.create(refs[0],tw)
            else:
                ref=refs[0]
            volumes.append(ref.data)
    return volumes
def total(volumes):
    tv=None
    for v in volumes:
        if not tv:
            print 'First tv = ',v.name
            tv=v
        else:
            print 'Adding tv to ',v.name
            tv=tv+v
    return tv
#
if __name__=='__main__':
    if (len(sys.argv) < 2):
        print """An input configuration file needs to specified: Below is an example
>>>>>> Configuration File START >>>>>>>>>>>>>>>>>
# A configuration input file for calculating volume from a tidefile and channel ids specified
[default]
# Input tidefile to use
tidefile=c:/temp/tidefile.h5
# Channel ranges define the channels that make up the control volume
channel_ranges=[(54,82),(84,105),(183,203),(125,145),(204,214),(216,231),(233,235),(252,259)]
# output dss file and path
output_dss_file=c:/temp/output.dss
output_dss_path=/CALC/SOUTH-DELTA/VOLUME//60MIN/CALC/
<<<<<< Configuration File END <<<<<<<<<<<<<<<<<<<<
        """
        exit(1)
    config_file=sys.argv[1]
    from ConfigParser import ConfigParser
    config=ConfigParser()
    config.read(config_file)
    tidefile=config.get('default','tidefile')
    cranges_raw=config.get('default','channel_ranges')
    try:
        twstr=config.get('default','timewindow');
    except:
        twstr=None
    try:
        reservoir_names=config.get('default','reservoir_names')
        reservoir_names=string.split(reservoir_names,',')
    except:
        reservoir_names=None
    print 'Calculating volume from tidefile: %s'%tidefile
    print 'Channel ranges: %s'+cranges_raw
    #channel_ranges=[(54,105),(183,203),(125,145),(204,225),(217,231),(233,235),(252,257)]
    #channel_ranges=[(54,82),(84,105),(183,203),(125,145),(204,214),(216,231),(233,235),(252,259)]
    channel_ranges=eval(cranges_raw)
    volumes=get_volumes_data(tidefile, channel_ranges, twstr)
    if reservoir_names != None:
        reservoir_volumes=get_reservoir_volumes_data(tidefile,reservoir_names,twstr)
        volumes.extend(reservoir_volumes)
    total_volume=total(volumes)
    outdssfile=config.get('default','output_dss_file')
    outdsspath=config.get('default','output_dss_path')
    print 'Writing out to %s as %s'%(outdssfile,outdsspath)
    writedss(outdssfile,outdsspath,total_volume)
    exit(0)
# 
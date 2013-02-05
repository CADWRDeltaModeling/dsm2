from vutils import *
from gov.ca.dsm2.input.parser import *
from gov.ca.dsm2.input.model import *
from java.io import FileInputStream
from vista.db.hdf5 import *
from math import *

class GetDsm2Model:
    ''' 
       class get_dsm2model(hydro_echo_file, gis_inp_file)
     - This class is used to obtain the latitude/longtitude/azimuth angle from 
       a given channel ID or channel name. 
     - Arguments: 
       hydro_echo_file  hydro echo file from DSM input 
       gis_inp_file     gis input file downloaded from DSM2 Grid Map interface
     - Usage examples:
       dsm2model = GetDsm2Model('D:\DSM2-SensTest\hydro_echo.inp','D:\DSM2-SensTest\gis.inp')
       channel_info = dsm2model.get_xy_by_chid('17') 
       channel_info = dsm2model.get_xy_by_name('calaveras')
    ''' 
    def __init__(self,hydro_echo_file,gis_inp_file):
        p = Parser()
        tables = p.parseModel(hydro_echo_file)
        p.parseAndAddToModel(tables,FileInputStream(gis_inp_file))
        self.data = tables.toDSM2Model()
        
    def get_xy_by_id(self,chid):
        res = {}        
        channels = self.data.getChannels()
        nodes = self.data.getNodes()
        out_chn = self.data.getOutputs()
        out_chn_data = out_chn.channelOutputs
        for chref in out_chn_data:
            if chid == chref.channelId: 
                #if there are multiple names for one channel id, the last name in hydro file will be picked up.
                res['channel_id'] = chref.channelId
                res['channel_name'] = chref.name   
        try:
            chtemp = channels.getChannel(chid)
            node_down = nodes.getNode(chtemp.downNodeId)
            node_up = nodes.getNode(chtemp.upNodeId)
            res['latitude'] = (node_down.latitude + node_up.latitude)/2
            res['longitude'] = (node_down.longitude + node_up.longitude)/2
            res['azimuth'] = get_flow_direction(node_up.longitude,node_up.latitude,node_down.longitude ,node_down.latitude)
        except:
            print chid + ' is not a valid channel ID!! '        
        return res
    
    def get_xy_by_name(self,chname):
        chid = self.get_id_by_name(chname)
        return self.get_xy_by_id(chid)
    
    def get_id_by_name(self,chname): 
        out_chn = self.data.getOutputs()
        out_chn_data = out_chn.channelOutputs
        for chref in out_chn_data:
            if chname.lower() == chref.name.lower():
                chid = chref.channelId
        return chid
            
def getxyz_from_hdf5(hydro_echo_file,gis_inp_file,hdf5_file,output_file,tw_string=None):
    '''
    getxyz_from_hdf5(hydro_echo_file,gis_inp_file,hdf5_file,output_file,tw_string=None)
    - This module summarizes lat/lng/variable_from_tide_file/flow_direction information.
    - Arguments:
      hydro_echo_file  hydro echo file from DSM input
      gis_inp_file     gis input file downloaded from DSM2 Grid Map interface
      hdf5_file        DMS2 model output tide file
      output_file      user specified output ascii filename
      tw_string        time window (optional)
    - Usage example:
      getxyz_from_hdf5('C:\delta\dsm2_v8\study_templates\historical\output\hydro_echo_historical.inp','C:\delta\dsm2_v8\study_templates\historical\output\gis.inp','C:\delta\dsm2_v8\study_templates\historical\output\historical.h5','C:\delta\dsm2_v8\study_templates\historical\output\getxyzfromhdf5.out')
    '''
    f = open(output_file, 'w')
    g = HDF5Group(hdf5_file)
    g = find(g,'FLOW','c')
    gfor_chid = find(g,'UPSTREAM$','b')
    if tw_string is not None:
        tw = timewindow(tw_string)
    from jarray import zeros       
    id = zeros(len(gfor_chid),'i')
    lat = zeros(len(gfor_chid),'d')
    lng = zeros(len(gfor_chid),'d')
    dir = zeros(len(gfor_chid),'d')
    arr_y = [] 
    j = 0
    dms2model = GetDsm2Model(hydro_echo_file,gis_inp_file)
    for ref in gfor_chid:
        chan_id = get_channel_id_from_hdf5ref(ref)            
        channel = dms2model.get_xy_by_id(chan_id)
        id[j] = int(chan_id)
        lat[j] = channel['latitude']
        lng[j] = channel['longitude']
        dir[j] = channel['azimuth']
        ''' obtain the time series data from HDF5 tide file '''
        g_upstream = find(g,chan_id+'_UPSTREAM','b')
        g_downstream = find(g,chan_id+'_DOWNSTREAM','b')
        if tw_string is not None:
            ref_up = g_upstream[0].data.createSlice(tw)
            ref_down = g_downstream[0].data.createSlice(tw)
        else:
            ref_up = g_upstream[0].data
            ref_down = g_downstream[0].data    
        ref_avg = (ref_up + ref_down) / 2    
        ref_filter = godin(ref_avg)
        ref_final = per_avg(ref_filter,'1day')
        ds = ref_final
        index = 0
        arr_ytmp = zeros(ref_final.size(),'d')
        for elem in ds:
            arr_ytmp[index] = elem.getY()
            index = index + 1 
        arr_y.append(arr_ytmp)
        j = j + 1     
    num_of_time = index
    num_of_chnl = j
    for i in range(num_of_time):
        f.write('Time Step: ' + str(i) + '\n')
        for j in range(num_of_chnl):       
            f.write(str(id[j]) + ',' + str(lat[j]) + ',' + str(lng[j])+ ',' + str(arr_y[j][i])+  ',' + str(dir[j])+ '\n')
    f.close()
    return None

def get_channel_id_from_hdf5ref(ref):
    ''' this function returns the channel ID from the reference 
        read from HDF5Group(hdf5File) function '''
    a = ref.getPathname().toString()
    b = a.split('_')
    c = b[0].split('/')
    return c[2]

def get_flow_direction(up_lng,up_lat,down_lng,down_lat):
    ''' this function returns the Azimuth angle from the specified 
        upstream Longitude/Latitude and downstream Longitude/Latitude '''
    del_lng = down_lng - up_lng
    del_lat = down_lat - up_lat
    if del_lng > 0 and del_lat > 0:
        angle = atan(abs(del_lng/del_lat))/(2*pi)*360
    elif del_lng > 0 and del_lat < 0:
        angle = 90 + atan(abs(del_lat/del_lng))/(2*pi)*360
    elif del_lng < 0 and del_lat > 0:
        angle = 270 + atan(abs(del_lat/del_lng))/(2*pi)*360
    elif del_lng < 0 and del_lat < 0:    
        angle = 180 + atan(abs(del_lng/del_lat))/(2*pi)*360    
    elif del_lng == 0 and del_lat > 0:
        angle = 0
    elif del_lng == 0 and del_lat < 0:
        angle = 180
    elif del_lng > 0 and del_lat == 0:
        angle = 90
    elif del_lng < 0 and del_lat == 0:
        angle = 270         
    else:        
        angle = 0     
    return angle

def varify_input_args(file_path):
    import os.path
    if os.path.exists(file_path):
        return True
    else:
        print 'File: ' + file_path + ' does not exist!'
        return False

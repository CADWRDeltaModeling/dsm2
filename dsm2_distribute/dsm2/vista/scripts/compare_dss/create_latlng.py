import os
import xyz
from xyz import *

def select_distinct_int(input_arr):
    output_arr = []
    tmp_arr = []
    for x in input_arr:
        if x not in tmp_arr:
            tmp_arr.append(x)
            output_arr.append(int(x))
    output_arr.sort()
    return output_arr

if __name__ == '__main__':
    script_path = str(os.getcwd()).replace("\\bin","\\scripts\\dsm2_reporting_tool")
    hydro_echo_inp = script_path +'\\preprocess\\hydro_echo_SC_ELT_MidRange.inp'
    gis_inp = script_path +'\\preprocess\\gis.inp'
    output_file = script_path +'\\js\\latlng.txt'
    dsm2model = GetDsm2Model(hydro_echo_inp,gis_inp)
    f = open(output_file, 'w')
    f.write('ID,Name,Longitude,Latitude\n')
    outchn = dsm2model.get_out_chn()
    a = []
    for chref in outchn:
        a.append(chref.channelId)
    channel_list = select_distinct_int(a)
    for i in channel_list:
        res=dsm2model.get_xy_by_id(str(i))
        f.write( res['channel_id']+','+res['channel_name'].upper()+','+str(res['latitude'])+','+str(res['longitude'])+'\n')
    f.close()  
    print "Done!!"

from java.lang import Float
def write_begin_data_array(file_handle):
    print >> file_handle, "data_list=["
def write_end_data_array(file_handle):
    print >> file_handle, "]"
def write_file(file_handle, refname, refvar, name, data_type, checked, diff_arr,latlng):
    print >> file_handle, """ {
    "output":"%s",
    "refvar":"%s",
    "name": "%s",
    "data_type": "%s",
    "checked": "%s",
    "latitude": "%s",
    "longitude": "%s",
    "diff": [""" %(refname, refvar, name, data_type, checked,latlng[2],latlng[3])
    for item in diff_arr:
        if len(diff_arr[0])==4:        # compare_mode = 5
            print >> file_handle, """ {"perc_rmse":"%0.2f","rmse":"%0.1f","perc_rmse2":"%0.2f","rmse2":"%0.1f"},""" %(item[0],item[1],item[2],item[3]) 
        else:    
            print >> file_handle, """ {"perc_rmse":"%0.2f","rmse":"%0.1f"},""" %(item[0],item[1])       
    print >> file_handle, "] }"
def format_for_nan(value):
    if Float.isNaN(value):
        return "NaN"
    else:
        return "%f"%value
    
def get_month(value):
    valueDic= {"JAN":"01", "FEB":"02", "MAR":"03", "APR":"04", "MAY":"05", "JUN":"06", "JUL":"07", "AUG":"08", "SEP":"09", "OCT":"10", "NOV":"11", "DEC":"12"}
    return valueDic[value]

''' create the water year type JavaScript file'''    
def write_begin_wyt_array(file_handle):
    print >> file_handle, "wyt=["
def write_end_wyt_array(file_handle):
    print >> file_handle, "]"
def write_wyt_file(file_handle, lines):
    color={'W':'#D9F7CC','AN':'#EBF7E6','BN':'#F7F8F7','D':'#FBEEF3','C':'#FFDFEC'}
    for line in lines:
        a = line.split(",")
        tw = a[0]
        wytype = a[1].replace("\n","")
        str_print='{start:new Date('+tw[5:9]+','+get_month(tw[2:5])+','+tw[0:2]+'), end:new Date('
        str_print+=tw[22:26]+','+get_month(tw[19:22])+','+tw[17:19]+'), code:"'+wytype+'", color:"'
        str_print+=color[wytype]+'"},'
        print >> file_handle, str_print

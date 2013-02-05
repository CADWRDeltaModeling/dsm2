from java.lang import Float
def write_begin_data_array(file_handle):
    print >> file_handle, "data=["
def write_end_data_array(file_handle):
    print >> file_handle, "]"
def write_file(file_handle, compare_mode, name, refvar, data, dataIndex, title, series_name, yaxis, xaxis, plot_type, data_type, per_opt):
    print >> file_handle, """ {
    "output":"%s",
    "refvar":"%s",
    "title": "%s",
    "plot_type": "%s",
    "data_type": "%s",
    "series_names": ["%s","%s","%s"],
    "yaxis_name": "%s %s",
    "xaxis_name": "%s", """%(name, refvar, title, plot_type, data_type, series_name[0], series_name[1], series_name[2], per_opt, yaxis, xaxis)
    write_chart_data(file_handle, compare_mode, data)
    print >> file_handle, "}"
def format_for_nan(value):
    if Float.isNaN(value) or value==-999.99:
        return "NaN"
    else:
        return "%0.2f"%value 
def write_chart_data(file_handle, compare_mode, data):
    n=len(data)
    i=0
    print >> file_handle, '"values" : ['
    while i < n:
        x=data[i][0]
        if compare_mode=='1':
            print >> file_handle, "{x:%.0f,y0:%s}"%(x,format_for_nan(data[i][1]))
        if compare_mode=='2':
            print >> file_handle, "{x:%.0f,y1:%s}"%(x,format_for_nan(data[i][1]))
        if compare_mode=='3':
            print >> file_handle, "{x:%.0f,y1:%s,y2:%s}"%(x,format_for_nan(data[i][1]),format_for_nan(data[i][2]))
        if compare_mode=='4':
            print >> file_handle, "{x:%.0f,y0:%s,y1:%s}"%(x,format_for_nan(data[i][1]),format_for_nan(data[i][2]))
        if compare_mode=='5':
            print >> file_handle, "{x:%.0f,y0:%s,y1:%s,y2:%s}"%(x,format_for_nan(data[i][1]),format_for_nan(data[i][2]),format_for_nan(data[i][3]))
        if i != n-1:
            print >> file_handle, ","
        i=i+1
    print >> file_handle, "]"

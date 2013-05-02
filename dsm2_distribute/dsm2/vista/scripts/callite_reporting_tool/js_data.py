from java.lang import Float
def write_begin_data_array(file_handle):
    print >> file_handle, "data=["
def write_end_data_array(file_handle):
    print >> file_handle, "]"
def write_file(file_handle, data, dataIndex, title, series_name, yaxis, xaxis, plot_type):
    print >> file_handle, """ {
    "title": "%s",
    "plot_type": "%s",
    "series_names": ["%s","%s"],
    "yaxis_name": "%s",
    "xaxis_name": "%s", """%(title, plot_type, series_name[0], series_name[1], yaxis, xaxis)
    write_chart_data(file_handle,data)
    print >> file_handle, "}"
def format_for_nan(value):
    if Float.isNaN(value):
        return "NaN"
    else:
        return "%f"%value 
def write_chart_data(file_handle,data):
    n=len(data)
    i=0
    print >> file_handle, '"values" : ['
    while i < n:
        x=data[i][0]
        y0=format_for_nan(data[i][1])
        y1=format_for_nan(data[i][2])
        print >> file_handle, "{x:%f,y1:%s,y2:%s}"%(x,y0,y1)
        if i != n-1:
            print >> file_handle, ","
        i=i+1
    print >> file_handle, "]"

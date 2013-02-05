import sys, os, shutil
import vtimeseries, vdiff
import js_data, js_data_list, compare_dss_utils
import logging
from shutil import copytree
class PlotType:
    TIME_SERIES="timeseries"
    EXCEEDANCE="exceedance"
class PathnameMap:
    def __init__(self, name):
        self.var_name = name;
        self.report_type = "Average"
        self.path1 = None
        self.path2 = None
        self.var_category = ""
            
def copy_basic_files(output_path):
    if output_path[-19:-1]=='scripts\\compare_ds':
        print "*************************************"
        print "  Please select the other directory  "
        print "*************************************"
    else:
        if os.path.isdir(output_path+"//js"):
            shutil.rmtree(output_path+"//js")
        if not os.path.isdir(output_path+"//data"):
            os.mkdir(output_path+"//data")
        vista_home = str(os.getenv('VISTA_HOME'))
        try:
            copytree(vista_home+"\scripts\compare_dss\js",output_path+"\js",[])
        except:
            copytree(vista_home+"\scripts\compare_dss\js",output_path+"//js",[])
            
def get_output_html(template_file):
    from gov.ca.dsm2.input.parser import Parser
    p = Parser()                                 
    tables = p.parseModel(template_file)
    scalar_table = tables.getTableNamed("SCALAR")
    scalar_values = scalar_table.getValues()
    nscalars = scalar_values.size()
    for i in range(nscalars):
        name = scalar_table.getValue(i, "NAME").encode('ascii')
        value = scalar_table.getValue(i, "VALUE").encode('ascii').replace('"','')
        if name=='OUTDIR': outdir = value
        if name=='OUTFILE': outfile = value
    if outdir!="" and outfile!="":
        return outdir, outfile
    else:
        logging.debug("Output directory and output html file are not specified!!")
        return None

def parse_template_file(template_file):
    from gov.ca.dsm2.input.parser import Parser
    p = Parser()                                 
    tables = p.parseModel(template_file)
    global_table = tables.getTableNamed("GLOBAL_CONTROL")
    global_values = global_table.getValues()
    nglobals = global_values.size()
    globals = {}
    for i in range(nglobals):
        name = global_table.getValue(i,"CONTROLLER").encode('ascii')
        value = global_table.getValue(i,"MODE").encode('ascii')
        globals[name] = value
    scalar_table = tables.getTableNamed("SCALAR")
    scalar_values = scalar_table.getValues()
    nscalars = scalar_values.size()
    scalars = {}
    for i in range(nscalars):
        name = scalar_table.getValue(i, "NAME").encode('ascii')
        value = scalar_table.getValue(i, "VALUE").encode('ascii').replace('"','')
        scalars[name] = value
    var_table = tables.getTableNamed("VARIABLE")
    var_values = var_table.getValues()
    output_table = tables.getTableNamed("OUTPUT")
    output_values = output_table.getValues()
    ov = []
    for name in output_values:
        ov.append(name[0].encode('ascii').replace('"',''))
    output_values = ov
    timewindow_table = tables.getTableNamed("TIME_PERIODS")
    tw_values = timewindow_table.getValues();
    return globals, scalars, var_values, output_values, tw_values

def do_processing(globals, scalars, var_values, output_values, tw_values):
    compare_mode = globals['COMPARE_MODE']
    time_interval = globals['DEFAULT_TIME_INTERVAL']
    # open files 1 and file 2 and loop over to plot
    from java.util import Date
    dss_group0, dss_group1, dss_group2, refvar, refname = compare_dss_utils.get_group_ref(globals, scalars, var_values)
    # obtain the new output list modified for *_ or _* cases
    output_values = compare_dss_utils.get_ref_list(compare_mode,dss_group0,dss_group1,output_values) 
    output_dir = scalars['OUTDIR'].replace('"','')
    wyt_arr = write_wyt(output_dir, tw_values)  #write the water type JavaScript file
    #out_name = column(output_values,0)
    #out_type = column(output_values,1)
    if compare_mode=='1':
        type_arr = compare_dss_utils.get_cpart_list(dss_group0)
    else:
        type_arr = compare_dss_utils.get_cpart_list(dss_group1)
    compare_dss_utils.chk_cpart_space(type_arr)
    if output_dir[-1]!='/': output_dir=output_dir+"/"
    time_windows = map(lambda val: val[1].replace('"',''), tw_values)
    tws = map(lambda x: vtimeseries.timewindow(x), time_windows)
    dIndex = 0
    dataIndex = {}
    fs_davg = {}
    fs_dmax = {}
    fs_dmin = {}
    fs_mavg = {}
    fs_orig = {}

    tbl_latlng = compare_dss_utils.lines2table(output_dir+"js/latlng.txt")
    fl = open(output_dir+"data/data_list.js",'w')
    for i in type_arr:
        initial_js = output_dir+"data/DSS_compare_spec_davg_"+i+".js"
        initial_pretab = i
        fs_davg[i] = open(output_dir+"data/DSS_compare_spec_davg_"+i+".js",'w')
        fs_dmax[i] = open(output_dir+"data/DSS_compare_spec_dmax_"+i+".js",'w')
        fs_dmin[i] = open(output_dir+"data/DSS_compare_spec_dmin_"+i+".js",'w')
        fs_mavg[i] = open(output_dir+"data/DSS_compare_spec_mavg_"+i+".js",'w')
        print >> fs_davg[i], """/* Comparison Output File Generated on : %s */"""%(str(Date()))
        print >> fs_dmax[i], """/* Comparison Output File Generated on : %s */"""%(str(Date()))
        print >> fs_dmin[i], """/* Comparison Output File Generated on : %s */"""%(str(Date()))
        print >> fs_mavg[i], """/* Comparison Output File Generated on : %s */"""%(str(Date()))        
        dataIndex['spec_davg_'+i] = 0
        dataIndex['spec_dmax_'+i] = 0
        dataIndex['spec_dmin_'+i] = 0
        dataIndex['spec_mavg_'+i] = 0
        js_data.write_begin_data_array(fs_davg[i])
        js_data.write_begin_data_array(fs_dmax[i])
        js_data.write_begin_data_array(fs_dmin[i])
        js_data.write_begin_data_array(fs_mavg[i])
        if globals['PLOT_ORIGINAL_TIME_INTERVAL']=='ON':
            fs_orig[i] = open(output_dir+"data/DSS_compare_spec_orig_"+i+".js",'w')
            print >> fs_orig[i], """/* Comparison Output File Generated on : %s */"""%(str(Date())) 
            dataIndex['spec_orig_'+i] = 0
            js_data.write_begin_data_array(fs_orig[i])
    js_data_list.write_begin_data_array(fl)
    
    wy_types = ['W','AN','BN','D','C']
    if globals['DONOT_SORT_STATION_NAME']=='OFF':
        output_values = compare_dss_utils.sort_alphabetically(output_values)
        refname = compare_dss_utils.sort_alphabetically(refname)
    else:
        refname = compare_dss_utils.get_not_sort_refname(refname,output_values)
        
    for name in refname:
        ref0 = None; ref1 = None; ref2 = None
        try:
            series_name = compare_dss_utils.get_series_name(compare_mode,scalars,refvar[name])
        except Exception,e:
            logging.error("Error building name for :%s and error is %s"%(name,e))
            continue
        if (globals['CALCULATE_SPECIFIED_RMSE_ONLY']=='OFF') or (globals['CALCULATE_SPECIFIED_RMSE_ONLY']=='ON' and name in output_values):
            conti = 0 
            str_refvar = compare_dss_utils.get_str_refvar(refvar[name],compare_mode)
            if compare_mode=='1' or compare_mode=='4' or compare_mode=='5':
                try:
                    if refvar[name][0][4]=='0': dss_group = dss_group0
                    if refvar[name][0][4]=='1': dss_group = dss_group1
                    if refvar[name][0][4]=='2': dss_group = dss_group2
                    ref0 = compare_dss_utils.get_ref(dss_group, refvar[name][0])
                    p = ref0.pathname
                    intv, var_name, data_units, data_type = compare_dss_utils.get_prop_of_ref(ref0)
                except:
                    print "**** Can't find matched time series for "+ name +" ****"
                    conti = 1
            if compare_mode=='3' or compare_mode=='5':
                try:
                    if refvar[name][2][4]=='0': dss_group = dss_group0
                    if refvar[name][2][4]=='1': dss_group = dss_group1
                    if refvar[name][2][4]=='2': dss_group = dss_group2
                    ref2 = compare_dss_utils.get_ref(dss_group, refvar[name][2])
                    p = ref2.pathname
                    intv, var_name, data_units, data_type = compare_dss_utils.get_prop_of_ref(ref2)
                except: 
                    print "**** Can't find matched time series for "+ name +" ****"
                    conti = 1
            if compare_mode=='2' or compare_mode=='3' or compare_mode=='4' or compare_mode=='5':
                try:
                    if refvar[name][1][4]=='0': dss_group = dss_group0
                    if refvar[name][1][4]=='1': dss_group = dss_group1
                    if refvar[name][1][4]=='2': dss_group = dss_group2
                    ref1 = compare_dss_utils.get_ref(dss_group, refvar[name][1])
                    p = ref1.pathname
                    intv, var_name, data_units, data_type = compare_dss_utils.get_prop_of_ref(ref1)
                except:
                    print "**** Can't find matched time series for "+ name +" ****"
                    conti = 1
                        
            if conti == 0: 
                cpart = p.getPart(p.C_PART).encode('ascii')
                if name in output_values:  
                    ref0_davg = None; ref0_dmax = None; ref0_dmin = None; ref0_mavg = None
                    ref1_davg = None; ref1_dmax = None; ref1_dmin = None; ref1_mavg = None
                    ref2_davg = None; ref2_dmax = None; ref2_dmin = None; ref2_mavg = None                    
                    if intv < 1000:
                        dataIndex['spec_davg_'+cpart]=dataIndex['spec_davg_'+cpart]+1
                        dataIndex['spec_dmax_'+cpart]=dataIndex['spec_dmax_'+cpart]+1
                        dataIndex['spec_dmin_'+cpart]=dataIndex['spec_dmin_'+cpart]+1
                        if dataIndex['spec_davg_'+cpart]>1:
                            fs_davg[cpart].write(",") 
                        if dataIndex['spec_dmax_'+cpart]>1:
                            fs_dmax[cpart].write(",")
                        if dataIndex['spec_dmin_'+cpart]>1:
                            fs_dmin[cpart].write(",")
                        if compare_mode=='1' or compare_mode=='4' or compare_mode=='5':    
                            ref0_godin = vtimeseries.godin(ref0)
                            ref0_davg = vtimeseries.per_avg(ref0_godin,'1day')
                            ref0_dmax = vtimeseries.per_max(ref0,'1day')
                            ref0_dmin = vtimeseries.per_min(ref0,'1day')
                        if compare_mode=='2' or compare_mode=='3' or compare_mode=='4' or compare_mode=='5':
                            ref1_godin = vtimeseries.godin(ref1)
                            ref1_davg = vtimeseries.per_avg(ref1_godin,'1day')
                            ref1_dmax = vtimeseries.per_max(ref1,'1day')
                            ref1_dmin = vtimeseries.per_min(ref1,'1day')  
                        if compare_mode=='3' or compare_mode=='5':
                            ref2_godin = vtimeseries.godin(ref2)
                            ref2_davg = vtimeseries.per_avg(ref2_godin,'1day')
                            ref2_dmax = vtimeseries.per_max(ref2,'1day')
                            ref2_dmin = vtimeseries.per_min(ref2,'1day')                                    
                        write_plot_data(fs_davg[cpart], compare_mode, name, str_refvar, compare_dss_utils.build_data_array(ref0_davg,ref1_davg,ref2_davg), dataIndex['spec_davg_'+cpart], "%s"%var_name, series_name, "%s (%s)"%(data_type,data_units), "Time", PlotType.TIME_SERIES, cpart,'Daily Average')           
                        write_plot_data(fs_dmax[cpart], compare_mode, name, str_refvar, compare_dss_utils.build_data_array(ref0_dmax,ref1_dmax,ref2_dmax), dataIndex['spec_dmax_'+cpart], "%s"%var_name, series_name, "%s (%s)"%(data_type,data_units), "Time", PlotType.TIME_SERIES, cpart,'Daily Maximum')           
                        write_plot_data(fs_dmin[cpart], compare_mode, name, str_refvar, compare_dss_utils.build_data_array(ref0_dmin,ref1_dmin,ref2_dmin), dataIndex['spec_dmin_'+cpart], "%s"%var_name, series_name, "%s (%s)"%(data_type,data_units), "Time", PlotType.TIME_SERIES, cpart,'Daily Minimum')
                    elif globals['DEFAULT_TIME_INTERVAL']=='1DAY':
                        ref0_davg = ref0; ref0_dmax = ref0; ref0_dmin = ref0
                        ref1_davg = ref1; ref1_dmax = ref1; ref1_dmin = ref1
                        ref2_davg = ref2; ref2_dmax = ref2; ref2_dmin = ref2
                    elif globals['DEFAULT_TIME_INTERVAL']=='1MON':
                        ref2_mavg = ref2; ref2_mmax = ref2; ref2_mmin = ref2
                    if globals['PLOT_ORIGINAL_TIME_INTERVAL']=='ON':
                        dataIndex['spec_orig_'+cpart]=dataIndex['spec_orig_'+cpart]+1
                        if dataIndex['spec_orig_'+cpart]>1:
                            fs_orig[cpart].write(",")        
                        write_plot_data(fs_orig[cpart], compare_mode, name, str_refvar, compare_dss_utils.build_data_array(ref0,ref1,ref2), dataIndex['spec_orig_'+cpart], "%s"%var_name, series_name, "%s (%s)"%(data_type,data_units), "Time", PlotType.TIME_SERIES, cpart,'Original Time Interval')
                           
                    if intv < 40000:
                        dataIndex['spec_mavg_'+cpart]=dataIndex['spec_mavg_'+cpart]+1
                        if dataIndex['spec_mavg_'+cpart]>1:
                            fs_mavg[cpart].write(",")         
                        if compare_mode=='1' or compare_mode=='4' or compare_mode=='5':      
                            ref0_mavg = vtimeseries.per_avg(ref0,'1month')
                        if compare_mode=='2' or compare_mode=='3' or compare_mode=='4' or compare_mode=='5':
                            ref1_mavg = vtimeseries.per_avg(ref1,'1month')
                        if compare_mode=='3' or compare_mode=='5':
                            ref2_mavg = vtimeseries.per_avg(ref2,'1month')
                        write_plot_data(fs_mavg[cpart], compare_mode, name, str_refvar, compare_dss_utils.build_data_array(ref0_mavg,ref1_mavg,ref2_mavg), dataIndex['spec_mavg_'+cpart], "%s"%var_name, series_name, "%s (%s)"%(data_type,data_units), "Time", PlotType.TIME_SERIES, cpart,'Monthly Average')

                if (globals['CALCULATE_SPECIFIED_RMSE_ONLY']=='OFF') or (globals['CALCULATE_SPECIFIED_RMSE_ONLY']=='ON' and name in output_values):
                    diff_arr = []
                    if compare_mode=='3' or compare_mode=='4' or compare_mode=='5':
                        for i in range(len(tws)):
                            if compare_mode=='3':   
                                rmse_val = vdiff.rmse(ref1, ref2, tws[i], time_interval)
                                perc_rmse_val = vdiff.perc_rmse(ref1, ref2, tws[i], time_interval)
                                diff_arr.append([perc_rmse_val,rmse_val ])
                            if compare_mode=='4':
                                rmse_val = vdiff.rmse(ref0, ref1, tws[i], time_interval)
                                perc_rmse_val = vdiff.perc_rmse(ref0, ref1, tws[i], time_interval)
                                diff_arr.append([perc_rmse_val,rmse_val ])
                            if compare_mode=='5':
                                rmse_val = vdiff.rmse(ref0, ref1, tws[i], time_interval)
                                perc_rmse_val = vdiff.perc_rmse(ref0, ref1, tws[i], time_interval)                    
                                rmse_val2 = vdiff.rmse(ref0, ref2, tws[i], time_interval)
                                perc_rmse_val2 = vdiff.perc_rmse(ref0, ref2, tws[i], time_interval)
                                diff_arr.append([perc_rmse_val,rmse_val,perc_rmse_val2,rmse_val2 ])         
                        for w in wy_types:
                            if compare_mode=='3':
                                diff_arr.append([vdiff.rmse_discrete_tws(ref1,ref2,wyt_arr[w],0), vdiff.rmse_discrete_tws(ref1,ref2,wyt_arr[w],1)])
                            if compare_mode=='4':
                                diff_arr.append([vdiff.rmse_discrete_tws(ref0,ref1,wyt_arr[w],0), vdiff.rmse_discrete_tws(ref0,ref1,wyt_arr[w],1)])
                            if compare_mode=='5':
                                diff_arr.append([vdiff.rmse_discrete_tws(ref0,ref1,wyt_arr[w],0), vdiff.rmse_discrete_tws(ref0,ref1,wyt_arr[w],1), vdiff.rmse_discrete_tws(ref0,ref2,wyt_arr[w],0), vdiff.rmse_discrete_tws(ref0,ref2,wyt_arr[w],1)])
    
                    latlng = compare_dss_utils.get_latlng(tbl_latlng,p.getPart(p.B_PART))
                    if globals['CALCULATE_SPECIFIED_RMSE_ONLY']=='OFF':
                        logging.debug('Working on index: %d/%d '%(dIndex+1,len(refname)))
                    else:
                        logging.debug('Working on index: %d/%d '%(dIndex+1,len(output_values)))
                    dIndex = dIndex + 1
                    if latlng==None:
                        latlng=['nan','nan','nan','nan']
                        print "can't find the Lat/Lng for ",p.getPart(p.B_PART)
                    if dIndex>1: 
                        fl.write(",")
                
                    if  name in output_values:      
                        write_list_data(fl,name, str_refvar, p.getPart(p.B_PART), cpart, 1, diff_arr,latlng)
                    else:
                        write_list_data(fl,name, str_refvar, p.getPart(p.B_PART), cpart, 0, diff_arr,latlng)              
            else:
                logging.debug("*** Please verify the path for "+name+" ***")               

                
    logging.debug('Writing end of data array')
    for i in type_arr:
        js_data.write_end_data_array(fs_davg[i])
        js_data.write_end_data_array(fs_dmax[i])
        js_data.write_end_data_array(fs_dmin[i])
        js_data.write_end_data_array(fs_mavg[i])
        fs_davg[i].close()
        fs_dmax[i].close()
        fs_dmin[i].close()
        fs_mavg[i].close()
        if globals['PLOT_ORIGINAL_TIME_INTERVAL']=='ON':
            js_data.write_end_data_array(fs_orig[i])
            fs_orig[i].close()
    js_data_list.write_end_data_array(fl)
    fl.close()
    # Generate the main html file
    fh = open(output_dir+scalars['OUTFILE'],'w')
    print >> fh, """ 
<html>
<head>
<meta http-equiv="X-UA-Compatible" content="IE=Edge,chrome=1">
<title>DSM2 Report: %s vs %s</title>
<script type="text/javascript" src="%s"></script>
<script type="text/javascript" src="js/wateryr.js"></script>
<script type="text/javascript" src="data/data_list.js"></script>
<script type="text/javascript" src="js/protovis-d3.3.js"></script>
<script type="text/javascript" src="js/plots.js"></script>
<script type="text/javascript" src="js/jquery-1.4.2.min.js"></script>
<script type="text/javascript" src="js/calendar.js"></script>
<link rel="stylesheet" type="text/css" media="print" href="js/print.css" /> 
<link rel="stylesheet" type="text/css" media="screen" href="js/screen.css" />
<link rel="stylesheet" type="text/css"  href="js/calendar.css" />
<script type="text/javascript" src="js/common.js"></script>
<script type="text/javascript" src="js/tabber.js"></script>
<link rel="stylesheet" href="js/tabber.css" TYPE="text/css" MEDIA="screen">
<link rel="stylesheet" href="js/tabber-print.css" TYPE="text/css" MEDIA="print">
<script src="http://maps.google.com/maps?file=js" type="text/javascript"></script>
<script type="text/javascript">
document.write('<style type="text/css">.tabber{display:none;}<\/style>');
var pre_tab="%s";
var pre_period="davg";
</script>
</head><body onunload="GUnload()"><a href="#top"></a>
 <!--[if IE]>
  <script type="text/javascript" src="http://ajax.googleapis.com/ajax/libs/chrome-frame/1/CFInstall.min.js"></script>
    <style>
     .chromeFrameInstallDefaultStyle {
       border: 5px solid blue;
     }
    </style>
  <script>
  function install_gcf() {
      CFInstall.check({
        mode: "overlay",
        node: "prompt"
      });
      return false;
     }
     window.attachEvent("onload", install_gcf);
  </script>
  <![endif]-->
"""%(scalars['NAME1'],scalars['NAME2'],initial_js,initial_pretab)
    tws = write_summary_table(fh,dss_group1, dss_group2, globals, scalars, tw_values, var_values, output_values)
    if globals['CALCULATE_SPECIFIED_RMSE_ONLY']=='ON':
        part_c = compare_dss_utils.get_cpart_from_spec(var_values, output_values)
    else:    
        part_c = compare_dss_utils.get_cpart_list(dss_group1)
    dt_arr = "dt_arr=["
    peri_name = "period_name=["
    peri_range = "period_range=["    
    for type_item in part_c:
        dt_arr+= "'"+type_item+"',"
    for i in range(len(tws)):
        peri_name+= '"'+tw_values[i][0].replace('"','')+'",'
    for tw in tws:
        peri_range+= '"'+compare_dss_utils.format_timewindow(tw)+'",'
    print >> fh, '<script type="text/javascript">'+dt_arr+"];"  
    print >> fh, peri_name+"];"
    print >> fh, peri_range+"]; </script>" 
    write_js_block(fh,globals,scalars)
    fh.close()
    #fireup(output_dir+scalars['OUTFILE'])
    logging.debug('Closed out data file')

def write_wyt(output_dir,tw_values):
    if output_dir[-1]!='/': output_dir=output_dir+"/"
    wyt_f = open(output_dir+"js/wateryr.txt","r")
    wyt_js = open(output_dir+"js/wateryr.js","w")
    wyt_lines = wyt_f.readlines()
    wyt_arr = compare_dss_utils.get_wyt_array(wyt_lines,tw_values[0][1][6:10],tw_values[0][1][23:27])
    js_data_list.write_begin_wyt_array(wyt_js)
    js_data_list.write_wyt_file(wyt_js,wyt_lines)
    js_data_list.write_end_wyt_array(wyt_js)
    wyt_f.close()
    wyt_js.close()
    return wyt_arr
    
    
def write_summary_table(fh, dss_group1,dss_group2,globals,scalars,tw_values, var_values, output_values):
    compare_mode = globals['COMPARE_MODE']
    if compare_mode=='1': print >> fh, "<h1><center>Observation Time Series Report<br>Data Set: %s </center></h1>"%(scalars['NAME0'])
    if compare_mode=='2': print >> fh, "<h1><center>Model Output Time Series Report<br>Study Name: %s </center></h1>"%(scalars['NAME1'])
    if compare_mode=='3': print >> fh, "<h1><center>DSM2 Output Comparison Report<br> %s <n>vs</n> %s</center></h1>"%(scalars['NAME1'], scalars['NAME2'])
    if compare_mode=='4': print >> fh, "<h1><center>Calibration Report<br> %s <n>vs</n> %s</center></h1>"%(scalars['NAME1'], scalars['NAME0'])
    if compare_mode=='5': print >> fh, "<h1><center>Calibration Report<br>[ %s <n>vs</n> %s ] <n> & </n> [ %s <n>vs</n> %s ]</center></h1>"%(scalars['NAME1'],scalars['NAME0'], scalars['NAME2'],scalars['NAME0'])
    print >> fh, '<div id="note">Note: %s</div>'%(scalars['NOTE'].replace('"',''))
    print >> fh, '<div id="assumptions">Assumptions: %s</div>'%(scalars['ASSUMPTIONS'].replace('"',''))   
    print >> fh, """<div id="control-panel"> 
<form>
Data Conversion for Plot: <select name="datatype" onChange="change_period()" id="data-conversion">
    <option value="davg"> Daily Average</option>
    <option value="dmax"> Daily Maximum</option>
    <option value="dmin"> Daily Minimim</option>
    <option value="mavg"> Monthly Average</option>
""" 
    if globals['PLOT_ORIGINAL_TIME_INTERVAL']=='ON':
        print >> fh, '<option value="orig">Original Data</option>'
    print >> fh, """
</select><br> 

Use Defined Time Window: <select name="tw" id="time-window-select"> 
"""
    for i in range(len(tw_values)):
        if i==0:
            selected = 'selected="selected"'
        else:
            selected = ""
        print >> fh, '<option value="%s" %s>%s</option>'%(compare_dss_utils.timewindow_option_value(vtimeseries.timewindow(tw_values[i][1].replace('"',''))), selected,tw_values[i][0].replace('"','')) 
    print >> fh, """ 
</select> 
<div>
Customize Time Window:
"""
    twstr = compare_dss_utils.timewindow_option_value(vtimeseries.timewindow(tw_values[0][1].replace('"',''))).split("-")
    a1=twstr[0].split(",")
    a2=twstr[1].split(",")
    print >> fh, """Start Date: <input name="SDate" id="SDate" value="%s" onclick="displayDatePicker('SDate');" size=12>"""%(a1[1]+"/"+a1[2]+"/"+a1[0])
    print >> fh, """End Date: <input name="EDate" id="EDate" value="%s" onclick="displayDatePicker('EDate');" size=12>"""%(a2[1]+"/"+a2[2]+"/"+a2[0])
    print >> fh, """
  <input type=button id="calendar" value="Re-draw">
</div>
"""
    if (compare_mode!='1' and compare_mode!='2'):
        print >> fh, '<div>'
        print >> fh, 'Show differences on plot:<input type="checkbox" name="diff_plot" value="1"/> &emsp; (Difference = %s - %s)'%(scalars['NAME2'],scalars['NAME1'])
        print >> fh, '</div>'   
    print >> fh, """
<div>
    Show water year types on plot: <input type="checkbox" name="wyt" value="1"/> &emsp;(<span style="font-size:100%"><span style="background-color:#D9F7CC"> &emsp;&emsp; Wet &emsp;&emsp;</span><span style="background-color:#EBF7E6"> &emsp; Above Normal &emsp;</span><span style="background-color:#F7F8F7">&emsp; Below Normal &emsp;</span><span style="background-color:#FBEEF3"> &emsp;&emsp; Dry &emsp;&emsp; </span><span style="background-color:#FFDFEC"> &emsp; Critical &emsp; </span></span>)
</div>
"""  
    if (compare_mode!='1' and compare_mode!='2'):
        print >> fh, """
<div> 
    Threshold value to highlight percentage differences
    <input type="text" id="threshold" value="50"/> 
</div>
<div>
Table Statistics: <select name="stat" id="stat" onChange="">
<option value=0>Percentage RMS Diff</option><option value=1>RMS Diff</option>
</select>
</div>
"""
    print >> fh, """
<div id="warning" style="color:red;font-weight:bold"></div>
<input type="hidden" name="ta" id="ta" value=""> 
</form> 
</div>"""
    time_windows = map(lambda val: val[1].replace('"',''), tw_values)
    tws = map(lambda x: vtimeseries.timewindow(x), time_windows)
    print >> fh, '<div class="tabber">'
    if globals['CALCULATE_SPECIFIED_RMSE_ONLY']=='ON':
        part_c = compare_dss_utils.get_cpart_from_spec(var_values, output_values)
    else:    
        part_c = compare_dss_utils.get_cpart_list(dss_group1)
    for type_item in part_c:
        print >> fh, '<div class="tabbertab" id="%s"><h2>%s</h2><p id="%s_p">'%(type_item,type_item,type_item)
        print >> fh, '</p></div>'
    print >> fh, '</div>'
    print >> fh, '<br><center><font style="font-family:arial,verdana,sans-serif;">Copyright &copy; 2010 State of California, Department of Water Resources<br>Last Modified: 1/25/2011</font></center>'
    return tws 
        
def write_plot_data(fh, compare_mode, name, refvar,data, dataIndex,  title, series_name, yaxis, xaxis, plot_type, data_type, per_opt):
    js_data.write_file(fh, compare_mode, name, refvar, data, dataIndex, title, series_name, yaxis, xaxis, plot_type, data_type, per_opt);
def write_list_data(fh, refname, str_refvar, name, data_type, checked, diff,latlng):
    js_data_list.write_file(fh, refname, str_refvar, name, data_type, checked, diff,latlng);
def write_js_block(fh,globals,scalars):
    print >> fh, """<script type="text/javascript">
    function reload_js(){
        tab_name = document.getElementById('ta').value; 
        per_name = document.getElementById('data-conversion').value;
        replacejscssfile("data/DSS_compare_spec_"+pre_period+"_"+pre_tab+".js", "data/DSS_compare_spec_"+per_name+"_"+tab_name+".js", "js");
        if (document.getElementById('ta').value=='STAGE' && document.getElementById('data-conversion').value=='davg')
           {document.getElementById('warning').innerHTML='Daily Average Stage is meaningless! Please select daily max/min for plotting.';}
        else {document.getElementById('warning').innerHTML=''; }
        pre_tab = tab_name;
        pre_period = per_name;       
    }
    function clear_and_draw(sdate, edate){
"""
    #if globals['DONOT_SORT_BY_STATION_NAME']=='ON':
    #    print >> fh,"""
    #    newdata=dont_sort(data);
    #    data=newdata;
    #    """
    print >> fh,"""
        $('.plot').empty();
        tab_name = document.getElementById('ta').value; 
        n=data.length; 
        plot_diff = $('input[name=diff_plot]').is(':checked') ? 1 : 0 ;
        plot_wyt = $('input[name=wyt]').is(':checked') ? 1 : 0 ;
        for(i=0; i < n; i++){
          if (data[i].data_type==tab_name){
            var div_id = "fig"+"_"+data[i].output;
            if (data[i]==null) continue;
            if ($("#"+div_id).length==0){
                $("#"+data[i].data_type).append('<a href="#'+div_id+'" title="'+data[i].refvar+'"><div class="plot" id="'+div_id+'"></div></a><!--<img width=20 height=20 src="js/csv.gif" onClick="downloadcsv(\\''+data[i].output+'\\')">-->');
            }
            if (data[i].plot_type=="timeseries"){ 
               if (plot_wyt==1){ 
                 plots.multi_time_series_plot(div_id,data[i],plot_diff,sdate,edate,wyt);
               }else {
                 plots.multi_time_series_plot(div_id,data[i],plot_diff,sdate,edate);
               }
            }else if (data[i].plot_type=="exceedance"){
                plots.exceedance_plot(div_id,data[i],null,sdate,edate);
            }
          }
        }      
    };
    function hideDiv(div_name) {
     if ($("#img").attr("src")=="js/open.JPG") {
       $("#"+div_name).show("slow");
       $("#img").attr("src","js/close.JPG");
     }else{ 
       $("#"+div_name).hide("slow"); 
       $("#img").attr("src","js/open.JPG");
     }
    }
    
    function location_list(){
       var tbl_sel=new Array();
       var tbl_unsel=new Array();
       ns=data_list.length;
       tab_name = document.getElementById('ta').value;
       $("#"+tab_name+"_p").empty();
       i = dt_arr.indexOf(tab_name);
       sd=extract_date(to_date_comma($("#SDate").val()));
       ed=extract_date(to_date_comma($("#EDate").val()));
       // write the header
       $("#"+dt_arr[i]+"_p").append('<a href="#" onClick="clear_and_draw(extract_date(to_date_comma($(\\'#SDate\\').val())),extract_date(to_date_comma($(\\'#EDate\\').val())));" onMouseover="this.style.background=\\'#C8F526\\'" onMouseout="this.style.background=\\'\\'"><img src="js/chart.JPG" width="20px" height="19px"> Show the time series plots</a><br>');
       wyt_txt=["Wet","Above Normal","Below Normal","Dry","Critical"];        
"""
    display_name = []
    if (globals['COMPARE_MODE']!='1' and globals['COMPARE_MODE']!='2'):
        if globals['COMPARE_MODE']=='3': display_name = [scalars['NAME2'],scalars['NAME1'],scalars['NAME2'],scalars['NAME1']]
        if globals['COMPARE_MODE']=='4': display_name = [scalars['NAME1'],scalars['NAME0'],scalars['NAME1'],scalars['NAME0']]
        if globals['COMPARE_MODE']=='5': display_name = [scalars['NAME1']+'/'+scalars['NAME2'],scalars['NAME0'],scalars['NAME1']+'/'+scalars['NAME2'],scalars['NAME0']]
        print >> fh, """
           tbl_head='<table class="alt-highlight" id="tbl_sel'+i+'" style="border-bottom-style: hidden;"><tr><th colspan=9>DSM2 Output Comparison - RMSE Statistics (<a href="#" onClick="initialize(this)"> View Map </a>)<br>This is calculated from the original time series in dss file based on its output time interval.'
           tbl_head+='<br><img src="js/up.png" align=middle> : %s is higher than %s; <img src="js/down.png" align=middle> : %s is lower than %s';
    """%(display_name[0],display_name[1],display_name[2],display_name[3])
        print >> fh,"""
       legend='Percentage RMS Diff<br>';    
       legend+='<img src="js/icon16.png" width="33%">: > 100% <br><img src="js/icon16.png" width="27%">: 80% - 100% <br><img src="js/icon16.png" width="23%">: 60% - 80% <br><img src="js/icon16.png" width="19%">: 40% - 60% <br><img src="js/icon16.png" width="16%">: 20% - 40% <br><img src="js/icon16.png" width="11%">: 10% - 20% <br><img src="js/icon16.png" width="7%">: 0% - 10% <br>';
       legend+='<img src="js/icon49.png" width="7%">: 0% - -10% <br><img src="js/icon49.png" width="11%">: -10% - -20% <br> <img src="js/icon49.png" width="16%">: -20% - -40% <br> <img src="js/icon49.png" width="19%">: -40% - -60% <br><img src="js/icon49.png" width="23%">: -60% - -80% <br><img src="js/icon49.png" width="27%">: -80% - -100% <br><img src="js/icon49.png" width="33%">: < -100% <br>';
       tbl_head+='<br><center><table class="list"><tr><td><div id="map_canvas'+tab_name+'" style="width: 800px; height: 800px;display:none"></div></td><td valign=top><div id="map_'+tab_name+'" style="display:none">'+legend+'</div></td></tr></table></center></th></tr></table>'; k1=0;
       $("#"+dt_arr[i]+"_p").append(tbl_head);
       // write the table
       num_stat=get_obj_size(data_list[0].diff[0]);
       """
        if(globals['COMPARE_MODE']=='5'):
            print >> fh, "multiplier = 2;"
        else:
            print >> fh, "multiplier = 1;"
        print >> fh,"""
       for(z=0;z<num_stat;z++){
         if(z==0) tbl_sel[z]='<div id="block_'+i+'_'+z+'" style="display:\\'\\'">';
         else tbl_sel[z]='<div id="block_'+i+'_'+z+'" style="display:none">';
         tbl_sel[z]+='<table class="alt-highlight" id="tbl_sel'+i+z+'" style="border-top-style: hidden;"><tr><td width=180></td>';
         if(z==0) tbl_sel[z]+='<td colspan='+(period_name.length+5)*multiplier+' class="timewindow">Percentage Root Mean Square Difference</td></tr>';
         if(z==1) tbl_sel[z]+='<td colspan='+(period_name.length+5)*multiplier+' class="timewindow">Root Mean Square Difference</td></tr>';
         tbl_sel[z]+='<tr><td></td>';
         for(k=0;k<period_name.length;k++) tbl_sel[z]+='<td class="timewindow" colspan='+multiplier+'>'+period_name[k]+'</td>';
         tbl_sel[z]+='<td class="timewindow" colspan='+5*multiplier+'>Water Year Type</td></tr><tr><td></td>';
         for(k=0;k<period_name.length;k++) tbl_sel[z]+='<td class="timewindow" colspan='+multiplier+'>'+period_range[k]+'</td>';
         for(k=0;k<5;k++) tbl_sel[z]+='<td class="timewindow" colspan='+multiplier+'>'+wyt_txt[k]+'</td>';
         tbl_sel[z]+='</tr>';
       """
        if(globals['COMPARE_MODE']=='5'):
            print >> fh, """
         tbl_sel[z]+='<tr><td></td>';
         for(k=0;k<period_name.length;k++) tbl_sel[z]+='<td class="timewindow" title="%s vs %s">M1</td><td class="timewindow" title="%s vs %s">M2</td>';
         for(k=0;k<5;k++) tbl_sel[z]+='<td class="timewindow" title="%s vs %s">M1</td><td class="timewindow" title="%s vs %s">M2</td>';
         tbl_sel[z]+='</tr>';
        """%(scalars['NAME0'],scalars['NAME1'],scalars['NAME0'],scalars['NAME2'],scalars['NAME0'],scalars['NAME1'],scalars['NAME0'],scalars['NAME2'])
        print >> fh, """
         tbl_unsel[z]='<div id="all_perc'+i+z+'" style="display:none"><table class="alt-highlight" id="tbl_unsel'+i+z+'"><tr><td colspan='+8*multiplier+'></td></tr>';k2=0;
         for(j=0;j<ns;j++){
          if(data_list[j].data_type==dt_arr[i] && data_list[j].checked=='1'){ k1++;
            tbl_sel[z]+='<tr class="d'+k1%2+'"><td><a href="#fig_'+ data_list[j].output+'" title="'+data_list[j].refvar+'">'+data_list[j].output+'</a></td>'
            ndiff = data_list[j].diff.length;
            if (getInternetExplorerVersion() > -1) ndiff = ndiff-1;
            for(k=0; k<ndiff; k++){           
               if(z==0) va=data_list[j].diff[k].perc_rmse;
               if(z==1) va=data_list[j].diff[k].rmse;
               tbl_sel[z]+='</td><td>'+Math.abs(va);
               if(z==0) tbl_sel[z]+='%';
               if (va<0) tbl_sel[z]+='<img src="js/down.png"></td>';
               if (va>0) tbl_sel[z]+='<img src="js/up.png"></td>';
               if (va==0) tbl_sel[z]+='</td>';
               if(data_list[j].diff[k].perc_rmse2!=undefined){
                   if(z==0) va=data_list[j].diff[k].perc_rmse2;
                   if(z==1) va=data_list[j].diff[k].rmse2;
                   tbl_sel[z]+='</td><td>'+Math.abs(va);
                   if(z==0) tbl_sel[z]+='%';
                   if (va<0) tbl_sel[z]+='<img src="js/down.png"></td>';
                   if (va>0) tbl_sel[z]+='<img src="js/up.png"></td>';
                   if (va==0) tbl_sel[z]+='</td>';               
               }
            }
            tbl_sel[z]+='</tr>';
          }
          if(data_list[j].data_type==dt_arr[i] && data_list[j].checked=='0'){ k2++;
            tbl_unsel[z]+='<tr class="d'+k2%2+'"><td width=180>'+data_list[j].output+'</td>';
            ndiff = data_list[j].diff.length;
            if (getInternetExplorerVersion() > -1) ndiff = ndiff-1;
            for(k=0; k<ndiff; k++){
               if(z==0) va=data_list[j].diff[k].perc_rmse;               
               if(z==1) va=data_list[j].diff[k].rmse;               
               tbl_unsel[z]+='</td><td>'+Math.abs(va);
               if(z==0) tbl_unsel[z]+='%';
               if (va<0) tbl_unsel[z]+='<img src="js/down.png"></td>';
               if (va>0) tbl_unsel[z]+='<img src="js/up.png"></td>';
               if (va==0) tbl_unsel[z]+='</td>';
               if(data_list[j].diff[k].perc_rmse2!=undefined){
                  if(z==0) va=data_list[j].diff[k].perc_rmse2;               
                  if(z==1) va=data_list[j].diff[k].rmse2;               
                  tbl_unsel[z]+='</td><td>'+Math.abs(va);
                  if(z==0) tbl_unsel[z]+='%';
                  if (va<0) tbl_unsel[z]+='<img src="js/down.png"></td>';
                  if (va>0) tbl_unsel[z]+='<img src="js/up.png"></td>';
                  if (va==0) tbl_unsel[z]+='</td>';           
               }
            }
            tbl_unsel[z]+='</tr>';             
          }
         }
         tbl_sel[z]+='</table>';      
         tbl_sel[z]+='<img id="img" src="js/open.JPG" onClick=hideDiv("all_perc'+i+z+'")> Open all stations<br>';
         tbl_unsel[z]+='</table></div></div>';           
         $("#"+dt_arr[i]+"_p").append(tbl_sel[z]);
         $("#"+dt_arr[i]+"_p").append(tbl_unsel[z]); 
       }
"""
    else:
        print >>fh,""" 
        tbl_head='<table class="alt-highlight" id="tbl_sel'+i+'" style="border-bottom-style: hidden;"><br>';
        $("#"+dt_arr[i]+"_p").append(tbl_head);
        k1=0;k2=0;
         tbl_sel='<div id="block_'+i+'" style="display:\\'\\'">';
         tbl_sel+='<table class="alt-highlight" id="tbl_sel'+i+'" style="border-top-style: hidden;"><tr><td colspan=5 class="timewindow">Selected Output Locations</td></tr>';
         tbl_unsel='<div id="all_perc'+i+'" style="display:none"><table class="alt-highlight" id="tbl_unsel'+i+'"><tr><td colspan=5></td></tr>';k2=0;
         for(j=0;j<ns;j++){
          if(data_list[j].data_type==dt_arr[i] && data_list[j].checked=='1'){ k1++;
            if(k1%5==1) tbl_sel+='<tr class="d'+k1%2+'">';
            tbl_sel+='<td width=180><a href="#fig_'+ data_list[j].output+'" title="'+data_list[j].refvar+'">'+data_list[j].output+'</a></td>'
            if(k1%5==0) tbl_sel+='</tr>';
          }
          if(data_list[j].data_type==dt_arr[i] && data_list[j].checked=='0'){ k2++;
            if(k2%5==1) tbl_unsel+='<tr class="d'+k2%2+'">';
            tbl_unsel+='<td width=180>'+data_list[j].output+'</td>';
            if(k2%5==0) tbl_unsel+='</tr>';             
          }
         }
         tbl_sel+='</table>';      
         tbl_sel+='<img id="img" src="js/open.JPG" onClick=hideDiv("all_perc'+i+'")> Open all stations<br>';
         tbl_unsel+='</table></div></div>';           
         $("#"+dt_arr[i]+"_p").append(tbl_sel);
         $("#"+dt_arr[i]+"_p").append(tbl_unsel); 
       """
    print >> fh,""" 
    };    
    $('#system-water-balance-table tr').click(function(){
        var_name = $($(this).find('td')[0]).text();
        var svg_element = $('div').filter(function(){ return $(this).text().indexOf(var_name)>=0;})
        if (svg_element && svg_element.length > 0){
            anchor_name = $($(svg_element[0]).parent()).attr("href");
            window.location.href=window.location.href.split('#')[0]+anchor_name;
        }
    });
</script> 
<script type="text/javascript"> 
    $('#time-window-select').change(function(){
        var changed_val = $('#time-window-select option:selected').val().split("-");
        $('#SDate').val(to_date_str(changed_val[0]));
        $('#EDate').val(to_date_str(changed_val[1]));
        clear_and_draw(extract_date(changed_val[0]),extract_date(changed_val[1]));
    });
    $('#threshold').change(function(){
        set_diff_threshold($('#threshold').val());
    });
    $('input[name=diff_plot]').change(function(){
        sdate = extract_date(to_date_comma($("#SDate").val()));
        edate = extract_date(to_date_comma($("#EDate").val()));
        clear_and_draw(sdate,edate);
    });
    $('input[name=wyt]').change(function(){
        clear_and_draw(extract_date(to_date_comma($('#SDate').val())),extract_date(to_date_comma($('#EDate').val())));        
    });
    $('#calendar').click(function(){
        clear_and_draw(extract_date(to_date_comma($('#SDate').val())),extract_date(to_date_comma($('#EDate').val())));
    });
    $('#stat').change(function(){
      a=$('#stat').val();
      tab_name = document.getElementById('ta').value;
      i = dt_arr.indexOf(tab_name);
      for(z=0;z<get_obj_size(data_list[0].diff[0]);z++){   
        if(z==a) $("#block_"+i+"_"+z).show();
        else $("#block_"+i+"_"+z).hide();
      }
    });
    function set_diff_threshold(threshold){
        $('td').each(function(){ 
            if ($(this).text().search('%')>=0){
                if(Math.abs(parseFloat($(this).text().split('%')[0])) >= threshold){
                    $(this).addClass('large-diff');
                }else{
                    $(this).removeClass('large-diff');
                } 
            }
        });
    }
    set_diff_threshold($('#threshold').val());
</script> 
<script>
if (!document.layers) document.write('<div id="divStayTopLeft" style="position:absolute">')
</script>
<layer id="divStayTopLeft">
<table border="0" width="30" cellspacing="0" cellpadding="0"><tr><td width="100%" bgcolor="#FFFFCC">
    <a href="#top" onmouseover="roll_over('but1', 'js/home_over.jpg')" onmouseout="roll_over('but1', 'js/home.jpg')">
    <img src="js/home.jpg" NAME="but1" BORDER="0"></a></td></tr>
</table>
</layer>
<script type="text/javascript" src="js/floating.js"></script>
</body> 
</html>"""
def fireup(html):
    if os.path.exists("C:/Program Files/Google/Chrome/Application/chrome.exe"):
        os.system('cmd /c start chrome "'+html+'"')        
    elif os.path.exists("C:/Program Files/Mozilla Firefox/firefox.exe"):
        os.system('cmd /c start firefox "'+html+'"')
    else:
        print "Please install Chrome and Firefox for best performance."
        os.system('cmd /c start "'+html+'"')

if __name__ == '__main__':
    LOG_FILENAME = 'example.log'
    logging.basicConfig(filename=LOG_FILENAME,level=logging.DEBUG)
    if len(sys.argv) != 2:
        print " **** Please specify the report inp file! ****"
        #template_file = 'D:/delta/dsm2_v8/report/case2/dsm2_output.inp'
        exit(1)
    else:
        template_file = sys.argv[1]
    outdir, outfile =get_output_html(template_file)
    copy_basic_files(outdir)
    logging.debug('Parsing input template file %s'%template_file)
    from time import strftime
    print "Starting at: ",strftime("%a, %d %b %Y %H:%M:%S")
    #parse template file
    globals, scalars, var_values, output_values, tw_values = parse_template_file(template_file)
    # do processing
    do_processing(globals, scalars, var_values, output_values, tw_values)
    logging.debug('Done processing. The end!')
    print "End at: ",strftime("%a, %d %b %Y %H:%M:%S")
    sys.exit(0)

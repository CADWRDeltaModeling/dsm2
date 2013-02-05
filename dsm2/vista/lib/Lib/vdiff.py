import vutils
from vutils import *
from math import sqrt
import xyz
from xyz import *
import logging
    
def ref2ds(ref,tw=None,time_interval=None):
    logging.basicConfig(level=logging.DEBUG)
    if isinstance(ref, DataReference):
        ds = ref.getData()
    else:
        ds = ref
    if tw!=None:
        ds = ds.createSlice(tw)
        #if ds==None:
        #    logging.debug("No data within the time window specified for "+str(ref))
    if not isinstance(ds,RegularTimeSeries) and ds!=None:
        logging.debug(ref + " is not a regular time-series data set and has been converted to a regular time series.")
        ds = its2rts(ds,tw,time_interval)
    return ds     
    
def amplitude_avg(ds):
    '''get the average amplitude for time series ds'''
    sump = 0
    sumn = 0
    lenp = 0
    lenn = 0
    avg_amplitude = 0 
    ds = ds.YArray
    for x in ds:
        if x <> -901.0:
            if x >=0: 
                sump = sump + x
                lenp = lenp + 1
            else: 
                sumn = sumn + x
                lenn = lenn + 1
    if lenp > 0: 
        avg_amplitude = sump/lenp
    if lenn > 0:
        avg_amplitude = avg_amplitude + abs(sumn/lenn)
    return avg_amplitude

def amplitude_maxmin(ds):
    '''get the amplitude by max-min for time series ds'''
    max_amp = -99999999
    min_amp = 99999999
    ds = ds.YArray
    for x in ds:
        if x!=-901.0 and x < min_amp:
            min_amp = x
        if x!=-901.0 and x > max_amp:
            max_amp = x
    return max_amp - min_amp

def rmse(ref1,ref2,tw=None,time_interval=None):
    ''' return the root mean square error from two references '''
    if chk_two_ds(ref1,ref2,tw,time_interval=None)==True:
        sumsqrt, n = segm_sqrt(ref1,ref2,tw,time_interval)
        if n > 0:
            root_mse = get_sign(sumsqrt) * sqrt(abs(sumsqrt)/n)
        else:
            root_mse = 0
    else:
        root_mse = 0
        logging.debug(" No data within time window "+str(tw)+" for "+str(ref1)+" and "+str(ref2))
    return root_mse 

def perc_rmse(ref1,ref2,tw=None,time_interval=None,amplitude=amplitude_maxmin):
    ''' return the root mean square error of percentage difference from two references '''    
    ds1 = ref2ds(ref1)
    ds1amp = amplitude(ds1)   
    tw_new=timewindow(str(tw))
    sumsqrt, n = segm_sqrt(ref1,ref2,tw_new,time_interval)
    if ds1amp > 0 and n > 0:
        root_mse = get_sign(sumsqrt) * sqrt(abs(sumsqrt)/n)/ds1amp*100
    else:
        root_mse = 0
    return root_mse
    
def rmse_discrete_tws(ref1,ref2,tw_arr,percentage,amplitude=amplitude_maxmin):
    '''return RMSE for specified discrete time windows
       set percentage = 0 to obtain percentage RMS Diff; otherwise, returns RMS Diff
    '''
    total_sumsqrt = 0
    total_sign = 0
    total_n = 0
    for timew in tw_arr: 
        tw = timewindow(timew)
        if chk_two_ds(ref1,ref2,tw)==True:
            ref1p = DataReference.create(ref1,tw)
            ref2p = DataReference.create(ref2,tw)
            sumsqrt, n = segm_sqrt(ref1p,ref2p)
            total_n+= n
            if percentage==0:
                ds1p = ref2ds(ref1p)
                ds1pamp = amplitude(ds1p)
                if ds1pamp > 0:
                    total_sumsqrt += abs(sumsqrt)/(ds1pamp*ds1pamp)*10000
                    total_sign += sumsqrt/(ds1pamp*ds1pamp)
            else:     
                total_sumsqrt+= abs(sumsqrt)
                total_sign += sumsqrt
    if total_n>0:
        rmse = get_sign(total_sign) * sqrt(total_sumsqrt/total_n)
    else:
        rmse = 0
    return rmse

def chk_two_ds(ref1,ref2,tw,time_interval=None):
    ds1 = ref2ds(ref1,tw,time_interval)
    ds2 = ref2ds(ref2,tw,time_interval)
    if ds1!=None and ds2!=None:
        return True
    else:
        return False
    
def segm_sqrt(ref1,ref2,tw=None,time_interval=None):
    ds1 = ref2ds(ref1,tw,time_interval)
    ds2 = ref2ds(ref2,tw,time_interval)
    try:
        ds3 = ds2 - ds1
        ds4 = ds3 * ds3
        sign = get_sign(Stats.total(ds3))
        sum_mse = sign * Stats.total(ds4)
        return sum_mse, len(ds3)
    except:
        #logging.debug("There is no matched period for "+str(ds1)+" and "+str(ds2))
        return 0, 0
    
def get_sign(num):
    if num < 0: 
        return -1
    else:
        return 1

def daily_rmse(ref1,ref2):
    ''' return the daily root mean square error from two references '''    
    ref1 = godin(ref1)
    ref1 = per_avg(ref1,'1day')
    ref2 = godin(ref2)
    ref2 = per_avg(ref2,'1day')    
    ds1 = ref2ds(ref1)
    ds2 = ref2ds(ref2)       
    ds3 = ds2 - ds1
    ds4 = ds3 * ds3 /len(ds3)
    sum_mse = Stats.total(ds4)
    root_mse = sqrt(sum_mse)    
    return root_mse

def daily_perc_rmse(ref1,ref2,amplitude=amplitude_maxmin):
    ''' return the daily root mean square error of percentage difference from two references '''    
    ref1 = godin(ref1)
    ref1 = per_avg(ref1,'1day')
    ref2 = godin(ref2)
    ref2 = per_avg(ref2,'1day')
    ds1 = ref2ds(ref1)
    ds2 = ref2ds(ref2)
    ds1amp = amplitude(ds1)         
    ds3 = ds2 - ds1
    ds4 = ds3 * ds3 /len(ds3)
    sum_mse = Stats.total(ds4)
    if sum_mse > 0:
        root_mse = sqrt(sum_mse)/ds1amp*100
    else:
        root_mse = 0
    return root_mse
      
def dss_ts_diff_metric(dss_file1, dss_file2, difference_metric=rmse, tw_string=None \
                       ,c_part=None, save_dss=None, diff_dssfile=None):
    '''
    dss_ts_diff_metric(dss_file1,dss_file2,difference_metric=rmse,tw_string=None)
    - This function computes the difference between two DSS file (dss_file2 - dss_file1)
    - Arguments:
      dss_file1         first DSS file name 
      dss_file2         second DSS file name
      difference_metric method used to compute the measure of difference between two time series
                        rmse: root mean square error
      tw_string         time window
                        If not specified, the default window is obtained from dss_file1.
      c_part            filter for PART C in the dss file (e.g.'FLOW','STAGE','EC')
                        If not specified, it returns all available time series
      save_dss          If you plan to save the difference to a DSS file, put 'y'; otherwise, leave it blank from this point on.
      diff_dssfile      If you put 'y' for save_dss, specify the output file name here.  
    - Return value:
      An associated array that contains information of the difference measure 
      and its corresponding pathname
   '''  
    g1 = opendss(dss_file1)
    g2 = opendss(dss_file2)
    metric = {}
    for ref in g1:
        if tw_string == None: 
            tw = ref.timeWindow
        else:
            tw = timewindow(tw_string)
        path = ref.getPathname()
        ''' find out the matched time series in dssFile2''' 
        if c_part == None:
            #g3 = findparts(g2,a=a[1],b=a[2],c=a[3])
            g3 = findparts(g2,b=path.getPart(Pathname.B_PART),c=path.getPart(Pathname.C_PART),e=path.getPart(Pathname.E_PART),f=path.getPart(Pathname.F_PART))
        else:
            #g3 = findparts(g2,a=a[1],b=a[2],c=c_part)
            g3 = findparts(g2,b=path.getPart(Pathname.B_PART),c=c_part,e=path.getPart(Pathname.E_PART),f=path.getPart(Pathname.F_PART))            
        if g3 is not None and (path.getPart(Pathname.C_PART) == c_part or c_part == None) and path.getPart(Pathname.E_PART)!='IR-YEAR':                      
            ref1 = DataReference.create(ref,tw)
            ref2 = DataReference.create(g3[0],tw)
            if not isinstance(ref1.getData(),RegularTimeSeries):
                break
                ref1 = ref2ds(ref1,tw_string,'15min')
                writedss(dss_file1,'/PLANNING+GATE/CONVERTED TimeSeries/OP-FROM-NODE//IR-YEAR/RegularTS/',ref1)
            if not isinstance(ref2.getData(),RegularTimeSeries):
                break
                ref2 = ref2ds(ref2,tw_string,'15min')
                writedss(dss_file2,'/PLANNING+GATE/CONVERTED TimeSeries/OP-FROM-NODE//IR-YEAR/RegularTS/',ref2)
            if ref1 is None:
                print 'Error: The time window ' + tw_string + ' is out of range for ' + str(ref.pathname) +'! Please specify the correct time window.'
                break
            elif ref2 is None:
                print 'Error: The time window ' + tw_string + ' is out of range for ' + str(g3[0].pathname) + '! Please specify the correct time window.'
                break                
            else:
                    ref3 = ref2 - ref1
            try:    
                rootmse = difference_metric(ref1,ref2)
                metric[str(path)] = rootmse
            except:
                print ref1," is irregular time series."
            if save_dss == 'y':
                if diff_dssfile is not None:
                    try:
                        writedss(diff_dssfile,outPath,ref3.getData())
                    except:
                        print 'Make sure the file path ' + diff_dssfile + ' is correct.'
                        break
                else:
                    print 'To save the difference to a DSS file, you need to specify the output file name.'
                    break
    return metric      

def save_metric2txt(metric,output_file):
    f = open(output_file, 'w')
    f.write('ID,Name,Val\n')
    for mpath,val in metric.iteritems():
        pname = get_name_from_path(mpath)
        f.write(mpath + ',' + pname + ',' + str(val) +'\n')
    f.close()
    return None

def get_metric_xy(metric,hydro_echo_file,gis_inp_file,output_file):
    '''
    get_metric_xy(metric,hydro_echo_file,gis_inp_file,output_file)
    - This function is used to retrieve the lat/lng information for the metric computed 
      from dss_ts_diff_metric.
    - Arguments:
      metric           metric obtained from dss_ts_diff_metric()
      hydro_echo_file  hydro echo file from DSM input 
      gis_inp_file     gis input file downloaded from DSM2 Grid Map interface
      output_file      output file path and name
    - Return:
      A text file and the channels that cannot retrieve a location
    - Usage Example:
      get_metric_xy(ms,'D:\DSM2-SensTest\hydro_echo.inp','D:\DSM2-SensTest\gis.inp','D:\DSM2-SensTest\metric_xy.txt')
    '''  
    dsm2model = GetDsm2Model(hydro_echo_file,gis_inp_file)
    f = open(output_file, 'w')
    f.write('ID,Name,Longitude,Latitude,Val\n')
    logstr = 'Stations failed to locate: '
    chkarr = []
    for mpath,val in metric.iteritems():
        pname = get_name_from_path(mpath)
        try:
            if if_id_not_exist(dsm2model.get_id_by_name(pname.lower()),chkarr):
                a = dsm2model.get_xy_by_name(pname.lower())
                f.write(a['channel_id'] + ',' + a['channel_name'].upper() + ',' + str(a['longitude'])+ ',' + str(a['latitude']) + ',' + str(val) +'\n')
                chkarr.append(dsm2model.get_id_by_name(pname.lower()))
        except:
            logstr += pname + ','
    f.close()
    print logstr
    return chkarr

def if_id_not_exist(id,exist_arr):
    for k in exist_arr:
        if id == k:
            return False
    return True
    
def get_name_from_path(pathname):
    pathname = pathname
    a = pathname.split('/')
    return a[2] 



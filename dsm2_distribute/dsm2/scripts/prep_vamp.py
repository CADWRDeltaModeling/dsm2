"""Script to prepare San Joaquin flow and EC, SWP pumping and CVP 
    pumping given a CALSIM output file.
    
"""
from calendar import monthrange,month_abbr
import sys
import config
from calsim_study_fpart import calsim_study_fpart
from vista.set import RegularTimeSeries,DataSetAttr,DataType,Constants
from vtimeseries import timeinterval,interpolate
from vdss import opendss,findpath,writedss
from vmath import tsmax,tsmin
from config import getAttr,setConfigVars
from jarray import zeros,array
CVP_MAX_PUMP=4600.
CVP_MIN_PUMP=800.
SWP_MAX_PUMP=8500.
DEBUG=0

monthlist=[m.upper() for m in month_abbr]
filter=Constants.DEFAULT_FLAG_FILTER
NA_VAL=-901,0

def dss_retrieve_ts(file, path):
    f=opendss(file)
    g=findpath(f,path)
    if g==None or len(g) != 1:
        raise ValueError("Path %s in file s% does not exist or is not unique" % (path,file))
    return g[0].getData()

def dss_store_ts(file,path,ts):
    writedss(file,path,ts)

    
    
def replace_vamp(non_pulse,pulse,include_shoulder=0):
    """Creates a new series based on non_pulse values, replacing them
        with pulse values during the VAMP season (April 15-May 15 inclusive)
        or April1 - May 31 if include_shoulder is set to true (1)

      Arguments:
      non_pulse: monthly or daily series of values to use for non-pulse
      pulse: pulse values, must have same start, length, interval as non-pulse
      
      Output: New series with non-pulse values replaced by pulse during vamp
    """
    if not isinstance(non_pulse,RegularTimeSeries):
        raise TypeError("Non-Pulse flow must be regular time series")
    if not isinstance(pulse,RegularTimeSeries):
        raise TypeError("Pulse flow must be regular time series")
        
    if include_shoulder:
        first_april_day=1
        last_may_day=31
    else:
        first_april_day=15
        last_may_day=15
    #if non_pulse.getTimeInterval() == timeinterval("1MON"):
    non_pulse=interpolate(non_pulse,"1DAY")
    #if pulse.getTimeInterval() == timeinterval("1MON"):
    pulse=interpolate(pulse,"1DAY")
    if non_pulse.getStartTime() != pulse.getStartTime() or \
        len(non_pulse) != len(pulse):
        raise ValueError("Pulse and Non-pulse must have the same start time,"
                       " interval and length")
    values=zeros(len(non_pulse),'d')
    for np,p,i in zip(non_pulse,pulse,range(len(values))):
        values[i]=np.getY()
        xstr=np.getXString()
        day,month=int(xstr[0:2]),xstr[2:5]
        if month=="APR" and day >= first_april_day: 
            values[i]=p.getY()
        elif month=="MAY" and day <= last_may_day: 
             values[i]=p.getY()

    out=RegularTimeSeries("/vamp//////", non_pulse.getStartTime().toString(),
                                non_pulse.getTimeInterval().toString(),values)

    out.getAttributes().setYUnits("CFS")
    out.getAttributes().setYType("PER-AVER")
    return out
   
def prep_vamp_vernalis(calsimfile,outfile,fpart,fpart_mod):
    """Driver routine for construct Vernalis SJR flow and EC. 
        This routine extracts flow and ec from the calsim file, 
        calls replace_vamp(non_pulse,pulse) and writes
        output.
    """
    non_pulse_path="/CALSIM/C639CYCLE2/FLOW-CYCLE2//1MON/fpart/".replace("fpart",fpart)
    pulse_path="/CALSIM/C639CYCLE5/FLOW-CYCLE5//1MON/fpart/".replace("fpart",fpart)
    outpath="/CALSIM-VAMP/C639/FLOW//1DAY/fpart/".replace("fpart",fpart_mod)

    non_pulse_flow=dss_retrieve_ts(calsimfile, non_pulse_path)
    pulse_flow=dss_retrieve_ts(calsimfile, pulse_path)
    vernalis_flow=replace_vamp(non_pulse_flow,pulse_flow)
    dss_store_ts(outfile,outpath, vernalis_flow)

    non_pulse_path="/CALSIM/VERNWQNONPULSEDV/SALINITY-EC//1MON/fpart/".replace("fpart",fpart)
    pulse_path="/CALSIM/VERNWQPULSEDV/SALINITY-EC//1MON/fpart/".replace("fpart",fpart)
    outpath="/CALSIM-VAMP/VERNWQ/EC//1DAY/fpart/".replace("fpart",fpart_mod)
   
    non_pulse_ec=dss_retrieve_ts(calsimfile, non_pulse_path)
    pulse_ec=dss_retrieve_ts(calsimfile, pulse_path)
    vernalis_ec=replace_vamp(non_pulse_ec,pulse_ec)
    dss_store_ts(outfile,outpath, vernalis_ec)


def project_export_limits(pulse_limit, ei_ratio,delta_inflow):
    """Refine export limits to include EI ratio and allocate
    limits to CVP and SWP.
   
      Arguments:
      pulse_limit: the raw combined export limit from CALSIM
      ei_ratio:     the maximum E/I ratio calculated by CALSIM
      delta_inflow: total inflow to the delta calculated by CALSIM
      
      Output:
      swp_limit,cvp_limit: Maximum pumping allowed during VAMP for
                           each of the individual projects. This routine
                           calculates maximum pumping, not actual pumping
    """
    if ei_ratio.getStartTime() != pulse_limit.getStartTime():
        raise ValueError("EI limit and total export limit must have same start time")

    # Limit pulse according to EI ratio
    eilimit=ei_ratio*delta_inflow
    
    # Now apply export limit. in the CALSIM file the limit probably
    # will have values only for APR and MAY, whereas the ei limit 
    # exists every month
    tsmonth=month_numbers(pulse_limit)
    is_april_may=(tsmonth==4)+(tsmonth==5)
    limit=ts_where(is_april_may *(pulse_limit < eilimit) > 0.,
                              pulse_limit,
                              eilimit)
    
    writedss("out","/CALC/LIM/////",limit) 

    # Try to allocate to cvp and swp equally. CVP has a 
    # mimimum pumping level of 800cfs in which case SWP takes the rest
    even_allocation=limit/2.
    cvp_min_pump=even_allocation*0. + CVP_MIN_PUMP # converts cvp min to time series with same start, interval
    cvp_limit=ts_where(even_allocation > cvp_min_pump,
                                   even_allocation,
                                   cvp_min_pump)
    swp_limit=limit-cvp_limit
    if DEBUG:
        writedss("out","/CALC/EVENALLOC/////",even_allocation)   
        writedss("out","/CALC/CVPLIM/////",cvp_limit)
        writedss("out","/CALC/SWPLIM/////",swp_limit)
        writedss("out","/CALC/PULSELIM/////",pulse_limit)
        writedss("out","/CALC/EILIM/////",eilimit)
    return swp_limit,cvp_limit

def month_numbers(series):
    n=len(series)
    values=zeros(n,'d')
    for el,i in zip(series,range(n)):
        t=el.getXString()
        values[i]=monthlist.index(t[2:5])
    out=RegularTimeSeries("/MONTH_NUMBER//////",
                                          series.getStartTime().toString(),
                                          series.getTimeInterval().toString(),
                                          values)
    out.getAttributes().setYType(series.getAttributes().getYType())
    return out

    
def days_in_month(series):
    n=len(series)
    values=zeros(n,'d')
    for el,i in zip(series,range(n)):
        t=el.getXString()
        m=monthlist.index(t[2:5])
        y=int(t[5:9])
        values[i]=monthrange(y,m)[1]
    out=RegularTimeSeries("/DAY_IN_MONTH//////",
                                          series.getStartTime().toString(),
                                          series.getTimeInterval().toString(),
                                          values)
    out.getAttributes().setYType(series.getAttributes().getYType())
    return out
   
   
def calculate_vamp_times(series):
    """Calculate vamp pulse period lengths for each month in series
    Arguments:
    series: series that provides the months
    
    Output: (total, pulse, nonpulse)
    total: total days in month
    pulse: days in month in pulse period
    nonpulse: days in month not in pulse period
    """
    total=days_in_month(series)
    n=len(series)
    first_month=monthlist.index(series.getStartTime().toString()[2:5])
    
    # Now switch to entirely zero-based arrays for remainder of routine
    first_month_ndx_yr1=first_month - 1   #zero based index of first month
    
    # This array has the number of pulse days per month for a single 
    # year starting in January. We are going to repeat it over and over
    # with a subarray at the beginning and end because the input doesn't
    # necessarily start in January and end in December.
    single_year_pulse_days=[0,0,0,16,15,0,0,0,0,0,0,0]
    
    if first_month_ndx_yr1 + n > 12:
       months_year1=12-first_month_ndx_yr1
       last_month_ndx_yr1=11
       repeat_months=(n-months_year1)/12
       extra_months=(n-months_year1)%12
    else:
       months_year1=n
       
       last_month_ndx_yr1=first_month_ndx_yr1+n-1
       
       repeat_months=0
       extra_months=0
    
    pulse_days=array(
               single_year_pulse_days[first_month_ndx_yr1:(last_month_ndx_yr1+1)] + 
               repeat_months*single_year_pulse_days +
               single_year_pulse_days[0:extra_months], 'd')
    pulse=RegularTimeSeries("/pulse//////",series.getStartTime().toString(),
                                             series.getTimeInterval().toString(),pulse_days)
    nonpulse=total - pulse
    return total,pulse,nonpulse

def ts_where(criteria,trueval,falseval):
    if not( len(criteria) == len(trueval) and \
            len(criteria) == len(falseval) ):
        raise ValueError("Criteria (len %s), trueval (len %s) and falseval (len %s)"
                      " must be the same length" % (len(criteria),len(trueval),len(falseval)))
    n=len(criteria)
    values=zeros(n,'d')
    for c,t,f,i in zip(criteria,trueval,falseval,range(n)):
        if filter.isAcceptable(c):
             if c.getY():
                 values[i]=t.getY()
             else:
                values[i]=f.getY()
        else:
             values[i]=NA_VAL
    out=RegularTimeSeries("/WHERE//////",criteria.getStartTime().toString(),
                                         criteria.getTimeInterval().toString(),values)
    out.getAttributes().setYType(trueval.getAttributes().getYType())
    return out

def get_calsim_path(sjr_process, fpart):
    process = sjr_process.upper()
    if (process!="SINGLE_STEP") and (process!="MULTI_STEP"):
    	raise ValueError("For preparing VAMP process, SJR_PROCESS has to be COMMON_ASSUMPTIONS, SINGLE_STEP or MULTI_STEP")
    if process=="MULTI_STEP":
        return "/CALSIM/PULSEVAMPEXP/EXPORT//1MON/fpart/".replace("fpart",fpart)
    else:
        return "/CALSIM/PULSEEXPCTRL/EXPORT-CTRL-PULSE//1MON/fpart/".replace("fpart",fpart)
   
def calculate_exports(limit,average_value):
    """Determines pulse and non-pulse export flows
      for cvp or swp given the refined limits on the
      export and the average for one of the projects.
      This routine assures that the limit is
      only used if it will reduce pumping 
      and that the pulse and non-pulse flows combine to 
      give the correct total monthly average pumping.
      
      Arguments:
      limit: time series of (refined) limits 
                     on exports for the project
                     (cvp or swp) being analyzed.
      average_value: time series of monthly average pumping 
                     for the project
      
      Output:
      export_value:  Time series of actual exports.
      
    """
    total_time_in_month,pulse_time_in_month,non_pulse_time_in_month = \
      calculate_vamp_times(limit)

    # Calculate a volumetrically correct non-pulse flow given that average_value gives 
    # the total volume of pumping for the month and that pulse pumping is at the limit
    limit_volume=limit*pulse_time_in_month
    total_volume = average_value*total_time_in_month
    non_pulse_volume = total_volume - limit_volume
    non_pulse_flow=non_pulse_volume/non_pulse_time_in_month
    volume_corrected_limit = replace_vamp(limit, # replace shoulder first
                                                     non_pulse_flow,  
                                                     include_shoulder=1)  
    volume_corrected_limit=replace_vamp(volume_corrected_limit, #now correct pulse period
                                                              limit,
                                                              include_shoulder=0)
    
   
    # Create an indicator time series to show months where the  pulse pumping 
    # limit is greater than average (the  pulse pumping limit should be a curtailment).
    # Note that this is converted to daily by "spreading it out" over the days, but it
    # is an indicator of a monthly condition and every day has the same value.
    limit_exceeds_average = limit > average_value
    limit_exceeds_average=interpolate(limit_exceeds_average,'1DAY')
    average_value=interpolate(average_value,'1DAY')
    # Now use average_value for months where the pulse limit would lead to an increase
    # and the volume-corrected pulse/non-pulse combination otherwise.
    export_value = ts_where(limit_exceeds_average,average_value, volume_corrected_limit)
    if (DEBUG):
        writedss("out.dss","/EXP/CVP/EXPORT////",export_value)        
        writedss("out.dss","/EXP/CVP/VCL////",volume_corrected_limit)
        writedss("out.dss","/EXP/CVP/NONPULSE////",interpolate(non_pulse_flow,'1DAY'))    
        writedss("out.dss","/EXP/CVP/AVE////",average_value)
        writedss("out.dss","/EXP/CVP/LIM////",interpolate(limit,'1DAY'))
        writedss("out.dss","/EXP/CVP/LIM_EXCEED_AVE////",limit_exceeds_average)
	
    export_value.getAttributes().setYUnits("CFS")
    export_value.getAttributes().setYType("PER-AVER")
    
    return export_value

   
   
def prep_vamp_exports(calsimfile,outfile,fpart,fpart_mod,sjr_process):
    """Driver routine to construct SWP and CVP export flows.
      This routine extracts required time series from the
      CALSIM file, calls a few high level routines to orchestrate
      the calculation, then writes the output
    """
   
    # retrieve data
    path="/CALSIM/D419/FLOW-DELIVERY//1MON/fpart/".replace("fpart",fpart)
    swp_average_exports=dss_retrieve_ts(calsimfile,path)
    path="/CALSIM/D418/FLOW-DELIVERY//1MON/fpart/".replace("fpart",fpart)
    cvp_average_exports=dss_retrieve_ts(calsimfile,path)
    path="/CALSIM/EXPRATIO_/EI-RATIO-STD//1MON/fpart/".replace("fpart",fpart)
    ei_ratio=dss_retrieve_ts(calsimfile,path)
    path="/CALSIM/DINFLOW/INFLOW-PULSE//1MON/fpart/".replace("fpart",fpart)
    delta_inflow=dss_retrieve_ts(calsimfile,path)
    path = get_calsim_path(sjr_process, fpart)
    total_export_limit=dss_retrieve_ts(calsimfile,path)
   
    swp_limit,cvp_limit=project_export_limits(
                    total_export_limit,ei_ratio,delta_inflow)
    swp=calculate_exports(swp_limit,swp_average_exports)
    #assert ts_max(swp) <= SWP_MAX_PUMP, "SWP pumping exceeds physical bounds. This was assumed not to happen, so the preprocessor needs fixing"
    cvp=calculate_exports(cvp_limit,cvp_average_exports)
    #assert ts_max(swp) <= CVP_MAX_PUMP, "CVP pumping exceeds physical bounds. This was assumed not to happen, so the preprocessor needs fixing"

    swp_path="/CALSIM-VAMP/D419/FLOW-EXPORT//1DAY/fpart/".replace("fpart",fpart_mod)
    dss_store_ts(outfile,swp_path,swp)
    cvp_path="/CALSIM-VAMP/D418/FLOW-EXPORT//1DAY/fpart/".replace("fpart",fpart_mod)
    dss_store_ts(outfile,cvp_path,cvp)

use = '''
Usage:
vscript prep_vamp.py configfile
- OR -
vscript.bat prep_vamp.py calsimdss outdss
    configfile  the input file for configuration variables
    calsimdss   the CALSIM file to be processed
    outdss      the destination dss file for calculated flows
    
The command line version does not include a dss FPART in its search.
It assumes that the CALSIM file does includes only one version (FPART)
for each of the required inputs. .
'''
def main():
    if len(sys.argv) == 3 or len(sys.argv) == 4:
        calsimdss=sys.argv[1]
        outdss=sys.argv[2]
        #config=0
        if len(sys.argv) == 4:
            fpart=sys.argv[3]
        else:            
            fpart=""   # will match anything, so duplicates will give unexpected behavior
        fpart_modified=fpart
        if not(calsimdss.endswith(".dss") and outdss.endswith("dss")):
            raise SystemExit(use)
    elif len(sys.argv) == 2:
        configfile=sys.argv[1]
        config.setConfigVars(configfile)
        calsimdss=config.getAttr("CALSIMFILE")
        sjr_process=config.getAttr("SJR_PROCESS")
        outdss=config.getAttr("CALSIM_VAMP")
        fpart=calsim_study_fpart(modify=0)
        fpart_modified=calsim_study_fpart(modify=1)
    else:
	    raise "wrong number of arguments in script prep_vamp"
	
    prep_vamp_vernalis(calsimdss,outdss,fpart,fpart_modified) 
    prep_vamp_exports(calsimdss,outdss,fpart,fpart_modified,sjr_process)
    sys.exit()
        
if __name__ == '__main__':
    main()

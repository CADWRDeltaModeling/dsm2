"""This script is a utility for obtaining the runtime and preprocessing
   times from the config file.
"""   
   
   
from config import getAttr
from vtimeseries import timewindow,timeinterval
from vista.time import TimeFactory

def planning_window():
    """ 
    Returns the runtime represented by START_DATE START_TIME END_DATE and END_TIME
    in the config file as a timewindow
    """
    return timewindow(getAttr("START_DATE") + " " + getAttr("START_TIME") 
                + " - " +
                + getAttr("END_DATE") + " " + getAttr("END_TIME"))

def prepro_window(prepro_window_option = None):
    """ 
    Returns the preprocessing window corresponding to the run
    The prepro_window_option can be set to "RUNDATE" "16yr" or "82yr"
    and the prepro time window will be buffered to the nearest day, 
    the standard 16yr preprocessing window or 82 years accordingly.
    The default is RUNDATE.
    """
    if prepro_window_option == None:
        prepro_window_option == getAttr("PREPRO_WINDOW")
    if not prepro_window_option:
        prepro_window_option = "RUNDATE"
    if (prepro_window_option == "RUNDATE"):
        st = getAttr("START_TIME")
        if st != "0000" and st != "2400": st = "0000"
        et = getAttr("END_TIME")
        if et != "2400" and et != "0000": et = "2400"
        return timewindow(getAttr("START_DATE") + " " + st
                        + " - "
                        + getAttr("END_DATE") + " " + et)
    elif prepro_window_option.lower() == "16yr":
        return timewindow("01OCT1974 0000 - 01OCT1991 0000")
    elif prepro_window_option.lower() == "82yr":
        return timewindow("01OCT1974 0000 - 01OCT1991 0000")
    else:
        raise "Prepro time window option not understood: %s" % prepro_window_option

def grow_window(tw,left,right=None):
    if right == None: right = left
    st = tw.getStartTime() - timeinterval(left)
    et = tw.getEndTime() + timeinterval(right)
    return TimeFactory.getInstance().createTimeWindow(st,et)
                
                
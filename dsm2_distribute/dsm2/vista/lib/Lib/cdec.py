from java.net import URL
from java.util import StringTokenizer
from java.io import LineNumberReader, InputStreamReader
from java.lang import String
from vista.set import DataSetAttr,DataType,Constants,RegularTimeSeries
from vista.time import TimeFactory
import string
# read urls from station cdec file and returns a time series with the data
def retrieve(station_name,sensor_number,pathname,units,\
	     start_date, end_date='now',_verbose=1):
    """
    retrieve(station_name,sensor_number,pathname,units,\
             start_date, end_date='now',_verbose=1)

    Retrieves data from cdec station and sensor and
    constructs a regular time series with given pathname, units.

    The start_date is a starting time value in format mm/dd/yyyy
    e.g. 15-Jun-2000 would be 06/15/2000

    The end_date can either be the keyword "now" or the same format as
    the start_date

    verbose=1 sets it to more verbose and 0 for silence
    """
    _debug=0
    c_url = URL('http://cdec.water.ca.gov/cgi-progs/queryCSV?'
		+'station_id='+station_name
		+'&dur_code=H&sensor_num='+sensor_number
		+'&start_date='+start_date
		+'&end_date='+end_date)
    if _verbose:
	print "station name:%s & sensor number:%s "%(station_name,sensor_number)
	print "pathname:%s & units:%s"%(pathname,units)
	#
	print 'URL: %s'%c_url
    lr = LineNumberReader(InputStreamReader(c_url.openConnection().getInputStream()))
    # jump all the way to data
    lr.readLine()
    lr.readLine()
    lr.readLine()
    # create starting date and time and of the format the
    # data is at cdec
    line = lr.readLine()
    tf = TimeFactory.getInstance()
    starray = string.split(line,',')
    dtm = starray[0] + ' ' + starray[1]
    tp = tf.createTime(dtm,'yyyyMMdd HHmm')
    tp = tf.createTime(tp.getTimeInMinutes())
    ti = tf.createTimeInterval('1hour')
    ltime = tp
    if _debug: print line, tp,dtm
    yvals = []
    if _verbose: print 'Data starting at ', tp
    # read in all the data and append missing values if no data 
    # is available
    while line != None:
	st = StringTokenizer(line,',')
	if ( st.countTokens() != 3 ) :
	    raise "Invalid CDEC format, need 3 tokens on line: " + line
	# get time 
	ctime = tf.createTime(st.nextToken()+' '+st.nextToken(),'yyyyMMdd HHmm')
	# if time is not in increasing order -> quit!
	if ctime.compare(ltime) < 0:
	    raise "Invalid time sequence: %s followed by %s"+\
		  " ? -> should be always increasing"%(str(ctime),str(ltime))
	# check if current time is only one time interval from last time
	nskip = ltime.getNumberOfIntervalsTo(ctime,ti)
	# if skip is greater than one then fill with -901's
	while nskip > 1:
	    yvals.append(Constants.MISSING_VALUE)
	    nskip=nskip-1
	ltime = ctime
	# now get current value
	val_str = st.nextToken()
	try :
	    if ( val_str == "m" ): # if missing
		val=Constants.MISSING_VALUE
	    else: # else just save the value
		val=float(val_str)
	    yvals.append(val)
	except:
	    yvals.append(Constants.MISSING_VALUE)
	    print "Exception! for string " ,val_str
	line = lr.readLine();
    # create a time series data set from the array of values,
    # the start time and the time interval
    attr = DataSetAttr(DataType.REGULAR_TIME_SERIES,'',units,'TIME','INST-VAL')
    rts = RegularTimeSeries(pathname,tp.toString(),'1hour',yvals,None,attr)
    return rts
#

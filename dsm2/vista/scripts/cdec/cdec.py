from java.net import URL
from java.util import StringTokenizer
from java.io import LineNumberReader, InputStreamReader
from java.lang import String
from vista.set import DataSetAttr,DataType,Constants,RegularTimeSeries,IrregularTimeSeries
from vista.time import TimeFactory
import string
from httplib import *
import re
from datetime import date, datetime
CDEC_BASE_URL="cdec.water.ca.gov"
DURATION_MAP={'daily':'D','hourly':'H','monthly':'M','event':'E'}
DSS_INTERVAL={'monthly': '1MON','daily':'1DAY', 'hourly':'1HOUR', 'event':'IR-DAY'}
#
class Sensor:
	def __init__(self, id):
		self.id=id
	def __repr__(self):
		return "Sensor: %s"%self.id
	def getDurationCode(self):
		return DURATION_MAP[self.duration]
#
class Station:
	def __init__(self,id):
		self.id=id
	def __repr__(self):
		return self.__str__()
	def __str__(self):
		return "Station ID: %s"%(self.id)
	def findSensor(self,type,subType,units,duration):
		for sensor in self.sensors:
			if sensor.type==type and sensor.subType==subType and sensor.duration==duration and sensor.units==units:
				return sensor
		return None
	def findSensorByNumber(self, number):
		for sensor in self.sensors:
			if (sensor.id==number):
				return sensor
		return None
#
def fetch_data_in_url(url):
	c=HTTPConnection(CDEC_BASE_URL)
	c.request("GET",url)
	r=c.getresponse()
	if r.status==200:
		data=r.read()
		return data
	else:
		return ""
def find_in_url(url,regexp):
	data = fetch_data_in_url(url)
	return find_match(data,regexp)
def find_match(data,regexp):
	if len(data)>0:
		m=re.findall(regexp,data)
		return m
	else:
		return []
def find_next_column(data,text):
	m=find_match(data,'<td>.*?%s.*?</td>.*?<td>(.*?)</td>'%text)
	if len(m) == 1:
		return m[0]
	else:
		return ""
def regex_tag_contents(tag='html'):
	return re.compile('<%s.*?>(.*?)</%s>'%(tag,tag),re.S | re.M | re.I)
def regex_table_contents():
	return regex_tag_contents('table')
def regex_row_contents():
	return regex_tag_contents('tr')
def regex_column_contents():
	return regex_tag_contents('td')
def retrieve_realtime_stations():
	"""
		Update the list of stations available from CDEC along with their information.
	"""
	matches = find_in_url("/misc/realStations.html", '<a href="/cgi-progs/queryF\?(.*)">(.*)</a>')
	return map(lambda x: Station(x[1]), matches)
def retrieve_daily_stations():
	matches = find_in_url("/misc/dailyStations.html", '<a href="/cgi-progs/queryDaily\?(.*)">(.*)</a>')
	return map(lambda x: Station(x[1]), matches)
def retrieve_station_metadata(station):
	"""
	Retrieves a stations metadata as long as the id for the station is populated
	"""
	data=fetch_data_in_url("/cgi-progs/stationInfo?station_id=%s"%station.id)
	station.id=find_next_column(data,"Station ID")
	station.elevation=find_next_column(data,"Elevation")
	station.riverBasin=find_next_column(data,"River Basin")
	station.county=find_next_column(data,"County")
	station.hydrologicArea=find_next_column(data,"Hydrologic Area")
	station.nearbyCity=find_next_column(data,"Nearby City")
	station.lat=find_next_column(data,"Latitude")
	station.lng=find_next_column(data,"Longitude")
	station.operator=find_next_column(data,"Operator")
	station.dataCollection=find_next_column(data,"Data Collection")
	tables=regex_table_contents().findall(data)
	for table in tables:
		if table.find('Sensor Description') > 0:
			rows = regex_row_contents().findall(table)
			sensors=[]
			for row in rows:
				cols=regex_column_contents().findall(row)
				id=find_match(cols[2],'sensor_no=(\d+)')[0]
				sensor=Sensor(id)
				fields=cols[0].replace('<b>','').replace('</b>','').split(',')
				fields=map(lambda(x): x.strip(), fields)
				sensor.type=fields[0]
				if len(fields) == 3:
					sensor.subType = fields[1]
				else:
					sensor.subType = ""
				if len(fields) == 3:
					sensor.units=fields[2]
				elif len(fields) == 2:
					sensor.units = fields[1]
				else:
					sensor.units=""
				sensor.duration=regex_tag_contents('a').findall(cols[1].strip())[0]
				sensor.plot=regex_tag_contents('a').findall(cols[2])[0]
				sensor.dataCollection=cols[3].strip()
				sensor.dataAvailable=cols[4].strip()
				sensors.append(sensor)
			station.sensors=sensors
	return station
def retrieve_station_sensors(station):
	data=fetch_data_in_url("/cgi-progs/queryCSV?station_id=%s&sensor_num=&dur_code=D&start_date=&end_date=&data_wish=View+CSV+Data"%station.id)
	divs=regex_tag_contents('div').findall(data)
	for div in divs:
		if div.find('Sensor numbers available') > 0:
			rows = regex_row_contents().findall(div)
			for row in rows:
				cols = regex_column_contents().findall(row)
		 		number=regex_tag_contents('font').findall(cols[0])[0]
		 		fields=regex_tag_contents('a').findall(cols[1])[0].split(',')
		 		type=fields[0].strip()
		 		if len(fields) > 1:
		 			subType=fields[1].strip()
		 		else:
		 			subType=''
		 		units=find_match(cols[1],'\((.*?)\)')[0].strip()
		 		duration=find_match(cols[2],'\((.*?)\)')[0].strip()
		 		sensor=station.findSensor(type,subType,units,duration)
		 		if sensor:
		 			sensor.sensor_number=number
		 		else:
		 			print 'No sensor found for type: %s, subtype: %s, units: %s, duration: %s'%(type,subType,units,duration)
def retrieve_csv(station,sensor,timewindow):
	start_date="1/1/1900"
	end_date=date.today().strftime("%m/%d/%Y")
	if timewindow:
		sd,ed=map(lambda x: x.strip(),timewindow.split('-'))
		start_date=datetime.strptime(sd,"%d%b%Y %H%I").strftime('%m/%d/%Y')
		end_date=datetime.strptime(ed,"%d%b%Y %H%I").strftime('%m/%d/%Y')
	url="/cgi-progs/queryCSV?station_id=%s&dur_code=%s&sensor_num=%s&start_date=%s&end_date=%s"%(station.id,sensor.getDurationCode(),sensor.sensor_number,start_date,end_date)
	print url
	return fetch_data_in_url(url)
# read urls from station cdec file and returns a time series with the data
def retrieve_ts(station,sensor,\
		 start_date, end_date='now',_verbose=1):
	"""
	retrieve(station,sensor, start_date, end_date='now',_verbose=1)

	Retrieves data from cdec station and sensor and
	constructs a regular time series with given pathname, units.

	The start_date is a starting time value in format mm/dd/yyyy
	e.g. 15-Jun-2000 would be 06/15/2000

	The end_date can either be the keyword "now" or the same format as
	the start_date

	verbose=1 sets it to more verbose and 0 for silence
	"""
	_debug=0
	station_name=station.id
	sensor_number=sensor.sensor_number
	c_part="%s_%s"%(sensor.type,sensor.subType)
	f_part="SENSOR %s"%(sensor.id)
	pathname='/%s/%s/%s//%s/%s/'%('CDEC-RAW',station_name,c_part,DSS_INTERVAL[sensor.duration],"SENSOR "+sensor.id)
	units=sensor.units
	dur_code = sensor.getDurationCode()
	c_url = URL('http://cdec.water.ca.gov/cgi-progs/queryCSV?'
		+'station_id='+str(station_name)
		+'&dur_code='+str(dur_code)+'&sensor_num='+str(sensor_number)
		+'&start_date='+start_date
		+'&end_date='+end_date)
	if _verbose:
		print "station name:%s & sensor number:%s "%(station_name,sensor_number)
	   	print "dur_code:%s "%(dur_code)
	   	print "pathname:%s & units:%s"%(pathname,units)
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
	ti_str='1HOUR'
	irreg=0 # indicates the data as irregular time series
	if dur_code=='E':
		irreg=1
		ti_str='IR-DAY'
	elif dur_code=='D':
		ti_str='1DAY'
	elif dur_code=='H':
		ti_str='1HOUR'
	elif dur_code=='M':
		ti_str='1MON'
	if not irreg:
		ti = tf.createTimeInterval(ti_str)
	ltime = tp
	if _debug: print line, tp,dtm
	yvals = []
	if irreg:
		tvals=[]
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
			if not irreg:
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
				if irreg:
					tvals.append(ctime)
			yvals.append(val)
		except:
			tvals.append(ctime)
			yvals.append(Constants.MISSING_VALUE)
			print "Exception! for string " ,val_str
		line = lr.readLine();
	# create a time series data set from the array of values,
	# the start time and the time interval
	if irreg:
		attr = DataSetAttr(DataType.IRREGULAR_TIME_SERIES,'',units,'TIME','INST-VAL')
		its = IrregularTimeSeries(pathname,tvals,yvals,None,attr)
		return its
	else:
		attr = DataSetAttr(DataType.REGULAR_TIME_SERIES,'',units,'TIME','INST-VAL')
		rts = RegularTimeSeries(pathname,tp.toString(),ti_str,yvals,None,attr)
		return rts
#
def retrieve(station_name,sensor_number,start_date,end_date='now',verbose=1):
	"""
	retrieve(station_name,sensor_number,\
			 start_date, end_date='now',_verbose=1)

	Retrieves data from cdec station and sensor and
	constructs a regular time series with given pathname, units.

	The start_date is a starting time value in format mm/dd/yyyy
	e.g. 15-Jun-2000 would be 06/15/2000

	The end_date can either be the keyword "now" or the same format as
	the start_date

	verbose=1 sets it to more verbose and 0 for silence
	"""
	station = Station(station_name)
	retrieve_station_metadata(station)
	retrieve_station_sensors(station)
	sensor = station.findSensorByNumber(str(sensor_number))
	return retrieve_ts(station,sensor,start_date, end_date,1)
# Script to access data from Water Data Library
import sys
import urllib
import re
import csv
import datetime
import com.xhaus.jyson.JysonCodec as json
from vista.set import DataSetAttr, DataType, Constants, RegularTimeSeries, IrregularTimeSeries
from vista.time import TimeFactory
from vutils import writedss
TF = TimeFactory.getInstance()
class WDLStationAnalyteData:
    def __init__(self, name, analyte, shortname, units):
        self.name = name
        self.analyte = analyte
        self.pathname = '/WDL/%s/%s//IR-DAY/%s/' % (name, analyte, shortname)
        self.vals = []
        self.units = units
    def add(self, date, value):
        try:    
            t = TF.createTime(date, 'MM/dd/yyyy HH:mm').getTimeInMinutes()
        except e:
            print 'Could not parse time: '+date
            raise e
        v = Constants.MISSING_VALUE
        try:
            v = float(value)
        except ValueError, ve:
            v = Constants.MISSING_VALUE
            print 'Missing value: ' + value
        self.vals.append([t, v])
    def toITS(self):
        svals = sorted(self.vals, lambda x, y: x[0] - y[0])
        tvals = map(lambda x: x[0], svals)
        yvals = map(lambda x: x[1], svals)
        attr = DataSetAttr(DataType.IRREGULAR_TIME_SERIES, '', self.units, 'TIME', 'INST-VAL')
        its = IrregularTimeSeries(self.pathname, tvals, yvals, None, attr)
        return its
#
DELTA_MAP_BOUNDS = {"LatNorth":38.538, "LatSouth":37.756, "LonWest":-122.237, "LonEast":-121.254}
TEST_MAP_BOUNDS = {"LatNorth":38.1969102804601,"LatSouth":38.08836429508079,"LonWest":-121.69795989990236,"LonEast":-121.49127960205078}
def get_all_stationids(bounds):
    latn = bounds["LatNorth"]
    lats = bounds["LatSouth"]
    lonw = bounds["LonWest"]
    lone = bounds["LonEast"]
    delta=0.1
    lat=latn
    station_ids=[]
    while lat >= lats:
        lon=lonw
        while lon <= lone:
            smaller_bounds={"LatNorth":lat, "LatSouth":lat-delta, "LonWest":lon, "LonEast":lon+delta}
            station_ids.extend(_get_station_ids(smaller_bounds))
            lon=lon+delta
        lat=lat-delta
    return sorted(set(station_ids))
def _get_station_ids(bounds):
    """
    queries within bounds and returns the json result as stations associative array
    """
    url = 'http://www.water.ca.gov/waterdatalibrary/maps/stnlocation.cfc?method=greg&returnFormat=json&argumentCollection=%s&_cf_nodebug=true&_cf_nocache=true&_cf_clientid=C664A0B77D0DF4B9EF22623335665E8E&_cf_rc=11'
    print 'Getting station ids for %s'%bounds
    argumentCollection = '{"LatNorth":%f,"LatSouth":%f,"LonWest":%f,"LonEast":%f,"getGW":false,"getWQ":true,"getHY":false,"getUser":"TRUE"}' % (bounds['LatNorth'], bounds['LatSouth'], bounds['LonWest'], bounds['LonEast'])
    argumentCollection = urllib.quote(argumentCollection)
    response = urllib.urlopen(url % (argumentCollection))
    result = response.readlines()
    response.close()
    result = reduce(lambda x, y: x + y, result)
    stations = json.loads(result);
    station_ids=[]
    for station in stations:
        if station.has_key('point'):
            for val in station['point']: 
                station_ids.append(station['point'][val]['STATIONNUMBER'])
    return station_ids
#
def get_station_ids_from_county(county):
    """
    returns a list of station ids belonging to the provided county
    """
    params = { 'ddmCounty':county, 'txtNumber':'', 'txtStation':''}
    params = urllib.urlencode(params)
    url = 'http://www.water.ca.gov/waterdatalibrary/waterquality/station_county/select_station.cfm'
    response = urllib.urlopen(url, params)
    result = response.readlines()
    response.close()
    result = reduce(lambda x, y: x + y, result)
    #station_infos = re.findall(re.compile('<td.*?>.*?value="(.*?)".*?</td>.*?<td.*?>.*?</td>.*?<td.*?>(.*?)</td>.*?<td.*?>(.*?)</td>.*?<td.*?>(.*?)</td>',re.S|re.M|re.I),result)
    return re.findall(re.compile('<td.*?>.*?value=".*?".*?</td>.*?<td.*?>.*?</td>.*?<td.*?>(.*?)</td>.*?<td.*?>.*?</td>.*?<td.*?>.*?</td>',re.S|re.M|re.I),result)
def get_data_for_station(station_id, from_map,sdate,edate):
    if from_map == 1:
        url = 'http://www.water.ca.gov/waterdatalibrary/waterquality/station_county/select_station.cfm?URLStation=%s&source=map' % station_id
        response = urllib.urlopen(url)
    else:
        params = { 'ddmCounty':'', 'txtNumber':(station_id+''), 'txtStation':''}
        params = urllib.urlencode(params)
        url = 'http://www.water.ca.gov/waterdatalibrary/waterquality/station_county/select_station.cfm'
        response = urllib.urlopen(url, params)
    result = response.readlines()
    response.close()
    result = reduce(lambda x, y: x + y, result)
    boxstation_id = re.findall(re.compile('<input.*? name="boxStationID".*? value="(.*?)" .*?>', re.S | re.M | re.I), result)[0]
    if len(sdate)<10:
        sdate = re.findall(re.compile('<input.*? name="MinDate" value="(.*?)".*?>', re.S | re.M | re.I), result)[0]
    if len(edate)<10:
        edate = re.findall(re.compile('<input.*? name="MaxDate" value="(.*?)".*?>', re.S | re.M | re.I), result)[0]
    now = datetime.date.today()
    before = now - datetime.timedelta(10) # 10days ago
    params = {'MaxDate':edate, 'MinDate':sdate,
            'begday':before.day, 'begmonth':before.month, 'begyear':before.year ,
            'endday':now.day, 'endmonth':now.month, 'endyear':now.year,
            'boxStationID': boxstation_id, 'cmdGet_Report': 'Get Data',
            'ddmOutputFormat': 'Text', 'rdoDate': 'AllDates', 'rdoFielddata': 'Yes'
            }
    params = urllib.urlencode(params)
    response = urllib.urlopen('http://www.water.ca.gov/waterdatalibrary/waterquality/station_county/report_content_xcl_txt.cfm', params)
    reader = csv.reader(response, delimiter=',')
    result = []
    for row in reader:
        result.append(row)
    response.close()
    return result

def parse_wdl_its(data):
    """
    parses an array of rows (each row is an array, where the first one is the name of each column) into
    an array of irregular time series
    """
    result = {}
    colnames=data[0]
    shortname_index=colnames.index('Short Station Name')
    name_index=colnames.index('Station Number')
    analyte_index=colnames.index('Analyte')
    units_index=colnames.index('Units')
    date_index=colnames.index('Collection Date')
    value_index=colnames.index('Result')
    for row in data[1:]:
        shortname = row[shortname_index]
        name = row[name_index]
        analyte = row[analyte_index]
        units = row[units_index]
        date = row[date_index]
        value = row[value_index]
        key = '%s_%s' % (name, analyte)
        if not result.has_key(key):
            result[key] = WDLStationAnalyteData(name, analyte, shortname, units);
        result[key].add(date, value)
    return result

def writewdl(filename, result):
    for key in result:
        its = result[key].toITS()
        writedss(filename, its.name, its)
#
def write_to_dss(filename, stationids, from_map,sdate,edate):
    for station_id in stationids:
        print 'Getting data for station: %s'%station_id
        sys.stdout.flush()
        try:
            data = get_data_for_station(station_id,from_map,sdate,edate)
            result = parse_wdl_its(data)
            writewdl(filename, result)
        except:
            print "Could not get data for: %s"%station_id
#
def download_delta_stations_from_map(filename,sdate,edate):
    print 'Retrieving all station ids with Delta Map Boundaries'
    sys.stdout.flush()
    stationids=get_all_stationids(DELTA_MAP_BOUNDS)
    print 'Done retrieving station ids: %d'%(len(stationids))
    sys.stdout.flush()
    print 'Downloading data to %s'%filename
    sys.stdout.flush()
    write_to_dss(filename,stationids,1,sdate,edate)
    print 'Done downloading data to %s'%filename
#
def download_counties(filename,idnumlst,sdate,edate): 
    for county_id in idnumlst:  #range(1,59):
        stationids=get_station_ids_from_county(county_id)
        write_to_dss(filename,stationids,0,sdate,edate)

#program can be run in several ways
#(1) by search map, by specifying county, by specifying station id
#program first read a configuration file
if __name__ == '__main__':
    filename='d:/temp/wdl.dss'
    searchby="MAP"
    sdate=""
    edate=""
    if len(sys.argv) > 1:  #search by map
        cfgfile = sys.argv[1]
        idnumfile = ""
        #read configuration file 
        fin=open(cfgfile,"r")
        lines = fin.readlines()
        fin.close()
        for line in lines:
            strlst = line.split()
            keywd = strlst[0].upper()
            if keywd == "DSSPATH":
                filename = strlst[1]
            if keywd == "SDATE":
                sdate = strlst[1]
            if keywd == "EDATE":
                edate = strlst[1]
            if keywd == "SEARCHBY":
                searchby = strlst[1].upper()
                if searchby == "STATIONID" or searchby == "COUNTY":
                    idnumfile = strlst[2]
                    if len(idnumfile)<1:  #no file specified
                        searchby = "MAP"
        if searchby == "MAP":
            download_delta_stations_from_map(filename,sdate,edate)
        else:
            fin=open(idnumfile,"r")
            lines = fin.readlines()
            fin.close()
            idnumlst = []
            for line in lines:
                if line.find("#") == -1:  #not use comment line
                    idnumlst.append(line.strip()) # get rid of \n
            if searchby == "STATIONID":
                write_to_dss(filename,idnumlst,0,sdate,edate)
            else:
                download_counties(filename,idnumlst,sdate,edate)
        print 'Done downloading data to %s'%filename
#

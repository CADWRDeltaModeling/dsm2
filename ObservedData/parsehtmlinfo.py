import string, re, urllib, urlparse
# Take a list of string objects and return the same list
# stripped of extra whitespace.
def striplist(l):
    return([x.strip() for x in l])

# get info about cdec stations
def readtable(filename):
    table = {}
    for line in open(filename).readlines():
        fields = striplist(line.split('\t'))
        table[fields[1]] = fields
    return table

if __name__ == "__main__":
    # read list of CDEC sensors
    senstab=readtable('cdec-sensors.tsv')
    #print senstab.keys()
    htmlsep=re.compile('(<[^>]+>)+')
    wanted_sens_num=set([1,5,20,25,70,92,100,102,110,141,146,148])
    # read list of station IDs
    stations=[]
    for line0 in open('./cdec_stas.txt').readlines():
        if re.match('^#',line0):
	    continue
        station=line0.split('\t')[0]
        file1 = open(station+'.html')
        sens=0;datum=''
        for line in file1:
            splitline=htmlsep.split(line)
            if re.search('Station ID',line):
                staid=splitline[4]
            if re.search('Latitude',line):
                line=line.replace('&#176',' ')
                splitline=htmlsep.split(line)
                latlon='Lat'+splitline[4] + ' Lon'+splitline[8]
            if re.search('Operator',line):
                oper=splitline[4].replace('CA Dept of Water Resources','DWR')
                oper=oper.replace('US Bureau of Reclamation','USBR')
                oper=oper.replace('US Geological Survey','USGS')
                oper=oper.replace('Contra Costa Water District','CCWD')
                oper=oper.replace('CA Dept of Fish and Game','CDFG')
                oper=oper.replace('East Bay Municipal Utility District','EBMUD')
		Oper=oper.replace('Central District','CD')
		oper=oper.replace('/','-')
                oper=oper.replace(' ','')
            if re.search('usedngvd.html>Datum',line):
                datum=splitline[2]+splitline[6]
                if len(splitline) >= 8: datum+=' '+splitline[8]
                if len(splitline) >= 10: datum+=' '+splitline[10]
                datum=datum.strip()
            if re.search('<\/table>',line):
                sens=0
            if re.search('Sensor Description',line):
	        print
                print staid+'\t'+latlon+'\t'+oper+'\t'+datum # output station info
                sens=1
                # split line on html newline tag (<p>)
                lf=line.find('<p>',5)
                if lf:
                    line=line[lf:]
                    splitline=htmlsep.split(line)
                #
            if sens==1:
                sens_desc=splitline[2]
                sens_units=splitline[4]
                sens_intvl=splitline[8]
                sens_key=splitline[14]
                sens_src=splitline[18]
                sens_num=senstab[sens_key][0]
                sensor=re.sub('[(),]','',\
      			sens_num+'\t'+sens_desc+'\t'+sens_units+'\t'+\
      			sens_key+'\t'+sens_src+'\t'+sens_intvl)
                if int(sens_num) in wanted_sens_num: #and (sens_intvl=='hourly' or sens_intvl=='daily'):
		  print staid+'\t'+sensor
		  # now download from CDEC a csv file of the data
		  url='http://cdec4gov.water.ca.gov/cgi-progs/queryCSV?station_id=' + staid + \
		       '&dur_code=' + sens_intvl.upper()[:1] + '&sensor_num=' + sens_num + \
		       '&start_date=1/1/1989&end_date=now&data_wish=Download+CSV+Data+Now'
		  file_sta_sens=staid + '_' + re.sub('[!@#$%^&*()/+=\'":;<>,?\\\[\]\{\}]','',oper) + '_' + \
				 re.sub(' ','',sens_key) + '_' + sens_intvl + '.csv'
		  print file_sta_sens
		  urllib.urlretrieve(url,file_sta_sens)

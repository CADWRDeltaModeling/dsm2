
# Import VTools time and numpy array creation function
from vtools import *
from vtools.data.api import *
from vtools.data.vtime import *
from vtools.data.constants import *
from vtools.data.timeseries import *   
from datetime import datetime         
from numpy import arange,sin,pi

# Import VTools dss utility function
from vtools.datastore.dss.api import *
# Import VTools Excel utility function
from vtools.datastore.excel.api import *
import os, sys, string, re
from numpy import zeros
from os import listdir
import types
import operator
import pdb

def striplist(l):
    return([x.strip() for x in l])

if __name__ == "__main__":

#  RegTS=True				# true or false for regular or irregular TS
    RegTS=False				# true or false for regular or irregular TS
    csvdir='CSV-Files'
    destination="CDEC-its.dss"

    intervals={'daily': '1DAY', 'hourly': '1HOUR', 'event': '15MIN'}
    ir_Epart={'1DAY': 'IR-YEAR', '1HOUR': 'IR-MON', '15MIN': 'IR-DAY'}
    xout_set={'EC': 40000, 'FLOW': 500000, 'DIV': 1000, 'STAGE': 20, \
	      'EXPORT': 12000, 'CL': 1000, 'TEMP': 100}
    MissVal=scipy.nan
    MissVal=-901.0
    csvfiles = [x for x in sorted(os.listdir('./CSV-Files')) if re.search('\.csv$',x)]
    Apart = "CDEC"
    for filen in csvfiles:
	#if not re.match('^R',filen): continue
	filen=csvdir+'/'+filen
	ts_time = []
	ts_data = []
	fs=filen.replace(csvdir+'/','').split('.')[0].split('_') # staid, operator, param, interval
	staid=fs[0]; oper=fs[1]; param=fs[2]; intvl=fs[3]
	Bpart=staid
	if re.match("ELCO?ND",param):
	    Cpart="EC"
	elif re.search("FLOW",param):
	    Cpart="FLOW"
	elif param=="DIVERSN":
	    Cpart="DIV"
	elif re.match("RIVST",param):
	    Cpart="STAGE"
	elif re.match("TEMPW",param):
	    Cpart="TEMP"
	elif param=="DCPUMP":
	    Cpart="EXPORT"
	elif param.upper()=="DISSCL":
	    Cpart="CL"
	else:
	    print "Unknown parameter",param
	    raise NameError("Unknown parameter")

	Epart = intervals[intvl]
	Fpart = oper
	if intvl=='event': DataType='INST-VAL'
	else: DataType='PER-AVER'

	lineno = 0
	nseqerr = 0
	nmissdates = 0
	noutx=0
	xout=0.0
	f1 = open(filen)
	for line in f1:
	    lineno=lineno+1
	    if line:
		if line.find("<!DOCTYPE") != -1:
		    break
		if lineno == 3:
		    Unit = line.split("(")[1].split(")")[0]
		if lineno > 3:
		    ls=striplist(line.split(','))
		    dday = ls[0]
		    dtime = ls[1]
		    time1 = parse_time(dday+' '+dtime)
		    if lineno==4:
			time0=time1-parse_interval(Epart)
		    year = int(dday[0:4])
		    mon = int(dday[4:6])
		    day = int(dday[6:8])
		    dhr = int(dtime[0:2])
		    dmin = int(dtime[2:4])
		    # check for out of sequence data; skip it
		    if time1-time0 < parse_interval('1minute'):
			#print 'Skipping out of sequence date:',dday,dtime
			nseqerr+=1
			continue
		    # check for dates that jump forward; fill with missing values
## 	  while RegTS and (time1-time0 > parse_interval(Epart)):
## 	    time0+=parse_interval(Epart)
## 	    ts_time.append(time0)
## 	    ts_data.append(MissVal)
## 	    nmissdates+=1
## 	    #print 'Filling missing data',time0
		    datatemp = ls[2]
		    time0=time1
		    try:
			datatemp=float(datatemp)
		    except:
			continue
		    ts_time.append(datetime(year,mon,day,dhr,dmin))
		    ts_data.append(datatemp)

	if lineno==1: continue
	# get rid of extreme outliers (presumed bad data)
	tsdlen=len(ts_data)
	if tsdlen > 100:
	    tssd=sorted(ts_data)
	    #xout=abs(tssd[5]),abs(tssd[tsdlen-5])
	    xout=xout_set[Cpart]
	    for i in range(tsdlen):
		if abs(ts_data[i]) >= xout:
		    ts_data[i]=MissVal
		    noutx+=1
	#
	f1.close()
	tsdlen=len(ts_data)
	if RegTS: path="/"+Apart+"/"+Bpart+"/"+Cpart+"//"+Epart+"/"+Fpart+"/"
	else: path="/"+Apart+"/"+Bpart+"/"+Cpart+"//"+ir_Epart[Epart]+"/"+Fpart+"/"

	print filen,path
	print '%d original values' % lineno, ' %d final value' % tsdlen
	print '%d date sequence errors' % nseqerr
	print '%d missing data filled' % nmissdates
	print '%d outliers removed' % noutx, 'using limit %d' % xout
	print
	if RegTS: ts1 = rts(ts_data,ts_time[0],Epart)
	else: ts1 = its(ts_time,ts_data)
	ts1.props[UNIT] = Unit
	ts1.props[AGGREGATION]=DataType

	dss_store_ts(ts1,destination,path)

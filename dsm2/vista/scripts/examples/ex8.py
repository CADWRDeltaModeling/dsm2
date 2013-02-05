from vutils import *
from java.lang import Math
# Capabilities and methods of Regular Time Series
x=range(50); y = map(Math.sin, x);
# Creating a new regular time series given the name, a start time, the time interval
# and an array containing the data
rts1 = RegularTimeSeries('sin','01jan1990 0100', '1hour', y)
# number of elements 
print 'Number of elements : ' , len(rts1)
# first element of time series
print 'The first element is ', rts1[0]
# last element of time series
print 'The last element is ', rts1[len(rts1)-1]
# elements can be indexed with their time, request is time is rounded to the next 
# available time point
rts1['01jan1990 0555']
# all the elements can be listed in series as
for element in rts1:
	print element
# a numbered loop over the elements in the series
for i in range(len(rts1)):
	print i, rts1[i]
#
# setting value of elements, sets the fifth element to -1
rts1[5] = -1 
rts1['01JAN1990 0100']=0.04
# to create another time series with the same values but containing
# only a slice of this time series with a given start time and a 
# given end time
rts2 = rts1.createSlice('01JAN1990 0500', '02JAN1990 0500')
# scalar operations
rts1=rts1+5.5
rts1=rts1-5.5
rts1=rts1*1.5
rts1=rts1/1.5
# vector operations
rtsx = (rts1+2.5*rts1)/(rts1*rts1)
# period average: per_avg(dataset, interval_as_string)
# period max: per_max
# period min: per_min
rts_per_avg=per_avg(rts1,'1day')
# a monthly average if you don't specify the second string
rts_per_mon_avg=per_avg(rts1)
# 
# creating a data reference to wrap this data set
# for regular time series the E part specification is made to 
# conform to its interval 
# a local reference
local_ref = DSSUtil.createDataReference('local','testdata/file1.dss','/A PART/B PART/C PART//E PART/F PART/',rts1)
# a remote reference
remote_ref = DSSUtil.createDataReference('aqua.water.ca.gov','/junk/file1.dss','/A PART/B PART/C PART//E PART/F PART/',rts1)
#
#
# The data set can be saved in a dss file as follows
writedss('../testdata/file_rts.dss','/A PART/B PART/C PART//E PART/F PART/',rts1)
# The data set can be saved in an ascii file as follows
writeascii('../testdata/file_rts.txt',rts1)
# if there are flags that need to be output to ascii and you need them
writeascii('../testdata/file_rts.txt',rts1, 1)
# to see this data in a table first wrap in a reference as explained above
# lets use local_ref for our purposes
# 
tabulate(local_ref)
# for a plot
plot(local_ref)
#

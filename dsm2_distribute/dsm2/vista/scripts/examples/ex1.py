from vutils import *
from java.lang import Math
# A sample script for introduction of vista scripting
# Creates time series data, writes them out to dss files,
# reads them back in again as references, plots them, 
# tabulates them. Also shows how to do simple math operations
# and write them back to a dss file.How to display the dss 
# file using vista group table 
#
#
# create an array of 0..49 numbers
x=range(50)
# create an array of sin's by mapping x onto y1 using Math.sin
y1=map(Math.sin,x)
# create a regular time series with name "sin" and
# start time of 01JAN1982 0100 with a regular time interval 
# of 5 minutes containing the array of sin's
ts1=RegularTimeSeries('sin','01jan1982 0100', '5min', y1)
# create an array of cos's by mapping x onto y2 using Math.cos
y2=map(Math.cos,x)
# create a regular interval time series containing y2
ts2=RegularTimeSeries('cos','01jan1982 0100', '5min', y2)
# write out these data sets to a dss file
writedss('../testdata/file1.dss','/vista-EX1/sin/flow//5min/sin-wave/',ts1)
writedss('../testdata/file1.dss','/vista-EX1/cos/flow//5min/cos-wave/',ts2)
# reading the newly created file, we open a group of 
# pathnames containing all the pathnames in that dss file. 
# opena dss file
g=opendss('../testdata/file1.dss')
# filter using the regular expression or string
g.filterBy('/vista-EX1/sin/flow')
# assign a new variable to the first DataReference in the dss file
ref_sin = g[0]
# do the same for the cosine data
g=opendss('../testdata/file1.dss')
g.filterBy('/vista-EX1/sin/flow')
ref_cos = g[0]
# plot the sin and cosine data 
plot(ref_sin, ref_cos)
# tabulate the sin and cosine data
tabulate(ref_sin,ref_cos)
#
## Some math operations on data sets
# simple or... 
m1 = ts1+ts2
m2 = ts1/5.6
# complex
m12 = 3.76*ts1+ts2/5.4-4.7*ts1*ts2
# also available for references where pathname are decided by default
ref12=5.08*ref_sin-4.44*ref_cos
# save the computations to the dss file
writedss('../testdata/file1.dss','/vista-EX2/sin+cos/flow//5min/wave/',m1)
writedss('../testdata/file1.dss','/vista-EX2/scalar-div-sin/flow//5min/wave/',m2)
# open up the new dss file in a group table frame
g=opendss('../testdata/file1.dss')
# show group in vista's group frame
GroupFrame(g)
#

from vutils import *
# generation of a data set from other data sets using conditionals
# get the sine data
g=opendss('../testdata/file1.dss')
g.filterBy('/vista-EX1/sin/flow')
ts1 = g[0].getData()
# get the cosine data
g=opendss('../testdata/file1.dss')
g.filterBy('/vista-EX1/cos/flow')
ts2 = g[0].getData()
# initialze to empty arrays
y1 = []; y2 = []
# initialize iterators pointing to beginning of time series 1 and 2
dsi1 = ts1.getIterator();
dsi2 = ts2.getIterator();
# set the filter to the default filter, takes care of -901.0, -902.0 etcetra...
filter = Constants.DEFAULT_FILTER;
# while there are more points...
while not dsi1.atEnd() and not dsi2.atEnd() :
	# get the element at the current iterator position
	e1 = dsi1.getElement(); e2 = dsi2.getElement();
	# if the values of both are acceptable then...
	if filter.isAcceptable(e1) and filter.isAcceptable(e2) :
		# if element 1's y value is greater than element 2's y values...
		if ( e1.getY() >= e2.getY() ) :
			y1.append(e2.getY() ) # add element 2 y value to array 1
		else :
			y1.append(e1.getY() ) # elsse add element 1 y value to array 1
		# if element 2's y value is greater than element 1's y value
		if ( e2.getY() > e1.getY() ) : 
			y2.append( e2.getY() - e1.getY() ) # add difference to array 2
		else :
			y2.append( 0 ) # else add 0 to array 2
	else : # if either value is missing or bad ...
		y1.append(Constants.MISSING_VALUE) # add missingto both arrays
		y2.append(Constants.MISSING_VALUE)
	dsi1.advance(); dsi2.advance(); # advance the iterator's to the next element
# end of while not... loop
#  create a regular time series containing the values is array 1 with a start time
#  same as time series 1 start time and with the same time interval
dsn1 = RegularTimeSeries('sin or cos', ts1.getStartTime(), 
			 ts1.getTimeInterval(), y1, None, 
			 ts1.getAttributes() );
# create a rts for second array
dsn2 = RegularTimeSeries('sin or cos', ts2.getStartTime(), 
			 ts2.getTimeInterval(), y2, None, 
			 ts2.getAttributes() );
# make up a dss file to output this data to along with the appropriate pathnames 
dssfile = '../testdata/file2.dss'
path1 = '/vista-EX2/SIN OR COS//01JAN1920/5min/COMPUTED/'
path2 = '/vista-EX2/SIN not cos/DIVERSION-ACTUAL//5min/COMPUTED/'
# write the data out to the dss file and the given pathname 
writedss(dssfile, path1, dsn1)
writedss(dssfile, path2, dsn2)

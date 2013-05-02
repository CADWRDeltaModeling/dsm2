from vutils import *
# filtering through values
# temporary imports 
from vista.set import CompositeFilter, DefaultReference, ElementFilterIterator
# Open a group and filter it some
g=opendss('../testdata/qual-2b.dss')
g.filterBy('clfct/ec')
g.filterBy('1hour')
# get the first data reference in the group
ref = g[0]
# get the data set contained in the data reference
ds = ref.getData()
# create a 1 year slice
st = ds.getTimeWindow().getStartTime()
#FIXME: ds = ds[str(st):str(st+'1year')]
# get the iterator on that data set, these iterates 
# through all the values in the data set. Till now there is no 
# difference between stepping thro the data set or the
# iterator
dsi = ds.getIterator()
# lets filter out the missing values...
missing_filter = Constants.DEFAULT_FILTER
# lets create an iterator that skips over values for which
# missing_filter.isAcceptable returns false
# this iterator takes another iterator and the filter
missing_iterator = ElementFilterIterator(dsi,missing_filter)

# the x value: element.getX()
# the y value: element.getY()
# the flag (if available else = 0): element.getFlag()
element = missing_iterator.getElement()
print element.getX(), element.getY(), element.getFlag()
# get the 30th good value
missing_iterator.positionAtIndex(29)
element = missing_iterator.getElement()
print element.getX(), element.getY(), element.getFlag()
# a loop to loop over all the values till it 
# reaches the end
# we get the element and advance, then check if are at end
# if not we continue doing so till we hit the end
while not missing_iterator.atEnd():
	element = missing_iterator.getElement();
	missing_iterator.advance()
#
# lets step through back, always retreat then get element then check
# 
while not missing_iterator.atStart():
	missing_iterator.retreat()
	element = missing_iterator.getElement()
#
# to rewind quickly
missing_iterator.resetIterator();
#
# Lets now do the flags
#
# each flag filter is predefined in FlagUtils class
questionable_filter = FlagUtils.QUESTIONABLE_FILTER
reject_filter = FlagUtils.REJECT_FILTER

# to make our job easier there is a composite filter called
# CompositeFilter. It accepts an array of ElementFilter's
# and its isAcceptable method returns true only when all the
# contained element filters consider the element acceptable

flag_filter = CompositeFilter([questionable_filter,reject_filter])

# we should add the missing values/flag filter just in case
# the missing values are not screened.
flag_filter.add(Constants.DEFAULT_FILTER)

# we went through all that for nothing, I already have such
# a filter pre-defined
flag_filter=Constants.DEFAULT_FLAG_FILTER

# lets create an element filter to step through the good values 
# only
good_iterator = ElementFilterIterator(dsi,flag_filter)

# lets step through all the values and create a new array
# which contains 1 for acceptable and 0 for unacceptable values
y = []  # an empty array

# lets now use the data set iterator
dsi = ds.getIterator()
# optional, we just created this iterator so its already at the start
dsi.resetIterator() 
#if acceptable add a 1 else add a 0 to array
while not dsi.atEnd():
	element = dsi.getElement() # get the element
	if flag_filter.isAcceptable(element): 	
		y.append(1)
	else :
		y.append(0)
	dsi.advance() 	# advance by one
# end of while
#
# we can create a data set (regular time series) out of this
#  array if we like...
# arguments to this constructor are
# name, start time, time interval, array of values, 
# array of flags ( 'None' keyword if no flags), 
# data set attributes ( 'None' keyword if we don't care about attributes)
# Data set attributes are attributes such as units, 
# interpretation type ( inst_val, per_val), optionally other names
#
#
new_ds = RegularTimeSeries('binary data',ds.getStartTime(), ds.getTimeInterval(), y, None, None)
#let us wrap this in a reference
#easy way with zero control, assigns funny filenames, pathnames etcetra
new_ref = wrap_data(new_ds)
#complicated way with more control
# arguments: dataset, filename, servername, pathname
new_ref = wrap_data(new_ds,'new.dss','local','/1/2/3/4/5/6/')
# or from the java classes:
#
server = 'local'; filename='new.dss'; pathname='/1/2/3/4/5/6/';
new_ref = DefaultReference(server, filename, pathname, new_ds)
# lets look at this newly create data reference along with source reference
tabulate(new_ref,ref)
plot(new_ref, ref)

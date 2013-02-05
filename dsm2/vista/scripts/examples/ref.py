from vutils import *
# DataReference methods
# This opens up a dss file and g is now a variable containing
# all the data references in that group
g=opendss('../testdata/file1.dss')
# for a list of all the references in the group loop over them
for ref in g:
	print ref
# to thin down this group to contain only given references
# the expression for this method is a regular expression. For
# more information on regular expressions see any Perl reference
g.filterBy('COS')
# to only thin down the group using selection or rejection 
# based on a particular pathname part
selecting=1 # 1 for select matching, 0 for reject matching
g.filterBy(PathPartPredicate('regexp', Pathname.B_PART),selecting)
# if you want to not thin down this original group use the clone command
gclone = g.clone()
gclone.filterBy('COS')
# or there is a short cut function defined called find
gnew = find(g,'COS')
#
# this is a reference. It contains the servername, pathname, filename
# and optionally the timewindow and the time interval
ref=g[0]
# show me the server name
print 'Servername = ', ref.getServername()
# show me the file name
print 'Filename = ', ref.getFilename();
# show me the pathname
print 'Pathname = ', ref.getPathname();
# the reference contains the data set or the values
ds = ref.getData();
# in addition a reference can also have a time interval and a time window
print 'Time Window = ', ref.getTimeWindow();
# the time interval
print 'Time Interval = ', ref.getTimeInterval();
# to create a reference with a new time window..
# first create a time window
tw = timewindow('01JAN1990 0100 - 01JAN1992 0100')
ref_tw = DataReference.create(ref,tw);
print 'Time Window of old reference is ', ref.getTimeWindow()
print 'Time Window of new reference is ', ref_tw.getTimeWindow()
# to create a reference with a new time interval (only if the reference
# represents a regular time - series
ref_per_avg_1day = per_avg(ref,'1day')
ref_per_avg_1month = per_avg(ref,'1mon') 
# if you don't specify second arg its taken to be == '1month'
# similarily there is also a per_min and per_max for the minimum value in
# period and maximum value in period.

# to create a reference from a data set we need a servername, a filename and
# a pathname and the data set.
DSSUtil.createReference('myServer','myFile.dss','MY/PATH/NAME/////',ds)
# to save the reference back to the database it came from 
storeFlags = 1 # 1 means store flags, 0 means don't store flags
userId = 'xyz' # a user id name recognized by the database server
DSSUtil.updateData(ref, 1, userId)
# moreover you will also be asked for the password if the server requires one

# in addition the basic math functions are defined for references.
# these math operations decide on a predefined way of figuring out
# the pathnames. For more control on the pathname do the math operations
# on the data sets themselves ( ref.getData(), etc..)
ref12 = ref1 + ref2
ref1s = ref1 + 5.6
# tabulate and plot functions are available 
# these functions take variable number of arguments ( "," separated)
plot(ref12, ref1s)
tabulate(ref12,ref1s)

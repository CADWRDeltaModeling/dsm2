__doc__="""
vchecker:
This module contains the functions for checking data based on its value, rate of
change, quality flag values etcetra.
"""
# clears missing_value flags on non-missing values to unscreened null and 
def checkFlags(ref):
    """
    checkFlags(ref):
    clears missing value flags on non-missing values to unscreened null and returns
    the reference if any flags were cleared or None if no flags had to be cleared
    """
    # get the filter for missing values
    filter = Constants.DEFAULT_FILTER
    # get the data
    if hasattr(ref,'getPathname'):
	ds = ref.getData()
    else:
	ds = ref
    # get the iterator on the data
    dsi = ds.getIterator()
    # a flag to check if any flag was cleared
    clearedFlag=0
    # while not at the end of data do...
    while not dsi.atEnd():
        # get the data element at the current position
        e = dsi.getElement()
        # if value is acceptable and flagged as missing then clear its flags
        if filter.isAcceptable(e) and \
           (FlagUtils.getQualityFlag(e) == FlagUtils.MISSING_FLAG) :
            FlagUtils.clearAllFlags(e,0)
            clearedFlag = 1
            dsi.putElement(e) # put the element so cleared into the data set
            print ds.getName() + " Cleared data @: " + e.getXString() + " : " + e.getYString()
        dsi.advance() # move to next value
    # end the while loop
    # update the dss files if flags have been cleared
    if not clearedFlag:
        return None
    else:
        return ref
# end of checkFlags
def checkRange(ref, min, max, log = 'checkrange.log'):
    """
    checkRange(ref, min, max, log = 'checkrange.log'):
    checks the range of values to lie within min to max. For the values
    outside this range, they are marked as reject. All values marked
    as reject are cached in the log file
    """
    # get the filter for missing values
    filter = Constants.DEFAULT_FILTER
    # get the data
    if hasattr(ref,'getPathname'):
	ds = ref.getData()
    else:
	ds = ref
    # get the iterator on the data
    dsi = ds.getIterator()
    # a flag to check if any flag was cleared
    clearedFlag=0
    # get user id for setting flags
    uId = DSSUtil.getUserId();
    # open log file
    logfile = open(log,'w')
    # while not at the end of data do...
    while not dsi.atEnd():
        # get the data element at the current position
        e = dsi.getElement()
        # if value is acceptable and flagged as missing then clear its flags
        if filter.isAcceptable(e) :
            if e.y < min or e.y > max :
                FlagUtils.setQualityFlag(e,FlagUtils.REJECT_FLAG,uId)
                clearedFlag = 1
                dsi.putElement(e) # put the element so cleared into the data set
                logfile.write('\n' +ds.getName() + " Rejected data @: " +
                              e.getXString() + " : " + e.getYString())
        dsi.advance() # move to next value
    # end the while loop
    logfile.close()
    if clearedFlag:
        return ref
    else:
        return None
#
def checkDiff(ref, min, max, log = 'checkslope.log'):
    """
    checkDiff(ref, min, max, log = 'checkdiff.log'):
    checks the range of 1st difference values to lie within min to max. For the values
    outside this range, they are marked as reject. All values marked
    as reject are cached in the log file
    """
    # get the filter for missing values
    filter = Constants.DEFAULT_FILTER
    # get the data
    if hasattr(ref,'getPathname'):
	ds = ref.getData()
    else:
	ds = ref
    # get the iterator on the data
    dsi = ds.getIterator()
    # a flag to check if any flag was cleared
    clearedFlag=0
    # get user id for setting flags
    uId = DSSUtil.getUserId();
    # open log file
    logfile = open(log,'w')
    # previous value
    have_previous_value = 0
    previous_value=0.0
    diff = 0.0
    # while not at the end of data do...
    while not dsi.atEnd():
        # get the data element at the current position
        e = dsi.getElement()
        # if value is acceptable and flagged as missing then clear its flags
        if filter.isAcceptable(e) :
            if have_previous_value:
                diff = previous_value - e.y
                if diff < min or diff > max :
                    FlagUtils.setQualityFlag(e,FlagUtils.REJECT_FLAG,uId)
                    clearedFlag = 1
                    dsi.putElement(e) # put the element so cleared into the data set
                    logfile.write('\n' +ds.getName() + " Rejected data @: " +
                                  e.getXString() + " : " + e.getYString())
            have_previous_value = 1
            previous_value = e.y
        else:
            have_previous_value = 0
        dsi.advance() # move to next value
    # end the while loop
    logfile.close()
    if clearedFlag:
        return ref
    else:
        return None
#
def display_missing(ds):
    """
    display_missing(ds)
    where
    ds is a data set or time series
    displays missing value ranges for given time series
    """
    dsi = ds.getIterator()
    while not dsi.atEnd():
	el = dsi.getElement()
	begin_date = None
	while not Constants.DEFAULT_FLAG_FILTER.isAcceptable(el):
	    if begin_date == None: begin_date = el.getXString()
	    end_date = el.getXString()
	    dsi.advance()
	    if dsi.atEnd():
		break
	    el = dsi.getElement()
	    #print el
	if begin_date != None:
	    print 'Missing for %s to %s'%(begin_date,end_date)
	if dsi.atEnd(): break
	dsi.advance()
#
def diff(rts1,rts2,outfile=None):
    '''
    diff(rts1,rts2,outfile=None):
    Prints to stdout the differences between rts1 and rts2 to
    outfile or if outfile is None to standard out. When writing
    to file it appends to existing outfile if any.
    '''
    if outfile ==None:
	fh=sys.stdout
    else:
	fh=open(outfile,'a+')
    if rts1.getTimeInterval().compare(rts2.getTimeInterval()) !=0 :
	raise "Incompatible time intervals for %s and %s"%(rts1.getName(),rts2.getName())
    tw = rts1.getTimeWindow()
    if not tw.isSameAs(rts2.getTimeWindow()):
	fh.write('TimeWindow for %s is %s & %s is %s\n'\
		 %(rts1.getName(),str(rts1.getTimeWindow()),\
		   rts2.getName(),str(rts2.getTimeWindow())))
	tw = tw.intersection(rts2.getTimeWindow())
    if tw == None:
	raise "No intersecting time window for %s and %s"%(rts1.getName(),rts2.getName())
    else:
	rts1 = rts1.createSlice(tw)
	rts2 = rts2.createSlice(tw)
    dsi1 = rts1.getIterator()
    dsi2 = rts2.getIterator()
    while not dsi1.atEnd():
	e1 = dsi1.getElement()
	e2 = dsi2.getElement()
	if e1.y != e2.y:
	    fh.write('Value difference @ %s , 1: %f , 2: %f\n'\
		     %(e1.getXString(),e1.y,e2.y))
	if e1.flag != e2.flag:
	    fh.write('Flag difference @ %s , 1: %s, 2: %s\n'\
		     %(e1.getXString(), e1.getFlagString(), e2.getFlagString()))
	dsi1.advance()
	dsi2.advance()
#

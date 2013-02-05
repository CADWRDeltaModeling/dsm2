#!/site/bin/env /site/scripts/vscript 
# clears missing_value flags on non-missing values to unscreened null and 
#
# argument:
# arg1: file of pathnames to scan flags
def getReference(url,dssfile,path):
    g=opendss(dssfile,url)
    if g == None:
        raise "DSS file" + url + ":" + dssfile + " is empty"
    g.filterBy(path)
    if g == None:
        raise "DSS file" + url + ":" + dssfile + " has no data for path = " + path
    if len(g) != 1:
        raise "DSS file" + url + ":" + dssfile + " has too many paths for path = " + path
    return DataReference.create(g[0])
#
def clearNonMissingValueFlags(ref):
    # get the filter for missing values
    filter = Constants.DEFAULT_FILTER
    # get the data
    ds = ref.getData()
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
            print ref.getPathname().toString() + " Cleared data @: " + e.getXString() + " : " + e.getYString()
        dsi.advance() # move to next value
    # end the while loop
    # update the dss files if flags have been cleared
    if not clearedFlag:
        return
    # if not local return
    if ref.getServername() != "local" :
        print "Can't update remote server, only to be used for local dss files"
        return
    # get filename and pathname as string
    dssfile = ref.getFilename()
    pathname = ref.getPathname().toString()
    # write the data set to the dssfile with given pathname
    DSSUtil.writeData(dssfile, pathname, ds)
# end of clearNonMissingValueFlags...

import string,re
# open file
f = open('scandata.data')
# for each line in all the lines
for line in f.readlines():
    # each line should have comma-separated URL, DSS filename, and DSS pathname
    # split line at every , and make sure you have a last comma after all the data
    splits = string.split(line,",")
    url = splits[0]
    dssfile = splits[1]
    path = splits[2]
    path=re.sub('\+','\+',path)
    path=re.sub('\/\/','/.*/',path)
    ref = getReference(url,dssfile,path) # get the reference
    if not ref: continue
    clearNonMissingValueFlags(ref) # scan and clear out
# end of path lines

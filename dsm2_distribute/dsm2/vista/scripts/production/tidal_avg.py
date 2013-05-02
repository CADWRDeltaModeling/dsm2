# gets all data references for the matching b and c parts and
# optional e,f and a parts
def getReferences(dssfile,bp,cp,ap=None,ep=None,fp=None):
    g=opendss(dssfile)
    if g == None:
        raise "DSS file" + dssfile + " is empty"
    g.filterBy(PathPartPredicate("^"+bp+"$",Pathname.B_PART),1)
    if g == None:
        raise "DSS file" + dssfile + " has no data for b part = " + bp
    g.filterBy(PathPartPredicate("^"+cp+"$",Pathname.C_PART),1)
    if g == None:
        raise "DSS file" + dssfile + " has no data for c part = " + cp
    if ap != None:
        g.filterBy(PathPartPredicate("^"+ap+"$",Pathname.A_PART),1)
    if g == None:
        raise "DSS file" + dssfile + " has no data for a part = " + ap
    if fp != None:
        g.filterBy(PathPartPredicate("^"+fp+"$",Pathname.F_PART),1)
    if g == None:
        raise "DSS file" + dssfile + " has no data for f part = " + fp
    if ep != None:
        g.filterBy(PathPartPredicate("^"+ep+"$",Pathname.E_PART),1)
    if g == None:
        raise "DSS file" + dssfile + " has no data for e part = " + ep
    return g.getAllDataReferences()
# calculate 25 hour moving average
def tidal_avg(input_file):
    import string
    from vista.set import MovingAverageProxy
    # open file
    f = open(input_file)
    # for each line in all the lines
    for line in f.readlines():
        # split line at every , and make sure you have a last comma after all the data
        splits = string.split(line,",")
        if len(splits) != 4:
            continue
        dssfile = splits[0]
        bpart = string.upper(string.strip(splits[1]))
        cpart = string.upper(string.strip(splits[2]))
        refs = getReferences(dssfile,bpart,cpart) # get all the references
        if not refs:
            print "No data found for " + cpart + " @ " + bpart
	    continue
        # do moving average for each and write out to calc.dss file 
        for ref in refs:
            ep = ref.getPathname().getPart(Pathname.E_PART)
            if ep == '15MIN' :
                ma = MovingAverageProxy(ref,99,0)
            elif ep == '1HOUR' :
                ma = MovingAverageProxy(ref,24,0)
            else :
                print 'Time interval ' + ep + ' not suitable for tidal average'
                continue
            fp = ma.getPathname().getPart(Pathname.F_PART)
            DSSUtil.writeData('calc.dss',
                              '/COMP+CHAN/'+bpart+'/'+cpart+'//'+ep+'/'+fp+'/',
                              ma.getData())
# end of tidal_avg

import vutils
def diff(rts1,rts2):
    '''
    diff(rts1,rts2):
    Prints to stdout the differences between rts1 and rts2
    '''
    if rts1.getTimeInterval().compare(rts2.getTimeInterval()) !=0 :
	raise "Incompatible time intervals for %s and %s"%(rts1.getName(),rts2.getName())
    tw = rts1.getTimeWindow()
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
	    print 'Value difference @ %s , 1: %f , 2: %f'%(e1.getXString(),e1.y,e2.y)
	if e1.flag != e2.flag:
	    print 'Flag difference @ %s , 1: %s, 2: %s'%(e1.getXString(), e1.getFlagString(), e2.getFlagString())
	dsi1.advance()
	dsi2.advance()
#


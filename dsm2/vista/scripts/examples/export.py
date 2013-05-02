import vutils
from vista.set import NaNFilter,MultiIterator,CompositeFilter, Constants
#
def dump2text(outfile,rtsarray,filter=Constants.DEFAULT_FLAG_FILTER):
    """
    """
    xfilter = CompositeFilter([filter,NaNFilter()])
    mi = MultiIterator(rtsarray)
    fh = open(outfile,'w')
    for i in range(len(rtsarray)):
	fh.write('%s\n'%rtsarray[i].getName())
    while not mi.atEnd():
	#
	el = mi.getElement()
	fh.write(element2text(el,xfilter))
	#
	mi.advance()
    fh.close()
#
def element2text(el,xfilter):
    """
    """
    line = '%20s'%el.getXString()
    i=0
    while i < el.getDimension()-1:
	line = line +'%15.3f'%el.getY(i)
	line = line +'%20s'%el.getFlagString(i)
	i=i+1
    line = line + '\n'
    return line
#

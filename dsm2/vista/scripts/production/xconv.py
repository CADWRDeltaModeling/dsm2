import xplot
from xplot import *
def convert(file):
    fh = open(file)
    lines = map(string.strip,fh.readlines())
    fh.close()
    pdata = []
    for line in lines[1:]:
	sarray = string.split(line,',')
	pd = PlotData()
	ndata = 0
	if len(sarray) == 9:
	    ndata = 2
	elif len(sarray) == 12:
	    ndata = 3
	else:
	    raise "Incorrect # of fields: " + str(len(sarray)) + " in line: " + line
	for x in range(ndata):
	    index = x*2
	    pd.addRow([sarray[index],sarray[index+1],sarray[index+6]])
	#
	aindex = ndata*3
	pd.axisLabel = sarray[aindex]
	pd.title = sarray[aindex+1]
	pd.tw = sarray[aindex+2]
	pdata.append(pd)
    gd = xplot.GraphData('graph title',pdata)
    dot_index = file.rfind('.')
    if dot_index >=0 : file = file[:dot_index]
    gd.save(file+'.xml')
#
if __name__ == '__main__' or __name__ == 'main':
    import sys
    if len(sys.argv) < 2:
	print 'Usage: vscript xconv.py file.data'
	sys.exit(-1)
    for file in sys.argv[1:]:
	print 'Converting ' , file
	convert(file)
    sys.exit()
#

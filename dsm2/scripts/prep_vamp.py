import sys
from vista.set import RegularTimeSeries, Constants, DataType
from vista.time import TimeFactory
from vista.set import DataReference, DataSetAttr
from vdss import opendss,findpath,writedss
from vtimeseries import timewindow

START_DATE = '01OCT1974'
END_DATE   = '30SEP1991'

def outPaths(fpart):
	d = { 
	  'C639' : '/CALSIM-VAMP/C639/FLOW-CHANNEL/01OCT1974/1DAY/%s/' % fpart,
	  'D418' : '/CALSIM-VAMP/D418/FLOW-DELIVERY/01OCT1974/1DAY/%s/' % fpart,
	  'D419' : '/CALSIM-VAMP/D419/FLOW-DELIVERY/01OCT1974/1DAY/%s/' % fpart,
	  'VERNWQFINAL' : '/CALSIM/VERNWQFINAL/SALINITY-EC/01OCT1974/1DAY/%s/' % fpart
	}
	return d

def inputPaths(fpart):
	d = {
	  'C639' :
		[ '/CALSIM/C639/FLOW-CHANNEL/01JAN1920/1MON/%s/' % fpart,
		  '/CALSIM/C639CYCLE2/FLOW-CYCLE2/01JAN1920/1MON/%s/' % fpart,
		  '/CALSIM/C639CYCLE5/FLOW-CYCLE5/01JAN1920/1MON/%s/' % fpart],

	  'VERNWQFINAL' :
		[ '/CALSIM/VERNWQFINAL/SALINITY-EC/01JAN1920/1MON/%s/' % fpart,
		  '/CALSIM/VERNWQNONPULSEDV/SALINITY-EC/01JAN1920/1MON/%s/' % fpart,
		  '/CALSIM/VERNWQPULSEDV/SALINITY-EC/01JAN1920/1MON/%s/' % fpart ]
	}
	return d

def exportInputPaths(fpart):
	d = [ '/CALSIM/C639CYCLE4/FLOW-CYCLE4/01JAN1920/1MON/%s/' % fpart,
		  '/CALSIM/DINFLOW/INFLOW-PULSE/01JAN1920/1MON/%s/' % fpart,
		  '/CALSIM/D418/FLOW-DELIVERY/01JAN1920/1MON/%s/' % fpart,
		  '/CALSIM/D419/FLOW-DELIVERY/01JAN1920/1MON/%s/' % fpart,
		  '/CALSIM/EXPRATIO_/EI-RATIO-STD/01JAN1920/1MON/%s/' % fpart,
		  '/CALSIM/PULSEVAMPEXP/EXPORT/01JAN1920/1MON/%s/' % fpart,
		  '/CALSIM/PULSEEXPCTRL/EXPORT-CTRL-PULSE/01JAN1920/1MON/%s/' % fpart
		]
	return d

def getData(fn, fp, paths, timewindow):
	print '... Reading %s' % fn
	data = []
	for path in paths:
		print path
		ref=DataReference.create(findpath(fp, path)[0], timewindow)
		data.append(ref.getData())
	return data


def fillin(stime, etime, val):

	tf = TimeFactory.getInstance()
	i = tf.createTimeInterval('1DAY')
	stime = tf.createTime(stime)
	etime = tf.createTime(etime)

	d = []
	t = stime+i
	while (t.compare(etime) <= 0):
		#print t,val
		t += i
		d.append(val)
	return d

'''
### VAMP rules
April (1-14)  = PreVAMP  = Cycle 2							
April (15-30) = VAMP     = Cycle 5							
May(1-15)     = VAMP     = Cycle 5							
May(16-31)    = PostVAMP = Cycle 2							
'''
def deriveVAMP(data=[]):

	d = []
	final = data[0]
	cyc2 = data[1]
	cyc5 = data[2]

	stime = '30SEP1974 2400'
	#for i in range(1,len(final[:8])):
	for i in range(1,len(final)):
		date = final[i].getXString()
		val = final[i].getY()

		if date[2:5] == 'APR':
			# startof month
			val = cyc2[i].getY()
			m = fillin(stime, '14'+date[2:], val)

			# endof month
			val = cyc5[i].getY()
			m += fillin('14'+date[2:], date, val)

		elif date[2:5] == 'MAY':
			# startof month
			val = cyc5[i].getY()
			m = fillin(stime, '15'+date[2:], val)

			# endof month
			val = cyc2[i].getY()
			m += fillin('15'+date[2:], date, val)

		else:
			m = fillin(stime, date, val)

		stime = date
		d = d + m
	return d


def writeDSS(fn, path, stime, interval, data, ytype, yunits):
	print '... Writing %s\n      ' % fn, path, stime, interval, len(data)
	attr = DataSetAttr(DataType.REGULAR_TIME_SERIES,"TIME",yunits,"",ytype)
	rts = RegularTimeSeries("",stime, interval, data, None, attr)
	writedss(fn,path,rts)


def getData2(fn, fp, paths, timewindow):
	print '... Reading %s' % fn
	data = {}
	for path in paths:
		key = path.split('/')[2]
		#print path,key
		ref=DataReference.create(findpath(fp, path)[0], timewindow)
		data[key] = ref.getData()
	return data


def tracyVAMP(PulseVampExport, D418, month='MAY'):

	preFinalPulse = max(800.,0.5*PulseVampExport.getY())
	FinalPulse = min(preFinalPulse, D418.getY())

	if month == 'APR':
		NonPulse = min( (D418.getY()-(FinalPulse*16/30.))*30/14. , 4600.)
	elif month == 'MAY':
		NonPulse = min( (D418.getY()-(FinalPulse*15/31.))*31/16. , 4600.)

	return NonPulse,FinalPulse


def banksVAMP(PulseVampExport, D419, month='MAY'):

	if PulseVampExport.getY() == 1500.:
		preFinalPulse = 700.
	else:
		preFinalPulse = 0.5*PulseVampExport.getY()
	FinalPulse = min(preFinalPulse, D419.getY())

	if month == 'APR':
		NonPulse = min( (D419.getY()-(FinalPulse*16/30.))*30/14. , 6680.)

	elif month == 'MAY':
		NonPulse = min( (D419.getY()-(FinalPulse*15/31.))*31/16. , 6680.)

	return NonPulse,FinalPulse


def deriveExportVAMP(data={}, part='D418'):

	PulExpRes = data['PULSEVAMPEXP']
	node = data[part]

	stime = '30SEP1974 2400'

	d = []
	for i in range(1,len(node)):
		date = node[i].getXString()
		val = node[i].getY()

		if date[2:5] == 'APR':

			if part == 'D418':
				NonPulse, Pulse = tracyVAMP(PulExpRes[i],node[i],'APR')
			elif part == 'D419':
				NonPulse, Pulse = banksVAMP(PulExpRes[i],node[i],'APR')
			#print NonPulse,Pulse

			m = fillin(stime, '14'+date[2:], NonPulse)
			m += fillin('14'+date[2:], date, Pulse)

		elif date[2:5] == 'MAY':

			if part == 'D418':
				NonPulse, Pulse = tracyVAMP(PulExpRes[i],node[i],'MAY')
			elif part == 'D419':
				NonPulse, Pulse = banksVAMP(PulExpRes[i],node[i],'MAY')
			#print NonPulse,Pulse

			m = fillin(stime, '15'+date[2:], Pulse)
			m += fillin('15'+date[2:], date, NonPulse)

		else:
			m = fillin(stime, date, val)

		stime = date
		d = d + m
	return d


def process(infile, outfile, fpart):

	fp=opendss(infile)           # open CALSIM file
	dateSpan = START_DATE+" 0000 - "+END_DATE+" 2400"
	tw=timewindow(dateSpan)

	outpath = outPaths(fpart);

	# C639, VERNWQFINAL
	bparts = ['C639','VERNWQFINAL']
	paths = inputPaths(fpart)
	for part in bparts:
		data = getData(infile, fp, paths[part], tw)
		d = deriveVAMP(data)
		if part == 'VERNWQFINAL':
			yunits = 'UMHOS/CM'
		else:
			yunits = 'CFS'
		writeDSS(outfile, outpath[part], '01OCT1974 2400', '1DAY', d, 'PER-AVER', yunits)

	# EXPORTS
	bparts = ['D418','D419']
	paths = exportInputPaths(fpart)
	data = getData2(infile, fp, paths, tw)

	for part in bparts:
		d = deriveExportVAMP(data,part)
		writeDSS(outfile, outpath[part], '01OCT1974 2400', '1DAY', d, 'PER-AVER', 'CFS')

def guessFpart(indss):
	#dss/SDIP_2020D09DDV_Base_12_1_3-14-03.dss
	fpart = ''
	if indss.find('2001') > -1:
		fpart = '2001D10A'
	elif indss.find('2020') > -1:
		fpart = '2020D09D'
	return fpart
 

use = '''
Usage: 
vscript prepVAMP.py configfile
- OR -
vscript.bat prepVAMP.py -c <-f fpart> [indss] <outdss> 

	configfile  the input file for configuration variables

	-c          command line version
	-f          <fpart> optional fpart designation, otherwise makes a guess

    indss   dss containing C639,D418,D419,VERNWQFINAL,etc data
    outdss  output dss file name, if not specified, VAMP.dss

    NOTE:  Assumes Action 3 is on.
    VERSION: [050913]

'''
if __name__ == '__main__':
	import getopt

	def usage():
		sys.stderr.write(use)
		sys.exit(1)

	try:
		(opts, args) = getopt.getopt(sys.argv[1:], 'cf:')
	except getopt.error:
		usage()

	fpart = ''
	cmdversion = 0
	for (opt,val) in opts:
		if opt == '-h':
			usage()
		elif opt == '-c':
			cmdversion = 1
		elif opt == '-f':
			fpart = val
		else:
			raise OptionError, opt
			usage()
	files = args

	if not files:
		usage()

	if not cmdversion:
		import config

		infile = files[0]
		config.setConfigVars(infile)
		print "... Generating VAMP info ..."

		indss = config.getAttr("CALSIM")
		fpart = config.getAttr("CALSIMSTUDY")
		outdss = config.getAttr("CALSIM-VAMP")
		print "Calsim input file: %" % indss
	        print "F-Part: %s" % fpart
		print "Processed output: %s" % outdss
		process(indss, outdss, fpart)
		sys.exit()

	indss = files[0]
	if len(files) == 2:
		outdss = files[1]
	else:
		outdss = 'VAMP.dss'

	if not fpart:
		fpart = guessFpart(indss)
	print fpart

	process(indss, outdss, fpart)
	sys.exit()


from java.lang import Runtime
from java.io import LineNumberReader, InputStreamReader
def system(cmd):
	"""
	system(cmd): executes cmd in a shell
	Jpython currently lacks a system command in its os module. This is
	a temporary filler in till a better one comes along the way
	"""
	r= Runtime.getRuntime()
	try:
		p = r.exec(cmd)
		p.waitFor()
	except:
		raise 'Error executing shell command: ' + cmd
	lnr_err = LineNumberReader(InputStreamReader(p.getErrorStream()))
	err_lines = []
	while 1:
		line_err = lnr_err.readLine()
		if not line_err:
			break
		else:
			print line_err
	#
	lnr = LineNumberReader(InputStreamReader(p.getInputStream()))
	#lines = []
	while 1:
		line = lnr.readLine()
		if not line:
			break
		else:
			print line
			#lines.append(line)
	#return lines
# exits from the interactor

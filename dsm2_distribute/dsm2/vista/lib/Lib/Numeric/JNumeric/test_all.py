print 'Starting Testing Suite'

import sys

if len(sys.argv) == 2:
	test_file = sys.argv[1]
else:
	test_file = "test_items.py"

lineno = 0
	
def test_lines(lines, expected_outs=None):
        global lineno
	last_out = ""
	start_index = 0
	for i in range(len(lines)):
		if lines[i][:3] == "***":
			start_index = i+1

	for i in range(len(lines)):
	        lineno = lineno + 1
		line = lines[i]
                #print "line =",line
		if line[:3] == "***": continue

		if line[0] == ">":
			#print line, line[1:-1]
			try: test = repr(eval(line[1:]))
			except: test = line[1:-1]
			if test != last_out:
				print type(last_out)
				print len(last_out), len(test), last_out, test
			        print '*** Test line', lineno,'failed'
				raise ValueError, "%s doesn't match expected %s" % (last_out, line[1:-1])
			continue

		try:
			try:
				out = eval(line)
			except(ValueError, IndexError, TypeError, ZeroDivisionError):
				out = "%s: %s" % (sys.exc_type, sys.exc_value)
				#print out
			
			if out is None:
				exec(line)
			else:
				if type(out) != type(""):
					out = repr(out)
				else:
					try: out = repr(eval(out))
					except: pass
				#print out
				last_out = out
				if i > start_index:
					print last_out

		except(SyntaxError):
			exec(line)

fp = open(test_file)
test_lines(fp.readlines())
fp.close()

print
print
print "*"*10+" Test Completed Successfully! "+"*"*10

#sys.exit()


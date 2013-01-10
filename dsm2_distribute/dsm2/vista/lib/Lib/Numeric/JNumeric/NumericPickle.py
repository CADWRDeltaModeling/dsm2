# NumericPickle
    
# These two functions are used in pickle.py so that
# matrices can be pickled.  Notice that matrices are written in 
# binary format for efficiency, but that they pay attention to
# byte-order issues for  portability.

from Numeric import *
import string

def DumpArray(m, fp):    
	s = m.shape
	if LittleEndian: endian = "L"
	else: endian = "B"
	fp.write("A%s%s%d " % (m.typecode(), endian, m.itemsize()))
	for d in s:
		fp.write("%d "% d)
	fp.write('\n')
	fp.write(m.tostring())

def LoadArray(fp):
	ln = string.split(fp.readline())
	if ln[0][0] == 'A': ln[0] = ln[0][1:] # Nasty hack showing my ignorance of pickle
	typecode = ln[0][0]
	endian = ln[0][1]
	
	shape = map(lambda x: string.atoi(x), ln[1:])
	itemsize = string.atoi(ln[0][2:])

	sz = reduce(multiply, shape)*itemsize
	data = fp.read(sz)
		
	m = fromstring(data, typecode)
	m = reshape(m, shape)

	if (LittleEndian and endian == 'B') or (not LittleEndian and endian == 'L'):
		return m.byteswapped()
	else:
		return m

import pickle, copy
class Unpickler(pickle.Unpickler):
	def load_array(self):
		self.stack.append(LoadArray(self))
	
	dispatch = copy.copy(pickle.Unpickler.dispatch)	
	dispatch['A'] = load_array

class Pickler(pickle.Pickler):
	def save_array(self, object):
		DumpArray(object, self)

	dispatch = copy.copy(pickle.Pickler.dispatch)		
	dispatch[ArrayType] = save_array

#Convenience functions
from StringIO import StringIO

def dump(object, file):
	Pickler(file).dump(object)

def dumps(object):
	file = StringIO()
	Pickler(file).dump(object)
	return file.getvalue()

def load(file):
	return Unpickler(file).load()

def loads(str):
	file = StringIO(str)
	return Unpickler(file).load()
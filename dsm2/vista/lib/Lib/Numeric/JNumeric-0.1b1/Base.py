"""Python code in support of array's, umath, and numeric 
"""
# Copyright (c) 1998 Timothy Hochberg, tim.hochberg@ieee.org

# TODO
# * Add doc strings to all functions.
# * Move PythonMultiarray functions into Multiarray.
# * Test Pickling stuff -- ask DA.
# * Once printing bugs in JPython are fixed, universalize ArrayPrinter and send to Numeric list.

# Import the Multiarray implementation
import sys

from JNumeric import PyMultiarray
import PythonMultiarray
# Import the universal math functions.
from JNumeric.umath import *

#
# Define the core Numeric functions.
#

def arrayrange(start, stop=None, step=1, typecode='\0'):
    return PyMultiarray.arrayRange(start, stop, step, typecode)

def argmax(a, axis=-1):
    return argmax_.reduce(a, axis)
    
def argsort(a, axis=-1):
    """This is implemented in python on top of list.sort and could be really slow!"""
    return PythonMultiarray.argsort(a, axis)
    
def argmin(a, axis=-1):
    return argmin_.reduce(a, axis)

def array(*args):
    """Create a Numeric array."""
    return apply(PyMultiarray.array, args)

def asarray(*args):
    return apply(PyMultiarray.asarray, args)

def bitwise_not(a):
    return asarray(a).__not__()
    
def choose(a, indices):
    return PyMultiarray.choose(a, indices)
    
def clip(a, a_min, a_max):
    """clip(a, a_min, a_max) = elements m or m_min/m_max if m is outside [m_min,m_max]"""
    return choose(less(a, a_min)+2*greater(a, a_max), (a, a_min, a_max))
    
def compress(condition, a, dimension=-1):
    """compress(condition, x, dimension=-1) = elements of x where condition is true.""" 
    return take(a, nonzero(condition), dimension)

def concatenate(a, axis=0):
    return PyMultiarray.concatenate(a, axis)
    
def convolve(a, b, mode=0): 
    return PyMultiarray.convolve(a, b, mode)
    
def diagonal(a, offset=0, axis=-2):
    return PyMultiarray.diagonal(a, offset, axis)

def dot(a, b, axisA=-1, axisB=0): 
    return PyMultiarray.innerProduct(a, b, axisA, axisB)

def fromfunction(function, dimensions):
    return PyMultiarray.fromFunction(function, dimensions)
    
def fromstring(string, typecode):
    return PyMultiarray.fromString(string, typecode)

def identity(n):
    return resize([1]+n*[0], (n,n))
    
def indices(dimensions, typecode=None):
    return PyMultiarray.indices(dimensions, typecode)
    
def innerproduct(a, b, axisA=-1, axisB=-1): 
    return PyMultiarray.innerProduct(a, b, axisA, axisB)
    
def nonzero(a):
    """Return the indices of the elements of a which are not zero, a must be 1d"""
    return repeat(arange(len(a)), not_equal(a, 0))
    
def ones(*args):
    """Like zeros only returns ones instead."""
    return apply(zeros, args) + array(1, '1')

def ravel(a):
    """Returns a 1d array corresponding to all the elements of it's argument."""
    return reshape(a, (-1,))
        
def reshape(a, shape):
    return PyMultiarray.reshape(a, shape)
    
def resize(a, new_shape):
    """Returns a new array with the specified (arbtirary) shape."""
    return PyMultiarray.resize(a, new_shape)
    
def repeat(a, repeats, axis=0): 
    return PyMultiarray.repeat(a, repeats, axis)    

def searchsorted(a, values):
    """This is implemented in python on top of list.sort and could be really slow!"""
    return PyMultiarray.searchSorted(a, values)    

def shape(o): 
    return tuple(PyMultiarray.shapeOf(o))
        
def sort(a, axis=-1):
    """This is implemented in python on top of list.sort and could be really slow!"""
    return PythonMultiarray.sort(a, axis)

def take(a, indices, axis=0): 
    return PyMultiarray.take(a, indices, axis)

def trace(a, offset=0, axis1=-2, axis2=-1):
	return add.reduce(diagonal(a, offset, axis1, axis2), -1)
    
def transpose(a, axes=None): 	
    return PyMultiarray.transpose(a, axes or arange(len(shape(a)))[::-1])
    
def where(condition, x, y):
	"""where(condition,x,y) has elements of x where condition is true otherwise y.""" 
	return choose(not_equal(condition, 0), (y, x))
    
def zeros(*args):
    return apply(PyMultiarray.zeros, args)

#
# Define some constants and abbreviations.
#

Int8, Int16, Int32, Int64 = '1', 's', 'i', 'l'
# I'm using Int32 here because that is the native JPython integer type and the default type for
# an integer multiarray. The documentation claim's this should be "the largest version of the 
# given type," but I feel this is more natural. This will have to be hashed out.
Int = Int32
Float32, Float64 = 'f', 'd'
Float = Float64
Complex64, Complex128 = 'F', 'D'
Complex = Complex128

ArrayType = type(PyMultiarray.array([]))
NewAxis = None
LittleEndian = fromstring("\001"+"\000"*7, 'i')[0] == 1

sum = add.reduce
cumsum = add.accumulate
product = multiply.reduce
cumproduct = multiply.accumulate
alltrue = logical_and.reduce
sometrue = logical_or.reduce

from math import pi
from math import e

arange = arrayrange

#
# Use Konrad Hinsens's printing function for str and repr.
#

from ArrayPrinter import array2string

def array_repr(a, max_line_width = None, precision = None, suppress_small = None):
	return array2string(a, max_line_width, precision, suppress_small, ', ', 1)

def array_str(a, max_line_width = None, precision = None, suppress_small = None):
	return array2string(a, max_line_width, precision, suppress_small, ' ', 0)

PyMultiarray.set_string_function(array_str, 0)
PyMultiarray.set_string_function(array_repr, 1)

#
# Import pickling support functions.
#

#from NumericPickle import *



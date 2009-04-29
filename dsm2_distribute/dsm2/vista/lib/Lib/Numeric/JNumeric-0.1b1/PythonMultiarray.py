# PythonMultiarray
# These functions will be moved to PyMultiarray when we hit Java 1.2
import math
from JNumeric.umath import *
from JNumeric.PyMultiarray import *

# Just a utility function.
def swapaxes(a, axis1, axis2):
	n = len(tuple(shapeOf(a)))
	if n <= 1: return a
	new_axes = arrayRange(0, n, 1, 'i')
	new_axes[axis1] = axis2
	new_axes[axis2] = axis1
	return transpose(a, new_axes)        

# These need to wait till Java 1.2 to move into java
    
def sort(a, axis=-1):
    axes = len(tuple(shapeOf(a)))
    if axis < 0: axis = axis + axes
    a = swapaxes(a, axis, axes-1) 
    shape = tuple(shapeOf(a))
    a = reshape(a, (-1, shape[-1]))
    for i in range(len(a)):
        l = a[i].tolist()
        l.sort()
        a[i] = l
    a = reshape(a, shape)
    return swapaxes(a, axis, axes-1) 
    
def argsort(a, axis=-1):
    axes = len(tuple(shapeOf(a)))
    if axis < 0: axis = axis + axes
    a = swapaxes(a, axis, axes-1)
    shape = tuple(shapeOf(a))
    a = reshape(a, (-1, shape[-1]))
    args = zeros(a.shape)
    for i in range(len(a)):
        l = map(None, a[i].tolist(), arrayRange(0, shape[-1], 1, 'i'))
        l.sort()
        args[i] = map(lambda a : a[1], l)
    args = reshape(args, shape)
    return swapaxes(args, axis, axes-1) 

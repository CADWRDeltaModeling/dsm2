"""This is a very incomplete implementation of an matrix type, adapted from that
in CNumeric. This is included more as a demonstration of subclassing as anything 
else."""
from Numeric import innerproduct, PyMultiarray, asarray, array

class Matrix(PyMultiarray):
    def __mul__(self, other): return innerproduct(self, other)
    def __rmul__(self, other): return innerproduct(other, self)
    def __pow__(self, other): raise TypeError, "x**y not implemented for matrices x"
    def __rpow__(self, other): raise TypeError, "x**y not implemented for matrices y"

if __name__ == '__main__':
	from Numeric import *
	m = Matrix( [[1,2,3],[11,12,13],[21,22,23]])
        a = array([[1,2,3],[11,12,13],[21,22,23]])
	print m*m
        print innerproduct(a,a)
	print asarray(m)*asarray(m)
        print a*a
	print transpose(m)
        print transpose(a)

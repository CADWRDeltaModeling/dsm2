# Hacked up by Tim Hochberg August 1998 to test out JAMA w/ Numeric

# This module is a lite version of LinAlg.py module which contains
# high-level Python interface to the LAPACK library.  The lite version
# only accesses the following LAPACK functions: dgesv, zgesv, dgeev,
# zgeev, dgesvd, zdesvd, dgelss, zgelss.

import Numeric
#import copy
#import lapack_lite
import sys
sys.add_package("Jama")
import Jama.Matrix

# Error object
LinAlgError = 'LinearAlgebraError'

# Helper routines
_lapack_type = {'f': 0, 'd': 1, 'F': 2, 'D': 3}
_lapack_letter = ['s', 'd', 'c', 'z']
_array_kind = {'i':0, 'l': 0, 'f': 0, 'd': 0, 'F': 1, 'D': 1}
_array_precision = {'i': 1, 'l': 1, 'f': 0, 'd': 1, 'F': 0, 'D': 1}
_array_type = [['f', 'd'], ['F', 'D']]

def _commonType(*arrays):
    kind = 0
#    precision = 0
#   force higher precision in lite version
    precision = 1
    for a in arrays:
        t = a.typecode()
        kind = max(kind, _array_kind[t])
        precision = max(precision, _array_precision[t])
    return _array_type[kind][precision]

def _castCopyAndTranspose(type, *arrays):
    cast_arrays = ()
    for a in arrays:
        if a.typecode() == type:
            cast_arrays = cast_arrays + (Numeric.array(Numeric.transpose(a)),)
        else:
            cast_arrays = cast_arrays + (Numeric.array(
                                       Numeric.transpose(a).astype(type)),)
    if len(cast_arrays) == 1:
            return cast_arrays[0]
    else:
        return cast_arrays

def _assertRank2(*arrays):
    for a in arrays:
        if len(a.shape) != 2:
            raise LinAlgError, 'Array must be two-dimensional'

def _assertSquareness(*arrays):
    for a in arrays:
        if max(a.shape) != min(a.shape):
            raise LinAlgError, 'Array must be square'
    

def solve_linear_equations(a, b):
    one_eq = len(b.shape) == 1
    if one_eq:
        b = b[:, Numeric.NewAxis]
    _assertRank2(a, b)
    _assertSquareness(a)
    n_eq = a.shape[0]
    n_rhs = b.shape[1]
    if n_eq != b.shape[0]:
        raise LinAlgError, 'Incompatible dimensions'
    t =_commonType(a, b)
    a, b = _castCopyAndTranspose(t, a, b) # Copying not required here
    if _array_kind[t] == 1:
        raise LinearAlgebraError, "Complex matrices not yet supported."
    else:
        ma = Jama.Matrix(Numeric.ravel(a), a.shape[1])
        mb = Jama.Matrix(Numeric.ravel(b), b.shape[1])
        mc = ma.solve(mb)
        c = Numeric.reshape(Numeric.array(mc.getRowPackedCopy()), (mc.getRowDimension(), -1))
    if one_eq:
        return Numeric.array(Numeric.ravel(c))
    else:
        return copy.copy(Numeric.transpose(c))
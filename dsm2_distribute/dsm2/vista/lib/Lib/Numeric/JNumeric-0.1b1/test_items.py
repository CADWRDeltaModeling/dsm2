import sys
from Numeric import *


# Another dot test.

a = reshape(arange(24), (1,2,3,4))
b = reshape(arange(4), (4,1))
dot(a,b)
>array([       [[[ 14],[ 38],[ 62]],[[ 86],[110],[134]]]])


# Test broadcasting

a = arange(5)

a[NewAxis,] + a
>array([       [0, 2, 4, 6, 8]])

a[:,NewAxis]+a
>array([[0, 1, 2, 3, 4],[1, 2, 3, 4, 5],[2, 3, 4, 5, 6],[3, 4, 5, 6, 7],[4, 5, 6, 7, 8]])

a[:,NewAxis,NewAxis]+a
>array([[        [0, 1, 2, 3, 4]],[        [1, 2, 3, 4, 5]],[        [2, 3, 4, 5, 6]],[        [3, 4, 5, 6, 7]],[     [4, 5, 6, 7, 8]]])

a[:,NewAxis] + (2,2)
>array([[2, 2],[3, 3],[4, 4],[5, 5],[6, 6]])

s = (3,2,5,1,4,0)
sm = [s, array(s)[::-1]]

sort( s )
>array([0, 1, 2, 3, 4, 5], 'i')

argsort( s )
>array([5, 3, 1, 0, 4, 2], 'i')

sort(sm, axis=-1)
>array([[0, 1, 2, 3, 4, 5], [0, 1, 2, 3, 4, 5]], 'i')


# Test convolve
# These results are from CNumeric, which don't match those from JNumeric
# I think CNumeric is wrong thought TAH. Someone should check me though.
#a = array([2, 8, 1, 4, 4, 3, 5, 3, 7])
#b = array([6, 1, 7])
#convolve(a, b)
#>array([27, 77, 38, 49, 62, 44, 82])
#convolve(a, b, 1)
#array([58, 27, 77, 38, 49, 62, 44, 82, 25])
#convolve(a, b, 2)
#>array([14, 58, 27, 77, 38, 49, 62, 44, 82, 25, 42])

## TAH Add tests of accumulate.

#Creation from a nested sequence
array([1,2,3])
#>array([1, 2, 3], 'l')
>array([1, 2, 3], 'i')

array([1,2.3,4])
>array([1.0, 2.3, 4.0], 'd')

array([1,2j,3.])
>array([(1+0j), 2j, (3+0j)], 'D')

array([1,2,3], Float32)
>array([1.0, 2.0, 3.0], 'f')

array([[1,2],[11,12]])
>array([[1, 2], [11, 12]], 'i')


array([[],[]]).shape
>(2,0)


array([])
>array([], 'i')

#These are not just convenience functions
zeros( (2,3) )
>array([[0, 0, 0], [0, 0, 0]], 'i')

ones( (2,3), Int16)
>array([[1, 1, 1], [1, 1, 1]], 's')

#arrayrange now
arrayrange(7, step=2)
>array([0, 2, 4, 6], 'i')

arrayrange(0, 1.5, .5)
>array([0.0, 0.5, 1.0], 'd')

#Creation from String Data
fromstring("\001\002\003", '1')
>array([1, 2, 3], '1')


#Structural array operations
m = array([[1,2,3], [11,12,13]])
a = arange(6)

a[2:-1]
>array([2, 3, 4], 'i')

a[::-1]
>array([5, 4, 3, 2, 1, 0], 'i')


m[0,2]
>3

m[...,1]
>array([2, 12], 'i')

#take

take(a, (3,2,1,2,3))
>array([3, 2, 1, 2, 3], 'i')

take(m, (2,2,1,1), -1)
>array([[3, 3, 2, 2], [13, 13, 12, 12]], 'i')

#Rearranging array elements

reshape(m, (-1,2))
>array([[1, 2], [3, 11], [12, 13]], 'i')

reshape(a, (2,3))
>array([[0, 1, 2], [3, 4, 5]], 'i')

reshape(a, (4,-1))
>ValueError: total size of new array must be unchanged

reshape(a, (5,6))
>ValueError: total size of new array must be unchanged

ravel(m)
>array([1, 2, 3, 11, 12, 13], 'i')


a[..., NewAxis]
>array([[0], [1], [2], [3], [4], [5]], 'i')

transpose(m, (1,0))
>array([[1, 11], [2, 12], [3, 13]], 'i')

#Replicating and combining array elements

repeat(a, (0,1,2,3,0,0))
>array([1, 2, 2, 3, 3, 3], 'i')

repeat(a, (0,)*6)
>array([], 'i')

repeat(m, (0,2,1), -1)
>array([[2, 2, 3], [12, 12, 13]], 'i')

al = [0,1,2,3,4,5]

concatenate( (a[:3], a[3:]) )
>array([0, 1, 2, 3, 4, 5], 'i')

concatenate( (m, m) )
>array([[1, 2, 3], [11, 12, 13], [1, 2, 3], [11, 12, 13]], 'i')

concatenate( (m, m), axis=1)
>array([[1, 2, 3, 1, 2, 3], [11, 12, 13, 11, 12, 13]], 'i')

#arithmetic and logic operators
x = array([1,2], 'l')
y = zeros((2,), 'l')

x+x
>array([2, 4], 'l')

add(x,x)
>array([2, 4], 'l')

add(x,x,y)
>array([2, 4], 'l')

y
>array([2, 4], 'l')

x-x
>array([0, 0], 'l')

subtract(x,x)
>array([0, 0], 'l')

x*x
>array([1, 4], 'l')

multiply(x, x)
>array([1, 4], 'l')

x/x
>array([1, 1], 'l')

divide(x,x)
>array([1, 1], 'l')

x**x
>array([1, 4], 'l')

power(x,x)
>array([1, 4], 'l')

x%x
>array([0, 0], 'l')

remainder(x,x)
>array([0, 0], 'l')

#Need more math functions here...

#Non-elementwise operations
add.reduce(a)
>15

add.reduce(m, 0)
>array([12, 14, 16], 'i')

add.reduce(m, -1)
>array([6, 36], 'i')

add.reduce([1])
>1

add.reduce([])
>0

multiply.reduce([])
>1


add.outer(a[:3], a[:3])
>array([[0, 1, 2], [1, 2, 3], [2, 3, 4]], 'i')

dot(a,a)
>55

#conditional selection

choose(a, (5,4,3,2,1,0))
>array([5, 4, 3, 2, 1, 0], 'i')

choose([[1,0], [0,1]], (66, [(1,2),(11,12)]))
>array([[1, 66], [66, 12]], 'i')

# TAH
#sorting

s = (3,2,5,1,4,0)
sm = [s, array(s)[::-1]]

sort( s )
>array([0, 1, 2, 3, 4, 5], 'i')

argsort( s )
>array([5, 3, 1, 0, 4, 2], 'i')

sort(sm, axis=-1)
>array([[0, 1, 2, 3, 4, 5], [0, 1, 2, 3, 4, 5]], 'i')

sort(sm, axis=0)
>array([[0, 2, 1, 1, 2, 0], [3, 4, 5, 5, 4, 3]], 'i')

searchsorted( arange(10), (5, 2))
>array([5, 2])

##argmax/min

argmax( s )
>2

argmax( sm, axis=-1 )
>array([2, 3], 'i')

argmax(sm, axis=0)
>array([0, 1, 0, 1, 0, 1], 'i')

argmin(sm, axis=-1)
>array([5, 0], 'i')

##Implementation methods

array([3,4], Int32).itemsize()
>4

a.astype('i').byteswapped()
>array([0, 16777216, 33554432, 50331648, 67108864, 83886080], 'i')

# Otherwise this gets interpreted as loop variable?
`a.typecode()`
>'i'


a.iscontiguous()
>1

a.astype('d')
>array([0.0, 1.0, 2.0, 3.0, 4.0, 5.0], 'd')

u = array([0,1,2], Int8)
repr(u.tostring())
>'\000\001\002'

c = array([0,1,1j])
c.real
>array([0.0, 1.0, 0.0], 'd')

c.imag
>array([0.0, 0.0, 1.0], 'd')

#init = "from multiarray import *\nm = array([(5,2,6,9,3,4,1,0), (8,0,1,3,6,2,0,4)])"
#sorts = [("sort(m)", array([(0,1,2,3,4,5,6,9),(0,0,1,2,3,4,6,8)], 'l') ),
#	("argmax(m)[1]", array([3,0], 'l') ),
#	("argmin(m)[1]", array([7,1], 'l') ),
#	]
#do_eval(sorts, initialize = init)



#sort and argsort need axis

a = arange(6)

#from Guido
a[...]
>array([0, 1, 2, 3, 4, 5], 'i')

array([None])
>array([None], 'O')


zeros( (2,), 'O')
>array([0, 0], 'O')

#from Janne
b = array(a)
b[0] = 66
a[0]
>0


zeros((-5,))
>ValueError: negative dimensions are not allowed

arange(5,3,2)
>array([], 'i')

minimum.reduce()
>TypeError: reduce(): expected 1-2 args; got 0

minimum.reduceat()
>TypeError: reduceat(): expected 2-3 args; got 0

b = array([[1, 2, 3, 4], [5, 6, 7, 8]]*2)

diagonal(b)
>array([1, 6, 3, 8], 'i')

diagonal(b, -1)
>array([5, 2, 7], 'i')

c = array([b,b])

diagonal(c, 1)
>array([[2, 7, 4], [2, 7, 4]], 'i')

#From Carlos
b[1:1].shape
>(0, 4)

b[1:1,:].shape
>(0, 4)

b[10:]
>zeros((0,4))

b[10:, :]
>zeros((0,4))

b[2:10]
>array([[1, 2, 3, 4], [5, 6, 7, 8]], 'i')

b[2:10, ...]
>array([[1, 2, 3, 4], [5, 6, 7, 8]], 'i')

1/array(0)
>ZeroDivisionError: divide by zero

#1/array(0.)
#Machine dependent value, just hope there's no system crash on this
#>1.#INF

#Tim Hochberg

choose((0,1,2),([1,1,1],[2,2,2]))
>ValueError: invalid entry in choice array

where((0,1,2), [1,1,1],[2,2,2])
>array([2, 1, 1], 'i')

divmod(array([2,1]), array([1,2]))
>(array([2, 0], 'i'), array([0, 1], 'i'))

4l*arange(1,3)
>array([4L, 8L], 'O')

arange(3L)
>array([0L, 1L, 2L], 'O')

import sys
from Numeric import *

#Creation from a nested sequence
array([1,2,3])
>array([1, 2, 3], 'i')

array([1,2.3,4])
>array([1.0, 2.3, 4.0], 'd')

array([1,2j,3.])
>array([(1+0j), 2j, (3+0j)], 'D')

array([1,2,3], Float32)
>array([1.0, 2.0, 3.0], 'f')

array([[1,2],[11,12]])
>array([[1, 2], [11, 12]], 'i')

array([[],[]]).shape
>(2, 0)

array([])
>array([], 'i')

#These are not just convenience functions
zeros( (2,3) )
>array([[0, 0, 0], [0, 0, 0]], 'i')

ones( (2,3), Int16)
>array([[1, 1, 1], [1, 1, 1]], 's')

#arrayrange now
arrayrange(7, step=2)
>array([0, 2, 4, 6], 'i')

arrayrange(0, 1.5, .5)
>array([0.0, 0.5, 1.0], 'd')

##Creation from String Data
fromstring("\001\002\003", '1')
>array([1, 2, 3], '1')


#Structural array operations
m = array([[1,2,3], [11,12,13]])
a = arange(6)

a[2:-1]
>array([2, 3, 4], 'i')

a[::-1]
>array([5, 4, 3, 2, 1, 0], 'i')


m[0,2]
>3

m[...,1]
>array([2, 12], 'i')

#take

take(a, (3,2,1,2,3))
>array([3, 2, 1, 2, 3], 'i')

take(m, (2,2,1,1), -1)
>array([[3, 3, 2, 2], [13, 13, 12, 12]], 'i')

#Rearranging array elements

reshape(m, (-1,2))
>array([[1, 2], [3, 11], [12, 13]], 'i')

reshape(a, (2,3))
>array([[0, 1, 2], [3, 4, 5]], 'i')

reshape(a, (4,-1))
>ValueError: total size of new array must be unchanged

reshape(a, (5,6))
>ValueError: total size of new array must be unchanged

ravel(m)
>array([1, 2, 3, 11, 12, 13], 'i')

a[..., NewAxis]
>array([[0], [1], [2], [3], [4], [5]], 'i')

transpose(m, (1,0))
>array([[1, 11], [2, 12], [3, 13]], 'i')

#Replicating and combining array elements

repeat(a, (0,1,2,3,0,0))
>array([1, 2, 2, 3, 3, 3], 'i')

repeat(a, (0,)*6)
>array([], 'i')

repeat(m, (0,2,1), -1)
>array([[2, 2, 3], [12, 12, 13]], 'i')

#al = [0,1,2,3,4,5]
concatenate( (a[:3], a[3:]) )
>array([0, 1, 2, 3, 4, 5], 'i')

#concatenate( (m, m) )
#>array([[1, 2, 3], [11, 12, 13], [1, 2, 3], [11, 12, 13]], 'l')

concatenate( (m, m), axis=1)
>array([[1, 2, 3, 1, 2, 3], [11, 12, 13, 11, 12, 13]], 'i')

#arithmetic and logic operators
x = array([1,2], 'l')
y = zeros((2,), 'l')

x+x
>array([2, 4], 'l')

add(x,x)
>array([2, 4], 'l')

add(x,x,y)
>array([2, 4], 'l')

y
>array([2, 4], 'l')

x-x
>array([0, 0], 'l')

subtract(x,x)
>array([0, 0], 'l')

x*x
>array([1, 4], 'l')

multiply(x, x)
>array([1, 4], 'l')

x/x
>array([1, 1], 'l')

divide(x,x)
>array([1, 1], 'l')

x**x
>array([1, 4], 'l')

power(x,x)
>array([1, 4], 'l')

x%x
>array([0, 0], 'l')

remainder(x,x)
>array([0, 0], 'l')

#Need more math functions here...

#Non-elementwise operations
add.reduce(a)
>15

add.reduce(m, 0)
>array([12, 14, 16], 'i')

add.reduce(m, -1)
>array([6, 36], 'i')

add.reduce([1])
>1

add.reduce([])
>0

multiply.reduce([])
>1

add.outer(a[:3], a[:3])
>array([[0, 1, 2], [1, 2, 3], [2, 3, 4]], 'i')

dot(a,a)
>55

#conditional selection

choose(a, (5,4,3,2,1,0))
>array([5, 4, 3, 2, 1, 0], 'i')

choose([[1,0], [0,1]], (66, [(1,2),(11,12)]))
>array([[1, 66], [66, 12]], 'i')

#sorting

#TAH
s = (3,2,5,1,4,0)
sm = [s, array(s)[::-1]]

sort( s )
>array([0, 1, 2, 3, 4, 5])

argsort( s )
>array([5, 3, 1, 0, 4, 2])

sort(sm, axis=-1)
>array([[0, 1, 2, 3, 4, 5], [0, 1, 2, 3, 4, 5]])

sort(sm, axis=0)
>array([[0, 2, 1, 1, 2, 0], [3, 4, 5, 5, 4, 3]])

searchsorted( arange(10), (5, 2))
>array([5, 2])

#argmax/min

argmax( s )
>2

argmax( sm, axis=-1 )
>array([2, 3])

argmax(sm, axis=0)
>array([0, 1, 0, 1, 0, 1])

argmin(sm, axis=-1)
>array([5, 0])

#Implementation methods

array([3,4], Int32).itemsize()
>4

a.astype('i').byteswapped()
>array([0, 16777216, 33554432, 50331648, 67108864, 83886080], 'i')

# i gets confused with loop variable unless quoted.
`a.typecode()`
>'i'

a.iscontiguous()
>1

a.astype('d')
>array([0.0, 1.0, 2.0, 3.0, 4.0, 5.0], 'd')


u = array([0,1,2], Int8)
repr(u.tostring())
>'\000\001\002'

c = array([0,1,1j])
c.real
>array([0.0, 1.0, 0.0], 'd')

c.imag
>array([0.0, 0.0, 1.0], 'd')

# TAH -- commented out in "original" so ignore.
#init = "from multiarray import *\nm = array([(5,2,6,9,3,4,1,0), (8,0,1,3,6,2,0,4)])"
#sorts = [("sort(m)", array([(0,1,2,3,4,5,6,9),(0,0,1,2,3,4,6,8)], 'l') ),
#	("argmax(m)[1]", array([3,0], 'l') ),
#	("argmin(m)[1]", array([7,1], 'l') ),
#	]
#do_eval(sorts, initialize = init)



#sort and argsort need axis

a = arange(6)

#from Guido
a[...]
>array([0, 1, 2, 3, 4, 5], 'i')

array([None])
>array([None], 'O')

zeros( (2,), 'O')
>array([0, 0], 'O')

#from Janne
b = array(a)
b[0] = 66
a[0]
>0

zeros((-5,))
>ValueError: negative dimensions are not allowed

arange(5,3,2)
>array([], 'i')

minimum.reduce()
>TypeError: reduce(): expected 1-2 args; got 0


minimum.reduceat()
>TypeError: reduceat(): expected 2-3 args; got 0


b = array([[1, 2, 3, 4], [5, 6, 7, 8]]*2)

add.reduceat(b, (1, 3), -1)
>array([[ 5,  4],[13,  8], [ 5,  4],[13,  8]])

add.reduceat(b, (1,3), 0)
>array([[ 6,  8, 10, 12],[ 5,  6,  7,  8]])

diagonal(b)
>array([1, 6, 3, 8], 'i')

diagonal(b, -1)
>array([5, 2, 7], 'i')

c = array([b,b])

diagonal(c, 1)
>array([[2, 7, 4], [2, 7, 4]], 'i')

#From Carlos
b[1:1].shape
>(0, 4)

b[1:1,:].shape
>(0, 4)

b[10:]
>zeros((0,4))

b[10:, :]
>zeros((0,4))

b[2:10]
>array([[1, 2, 3, 4], [5, 6, 7, 8]], 'i')

b[2:10, ...]
>array([[1, 2, 3, 4], [5, 6, 7, 8]], 'i')


1/array(0)
>ZeroDivisionError: divide by zero

#1/array(0.)
#Machine dependent value, just hope there's no system crash on this
#>1.#INF

#Tim Hochberg

choose((0,1,2),([1,1,1],[2,2,2]))
>ValueError: invalid entry in choice array

where((0,1,2), [1,1,1],[2,2,2])
>array([2, 1, 1], 'i')

divmod(array([2,1]), array([1,2]))
>(array([2, 0], 'i'), array([0, 1], 'i'))

4l*arange(1,3)
>array([4L, 8L], 'O')

arange(3L)
>array([0L, 1L, 2L], 'O')

# Test of the exception-raising comparisons
# david ascher - march 16, 1998
x = array([1,2])
y = array([1,2])
z = array([1,2,3])
x < y
>TypeError: Comparison of multiarray objects is not implemented.
x == z
>TypeError: Comparison of multiarray objects is not implemented.
cmp(y,z)
>TypeError: Comparison of multiarray objects is not implemented.



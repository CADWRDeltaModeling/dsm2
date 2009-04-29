#
import string, time
from Numeric import *
from copy import copy

def factor(num):
    """Determine factors of an integer num. """
    # Take care of special cases.
    if   type(num) != type(1) or num <= 0: raise ValueError
    elif num == 4: return [2,2]
    elif num <= 5: return [num]
    # Fall through to general case.
    sr = int(sqrt(float(num)))
    cur = concatenate((arange(2, num/sr), num/arange(sr,0,-1)))
    cur = compress(equal(num%cur,0), cur)
    factors = [cur[0]]
    while len(cur) > 1:
        cur = choose(not_equal(cur[1:]%cur[0],0), (cur[1:]/cur[0], cur[1:]))
        if cur[0] != 1: factors.append(cur[0])
    return factors

def dft(f, inverse=0):
    """Compute the discrete (or inverse) Fourier transform of ~f~. """
    f = asarray(f)
    n = len(f)
    k = arange(0, n)
    omega0 = -2j * pi / n
    if inverse: omega0 = -omega0
    F = zeros(f.shape, Complex128)
    mult = exp(k*omega0)
    work = copy(f)
    for r in k:
        F[r] = sum(work)
        work = work * mult
    if inverse: return F / n
    else:       return F

def fft_base2(f, inverse=0, l2n=None):
    """Compute the 'classic' (inverse) fft of a number thats length 2**l2n.
    """
    # This originally came from Numerical recipes, but you should
    # compare the two, there is no longer any resemblance!
    f = asarray(f, Complex128)
    n = len(f)
    # Find the log2 size of the array.
    if l2n==None:
        l2n = int(round(log(len(f))/log(2)))
    if 2**l2n != n: raise ValueError, 'Array len not 2**l2n.'
    # Bit reverse f.
    if l2n > 1: f = transpose(reshape(f, (2,)*l2n) )
    # Build up the fft.
    if inverse: sign = -1
    else:       sign =  1
    step = 2
    w = exp((-2j*pi / sign / n) * arange(n/2))
    t = zeros((n/2,), Complex128)       #
    while n > step/2:
        f = reshape(f, (n/step, 2, step/2))
        t = reshape(t, (n/step, step/2))#
        multiply(w[::n/step], f[:,1,:], t)#t = w[::n/step]*f[:,1,:]
        subtract(f[:,0,:], t, f[:,1,:])   #f[:,1,:] = f[:,0,:] - t
        add(     f[:,0,:], t, f[:,0,:])   #f[:,0,:] = f[:,0,:] + t
        step = step * 2
    #
    if inverse: return ravel(f) / n
    else:       return ravel(f)

def fft_factored(f, inverse=0, fcts=None):
    """Compute the (inverse) dft using a generalization of the fft algorithm."""
    # This decomposes the length of ~f~ into its prime factors.These
    # are used to perform the fft instead of base 2.
    f = asarray(f, Complex128)
    n = len(f)
    # Find the factors of the array.
    if fcts == None: fcts = asarray(factor(n))
    else:            fcts = asarray(fcts)
    fore = concatenate(([1],cumproduct(fcts)[:-1]))
    aftr = n / (fcts*fore)
    # 'Bit' reverse f.
    f = transpose(reshape(f, fcts ))
    # Build up the fft.
    if inverse: sign = -1
    else:       sign =  1
    w = exp((-2j*pi / sign / n) * arange(n))
    t = zeros((n,), Complex128)
    for i in range(0,len(fcts)):
        f = reshape(f, (aftr[i], fcts[i], fore[i]))
        t = reshape(t, (aftr[i], fcts[i], fore[i]))
        w1 = 1
        for j  in range(fcts[i]):
            t[:,j,:] = w1 * f[:,j,:]
            w1 = w1*w[:n/fcts[i]:aftr[i]]
        w1 = reshape(w[::n/fcts[i]], (1,fcts[i],1))
        for j in range(fcts[i]):
            f[:,j,:] = sum(t, 1)
            t = t*w1
    #
    if inverse: return ravel(f) / n
    else:       return ravel(f)

def pyfft(f, inverse=0):
    """Perform an fft in python."""
    n = len(f)
    #
    l2n = int(round(log(len(f))/log(2)))
    if 2**l2n == n:
        return fft_base2(array(f,Complex128),
                         inverse=inverse,l2n=l2n)
    #
    fcts=factor(n)
    if len(fcts) > 1:
        return fft_factored(array(f,Complex128),
                            inverse=inverse,fcts=fcts)
    #
    return dft(f,inverse=inverse)

try:
    from FFT import fft         # Use the real fft.
except:
    #print "Using pyfft."
    fft = pyfft                 # Can't so use pyfft.

def check(n, tol=1e-5):
    """Check and time (vs. C fft) pyfft."""
    a = arange(n)
    t0 = time.clock()
    #t0 = os.times()[4]
    F = fft(a)
    t1 = time.clock()
    #t1 = os.times()[4]
    a2 = pyfft(F,inverse=1)
    t2 = time.clock()
    #t2 = os.times()[4]
    err = add.reduce(abs(a - a2)) / n
    if err > tol: print '(ERR=', err, ')'
    return t1 - t0, t2 - t1

#

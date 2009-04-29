package JNumeric;
import org.python.core.*;
import java.lang.reflect.Array;

// Copyright (c) 1998 Timothy Hochberg, tim.hochberg@ieee.org

// XXX Add Float and ComplexFloat, change double_ to Double and complex to ComplexDouble
// Add basic float and ComplexFloat helper classes.
// Change signature to Double(source, result)

public class UnaryUfunc extends PyObject {
	
	static final public UnaryFunction arccos = new Arccos();
	static final public UnaryFunction arccosh = new Arccosh();
	static final public UnaryFunction arcsin = new Arcsin();
	static final public UnaryFunction arcsinh = new Arcsinh();
	static final public UnaryFunction arctan = new Arctan();
	static final public UnaryFunction arctanh = new Arctanh();
	static final public UnaryFunction conjugate = new Conjugate();
	static final public UnaryFunction imaginary = new Imaginary();
	static final public UnaryFunction cos = new Cos(); 
	static final public UnaryFunction cosh = new Cosh(); 
	static final public UnaryFunction exp = new Exp();
	static final public UnaryFunction log = new Log();
	static final public UnaryFunction log10 = new Log10();
	static final public UnaryFunction logicalNot = new LogicalNot();
	static final public UnaryFunction real = new Real();
	static final public UnaryFunction sin = new Sin();
	static final public UnaryFunction sinh = new Sinh();
	static final public UnaryFunction sqrt = new Sqrt();
	static final public UnaryFunction tan = new Tan(); 
	static final public UnaryFunction tanh = new Tanh(); 

	UnaryFunction function;

	public PyObject __findattr__(String name) {
		if (name == "__doc__") return new PyString(function.docString());
		return super.__findattr__(name);
	}

	UnaryUfunc(UnaryFunction function) {
		this.function = function;
	}

	public PyObject __call__(PyObject o) {
		PyMultiarray result;
		PyMultiarray a = PyMultiarray.asarray(o);
		switch (a._typecode) {
		case 'F': case 'D':
			result = function.complex(PyMultiarray.array(a,'D'));
			break;
		case '1': case 's': case 'i': case 'l': case 'f': case 'd':
			result = function.double_(PyMultiarray.array(a,'d'));
			break;
		default:
			throw Py.ValueError("typecode must be in [1silfFdD]");
		}
		if (a._typecode == 'f' || a._typecode == 'F')
			return result.astype(a._typecode);
		return PyMultiarray.returnValue(result);
	}

	// Two argument unary functions are provided for compatibility with
	// CNumeric. As implemented, they are no more efficient in memory usage
	// or speed than one argument unary functions (and perhaps less so). 
	// However, I have never used a two argument unary ufunc, so I'm not too
	// concerned. However if people complain I suppose I'll do something
	// about it. (Nah I don't sound defensive!)
	public PyObject __call__(PyObject o, PyMultiarray result) {
		PyMultiarray.copyAToB(PyMultiarray.asarray(__call__(o)), result);
		return result;
	}
}

class UnaryFunction {
	String docString() {return "unary_function(a, [,r])\n";}
	// Some constants that are useful when using complex numbers
	static final PyMultiarray cp1 = PyMultiarray.array(new PyComplex( 1,  0));
	static final PyMultiarray cpj = PyMultiarray.array(new PyComplex( 0,  1));
	static final PyMultiarray cn1 = PyMultiarray.array(new PyComplex(-1,  0));
	static final PyMultiarray cnj = PyMultiarray.array(new PyComplex( 0, -1));
	static final PyMultiarray c0 = PyMultiarray.array( new PyComplex( 0,  0));
	static final PyMultiarray cpj_2 = PyMultiarray.array(new PyComplex(0,   0.5));
	static final PyMultiarray cp1_2 = PyMultiarray.array(new PyComplex(0.5, 0));
	static final PyMultiarray cp2 = PyMultiarray.array(new PyComplex(2,   0));
	// a is used as scratch space by these functions, so make sure to pass a copy!
	PyMultiarray double_(PyMultiarray a) {throw Py.ValueError("double_ not implemented");}
	PyMultiarray complex(PyMultiarray a) {throw Py.ValueError("complex not implemented");} 
}

final class Arccos extends UnaryFunction {
	String docString() {return "arccos(a [,r]) returns arccos(a) and stores the result in r if supplied.\n";}
	public PyMultiarray double_(PyMultiarray a) {
		for (int i = 0; i < Array.getLength(a.data); i++)
			Array.setDouble(a.data, i, Math.acos(Array.getDouble(a.data,i)));
		return a;
	}
	public PyMultiarray complex(PyMultiarray a) {
		return PyMultiarray.asarray(cnj.__mul__(umath.log.__call__(a.__add__(
			   cpj.__mul__(umath.sqrt.__call__(cp1.__sub__(a.__mul__(a))))))));
	}
}

final class Arccosh extends UnaryFunction {
	String docString() {return "arccosh(a [,r]) returns arccosh(a) and stores the result in r if supplied.\n";}
	public PyMultiarray double_(PyMultiarray a) {
		for (int i = 0; i < Array.getLength(a.data); i++) {
			double d = Array.getDouble(a.data,i);
			Array.setDouble(a.data, i, Math.log(d+Math.sqrt(d*d-1)));
		}
		return a;
	}
	public PyMultiarray complex(PyMultiarray a) {
		return PyMultiarray.asarray(umath.log.__call__(a.__add__(
			   cpj.__mul__(umath.sqrt.__call__(cp1.__sub__(a.__mul__(a)))))));
	}
}

final class Arcsin extends UnaryFunction {
	public PyMultiarray double_(PyMultiarray a) {
		for (int i = 0; i < Array.getLength(a.data); i++)
			Array.setDouble(a.data, i, Math.asin(Array.getDouble(a.data,i)));
		return a;
	}
	public PyMultiarray complex(PyMultiarray a) {
		return PyMultiarray.asarray(cnj.__mul__(umath.log.__call__(cpj.__mul__(a).__add__(
			   umath.sqrt.__call__(cp1.__sub__(a.__mul__(a)))))));
	}
}

final class Arcsinh extends UnaryFunction {
	public PyMultiarray double_(PyMultiarray a) {
		for (int i = 0; i < Array.getLength(a.data); i++) {
			double d = Array.getDouble(a.data,i);
			Array.setDouble(a.data, i, -Math.log(Math.sqrt(1+d*d)-d));
		}
		return a;
	}
	public PyMultiarray complex(PyMultiarray a) {
		return PyMultiarray.asarray(umath.log.__call__(umath.sqrt.__call__(
			cp1.__add__(a.__mul__(a))).__sub__(a)).__neg__());
	}
}

final class Arctan extends UnaryFunction {
	public PyMultiarray double_(PyMultiarray a) {
		for (int i = 0; i < Array.getLength(a.data); i++)
			Array.setDouble(a.data, i, Math.atan(Array.getDouble(a.data,i)));
		return a;
	}
	public PyMultiarray complex(PyMultiarray a) {
		return PyMultiarray.asarray(cpj_2.__mul__(umath.log.__call__(cpj.__add__(a).__div__(cpj.__sub__(a)))));
	}
}

final class Arctanh extends UnaryFunction {
	public PyMultiarray double_(PyMultiarray a) {
		for (int i = 0; i < Array.getLength(a.data); i++) {
			double d = Array.getDouble(a.data,i);
			Array.setDouble(a.data, i, 0.5*Math.log((1.+d)/(1.-d)));
		}
		return a;
	}
	public PyMultiarray complex(PyMultiarray a) {
		return PyMultiarray.asarray(cp1_2.__mul__(umath.log.__call__(cp1.__add__(a).__div__(cp1.__sub__(a)))));
	}
}

final class Conjugate extends UnaryFunction {
	public PyMultiarray double_(PyMultiarray a) {
		return a;
	}
	public PyMultiarray complex(PyMultiarray a) {
		for (int i = 0; i < Array.getLength(a.data); i+=2)
			Array.setDouble(a.data, i+1, -Array.getDouble(a.data,i+1));
		return a;
	}
}

final class Cos extends UnaryFunction {
	public PyMultiarray double_(PyMultiarray a) {
		for (int i = 0; i < Array.getLength(a.data); i++)
			Array.setDouble(a.data, i, Math.cos(Array.getDouble(a.data,i)));
		return a;
	}
	public PyMultiarray complex(PyMultiarray a) {
		for (int i = 0; i < Array.getLength(a.data); i+=2) {
			double re = Array.getDouble(a.data,i), im = Array.getDouble(a.data,i+1);
			double eim = Math.exp(im), cosre = Math.cos(re), sinre = Math.sin(re);
			Array.setDouble(a.data, i,    0.5*cosre*(eim+1./eim));
			Array.setDouble(a.data, i+1, -0.5*sinre*(eim-1./eim));
		}
		return a;
	}
}

final class Cosh extends UnaryFunction {
	public PyMultiarray double_(PyMultiarray a) {
		for (int i = 0; i < Array.getLength(a.data); i++) {
			double d = Array.getDouble(a.data,i);
			double ed = Math.exp(d); 
			Array.setDouble(a.data, i, 0.5*ed+0.5/ed);
		}
		return a;
	}
	public PyMultiarray complex(PyMultiarray a) {
		PyObject ea = umath.exp.__call__(a);
		return PyMultiarray.asarray(cp1_2.__mul__(ea).__add__(cp1_2.__div__(ea)));
	}
}

final class Exp extends UnaryFunction {
	public PyMultiarray double_(PyMultiarray a) {
		for (int i = 0; i < Array.getLength(a.data); i++)
			Array.setDouble(a.data, i, Math.exp(Array.getDouble(a.data,i)));
		return a;
	}
	public PyMultiarray complex(PyMultiarray a) {
		for (int i = 0; i < Array.getLength(a.data); i+=2) {
			double re = Array.getDouble(a.data,i), im = Array.getDouble(a.data,i+1);
			double ere = Math.exp(re), cosim = Math.cos(im), sinim = Math.sin(im);
			Array.setDouble(a.data, i,   ere*cosim);
			Array.setDouble(a.data, i+1, ere*sinim);
		}
		return a;
	}
}

final class Imaginary extends UnaryFunction {
	public PyMultiarray double_(PyMultiarray a) {
		return PyMultiarray.zeros(a.dimensions, 'd');
	}
	public PyMultiarray complex(PyMultiarray a) {
		return a.getImag();
	}
}

final class Log extends UnaryFunction {
	public PyMultiarray double_(PyMultiarray a) {
		for (int i = 0; i < Array.getLength(a.data); i++)
			Array.setDouble(a.data, i, Math.log(Array.getDouble(a.data,i)));
		return a;
	}
	public PyMultiarray complex(PyMultiarray a) {
		for (int i = 0; i < Array.getLength(a.data); i+=2) {
			double re = Array.getDouble(a.data,i), im = Array.getDouble(a.data,i+1);
			Array.setDouble(a.data, i,   Math.log(Math.sqrt(im*im+re*re)));
			Array.setDouble(a.data, i+1, Math.atan2(im, re));
		}
		return a;
	}
}

final class Log10 extends UnaryFunction {
	final double log10 = Math.log(10);
	public PyMultiarray double_(PyMultiarray a) {
		for (int i = 0; i < Array.getLength(a.data); i++)
			Array.setDouble(a.data, i, Math.log(Array.getDouble(a.data,i))/log10);
		return a;
	}
	public PyMultiarray complex(PyMultiarray a) {
		for (int i = 0; i < Array.getLength(a.data); i+=2) {
			double re = Array.getDouble(a.data,i), im = Array.getDouble(a.data,i+1);
			Array.setDouble(a.data, i,   Math.log(Math.sqrt(im*im+re*re))/log10);
			Array.setDouble(a.data, i+1, Math.atan2(im, re)/log10);
		}
		return a;
	}
}

final class LogicalNot extends UnaryFunction {
	public PyMultiarray double_(PyMultiarray a) {
		return PyMultiarray.asarray(a.__eq(Py.Zero));
	}
	public PyMultiarray complex(PyMultiarray a) {
		return PyMultiarray.asarray(a.__eq(Py.Zero));
	}
}

final class Real extends UnaryFunction {
	public PyMultiarray double_(PyMultiarray a) {
		return a;
	}
	public PyMultiarray complex(PyMultiarray a) {
		return a.getReal();
	}
}

final class Sin extends UnaryFunction {
	public PyMultiarray double_(PyMultiarray a) {
		for (int i = 0; i < Array.getLength(a.data); i++)
			Array.setDouble(a.data, i, Math.sin(Array.getDouble(a.data,i)));
		return a;
	}
	public PyMultiarray complex(PyMultiarray a) {
		for (int i = 0; i < Array.getLength(a.data); i+=2) {
			double re = Array.getDouble(a.data,i), im = Array.getDouble(a.data,i+1);
			double eim = Math.exp(im), cosre = Math.cos(re), sinre = Math.sin(re);
			Array.setDouble(a.data, i,   0.5*sinre*(eim+1./eim));
			Array.setDouble(a.data, i+1, 0.5*cosre*(eim-1./eim));
		}
		return a;
	}
}

final class Sinh extends UnaryFunction {
	public PyMultiarray double_(PyMultiarray a) {
		for (int i = 0; i < Array.getLength(a.data); i++) {
			double d = Array.getDouble(a.data,i);
			double ed = Math.exp(d); 
			Array.setDouble(a.data, i, 0.5*ed-0.5/ed);
		}
		return a;
	}
	public PyMultiarray complex(PyMultiarray a) {
		PyObject ea = umath.exp.__call__(a);
		return PyMultiarray.asarray(cp1_2.__mul__(ea).__sub__(cp1_2.__div__(ea)));
	}
}

final class Sqrt extends UnaryFunction {
	public PyMultiarray double_(PyMultiarray a) {
		for (int i = 0; i < Array.getLength(a.data); i++)
			Array.setDouble(a.data, i, Math.sqrt(Array.getDouble(a.data,i)));
		return a;
	}
	public PyMultiarray complex(PyMultiarray a) {
		for (int i = 0; i < Array.getLength(a.data); i+=2) {
			double re = Array.getDouble(a.data,i), im = Array.getDouble(a.data,i+1);
			double mag = Math.pow(re*re+im*im, 0.25), phi = Math.atan2(im, re)/2.;
			Array.setDouble(a.data, i, mag*Math.cos(phi));
			Array.setDouble(a.data, i+1, mag*Math.sin(phi));
		}
		return a;
	}
}

final class Tan extends UnaryFunction {
	public PyMultiarray double_(PyMultiarray a) {
		for (int i = 0; i < Array.getLength(a.data); i++)
			Array.setDouble(a.data, i, Math.tan(Array.getDouble(a.data,i)));
		return a;
	}
	public PyMultiarray complex(PyMultiarray a) {
		PyMultiarray sina = umath.sin.function.complex(PyMultiarray.array(a));
		return PyMultiarray.asarray(sina.__div__(umath.cos.function.complex(a)));
	}
}

final class Tanh extends UnaryFunction {
	public PyMultiarray double_(PyMultiarray a) {
		for (int i = 0; i < Array.getLength(a.data); i++) {
			double d = Array.getDouble(a.data,i);
			double e2d = Math.exp(2*d); 
			Array.setDouble(a.data, i, (e2d-1)/(e2d+1));
		}
		return a;
	}
	public PyMultiarray complex(PyMultiarray a) {
		PyObject e2a = umath.exp.__call__(cp2.__mul__(a));
		return PyMultiarray.asarray(e2a.__sub__(cp1).__div__(e2a.__add__(cp1)));
	}
}

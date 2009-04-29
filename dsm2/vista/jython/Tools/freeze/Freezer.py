from org.python.core import parser
from org.python.compiler.Module import compile
from org.python.compiler import JavaMaker
import org, java
import string, os

from FreezeVisitor import FreezeVisitor
from depend import depend

suffix = '$py'
prefix = 'pycode.'

class Freezer:
	def __init__(self, out, shallow=0, useProxy=0, dependFilter=None):
		self.proxies = {}
		self.modules = {}
		self.packages = {}
		
		self.classes = {}
		self.props = {}
		self.events = {}
		self.javaclasses = []
		
		self.out = out
		self.shallow = shallow
		self.dependFilter = dependFilter
		self.useProxy = useProxy

	def getPackage(self, package):
		fp = open(package.__file__, 'r')
		data = fp.read()
		fp.close()
		return data
		
	def freeze(self, myname, data=None, file='', package=None):
		print 'freezing', myname

		if data is None:
			package = __import__(myname)
			data = self.getPackage(package)
			if file == '':
				file = package.__file__

		f = FreezeVisitor(self.events, self.props, classes=self.classes, classname=myname)
		node = parser.parse(data, 'exec')
		f.suite(node)
				
		if not self.shallow:
			for proxy in f.proxies.values():
				#if proxy.__name__ != 'java.applet.Applet':
				self.proxies[proxy] = 1
						
			for package in f.packages.values():
				name = package.__name__
				if self.modules.has_key(package):
					continue
				elif hasattr(package, '__path__'):
					self.packages[package] = 1
				elif isinstance(package, org.python.core.PyModule):
					self.modules[package] = 1
					self.freeze(name, self.getPackage(package), package.__file__)
				else:
					self.packages[package] = 1
				
			ofp = java.io.ByteArrayOutputStream()
	
			nm = myname+suffix
			compile(node, ofp, nm, file, 1, 0)
			self.out.write(nm, ofp)
		
		if f.realclass != None:
			self.javaclasses.append( (f.realclass.__name__, myname, f.methods) )
			if self.useProxy:
				self.proxies[f.realclass] = 1
				
	def makeProxy(self, proxy):
		os = java.io.ByteArrayOutputStream()
		org.python.compiler.ProxyMaker.makeProxy(proxy, os)
		#print proxy, os, os.size()
		self.out.write('org.python.proxies.'+proxy, os)		


	def makeJavaClass(self, javaclass, myname, methods, main=0, frozen=1):
		#print methods
		jm = JavaMaker(javaclass, myname, myname, myname, 
					self.requiredPackages, 
					[], methods, frozen, main)
		jm.build()
		
		os = java.io.ByteArrayOutputStream()
		jm.classfile.write(os)
		self.out.write(myname, os)


	def finish(self, main=0):
		names = {}
		
		if not self.shallow:
			pkgText = "__path__ = []"
			pkgNode = parser.parse(pkgText, 'exec')
	
			for package in self.packages.keys():
				if isinstance(package, org.python.core.PyModule):
					myname = package.__name__
					ofp = java.io.ByteArrayOutputStream()
					compile(pkgNode, ofp, myname+suffix, package.__path__[0], 1, 0)
					self.out.write(myname+suffix, ofp)
					
					self.freeze(myname+'.__init__')
				else:
					names[package.__name__] = package
				
			for prop in self.props.keys():
				#print 'prop', prop
				if self.events.has_key(prop):
					print 'adding event', prop, self.events[prop]
					self.proxies[self.events[prop]] = 1	
			#print self.events
			
			# Make proxies
			for proxy in self.proxies.keys():
				self.makeProxy(proxy.__name__)
	
	
			for name in names.keys():
				parts = string.split(name, '.')
				name = parts[0]
				for i in range(1, len(parts)):
					if names.has_key(name):
						del names[name]
					name = name+'.'+parts[i]
					
			self.requiredPackages = names.keys()
	
			cs = {}
			for c in self.classes.values():
				if not isinstance(c, org.python.core.PyJavaClass):
					continue
				name = c.__name__
				if name[:5] == 'java.': continue
				if name[:16] == 'org.python.core.': continue
				cs[c] = c
				
			depends = {}
			for c in cs.keys():
				print 'class', c, c.__name__
			
				depend(c.__name__, depends, self.dependFilter)
			
			for name, file in depends.items():
				print 'adding', name
				self.out.write(name, file)
		else:
			self.requiredPackages = None

		for javaclass, name, methods in self.javaclasses:
			if self.useProxy: methods = None
			self.makeJavaClass(javaclass, name, methods, main, not self.shallow)

	def addPackage(self, directory, skiplist=[]):
		for file in os.listdir(directory):
			if file[-6:] != '.class': continue
			name = 'org.python.core.'+file[:-6]
			if name in skiplist: continue
			
			self.out.write(name, os.path.join(directory, file))

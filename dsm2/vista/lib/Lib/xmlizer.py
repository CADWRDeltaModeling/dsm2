import types
# XML library
from com.sun.xml.tree import XmlDocument, TreeWalker
from org.w3c.dom import Element
#
from java.io import FileOutputStream, FileInputStream
def dump(obj,file):
    x = Xmlizer()
    xdoc = XmlDocument()
    xe = x.toXml(obj,xdoc)
    xdoc.appendChild(xe)
    #
    fos = FileOutputStream(file)
    xdoc.write(fos)
    fos.close()
#
def load(obj,file):
    x = Xmlizer()
    xdoc = XmlDocument.createXmlDocument(FileInputStream(file),0)
    x.fromXml(obj,xdoc.getDocumentElement())
#
def toxml(obj):
    x = Xmlizer()
    xdoc = XmlDocument()
    return x.toXml(obj,xdoc)
#
def fromxml(obj,root):
    x = Xmlizer()
    x.fromXml(obj,root)
    return obj
#
class Xmlizer:
    def __init__(self):
	pass
    def _xmlize_field(self,field,xdoc,xe,key):
	if hasattr(field,'__dict__'):
	    xe.appendChild(self.toXml(field,xdoc))
	elif type(field) == types.ListType:
	    le = xdoc.createElement("list_"+key)
	    xe.appendChild(le)
	    for item in field:
		self._xmlize_field(item,xdoc,le,key)
	else:
	    xe.setAttribute(key,str(field))
    def toXml(self,obj,xdoc):
	if hasattr(obj,'__dict__'):
	    name = str(obj.__class__)
	    xe = xdoc.createElement(name)
	    namedict = obj.__dict__
	    for key in namedict.keys():
		print key
		field = obj.__dict__[key]
		self._xmlize_field(field,xdoc,xe,key)
	else:
	    raise "No __dict__ for object: " + str(obj)
	return xe
    def fromXml(self,obj,root):
	if hasattr(obj,'__dict__'):
	    name = str(obj.__class__)
	    if root.getName() == name:
		xe = root
	    else:
		tw = TreeWalker(root)
		xe = tw.getNextElement(name)
		if xe == None: raise "No matching object named %s under %s"%(name,str(obj))
	    namedict = obj.__dict__
	    for key in namedict.keys():
		field = obj.__dict__[key]
		if hasattr(field,'__dict__'):
		    tw = TreeWalker(xe)
		    self.fromXml(field,obj,root)
		else:
		    str = xe.getAttribute(key)
		    if type(field) == types.IntType:
			field = int(str)
		    elif type(field) == types.FloatType:
			field = float(str)
		    else:
			field = str
		obj.__dict__[key] = field
	else:
	    raise "No __dict__ for object: " + str(obj)

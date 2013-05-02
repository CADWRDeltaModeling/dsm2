import string
# XML library
from org.w3c.dom import Document
from org.w3c.dom.traversal import TreeWalker
#from com.sun.xml.tree import XmlDocument, TreeWalker
from org.w3c.dom import Element
# gui stuff
import javax.swing
from javax.swing import JFrame, JDialog, \
     JPanel, JComboBox, JColorChooser, JButton, \
     JTextField, JCheckBox, JTabbedPane, \
     BorderFactory, JTable, DefaultCellEditor, JScrollPane
from java.awt import Rectangle, Color, BorderLayout, Insets, \
     GridLayout
from java.awt.event import ActionListener, ItemListener
# vista stuff
import vista.gui
from vista.gui import XYGridLayout, TableCellColorRenderer, TableCellColorEditor
from vista.graph import SymbolFactory, GraphicElement
from javax.swing.table import DefaultTableModel
class SymbolProps:
    """
    Symbol properties
    """
    sym_shape_map = { "cross": SymbolFactory.createCross,
		      "circle": SymbolFactory.createCircle,
		      "triangle": SymbolFactory.createTriangle,
		      "upright triangle": SymbolFactory.createUprightTriangle,
		      "square": SymbolFactory.createSquare,
		      "butterfly": SymbolFactory.createButterfly,
		      "hour glass": SymbolFactory.createHourGlass
		      }
    def __init__(self, shape, color, size, filled):
	self.color = color
	self.size = size
	self.filled = filled
	self.shape = shape
	self.sym = self.createSymbol()
    def createSymbol(self):
	if not SymbolProps.sym_shape_map.has_key(self.shape):
	    raise "No such symbol -> " + self.shape + " defined"
	func = SymbolProps.sym_shape_map[self.shape]
	return func(self.filled, self.color, self.size)
    def __repr__(self):
	xstr = 'Symbol Properties:\n'
	xstr = xstr + 'Color: ' + repr(self.color)
	xstr = xstr + '\nSize: ' + repr(self.size)
	xstr = xstr + '\nFilled: ' + repr(self.filled)
	return xstr
    def toXml(self,xdoc):
	el = xdoc.createElement("sym_props")
	el.setAttribute("color",str(self.color))
	el.setAttribute("size",str(self.size))
	el.setAttribute("filled",str(self.filled))
	el.setAttribute("shape",str(self.shape))
	return el
    def fromXml(self,root):
	el = None
	tw = TreeWalker(root)
	if root.getNodeName() == "sym_props":
	    el = root
	else:
	    el = tw.getNextElement("sym_props")
	color = GraphicElement.parseColorProperty(el.getAttribute("color"))
	size = string.atoi(el.getAttribute("size"))
	filled = string.atoi(el.getAttribute("filled"))
	shape = el.getAttribute("shape")
	self.__init__(shape,color,size,filled)
#	
class CurveProps:
    DASH_TYPES = { "plain": [1.],\
		   "dotted": [2.,2.],\
		   "short dashed": [4.,4.],\
		   "long dashed": [8.,8.],\
		   "short long dashed": [4.,4.,8.,4.]\
		   }
    def __init__(self, color=Color.blue, sym_prop=None, thickness=1, dps=4):
	self.color = color
	self.sym_prop = sym_prop
	if self.sym_prop != None:
	    self.draw_symbol = 1
	else:
	    self.draw_symbol = 0
	    self.sym_prop = SymbolProps("circle",color,2,1)
	self.thickness = thickness
	self.data_per_symbol = dps
	self.dash_type = "plain"
    def __repr__(self):
	xstr = 'Curve Properties:\n'
	xstr = xstr +'Color: ' + repr(self.color)
	xstr = xstr +'\nThickness: ' + repr(self.thickness)
	xstr = xstr + '\n'+ repr(self.sym_prop)
	return xstr
    def toXml(self,xdoc):
	el = xdoc.createElement("curve_props")
	el.setAttribute("color",str(self.color))
	el.setAttribute("thickness",str(self.thickness))
	el.appendChild(self.sym_prop.toXml(xdoc))
	el.setAttribute("draw_symbol",str(self.draw_symbol))
	el.setAttribute("data_per_symbol",str(self.data_per_symbol))
	el.setAttribute("dash_type",self.dash_type)
	return el
    def fromXml(self,root):
	el = None
	tw = TreeWalker(root)
	if root.getNodeName() == "curve_props":
	    el = root
	else:
	    el = tw.getNextElement("curve_props")
	self.color = GraphicElement.parseColorProperty(el.getAttribute("color"))
	self.thickness = string.atoi(el.getAttribute("thickness"))
	self.draw_symbol = string.atoi(el.getAttribute("draw_symbol"))
	self.data_per_symbol = string.atoi(el.getAttribute("data_per_symbol"))
	if el.getAttribute("dash_type") != "":
	    self.dash_type = el.getAttribute("dash_type")
	else:
	    self.dash_type = "plain"
	self.sym_prop.fromXml(root)
class EditSymbolAttr(JPanel):
    def __init__(self, sattr):
	self.attr = sattr
	self.cbox = JColorChooser(self.attr.color)
	self.sz_field = JTextField(str(self.attr.size))
	szpanel = JPanel()
	szpanel.add(self.sz_field)
	szpanel.setBorder(BorderFactory.createTitledBorder("symbol size (integer)"))
	self.filled_box = JCheckBox("Filled ?:",self.attr.filled)
	self.shape_cbox = JComboBox(SymbolProps.sym_shape_map.keys())
	self.shape_cbox.setSelectedItem(self.attr.shape)
	self.shape_cbox.setBorder(BorderFactory.createTitledBorder("Shape"))
	panel1 = JPanel()
	panel1.setLayout(BorderLayout())
	panel1.add(szpanel,BorderLayout.NORTH)
	panel2 = JPanel()
	panel2.setLayout(GridLayout(1,2))
	panel2.add(self.shape_cbox)
	panel2.add(self.filled_box)
	panel1.add(panel2,BorderLayout.SOUTH)
	self.setLayout(BorderLayout())
	self.add(self.cbox,BorderLayout.CENTER)
	self.add(panel1,BorderLayout.SOUTH)
    def setAttribute(self,sattr):
	self.attr = sattr
	self.cbox.color = self.attr.color
	self.sz_field.text = str(self.attr.size)
	self.shape_cbox.setSelectedItem(self.attr.shape)
    def update(self):
	self.attr.color = self.cbox.getColor()
	self.attr.size = string.atoi(self.sz_field.getText())
	self.attr.filled = self.filled_box.isSelected()
	self.attr.shape = self.shape_cbox.getSelectedItem()
	self.attr.sym = self.attr.createSymbol()
#
class EditCurveAttr(JPanel):
    def __init__(self, cattr):
	self.attr = cattr
	self.cbox = JColorChooser(self.attr.color)
	self.sym_panel = EditSymbolAttr(cattr.sym_prop)
	self.thickness_field = JTextField(str(cattr.thickness),2)
	self.draw_symbol_box = JCheckBox("Draw Symbol?",cattr.draw_symbol)
	self.dps_field = JTextField(str(self.attr.data_per_symbol),2)
	self.dash_box = JComboBox(CurveProps.DASH_TYPES.keys())
	self.dash_box.setSelectedItem(self.attr.dash_type)
	self.dash_box.setBorder(BorderFactory.createTitledBorder("Dash type: (Only JDK2 & Slow!)"))
	tpanelx = JPanel()
	tpanelx.add(self.thickness_field)
	tpanelx.setBorder(BorderFactory.createTitledBorder("curve thickness (integer)"))
	tpanely = JPanel()
	tpanely.add(self.dps_field)
	tpanely.setBorder(BorderFactory.createTitledBorder("data per symbol(integer)"))
	tpanel = JPanel();tpanel.setLayout(GridLayout(2,2));
	tpanel.add(self.draw_symbol_box); tpanel.add(tpanelx);
	tpanel.add(tpanely); tpanel.add(self.dash_box);
	panel1 = JPanel()
	panel1.setLayout(BorderLayout())
	panel1.add(self.cbox,BorderLayout.CENTER)
	panel1.add(tpanel, BorderLayout.SOUTH)
	panel2 = JPanel()
	panel2.setLayout(BorderLayout())
	panel2.add(self.sym_panel,BorderLayout.CENTER)
	tp1 = JTabbedPane()
	tp1.addTab("Curve Attributes",panel1)
	tp1.addTab("Symbol Attributes",panel2)
	tp1.setSelectedComponent(panel1)
	self.setLayout(BorderLayout())
	self.add(tp1,BorderLayout.CENTER)
    def setAttribute(self,cattr):
	self.attr = cattr
	self.cbox.color = self.attr.color
	self.sym_panel.setAttribute(cattr.sym_prop)
	self.thickness_field.text = str(cattr.thickness)
	self.dps_field.text = str(cattr.data_per_symbol)
	self.draw_symbol_box.setSelected(cattr.draw_symbol)
	self.dash_box.setSelectedItem(cattr.dash_type)
    def update(self):
	self.attr.color = self.cbox.getColor()
	self.attr.thickness = string.atoi(self.thickness_field.text)
	self.attr.data_per_symbol = string.atoi(self.dps_field.text)
	self.attr.draw_symbol = self.draw_symbol_box.isSelected()
	self.attr.dash_type = self.dash_box.getSelectedItem()
	#print 'Updating Self.draw_symbol',self.draw_symbol,self.attr
	self.sym_panel.update()
class CurveDialog(JDialog):
    def __init__(self, cattrs):
	#JDialog.__init__(self,"Curve Attribute Editor")
	#("Curve Attribute Editor")
	if cattrs == None or len(cattrs) == 0:
	    raise "No curve attributes specified"
	self.attrs = cattrs
	self.cpanel = EditCurveAttr(cattrs[0])
	pane = self.getContentPane()
	pane.setLayout(BorderLayout())
	x = map(lambda x: x+1,range(len(self.attrs)))
	self.curveBox = JComboBox(x)
	self.curveBox.setBorder(BorderFactory.createTitledBorder("Curve #"))
	self.curveBox.setSelectedItem(0)
	class CListener(ItemListener):
	    def __init__(self,cbox,cpanel,cattrs):
		self.cbox = cbox
		self.cpanel = cpanel
		self.attrs = cattrs
	    def itemStateChanged(self, evt):
		crvNo = self.cbox.getSelectedItem()
		self.cpanel.update()
		self.cpanel.setAttribute(self.attrs[crvNo-1])
	self.curveBox.addItemListener(CListener(self.curveBox, self.cpanel,self.attrs))
	okBtn = JButton("OK",actionPerformed=self.ok_action)
	cancelBtn = JButton("Cancel",actionPerformed=self.cancel_action)
	btnPanel = JPanel()
	btnPanel.setLayout(GridLayout(1,3))
	btnPanel.add(self.curveBox)
	btnPanel.add(okBtn)
	btnPanel.add(cancelBtn)
	pane.add(self.cpanel,BorderLayout.CENTER)
	pane.add(btnPanel, BorderLayout.SOUTH)
	self.setLocation(100,100)
	self.pack()
	self.setVisible(1)
    def ok_action(self,evt):
	#crvNo = self.curveBox.getSelectedItem()
	#print 'Setting attributes for ', crvNo
	#print self.attrs[crvNo-1]
	self.cpanel.update()
	self.cancel_action(evt)
    def cancel_action(self,evt):
	self.dispose()
#

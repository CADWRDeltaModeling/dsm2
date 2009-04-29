import string,sys
try:
  import com.sun.xml.tree
  from com.sun.xml.tree import XmlDocument
except:
  'Xml.jar not in classpath'
from java.awt import BorderLayout, GridLayout
from java.awt.event import ActionListener
from javax.swing import JTree,JFrame,JPanel, JScrollPane
from javax.swing import JFrame, JPanel, JTextField, JRadioButton
from javax.swing import JButton, JComboBox, DefaultComboBoxModel
from javax.swing import JTable, Box, BoxLayout
from javax.swing.table import AbstractTableModel,DefaultTableModel
from javax.swing.tree import DefaultMutableTreeNode
from javax.swing.event import TreeSelectionListener, TableModelEvent
from com.sun.xml.tree import XmlDocument, TreeWalker, ElementNode
from vista.gui import TableSorter
from java.io import FileInputStream
import sys; from sys import exit
#
class TableRow:
  def __init__(self,name,headers,values):
    """
    TableRow(name,headers,values)
    a row in a table in dsm2 consisting of a name,
    headers and associated values.
    """
    self.name = name
    self.data = {}
    self.headers = headers
    for i in range(len(values)):
      self.data[headers[i]] = values[i]
  def __repr__(self):
    str_rep = ''
    keys = self.data.keys()
    for key in self.headers:
      if not key in keys: continue
      str_rep = str_rep + '\t' + str(key)
    str_rep = str_rep + '\n'
    for key in self.headers:
      if not key in keys: continue
      str_rep = str_rep + '\t' + str(self.data[key])
    str_rep = str_rep + '\n'
    return str_rep
  def toXml(self,xdoc):
    """
    returns an element containing this table
    """
    xe = xdoc.createElement("table_row")
    keys = self.data.keys()
    for xkey in self.headers:
      if not xkey in keys: continue
      xe.setAttribute(str(xkey),str(self.data[xkey]))
    return xe
#
class DSM2Data:
  def __init__(self,name):
    self.name = string.upper(name)
    self.titles = []
    self.channels = {}
    self.irreg_geom = {}
    self.scalars = {}
    self.inputpaths = {}
    self.outputpaths = {}
    self.io_files = {}
    self.junctions = {}
    self.xsects = {}
    self.list_chan = []
    self.reservoirs = {}
    self.gates = {}
    self.translations = {}
    self.types = {}
  def dump(self):
    print 'Name: ',self.name
  def appendElementsFromArray(self,array,xdoc,root,name):
    for x in array:
      te = xdoc.createElement("table_row")
      te.setAttribute("value",str(x))
      root.appendChild(te)
  def appendElementsFromAssoc(self,assoc,xdoc,root):
    for x in assoc.keys():
      root.appendChild(assoc[x].toXml(xdoc))
  def createXSectElement(self,xdoc, xsect):
    xe = xdoc.createElement("xsect")
    for xkey in xsect.data.keys():
      xe.setAttribute(str(xkey),str(xsect.data[xkey]))
    return xe
  def createChannelElements(self,xdoc,root):
    for key in self.channels.keys():
      channel_info = self.channels[key]
      ce = xdoc.createElement("channel")
      for ckey in channel_info.data.keys():
        if string.find(string.lower(str(ckey)),'xsect') != -1:
          xe = self.createXSectElement(xdoc,self.xsects[channel_info.data[ckey]])
          ce.appendChild(xe)
        else:
          ce.setAttribute(ckey,channel_info.data[ckey])
      root.appendChild(ce)
  def toXml(self):
    xdoc = XmlDocument()
    root = xdoc.createElement("dsm2")
    xdoc.appendChild(root)
    node = xdoc.createElement("titles")
    root.appendChild(node)
    self.appendElementsFromArray(self.titles,xdoc,node,"titles")
    node = xdoc.createElement("scalars")
    root.appendChild(node)
    self.appendElementsFromAssoc(self.scalars,xdoc,node)
    node = xdoc.createElement("inputpaths")
    root.appendChild(node)
    self.appendElementsFromAssoc(self.inputpaths,xdoc,node)
    node = xdoc.createElement("outputpaths")
    root.appendChild(node)
    self.appendElementsFromAssoc(self.outputpaths,xdoc,node)
    node = xdoc.createElement("io_files")
    root.appendChild(node)
    self.appendElementsFromAssoc(self.io_files,xdoc,node)
    node = xdoc.createElement("junctions")
    root.appendChild(node)
    self.appendElementsFromAssoc(self.junctions,xdoc,node)
    node = xdoc.createElement("list_chan")
    root.appendChild(node)
    self.appendElementsFromArray(self.list_chan,xdoc,node,"list_chan")
    node = xdoc.createElement("reservoirs")
    root.appendChild(node)
    self.appendElementsFromAssoc(self.reservoirs,xdoc,node)
    node = xdoc.createElement("channels")
    root.appendChild(node)
    self.appendElementsFromAssoc(self.channels,xdoc,node)
    node = xdoc.createElement("irreg_geom")
    root.appendChild(node)
    self.appendElementsFromAssoc(self.irreg_geom,xdoc,node)
    node = xdoc.createElement("xsects")
    root.appendChild(node)
    self.appendElementsFromAssoc(self.xsects,xdoc,node)
    node = xdoc.createElement("gates")
    root.appendChild(node)
    self.appendElementsFromAssoc(self.gates,xdoc,node)
    node = xdoc.createElement("translations")
    root.appendChild(node)
    self.appendElementsFromAssoc(self.translations,xdoc,node)
    node = xdoc.createElement("types")
    root.appendChild(node)
    self.appendElementsFromAssoc(self.types,xdoc,node)
    return xdoc
# a parser for dsm2 input files
class DSM2Parser:
  def __init__(self,file='dsm2.inp'):
    """
    def DSM2Parser(self,file='dsm2.inp')
    initializes a parser for dsm2 and parses the file.
    All the data is contained in dsm2_data field which
    is an instance of DSM2Data
    """
    self.dsm2_data = None
    import javapath
    x = javapath.split(file)
    self.dsm2dir = x[0]
    self.dsm2file = x[1]
    #self.parse_dsm2(file)#self.dsm2file)
    self.parse_dsm2(self.dsm2file)
  #
  def parse_dsm2(self,file='dsm2.inp'):
    """
    parse_dsm2(self,file='dsm2.inp')
    This is the main entry point for parsing dsm2 input files.
    Be careful as the constructor has already parsed an input file
    Calling this method will only serve to overwrite whats already been
    parsed.
    """
    import javapath,os
    # if not absolute path go relative to original directory
    if file[0] != os.sep:
	file = javapath.join(self.dsm2dir,file)
    fh = open(file)
    while 1:
      line = fh.readline()
      if not line:
        fh.close()
        break
      # upper case line
      line = self.process_line(line)
      # if blank skip it
      if len(line) == 0: continue
      # initialize the global input data
      if not self.dsm2_data:
        self.dsm2_data = DSM2Data(file)
      # check for header and send for processing to appropriate parse 
      if line == "TITLES":
        self.parse_titles(fh)
      elif line == "INP_FILES":
        self.parse_inp_files(fh)
      elif line == "CHANNELS":
        self.parse_channels(fh)
      elif line == "IRREG_GEOM":
        self.parse_irreg_geom(fh)
      elif line == "JUNCTIONS":
        self.parse_junctions(fh)
      elif line == "XSECTS":
        self.parse_xsects(fh)
      elif line == "RESERVOIRS":
        self.parse_reservoirs(fh)
      elif line == "GATES":
        self.parse_gates(fh)
      elif line == "INPUTPATHS":
        self.parse_inputpaths(fh)
      elif line == "OUTPUTPATHS":
        self.parse_outputpaths(fh)
      elif line == "IO_FILES":
        self.parse_io_files(fh)
      elif line == "TRANSLATION":
        self.parse_translations(fh)
      elif line == "TYPE":
        self.parse_type(fh)
      elif line == "SCALAR":
        self.parse_scalar(fh)
      elif line == "LIST_CHAN":
        self.parse_list_chan(fh)
      elif line == "TIDEFILE":
        self.parse_tidefile(fh)
      elif line == "RATE_COEFFS":
        self.parse_rate_coeffs(fh)
      elif line == "OBJ2OBJ":
        self.parse_obj2obj(fh)
      elif line == "ENVVARS":
        self.parse_envvars(fh)
      else:
        fh.close()
        raise "Expecting one of ...."
  #
  def process_line(self,line):
    """
    processes a line, stripping it of whitespace and comments
    and upcasing it and then returns the processed line
    """
    line = string.strip(line) # take of eol marker and extra white space
    # strip comments
    comment_index = string.find(line,'#')
    if comment_index != -1:
      line = string.strip(line[:comment_index])
    # upper case line
    line = string.upper(line)
    return line
  #
  def get_parsed_lines_to_end(self,fh):
    """
    gets all the parsed and processed lines till the END keyword
    is encountered. a exception is raised if <EOF> is encountered
    """
    lines = []
    while 1:
      line = fh.readline()
      if not line:
        fh.close()
        raise 'Unexpected end of file while doing titles'
      #
      line = self.process_line(line)
      if line == "": continue
      if line == "END": break
      lines.append(line)
    return lines
  #
  def parse_table(self,fh,name,tdata,keyId=0,headers=None):
    """
    def parse_table(self,fh,name,tdata,keyId=0,headers=None):
    parses table data 
    """
    keySeparator='_' # for key derived from multiple keys
    lines = self.get_parsed_lines_to_end(fh)
    if not lines: return
    if not headers: headers = string.split(lines[0])
    for line in lines[1:]:
      if keyId == -1:
        tdata.append(line)
      else:
        fs = string.split(line)
        fs = map(string.strip,fs)
        if hasattr(keyId,'append'):
          key = str(fs[keyId[0]])
          for id in keyId[1:]:
            key = str(key) + keySeparator + str(fs[id])
        else:
          key = fs[keyId]
        tdata[key] = TableRow(name,headers,fs);
  #
  def parse_titles(self,fh):
    self.parse_table(fh,'titles',self.dsm2_data.titles,-1,['TITLES'])
  #
  def parse_inp_files(self,fh):
    lines = self.get_parsed_lines_to_end(fh)
    if not lines: return
    for line in lines:
      inp_file = string.lower(line) # temp fix by lowering case. should use glob
      self.parse_dsm2(inp_file)
  #
  def parse_channels(self,fh):
    headers = [ 'chan', 'length', 'manning', 'disp', 'downnode', 'upnode', 'xsect1', 'dist1', 'xsect2', 'dist2', 'xsect3', 'dist3', 'xsect4', 'dist4']
    self.parse_table(fh,'channels',self.dsm2_data.channels,0,headers)
  #
  def parse_irreg_geom(self,fh):
    self.parse_table(fh,'irreg_geom',self.dsm2_data.irreg_geom,[0,1])
  #
  def parse_scalar(self,fh):
    self.parse_table(fh,'scalars',self.dsm2_data.scalars,0,['SCALAR','VALUES'])
  #
  def parse_inputpaths(self,fh):
    self.parse_table(fh,'inputpaths',self.dsm2_data.inputpaths)
  #
  def parse_outputpaths(self,fh):
    self.parse_table(fh,'outputpaths',self.dsm2_data.outputpaths)
  #
  def parse_io_files(self,fh):
    self.parse_table(fh,'io_files',self.dsm2_data.io_files)
  #
  def parse_junctions(self,fh):
    self.parse_table(fh,'junctions',self.dsm2_data.junctions)
  #
  def parse_xsects(self,fh):
    self.parse_table(fh,'xsects',self.dsm2_data.xsects)
  #
  def parse_list_chan(self,fh):
    self.parse_table(fh,'list_chan',self.dsm2_data.list_chan,-1,['LIST_CHAN'])
  #
  def parse_reservoirs(self,fh):
    headers = [ 'name', 'area', 'stage', 'botelv', 
                'node1', 'coeff2res1', 'coeff2chan1',
                'node2', 'coeff2res2', 'coeff2chan2',
                'node3', 'coeff2res3', 'coeff2chan3',
                'node4', 'coeff2res4', 'coeff2chan4',
                'node5', 'coeff2res5', 'coeff2chan5',
                'node6', 'coeff2res6', 'coeff2chan6',
                'node7', 'coeff2res7', 'coeff2chan7'
                ]
    self.parse_table(fh,'reservoirs',self.dsm2_data.reservoirs,0,headers)
  #
  def parse_gates(self,fh):
    self.parse_table(fh,'gates',self.dsm2_data.gates)
  #
  def parse_translations(self,fh):
    self.parse_table(fh,'translations',self.dsm2_data.translations)
  #
  def parse_type(self,fh):
    self.parse_table(fh,'types',self.dsm2_data.types)
  #
  def parse_tidefile(self,fh):
    self.parse_table(fh,'tidefile',self.dsm2_data.tidefiles)
  #
  def parse_rate_coeffs(self,fh):
    self.parse_table(fh,'rate_coeffs',self.dsm2_data.rate_coeffs)
  #
  def parse_envvars(self,fh):
    self.parse_table(fh,'envvars',self.dsm2_data.envvars)
#
class TreeViewer:
  def __init__(self,xmlfile=None):
    if xmlfile:
      self.readDoc(xmlfile)
  # create xml doc
  def readDoc(self,xmlfile = "dsm2.xml"):
    self.xdoc = XmlDocument.createXmlDocument(FileInputStream(xmlfile),0)
  # creates a jtree displayable node structure
  def createTree(self,xnode):
    if xnode.hasChildNodes():
      tnode = DefaultMutableTreeNode(xnode.getNodeName())
    else:
      tnode = DefaultMutableTreeNode(xnode)
      return tnode
    child_nodes = xnode.getChildNodes()
    child_count = child_nodes.getLength()
    for i in range(child_count):
      element = child_nodes.item(i)
      if xnode.getTagName() == "table_row": continue
      if isinstance(element,ElementNode):
        tnode.add( self.createTree(child_nodes.item(i)) )
    return tnode
  def gui(self):
    xnode = self.xdoc.getDocumentElement()
    tnode = self.createTree(xnode)  
    # create tree and display
    jt = JTree(tnode)
    jsp = JScrollPane(jt)
    tree_box = Box(BoxLayout.Y_AXIS)
    tree_box.add(jsp)
    tree_box.add(Box.createHorizontalStrut(10))
    headerSorter = TableSorter(DefaultTableModel())
    jtb = JTable(headerSorter)
    headerSorter.addMouseListenerToHeaderInTable(jtb)
    jtb.setAutoResizeMode(JTable.AUTO_RESIZE_OFF)
    jsp2 = JScrollPane(jtb)
    table_box = Box(BoxLayout.Y_AXIS)
    table_box.add(jsp2)
    table_box.add(Box.createHorizontalStrut(500))
    mp = JPanel()
    mp.setLayout(BoxLayout(mp,BoxLayout.X_AXIS))
    mp.add(tree_box)
    mp.add(table_box)
    # add listeners
    nsl = NodeSelectionListener(jtb,xnode)
    jt.addTreeSelectionListener(nsl)
    #
    return mp
#
class NodeTableModel(AbstractTableModel):
  def __init__(self,xnode):
    self.xnode = xnode
    cnodes = xnode.getChildNodes()
    self.table_nodes = []
    count = cnodes.getLength()
    for i in range(count):
      element = cnodes.item(i)
      if element.getNodeName() == "table_row":
        self.table_nodes.append(element)
    self.col_list = []
    for x in self.table_nodes:
      attrs = x.getAttributes()
      ncol = attrs.getLength()
      for i in range(ncol):
        ax = attrs.item(i)
        nx = ax.getNodeName()
        if nx in self.col_list:
          pass
        else:
          self.col_list.append(nx)
  def getRowCount(self):
    return len(self.table_nodes)
  def getColumnCount(self):
    return len(self.col_list)
  def getColumnName(self,cindex):
    return self.col_list[cindex]
  def isCellEditable(self,rindex,cindex):
    return 1
  def getValueAt(self,rindex,cindex):
    val = self.table_nodes[rindex].getAttribute(self.col_list[cindex])
    if val:
      return val
    else:
      return ""
  def setValueAt(self,value,rindex,cindex):
    self.table_nodes[rindex].setAttribute(self.col_list[cindex],value)
# end of NodeTableModel class
class NodeSelectionListener(TreeSelectionListener):
  def __init__(self,jtb,xroot):
    self.jtb = jtb
    self.xroot = xroot
    self.empty_model = DefaultTableModel()
  def valueChanged(self,evt):
    tnode = evt.getPath().getLastPathComponent()
    xnode_name = tnode.getUserObject()
    tw=TreeWalker(self.xroot)
    xnode=tw.getNextElement(xnode_name)
    if not xnode: return
    if isinstance(xnode,ElementNode):
      headerSorter = TableSorter(NodeTableModel(xnode))
    else:
      headerSorter = TableSorter(DefaultTableModel())
    headerSorter.addMouseListenerToHeaderInTable(self.jtb)
    self.jtb.setModel(headerSorter)
    #self.sizeColumns()
    self.jtb.sizeColumnsToFit(-1)
  #
  def sizeColumns(self):
    col_model = self.jtb.getColumnModel()
    e = col_model.getColumns()
    cols = []
    while e.hasMoreElements():
      cols.append(e.nextElement())
    tm = self.jtb.getModel()
    col_range = range(tm.getColumnCount())
    col_sizes = []
    row_range = range(min(20,tm.getRowCount()))
    for i in col_range:
      col_sizes.append(len(tm.getColumnName(i)))
    for i in row_range:
      for j in col_range:
        val = tm.getValueAt(i,j)
        col_sizes[j] = max(col_sizes[j],len(val))
    for i in range(len(cols)):
      cols[i].setMaxWidth(col_sizes[i]*10)
      cols[i].setMinWidth(col_sizes[i]*10)
      cols[i].setWidth(col_sizes[i]*10)
#
class RegPlot:
  def __init__(self, channel, xsect, dist):
    self.channel = channel
    self.xsect = xsect
    self.dist = dist
    w1 = string.atof(xsect.data['WIDTH'])
    be1 = string.atof(xsect.data['BOTELV'])
    self.stations =   [ -w1/2, -w1/2, w1/2, w1/2]
    self.elevations = [ 5.0, be1, be1, 5.0]
  def getPlot(self,symbol="circle-filled"):
    xlabel = "stations"
    ylabel = "elevations"
    title = "Channel: " + self.channel + " @ dist: " + self.dist
    legend = "Regular XSect @ dist: " + self.dist
    color = "red"
    gridy = 1
    gridx = 0
    import vutils
    pl = vutils.xyplot(self.stations,self.elevations,
                       xlabel, ylabel, title,
                       legend,
                       color, symbol, gridy, gridx)
    return pl
    
#reads an irregular xsect file and plots it
class IrregPlot:
  def __init__(self,fname):
    self.parse(fname)
  #
  def parse(self,fname):
    """
    parses an irreg geom file to construct a
    table with name of the cross-section and
    the elevation, area, perimeter, width, rh,
    xc, zc of the cross-section
    """
    cfile = open(fname)
    # get table name first
    while 1:
      line = string.strip(cfile.readline())
      if not line:
        raise "No data found for "+fname
      if string.find(line,"Cross-section") != -1:
        zfs = string.split(line,':')
        zfs = string.split(string.strip(zfs[1]),'_')
        self.channel = zfs[0]
        self.dist = zfs[1]
        break
      else:
        continue
    #find header line and then skip one line
    while 1:
      line = string.strip(cfile.readline())
      if string.find(line,"Elev") != -1:
        break
      else:
        continue
    self.irreg_fields = string.split(line)
    # skip till == line
    while 1:
      line = string.strip(cfile.readline())
      if string.find(line,"==") != -1:
        break
      else:
        continue
    # now put data in fields
    self.data_strs = []
    while 1:
      line = string.strip(cfile.readline())
      zfs = string.split(line)
      if len(zfs) != len(self.irreg_fields):
        break
      else:
        self.data_strs.append(zfs)
    # read station
    while line.find('station') == -1 :
      line = string.strip(cfile.readline())
    zfs = string.split(line,':')
    self.stations = map(string.atof,string.split(zfs[1]))
    # read elevation
    while line.find('elevation') == -1 :
      line = string.strip(cfile.readline())
    zfs = string.split(line,':')
    self.elevations = map(string.atof,string.split(zfs[1]))
    #  
    cfile.close()
  #
  def getPlot(self,type="XSection",symbol="circle-filled"):
    if type == "XSection":
      xarray = self.stations
      yarray = self.elevations
      color = "black"
      return self.mkPlot(xarray,yarray,"stations","elevations",symbol,color)
    elif type == "Area":
      index = 1
      ylabel = "area"
      color = "red"
    elif type == "Perimeter":
      index = 2
      ylabel = "perimeter"
      color = "green"
    elif type == "Width":
      index = 3
      ylabel = "width"
      color = "blue"
    elif type == "Rh":
      index = 4
      ylabel = "Rh"
      color = "cyan"
    elif type == "Xc":
      index = 5
      ylabel = "Xc"
      color = "magenta"
    elif type == "Zc":
      index = 6
      ylabel = "Zc"
      color = "yellow"
    yarray = []
    for x in self.data_strs:
      yarray.append(x[index])
    yarray = map(string.atof,yarray)
    xarray = []
    for x in self.data_strs:
      xarray.append(x[0])
    xarray = map(string.atof,xarray)
    return self.mkPlot(xarray,yarray,"elevations",ylabel,symbol,color)
  #
  def mkPlot(self, xarray, yarray,
             xlabel="stations", ylabel="elevations",
             symbol="circle-filled",color = "red"):
    """
    plots the last parsed irregular x section
    """
    title = "Channel: " + self.channel + " @ dist: " + self.dist
    legend = "Irregular XSect @ dist: " + self.dist
    gridy = 1
    gridx = 0
    import vutils
    pl = vutils.xyplot(xarray,yarray,
                       xlabel, ylabel, title,
                       legend,
                       color, symbol, gridy, gridx)
    return pl
  #
#
class IrregGeom:
  def __init__(self, dsm2file = "dsm2.inp"):
    dd = DSM2Parser(dsm2file)
    # store channel keys
    self.dsm2_data = dd.dsm2_data
    self.irreg_geom = self.dsm2_data.irreg_geom
    self.all_keys = self.dsm2_data.irreg_geom.keys()
    self.xsects = self.dsm2_data.xsects
    #
    self.channels = None
  #
  def getChanList(self):
    if self.channels:
      return self.channels
    self.channels = []
    for key in self.all_keys:
      chan = string.split(key,'_')[0]
      if chan in self.channels:
        continue
      else:
        self.channels.append(chan)
    self.channels.sort()
    return self.channels
  def plot(self,channel,type='XSection'):
    """
    """
    chan_keys = []
    for key in self.all_keys:
      if channel == string.split(key,'_')[0]:
        chan_keys.append(key)
        irreg_plots = 1
    if len(chan_keys) == 0 :
      print 'No irregular geometry found for ' + channel
      # look for regular x sect geom
      try:
        xsect1 = self.dsm2_data.channels[channel].data['xsect1']
        xsect1 = self.dsm2_data.xsects[xsect1]
        dist1 =  self.dsm2_data.channels[channel].data['dist1']
        xsect2 = self.dsm2_data.channels[channel].data['xsect2']
        xsect2 = self.dsm2_data.xsects[xsect2]
        dist2 =  self.dsm2_data.channels[channel].data['dist2']
        irreg_plots = 0
      except KeyError:
        print 'No channel: ' + channel + ' exists!'
        return
    #
    plots = []
    if irreg_plots:
      for key in chan_keys :
        irreg_file = string.lower(self.irreg_geom[key].data['FILENAME'])
        ip = IrregPlot(irreg_file)
        plots.append(ip.getPlot(type))
    else:
      ip1 = RegPlot(channel,xsect1,dist1)
      ip2 = RegPlot(channel,xsect2,dist2)
      plots.append(ip1.getPlot())
      plots.append(ip2.getPlot())
    #
    from vista.graph import Graph, MultiPlot, AxisAttr
    from vista.app import DataGraph
    graph = Graph()
    mp = MultiPlot(len(plots),1)
    lx = plots[0].getAxis(AxisAttr.LEFT)
    bx = plots[0].getAxis(AxisAttr.BOTTOM)
    lx_range = [lx.getScale().getDataMinimum(),lx.getScale().getDataMaximum()]
    bx_range = [bx.getScale().getDataMinimum(),bx.getScale().getDataMaximum()]
    #
    for pl in plots:
      mp.add(pl)
      lx = pl.getAxis(AxisAttr.LEFT)
      bx = pl.getAxis(AxisAttr.BOTTOM)
      lx_range[0] = min(lx.getScale().getDataMinimum(),lx_range[0])
      bx_range[0] = min(bx.getScale().getDataMinimum(),bx_range[0])
      lx_range[1] = max(lx.getScale().getDataMaximum(),lx_range[1])
      bx_range[1] = max(bx.getScale().getDataMaximum(),bx_range[1])
    #
    for pl in plots:
      lx = pl.getAxis(AxisAttr.LEFT)
      lx.setDCRange(lx_range[0],lx_range[1])
      bx = pl.getAxis(AxisAttr.BOTTOM)
      bx.setDCRange(bx_range[0],bx_range[1])
    graph.add(mp)
    if irreg_plots:
      graph.setTitle("Irreg XSects for " + channel)
    else:
      graph.setTitle("Regular XSects for " + channel)
    dg = DataGraph(graph,channel,0)
    dg.setLocation(100,100)
    dg.setVisible(1)
    #dg.setSize(600,400)
#
class DSM2GeomViewer:
  def __init__(self,dsm2file="dsm2.inp"):
    """
    creates DSM2GeomViewer
    """
    self.dsm2file = dsm2file
  def _test1(self):
    ig = IrregGeom()
    chan_list =['441','6','100']
    for chan in chan_list:
      ig.plot(chan)
  def _test2():
    ip = IrregPlot('../dsm2-input-files/irregular_xsects/6_0.62089.txt')
    pl = ip.getPlot()
    from vista.graph import Graph, MultiPlot, AxisAttr
    from vista.app import DataGraph
    graph = Graph()
    graph.add(pl)
    dg = DataGraph(graph,'',1)
  def gui(self):
    file_field = JTextField(self.dsm2file,15)
    load_btn = JButton('Load input   ')
    chan_box = JComboBox()
    chan_box.setEditable(1)
    do_btn = JButton('Plot channel')
    choices = [JRadioButton('XSection'),
               JRadioButton('Area'),
               JRadioButton('Width'),
               JRadioButton('Perimeter'),
               JRadioButton('Rh'),
               JRadioButton('Xc'),
               JRadioButton('Zc')]
    #bg = ButtonGroup()
    #for choice in choices: bg.add(choice)
    class load_listener(ActionListener):
      def __init__(self,file_field,load_btn,
                   chan_box,do_btn,
                   choices):
        self.file_field = file_field
        self.load_btn = load_btn
        self.chan_box = chan_box
        self.do_btn = do_btn
        self.do_btn.setEnabled(0)
        self.load_btn.addActionListener(self)
        self.do_btn.addActionListener(self)
        self.choices=choices
      def actionPerformed(self,evt):
        if evt.getSource() == self.load_btn :
          self.ig = IrregGeom(self.file_field.getText())
          import javax.swing
          md = javax.swing.DefaultComboBoxModel(self.ig.getChanList())
          self.chan_box.setModel(md)
          self.do_btn.setEnabled(1)
        elif evt.getSource() == self.do_btn :
          if self.ig:
            for choice in self.choices:
              if choice.isSelected():
                self.ig.plot(self.chan_box.getSelectedItem(),choice.getText())
    ll = load_listener(file_field, load_btn, chan_box, do_btn,choices)
    p1 = JPanel()
    p1.setLayout(BorderLayout())
    p1.add(file_field,BorderLayout.CENTER)
    p1.add(load_btn,BorderLayout.EAST)
    p2 = JPanel()
    p2.setLayout(BorderLayout())
    p2.add(chan_box,BorderLayout.CENTER)
    p2.add(do_btn,BorderLayout.EAST)
    p3 = JPanel()
    p3.setLayout(GridLayout(1,2))
    p3.add(choices[0]); p3.add(choices[1])
    p4 = JPanel()
    p4.setLayout(GridLayout(1,2))
    p4.add(choices[2]); p4.add(choices[3])
    p5 = JPanel()
    p5.setLayout(GridLayout(1,2))
    p5.add(choices[4]); p5.add(choices[5])
    p6 = JPanel()
    p6.setLayout(GridLayout(1,2))
    p6.add(choices[6])
    #
    mp = JPanel()
    mp.setLayout(GridLayout(6,1))
    mp.add(p1)
    mp.add(p2)
    mp.add(p3)
    mp.add(p4)
    mp.add(p5)
    mp.add(p6)
    return mp
#
def geom_viewer(dsm2file = "dsm2.inp"):
  """
  geom_viewer(dsm2file = "dsm2.inp")
  starts off a dsm2 geometry viewer for dsm2 input data
  Irregular xsections are plotted if available otherwise
  regular xsections are plotted.
  """
  dgv = DSM2GeomViewer(dsm2file)
  mp = dgv.gui()
  fr = JFrame()
  fr.setTitle('Geom Viewer')
  fr.getContentPane().add(mp)
  fr.setLocation(300,100)
  fr.pack()
  sz = fr.getSize()
  fr.setSize(250,sz.height)
  fr.setVisible(1)
# create a tree node structure
from javax.swing import JPanel, JButton, JTextField
from java.awt import BorderLayout
def tree():
  """
  tree(xmlfile="dsm2.xml")
    creates a tree view on a given xml file of dsm2 input data
  """
  tv = TreeViewer()
  mp2 = JPanel()
  mp2.setLayout(BorderLayout())
  tf = JTextField("dsm2.inp")
  pb = JButton("parse")
  mp2.add(tf,BorderLayout.CENTER)
  mp2.add(pb,BorderLayout.EAST)
  class ParseListener(ActionListener):
    def __init__(self,tf,tv,fr):
      self.tf = tf
      self.tv = tv
      self.fr = fr
    def actionPerformed(self,evt):
      dsm2file = self.tf.getText()
      parser = DSM2Parser(dsm2file)
      self.tv.xdoc = parser.dsm2_data.toXml()
      self.fr.getContentPane().add(self.tv.gui(),BorderLayout.CENTER)
      self.fr.pack()
      self.fr.setVisible(1)
  fr = JFrame()
  fr.setTitle("DSM2Tree")
  fr.setLocation(100,100)
  fr.setSize(600,60)
  fr.getContentPane().setLayout(BorderLayout())
  fr.getContentPane().add(mp2,BorderLayout.NORTH)
  al = ParseListener(tf,tv,fr)
  pb.addActionListener(al)
  fr.pack()
  fr.setVisible(1)
#
def xmlize(xmlfile = "dsm2.xml", dsm2file = "dsm2.inp"):
  """
  xmlize(xmlfile = "dsm2.xml", dsm2file = "dsm2.inp")
    xmlizes a dsm2file into an xml file
  """
  parser = DSM2Parser(dsm2file)
  from java.io import FileOutputStream
  try:
    from com.sun.xml.tree import XmlDocument
  except:
    print 'Xml.jar not in classpath'
  xdoc = parser.dsm2_data.toXml()
  fos = FileOutputStream(xmlfile)
  xdoc.write(fos)
  fos.close()
#
if __name__ == "main":
  print "Being run as main"
#

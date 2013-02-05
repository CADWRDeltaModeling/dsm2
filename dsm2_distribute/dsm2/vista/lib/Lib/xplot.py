import vutils, re, string, os, posixpath
import vdss
from vutils import *
from vista.app import GroupFrame
from vista.set import Group
from vista.gui import VistaUtils
from vista.graph import TextLine, GEAttr, GraphicElement, CurveAttr
from vista.app import CurveFactory
# XML library
from org.w3c.dom import Document
from org.w3c.dom.traversal import TreeWalker
#from com.sun.xml.tree import XmlDocument, TreeWalker
from org.w3c.dom import Element
# basic java stuff
from java.io import FileOutputStream, BufferedOutputStream, PrintWriter, FileInputStream
from java.io import FileOutputStream,File
from java.io import PrintWriter,BufferedWriter,FileWriter
from java.lang import Class, System
from java.util import Date
# gui stuff
from javax.swing.table import DefaultTableModel
from javax.swing.tree import DefaultMutableTreeNode, TreePath
from javax.swing.tree import TreePath, DefaultTreeModel
from java.awt import Font,Rectangle
# attribute stuff
import xattr
from xattr import *
# graph sizes for particular orientation
__pwidth__=600
__pheight__=800
__lwidth__=750
__lheight__=650
# font sizes
__legend_font_size__ = 10
__left_axis_font_size__ = 10
__bottom_axis_font_size__ = 12
__plot_title_font_size__ = 18
# writers
class ReportWriter:
    def __init__(self):
        raise "Define __init__ in subclass!"
    def write_header_info(self,outf,txtfile,ref,path):
        raise "define write_header_info in subclass!"
    def write_data(self,outf,txtfile,ref,path):
        raise "define write_data in subclass!"
    def write_footer_info(self,outf,txtfile,ref,path):
        raise "define write_footer_info in subclass!"
#
class DefaultReportWriter(ReportWriter):
    def __init__(self):
        pass
    def write_header_info(self,outf,txtfile,ref,path):
        """
        header info
        """
        outf.println(txtfile)
        outf.println(path)
    def write_data(self,outf,txtfile,ref,path,el):
        """
        data info
        """
        outf.println("%-20s%-20s%-20s"%(el.getXString(),el.getYString(),el.getFlagString()))
    def write_footer_info(self,outf,txtfile,ref,path):
        """
        footer info
        """
        outf.println()
#
_writers = [ DefaultReportWriter() ]
_txt_writer = DefaultReportWriter()
# caching of dss file info
gfilemap = {}
#
def dynamic_eval(x,gdict,ldict):
    """
    returns the dynamic value by first attempting
    to evaluate x and if that fails to set the
    value to x itself and then returning that value
    """
    try:
        return eval('%s'%x,gdict,ldict)
    except:
        return x
# is this jdk 2 or more
def isJDK2():
    """
    returns 1 if java version is 1.2 or greater
    """
    return System.getProperty("java.version") > "1.1z"
#
def create2DCurve(ref,xpos,ypos,name):
    """
    creates a 2d curve & only works for 1.2 or greater versions of JDK
    """
    from vista.graph import FlaggedCurve2D
    cdm = CurveFactory.createCurveDataModel(ref.getData(),xpos,ypos,name)
    c2d = FlaggedCurve2D(CurveAttr(), cdm)
    return c2d
#
import re
mo = re.compile('a[0-9]+')
def subrefs(x):
    return mo.sub(refsubs,x)
def refsubs(mo):
    if mo.group(0).find('a') == 0:
        id = string.atoi(mo.group(0)[1:])-1
        return 'refs['+str(id)+'].getData()'
    else:
        return mo.group(0)
#
class MyTreeNode(DefaultMutableTreeNode):
    """
    A tree node to retain a user object. This was needed as the attributes
    of python objects are invisible to java classes.
    """
    def __init__(self,obj,name,folder=0):
        DefaultMutableTreeNode.__init__(self,name,folder)
        self.obj = obj
    def getUserObject(self):
        return self.obj
    def setUserObject(self,obj):
        if type(obj) == type(''):
            DefaultMutableTreeNode.setUserObject(self,obj)
        else:
            self.obj = obj
class ScriptData:
    def __init__(self,lines=None,type='pre'):
        if lines !=None:
            self.lines = lines
        else:
            self.lines = []
        self.type = type
    def _mkclone(self):
        sdata = ScriptData(self.lines)
        return sdata
    def toXml(self,xdoc):
        el = xdoc.createElement("%s_script_data"%self.type)
        for line in self.lines:
            le=xdoc.createElement("line")
            le.setAttribute("content",line)
            el.appendChild(le)
        return el
    def fromXml(self,root):
        if root == None: return
        el = None
        tw = TreeWalker(root)
        if root.getNodeName() == "%s_script_data"%self.type:
            el = root
        else:
            el = tw.getNextElement("%s_script_data"%self.type)
        if el == None: return
        tw = TreeWalker(el)
        self.lines = []
        while 1:
            le = tw.getNextElement("line")
            if le == None: break
            self.lines.append(le.getAttribute("content"))
#
class StudyData:
    def __init__(self,name,gdata=None,init_script_data=None):
        self.root = MyTreeNode(self,name,1)
        self.name = name
        self.gdata=[]
        if gdata == None:
            self.addGraphData(GraphData(''))
        else:
            for gd in gdata:
                self.addGraphData(gd)
        self.current_gd = self.gdata[0]
        if init_script_data == None: 
            init_script_data = ScriptData(None,'sinit')
        self.init_script_data = init_script_data
        self.file = None
    def addGraphData(self,gd):
        gd.study = self
        self.gdata.append(gd)
        self.root.add(gd.root)
    def add(self,gd):
        pass
    def remove(self,gd):
        pass
    def insert(self,gd,i):
        pass
    def execfile(self,file):
        execfile(file)
    def _update(self):
        e = self.root.children()
        data = []
        while e.hasMoreElements():
            x = e.nextElement()
            data.append(x.getUserObject())
        self.gdata = data
    def graph(self):
        self._update()
        for gd in self.gdata: gd.graph()
    def run_script(self):
        self._update()
        for gd in self.gdata: gd.run_script()
    def tabulate(self):
        self._update()
        for gd in self.gdata: gd.tabulate()
    def saveimg(self,type):
        self._update()
        for gd in self.gdata: gd.saveimg(type)
    def saveweb(self,file):
        """
        outputs the graphs and tables for this study in
        html format
        """
        self._update()
        fh = open(file,'w')
        fh.write('<html> <body> <ol title = "Study %s">\n'%self.name)
        graph_count = 1
        dir_name = os.path.dirname(file)
        file_noext = string.split(os.path.basename(file),'.')[0]
        for gd in self.gdata:
            fh.write('<li><a href="%sG%02d_index.html">%s</a>\n'%(file_noext,graph_count,gd.name))
            gd.saveweb('%s/%sG%02d_index.html'%(dir_name,file_noext,graph_count))
            graph_count=graph_count+1
        fh.write('</body> </html>\n')
        fh.close()
    def writedss(self,dssfile):
        self._update()
        for x in self.gdata: x.writedss(dssfile)
    def writetxt(self,txtfile,outf=None):
        created_here = 0
        if outf==None:
            created_here = 1
            outf = PrintWriter(BufferedWriter(FileWriter(txtfile)))
        self._update()
        for x in self.gdata: x.writetxt(self,txtfile,outf)
        if created_here: outf.close()
    def save(self, file=None):
        if file == None: file = self.file
        if file == None: return
        saveXml(file,self)
        self.file = file
    def toXml(self,xdoc):
        el = xdoc.createElement("study_data")
        el.setAttribute("name",self.name)
        self._update()
        for data in self.gdata:
            el.appendChild(data.toXml(xdoc))
        el.appendChild(self.init_script_data.toXml(xdoc))
        return el
    def load(self,file):
        loadXml(file,self)
        self.file = file
    def fromXml(self,xdoc):
        root = xdoc.getDocumentElement()
        if root == None: return
        tw = TreeWalker(root)
        if root.getNodeName() == "study_data":
            ge = root
        else:
            ge = tw.getNextElement("study_data")
        if ge == None: return
        name = str(ge.getAttribute("name"))
        init_script_data = ScriptData(None,'sinit')
        init_script_data.fromXml(ge)
        tw = TreeWalker(ge)
        data = []
        while 1:
            pe = tw.getNextElement("graph_data")
            if pe == None: break
            pd = GraphData('')
            pd.fromXml(pe)
            pd.study = self
            data.append(pd)
        self.__init__(name,data,init_script_data)
#
class FontSizeData:
    def __init__(self):
        self.legend = 10
        self.laxis = 10
        self.baxis = 10
        self.footer = 6
        self.plot_title=16
        self.title = 18
    def _mkclone(self):
        clone = FontSizeData()
        clone.legend = self.legend
        clone.laxis = self.laxis
        clone.baxis = self.baxis
        clone.footer = self.footer
        clone.plot_title = self.plot_title
        clone.title = self.title
    def toXml(self, xdoc):
        el = xdoc.createElement("font_data")
        el.setAttribute("legend",str(self.legend))
        el.setAttribute("laxis",str(self.laxis))
        el.setAttribute("baxis",str(self.baxis))
        el.setAttribute("footer",str(self.footer))
        el.setAttribute("plot_title",str(self.plot_title))
        el.setAttribute("title",str(self.title))
        return el
    def fromXml(self,root):
        if root == None: return
        el=None
        tw=TreeWalker(root)
        if root.getNodeName() == "font_data":
            el = root
        else:
            el = tw.getNextElement("font_data")
        if el == None: return
        self.legend = int(el.getAttribute("legend"))
        self.laxis = int(el.getAttribute("laxis"))
        self.baxis = int(el.getAttribute("baxis"))
        self.footer = int(el.getAttribute("footer"))
        self.plot_title = int(el.getAttribute("plot_title"))
        self.title = int(el.getAttribute("title"))
# add delete, new, move up/down functionality
class GraphData:
    def __init__(self, title, pdata = None, diffplots = 0, rows = 3, cols = 1,\
         name='Graph',init_script_data=None,font_data =None):
        self.pdata = pdata
        self.title = title
        self.name = name
        self.current_gd = self
        self.root = MyTreeNode(self,self.name,1)
        self.current_plot_id = 0
        self._mkroot()
        self.difference_plots= diffplots
        self.plot_rows, self.plot_columns = rows, cols
        self.file = None
        self._initattrs()
        self.mkCurveAttributes()
        if init_script_data == None:
            self.init_script_data = ScriptData(None,'ginit')
        else:
            self.init_script_data = init_script_data
        if font_data == None:
            self.font_data = FontSizeData()
        else:
            self.font_data = font_data
    def _getNewPlotId(self):
        self.current_plot_id = self.current_plot_id + 1
        return "PlotData # " + str(self.current_plot_id)
    def addPlotData(self, pd):
        if pd.name == '':
            pd.name = self._getNewPlotId()
        self.root.add(MyTreeNode(pd,pd.name))
    def _mkroot(self):
        if self.pdata != None:
            for i in range(len(self.pdata)):
                self.addPlotData(self.pdata[i])
    def _update(self):
        e = self.root.children()
        pdata = []
        while e.hasMoreElements():
            x = e.nextElement()
            pdata.append(x.getUserObject())
        self.pdata = pdata
    def _mkclone(self):
        pdata = []
        if self.pdata != None:
            for data in self.pdata:
                pdata.append(data._mkclone())
        clone = GraphData(self.title,pdata, self.difference_plots, \
                  self.plot_rows, self.plot_columns, self.name + '(copy)',
                  self.init_script_data._mkclone(),self.font_data._mkclone())
        clone.graph_attrs = GraphAttr()
        clone.graph_attrs.backgroundColor = self.graph_attrs.backgroundColor
        clone.graph_attrs.foregroundColor = self.graph_attrs.foregroundColor
        return clone
    def _initattrs(self):
        crvAttr1 = CurveProps(Color.blue,None, 1)
        crvAttr2 = CurveProps(Color.green,None,1)
        crvAttr3 = CurveProps(Color.red,SymbolProps("cross",Color.red,2,0),1)
        crvAttr4 = CurveProps(Color.cyan,SymbolProps("square",Color.cyan,2,0),1)
        crvAttr5 = CurveProps(Color.gray,SymbolProps("circle",Color.gray,2,0),1)
        crvAttr6 = CurveProps(Color.pink,SymbolProps("circle",Color.pink,2,0),1)
        crvAttr7 = CurveProps(Color.magenta,SymbolProps("circle",Color.magenta,2,0),1)
        GraphData.default_attrs = [crvAttr1,crvAttr2,crvAttr3,crvAttr4, crvAttr5,\
                   crvAttr6, crvAttr7]
        if not hasattr(self,'attrs') or self.attrs == None:
            self.attrs = GraphData.default_attrs[:]
        if not hasattr(self,'graph_attrs') or self.graph_attrs == None:
            self.graph_attrs = GraphAttr()
            self.graph_attrs.backgroundColor = Color.white
            self.graph_attrs.foregroundColor = Color.black
        GraphData.landscape = 1
    def _updateattrs(self):
        if self.attrs < len(GraphData.default_attrs):
            x = self.attrs
            map(lambda y: x.append(y), GraphData.default_attrs[len(self.attrs)-1:])
    def mkCurveAttributes(self):
        self.crvAttrs = []
        for attr in self.attrs:
            ca = CurveAttr()
            ca._foregroundColor = attr.color
            ca._thickness = attr.thickness
            ca._dashArray = CurveProps.DASH_TYPES[attr.dash_type]
            if attr.draw_symbol:
                ca._drawSymbol = 1
                ca._symbol = attr.sym_prop.sym
                ca._dataPerSymbol = attr.data_per_symbol
            self.crvAttrs.append(ca)
    def getPdata(self):
        self._update()
        return self.pdata
    def setPdata(self,pdata):
        self.pdata = pdata
        self._mkroot()
    def save(self, file=None):
        if file == None: file = self.file
        if file == None: return
        saveXml(file,self)
        self.file = file
    def toXml(self,xdoc):
        el = xdoc.createElement("graph_data")
        el.setAttribute("name",str(self.name))
        el.setAttribute("title",str(self.title))
        el.setAttribute("difference_plots",str(self.difference_plots))
        el.setAttribute("plot_rows",str(self.plot_rows))
        el.setAttribute("plot_columns",str(self.plot_columns))
        el.setAttribute("landscape",str(GraphData.landscape))
        self._update()
        ae = xdoc.createElement("curve_attributes")
        for attr in self.attrs:
            ae.appendChild(attr.toXml(xdoc))
        el.appendChild(ae)
        ge = xdoc.createElement("graph_attributes")
        ge.setAttribute("foreground",str(self.graph_attrs.foregroundColor))
        ge.setAttribute("background",str(self.graph_attrs.backgroundColor))
        el.appendChild(ge)
        el.appendChild(self.font_data.toXml(xdoc))
        el.appendChild(self.init_script_data.toXml(xdoc))
        for data in self.pdata:
            el.appendChild(data.toXml(xdoc))
        return el
    def load(self,file):
        self.file = file
        loadXml(file,self)
    def fromXml(self,xdoc):
        if hasattr(xdoc,'getDocumentElement'):
            root = xdoc.getDocumentElement()
        else:
            root = xdoc
        if root == None: return
        tw = TreeWalker(root)
        if root.getNodeName() == "graph_data":
            ge = root
        else:
            ge = tw.getNextElement("graph_data")
        if ge == None: return
        title = str(ge.getAttribute("title"))
        name = str(ge.getAttribute("name"))
        difference_plots = string.atoi(ge.getAttribute("difference_plots"))
        plot_rows = string.atoi(ge.getAttribute("plot_rows"))
        plot_columns = string.atoi(ge.getAttribute("plot_columns"))
        lattr = ge.getAttribute("landscape")
        if lattr != None and len(lattr) > 0:
            GraphData.landscape = string.atoi(lattr)
        tw = TreeWalker(ge)
        ae = tw.getNextElement("curve_attributes")
        if ae != None:
            self.attrs = GraphData.default_attrs
            tw2 = TreeWalker(ae)
            aei = tw2.getNextElement("curve_props")
            propCount = 1
            while aei != None:
                cprops = CurveProps()
                cprops.fromXml(aei)
                if propCount >= len(GraphData.default_attrs):
                    self.attrs.append(cprops)
                else:
                    self.attrs[propCount-1] = cprops
                aei = tw2.getNextElement("curve_props")
                propCount = propCount+1
        tw.reset()
        ae = tw.getNextElement("graph_attributes")
        if ae != None:
            fgattr = ae.getAttribute("foreground")
            self.graph_attrs.foregroundColor = GraphicElement.parseColorProperty(fgattr)
            bgattr = ae.getAttribute("background")
            self.graph_attrs.backgroundColor = GraphicElement.parseColorProperty(bgattr)
        tw.reset()
        #
        font_data = FontSizeData()
        font_data.fromXml(ge)
        #
        tw = TreeWalker(ge)
        pdata = []
        while 1:
            pe = tw.getNextElement("plot_data")
            if pe == None: break
            pd = PlotData(self)
            pd.fromXml(pe)
            pdata.append(pd)
        init_script_data = ScriptData(None,'ginit')
        init_script_data.fromXml(ge)
        self.__init__(title,pdata,difference_plots,plot_rows,plot_columns,\
                  name,init_script_data,font_data)
        self._updateattrs()
        self.mkCurveAttributes()
    def saveweb(self,file):
        imgtype = "gif"
        if isJDK2():
            imgtype = "gif"
        else:
            imgtype = "gif"
        self._update()
        base_filename = os.path.basename(file)
        base_file_noext = string.split(base_filename,'.')[0]
        file_noext = string.split(file,'.')[0]
        ngraphs = self.saveimg("",imgtype,"%s_"%file_noext)
        fh = open(file,'w')
        fh.write('<html><body>\n')
        fh.write('<a href="./%s"><h3> Graphs: %s </h3></a>\n'%(base_filename,self.name))
        fh.write('<ol title="Graphs">\n')
        pdata = self.getPdata()
        pindex = 0
        for i in range(ngraphs):
            # add information of which plots the graph contains...
            fh.write('<li>')
            listr = 'Plots: '
            maxnplots = min(pindex+self.plot_rows*self.plot_columns,len(pdata))
            for j in range(pindex,maxnplots-1):
                listr = listr + ' %s & '%pdata[j].name
                listr = listr + ' %s'%pdata[maxnplots-1].name
                pindex = maxnplots
                fh.write('<a href="%s_graph%02d.%s">%s</a>\n'\
                     %(base_file_noext,(i+1),imgtype,listr))
        fh.write('</ol>\n')
        old_rc = self.plot_rows
        old_cc = self.plot_columns
        self.plot_rows = 1
        self.plot_columns = 1
        fh.write('<ol title="Plots">\n')
        try:
            nplots = self.saveimg("",imgtype,"%s_P"%file_noext)
            #print nplots, file_noext, base_file_noext
            for i in range(nplots):
                fh.write('<li>')
                pd = pdata[i]
                fh.write('<a href="%s_Pgraph%02d.%s">Plot %s</a>\n'\
                     %(base_file_noext,(i+1),imgtype,pd.name))
        except Exception, exc:
            self.plot_rows = old_rc
            self.plot_columns = old_cc
            raise exc
        self.plot_rows = old_rc
        self.plot_columns = old_cc
        fh.write('</ol>\n')
        fh.write('</body></html>\n')
        fh.close()
    def saveimg(self,dir_name="",type="gif",prefix=""):
        vid = System.getProperty("java.version")
            #if type == "jpg":
            #    if isJDK2():
            #        pass
            #    else:
            #        raise "No JPEG output in JDK 1.1! Use JDK 1.2 instead"
        img = None
        r = Rectangle(0,0,10,10)
        self._dummy_fr=JFrame();self._dummy_fr.setVisible(1);self._dummy_fr.setVisible(0)
        if isJDK2() and type == "jpg":
            from java.awt.image import BufferedImage
            if GraphData.landscape:
                img = BufferedImage(__lwidth__,__lheight__,BufferedImage.TYPE_INT_RGB)
                r.width,r.height = __lwidth__,__lheight__
            else:
                img = BufferedImage(__pwidth__,__pheight__,BufferedImage.TYPE_INT_RGB)
                r.width,r.height = __pwidth__,__pheight__
        else:
            if GraphData.landscape:
                img = self._dummy_fr.createImage(__lwidth__,__lheight__)
                r.width,r.height = __lwidth__,__lheight__
            else:
                img = self._dummy_fr.createImage(__pwidth__,__pheight__)
                r.width,r.height = __pwidth__,__pheight__
        self._update()
        gtitle = self.title
        diffplot = self.difference_plots
        plot_rows = self.plot_rows
        plot_cols = self.plot_columns
        plots = []
        pdata = self.getPdata()
        for x in pdata:
            plots.append(x.plot(self.crvAttrs,diffplot))
        #print 'Rows: ', self.plot_rows
        #print 'Cols: ', self.plot_columns
        #self._graph(plots, gtitle, plot_rows, plot_cols,0)
        self.graph()
        if len(self.graphs) == 0:
            raise "No graphs to save to gif"
        count = 1
        fos = None
        sep = System.getProperty("file.separator")
        for graph in self.graphs:
            if type == "jpg":
                    fos = BufferedOutputStream(FileOutputStream(dir_name+sep+\
                                    prefix+"graph%02d.jpg"%count))
            else:
                fos = BufferedOutputStream(FileOutputStream(dir_name+sep+\
                                        prefix+"graph%02d.gif"%count))
            orgBounds = graph.getPreferredSize()
            rratio = Math.min( r.width*1.0/orgBounds.width, r.height*1.0/orgBounds.height)
            if hasattr(graph,'setFontByRatio'): graph.setFontByRatio(rratio)
            graph.setBounds(r)
            graph.draw(img.getGraphics())
            if type=="jpg":
                if isJDK2():
                    from com.sun.image.codec.jpeg import JPEGCodec
                    JPEGCodec.createJPEGEncoder(fos).encode(img)
                else:
                    from vista.graph import JpegEncoder
                    JpegEncoder(img,90,fos).Compress()
            else:
                GifEncoder(img,fos).encode()
            fos.close()
            count = count+1
        #
        img.flush()
        for dg in self.data_graphs:
            dg.dispose()
            dg.cleanup()
        return count-1
        #
    def writetxt(self,txtfile,outf=None):
        created_here=0
        if outf==None:
            created_here=1
            outf = PrintWriter(BufferedWriter(FileWriter(txtfile)))
        self._update()
        for x in self.getPdata(): x.writetxt(txtfile,outf)
        if created_here: outf.close()
    #
    def writedss(self,dssfile):
        self._update()
        for x in self.getPdata():
            x.writedss(dssfile)
    def tabulate(self):
        self._update()
        pdata = self.getPdata()
        for x in pdata:
            x.display_table()
        #
    def run_script(self):
        self._update()
        pdata = self.getPdata()
        for x in pdata: x.run_script()
        #
    def graph(self):
        self._update()
        gtitle = self.title
        diffplot = self.difference_plots
        plot_rows = self.plot_rows
        plot_cols = self.plot_columns
        plots = []
        pdata = self.getPdata()
        for x in pdata:
            plots.append(x.plot(self.crvAttrs,diffplot))
        self._graph(plots, gtitle, plot_rows, plot_cols)
    #
    def _graph(self,plots, gtitle, plot_rows=3, plot_cols=1, display=1):
        """
        creates a graph from an array of plots, arranging the plots based on the plots_per_graph, 
        diff_plot and together parameters
        """
        # create Graph
        from java.util import Date
        from vista.graph import TextLine, GEBorderLayout
        ppg = plot_rows
        mp = None
        count = 0
        if len(plots) == 0 : return
        cpg = plot_cols # Columns of plots per page
        self.graphs = []
        self.data_graphs = []
        for pl in plots:
            if not pl: continue
            # create a graph if ppg so dictates
            if count%(ppg*cpg) == 0:
                graph = Graph()
            mp = MultiPlot(ppg,cpg)
            #graph.setInsets(Insets(20,0,0,0))
            graph.add(mp)
            self.set_attr(graph)
            graph.setTitle(gtitle)
            graph.getTitle().setFontSize(self.font_data.title)
            self.graphs.append(graph)
            #footer = self._mkfooter()
            footer = self._mkdatefooter()
            graph.getLayout().addLayoutElement(GEBorderLayout.SOUTH, footer)
            graph.add(footer)
            # add the plot to the multiplot
            mp.add(pl);
            count = count + 1
            # finally if we have enough plots lets make a frame
            if count%(ppg*cpg) == 0:
                self.data_graphs.append(self._mkgraph(graph,display))
        #do the last one after loop
        if count%(ppg*cpg) != 0:
            self.data_graphs.append(self._mkgraph(graph,display))
    def _mkgraph(self,graph,display=1):
        #if not display: return
        global __graph_number__
        __graph_number__ = __graph_number__+1
        dg=DataGraphFrame(graph,'graph#'+str(__graph_number__),0)
        self.set_attr(graph)
        dg.setLocation(100,100)
        if GraphData.landscape:
            dg.setSize(__lwidth__,__lheight__)
        else:
            dg.setSize(__pwidth__,__pheight__)
        dg.setVisible(display)
        return dg
    def _mkdatefooter(self):
        date_tl = TextLine(str(Date()))
        date_attr = date_tl.getAttributes()
        date_attr._originalFontSize=6
        date_attr.foregroundColor = Color.red
        date_attr._justification = date_attr.RIGHT
        date_tl.setFontSize(self.font_data.footer)
        return date_tl
    def _mkfooter(self):
        gdict = globals()
        ldict=locals()
        left=dynamic_eval(self.footer.left,gdict,ldict)
        right=dynamic_eval(self.footer.right,gdict,ldict)
        center=dynamic_eval(self.footer.center,gdict,ldict)
        left_tl = TextLine(left)
        left_attr = left_tl.getAttributes()
        left_attr._originalFontSize=6
        left_attr.foregroundColor = Color.red
        left_attr._justification = left_attr.LEFT
        center_tl = TextLine(center)
        center_attr = center_tl.getAttributes()
        center_attr._originalFontSize=6
        center_attr.foregroundColor = Color.red
        center_attr._justification = center_attr.CENTER
        right_tl = TextLine(right)
        right_attr = right_tl.getAttributes()
        right_attr._originalFontSize=6
        right_attr.foregroundColor = Color.red
        right_attr._justification = right_attr.RIGHT
        footer = GEContainer()
        footer.setLayout(GELineLayout())
        footer.add(left_tl); footer.add(center_tl); footer.add(left_tl)
        return footer
    def set_attr(self,ge):
        if isinstance(ge,Graph):
            self.graph_attrs.copyInto(ge.getAttributes())
            count = ge.getElementCount()
            for i in range(count):
                el = ge.getElement(i)
                self.set_attr(el)        # don't set background for children !
        elif hasattr(ge,'getElementCount'):
            count = ge.getElementCount()
            if isinstance(ge,Plot):
                ge.getAttributes().foregroundColor = self.graph_attrs.foregroundColor
                ge.getAttributes().backgroundColor = None
            for i in range(count):
                el = ge.getElement(i)
                self.set_attr(el)        # don't set background for children !
        elif hasattr(ge,'setTickDimensions') or hasattr(ge,'setText'):
            ge.getAttributes().foregroundColor = self.graph_attrs.foregroundColor
            ge.getAttributes().backgroundColor = None
        else:
            ge.getAttributes().backgroundColor = None
global __graph_number__
__graph_number__ = 0
_gd = GraphData('')
_gd._initattrs()
_gd.mkCurveAttributes()
# delete, insert, add row functionality?
class ExprData(DefaultTableModel):
    def __init__(self, name_array=None,\
         expr_array=None, path_array=None, legend_array=None,\
         units_array=None, types_array=None):
        self.column_names = ['Name','Expressions','Pathnames','Legend','Units','Types']
        DefaultTableModel.__init__(self,self.column_names,0)
        names = name_array
        exprs = expr_array
        paths = path_array
        legends = legend_array
        units = units_array
        types = types_array
        if exprs != None:
            count = len(exprs)
            for i in range(count):
                name,expr,path,legend,unit,type = names[i],exprs[i],paths[i],legends[i], units[i],types[i]
                self.addRow([name,expr,path,legend,unit,type])
    def _mkclone(self):
        clone = ExprData(self.getNames(),self.getExprs(),\
                 self.getPaths(),self.getLegends(),\
                 self.getUnits(),self.getTypes())
        return clone
    def getColumnData(self,colno):
        rc = self.getRowCount()
        data = []
        for i in range(rc):
            val = self.getValueAt(i,colno)
            if val == None: val = ""
            data.append(val)
        return data
    def getNames(self):
        return self.getColumnData(0)
    def getExprs(self):
        return self.getColumnData(1)
    def getPaths(self):
        return self.getColumnData(2)
    def getLegends(self):
        return self.getColumnData(3)
    def getUnits(self):
        return self.getColumnData(4)
    def getTypes(self):
        return self.getColumnData(5)
    def toXml(self, xdoc):
        el = xdoc.createElement("expr_data")
        names = self.getNames()
        exprs = self.getExprs()
        paths = self.getPaths()
        legends = self.getLegends()
        units = self.getUnits()
        types = self.getTypes()
        count = len(exprs)
        for i in range(count):
            xe = xdoc.createElement("expr_row")
            xe.setAttribute("name",names[i])
            xe.setAttribute("expr",exprs[i])
            xe.setAttribute("path",paths[i])
            xe.setAttribute("legend",legends[i])
            xe.setAttribute("unit",units[i])
            xe.setAttribute("type",types[i])
            el.appendChild(xe)
        return el
    def fromXml(self,root):
        if root == None: return
        el=None
        tw=TreeWalker(root)
        if root.getNodeName() == "expr_data":
            el = root
        else:
            el = tw.getNextElement("expr_data")
        if el == None: return
        tw = TreeWalker(el)
        self.setNumRows(0)
        while 1:
            xe = tw.getNextElement("expr_row")
            if xe == None: break
            name = xe.getAttribute("name")
            expr = xe.getAttribute("expr")
            path =xe.getAttribute("path")
            legend =xe.getAttribute("legend")
            unit =xe.getAttribute("unit")
            type =xe.getAttribute("type")
            self.addRow([name,expr,path,legend,unit,type])
# delete, insert, add row functionality?
class TextLocData(DefaultTableModel):
    def __init__(self, text_array=None, size_array=None, 
         x_array=None, y_array=None):
        self.column_names = ['Text','Size','x','y']
        DefaultTableModel.__init__(self,self.column_names,0)
        texts = text_array
        sizes = size_array
        xs = x_array
        ys = y_array
        if texts != None:
            count = len(texts)
            for i in range(count):
                text,size,x,y = texts[i],sizes[i],xs[i],ys[i]
                self.addRow([text,size,x,y])
    def _mkclone(self):
        clone = TextLocData(self.getTexts(), self.getSizes(), self.getXs(), self.getYs())
        return clone
    def getColumnData(self,colno):
        rc = self.getRowCount()
        data = []
        for i in range(rc):
            val = self.getValueAt(i,colno)
            if val == None: val = ""
            data.append(val)
        return data
    def getTexts(self):
        return self.getColumnData(0)
    def getSizes(self):
        return self.getColumnData(1)
    def getXs(self):
        return self.getColumnData(2)
    def getYs(self):
        return self.getColumnData(3)
    def toXml(self, xdoc):
        el = xdoc.createElement("text_data")
        texts = self.getTexts()
        sizes = self.getSizes()
        xs = self.getXs()
        ys = self.getYs()
        count = len(texts)
        for i in range(count):
            xe = xdoc.createElement("text_row")
            xe.setAttribute("text",texts[i])
            xe.setAttribute("size",sizes[i])
            xe.setAttribute("x",xs[i])
            xe.setAttribute("y",ys[i])
            el.appendChild(xe)
        return el
    def fromXml(self,root):
        if root == None: return
        el=None
        tw=TreeWalker(root)
        if root.getNodeName() == "text_data":
            el = root
        else:
            el = tw.getNextElement("text_data")
        if el == None: return
        tw = TreeWalker(el)
        self.setNumRows(0)
        while 1:
            xe = tw.getNextElement("text_row")
            if xe == None: break
            text = xe.getAttribute("text")
            size = xe.getAttribute("size")
            x =xe.getAttribute("x")
            y =xe.getAttribute("y")
            self.addRow([text,size,x,y])
class Footer:
    from java.util import Date
    def __init__(self):
        self.right = 'Date()'
        self.center = ''
        self.left = ''
    def _mkclone(self):
        clone = Footer()
        clone.right,clone.center,clone.left = self.right,self.center,self.left
        return clone
    def toXml(self, xdoc):
        el = xdoc.createElement("footer")
        el.setAttribute("l",self.left)
        el.setAttribute("c",self.center)
        el.setAttribute("r",self.right)
        return el
    def fromXml(self,root):
        if root == None: return
        el=None
        tw=TreeWalker(root)
        if root.getNodeName() == "footer":
            el = root
        else:
            el = tw.getNextElement("footer")
        if el == None: return
        self.left = el.getAttribute("l")
        self.center = el.getAttribute("c")
        self.right = el.getAttribute("r")
class PlotData(DefaultTableModel):
    def __init__(self, gdata,\
             pitems = None, axisLabel = '', title = '', tw=None, expr_data=None, \
             pre_script_data=None,init_script_data=None,plot_text=None,footer=None,\
             comment_data=None):
        self.gdata = gdata
        self.column_names = ['Name','File', 'Path']
        DefaultTableModel.__init__(self,self.column_names,0)
        if pitems != None:
            for pitem in pitems:
                self.addRow([pitem.name,pitem.file,pitem.path])
        self.axisLabel = axisLabel
        self.title = title
        self.tw = tw
        if expr_data == None: expr_data = ExprData()
        self.expr_data = expr_data
        if pre_script_data == None: pre_script_data = ScriptData(None,'pre')
        self.pre_script_data = pre_script_data
        if init_script_data == None: init_script_data = ScriptData(None,'init')
        self.init_script_data = init_script_data
        self.range = ''
        self.name = ''
        if plot_text==None: plot_text = TextLocData()
        self.plot_text = plot_text
        if footer==None: footer = Footer()
        self.footer = footer
        if comment_data == None: comment_data = ScriptData(None,'comment')
        self.comment_data = comment_data
    def _mkclone(self):
        clone = PlotData(self.gdata,\
                 self.getItems(),str(self.axisLabel), str(self.title), \
                 str(self.tw), self.expr_data._mkclone(), \
                 self.pre_script_data._mkclone(),\
                 self.init_script_data._mkclone(),self.plot_text._mkclone(),\
                 self.footer._mkclone(),\
                 self.comment_data._mkclone())
        clone.name = self.name + '(copy)'
        return clone
    def __str__(self):
        return "PlotData " + self.name
    def __repr__(self):
        return "PlotData " + self.name
    def newXMLDocument(self):
        return DocumentBuilderFactory.newInstance().newDocumentBuilder().newDocument();
    def saveXml(self, file):
        xdoc = self.newXmlDocument()
        xdoc.appendChild(xdoc.createElement("root"))
        root = self.toXml(xdoc)
        xdoc.getDocumentElement().appendChild(root)
        pw = PrintWriter(FileOutputStream(file))
        xdoc.write(pw)
        pw.close()
    def loadXml(self,file):
        xdoc = XmlDocument.createXmlDocument(FileInputStream(file),0)
        self.fromXml(xdoc)
    def toXml(self, xdoc):
        el = xdoc.createElement("plot_data")
        el.setAttribute("name",self.name)
        el.setAttribute("title",self.title)
        el.setAttribute("axis_label",self.axisLabel)
        #print self, el
        if self.tw == None:
            el.setAttribute("time_window",'')
        else:
            el.setAttribute("time_window",self.tw)
        el.setAttribute("range",self.range)
        rc = self.getRowCount()
        self.column_names = ['Name','File', 'Path']
        cc = len(self.column_names)
        #self.column_names = string.split(pe.getAttribute("column_names"),',')
        self.setColumnIdentifiers(self.column_names)
        cnames = None
        for column in range(cc):
            if cnames == None:
                cnames = self.getColumnName(column)
            else:
                cnames = cnames + ',' + self.getColumnName(column)
        el.setAttribute("column_names",cnames)
        for row in range(rc):
            re = xdoc.createElement("row")
            for column in range(cc):
                    val = self.getValueAt(row,column)
                    if val == None: val = ""
            re.setAttribute(self.getColumnName(column),val)
            el.appendChild(re)
        el.appendChild(self.expr_data.toXml(xdoc))
        el.appendChild(self.pre_script_data.toXml(xdoc))
        el.appendChild(self.init_script_data.toXml(xdoc))
        el.appendChild(self.comment_data.toXml(xdoc))
        el.appendChild(self.plot_text.toXml(xdoc))
        el.appendChild(self.footer.toXml(xdoc))
        return el
    def fromXml(self, root):
        if root == None: return
        pe=None
        tw=TreeWalker(root)
        if root.getNodeName() == "plot_data":
            pe = root
        else:
            pe = tw.getNextElement("plot_data")
        self.setNumRows(0)
        self.name = pe.getAttribute("name")
        self.title = pe.getAttribute("title")
        self.axisLabel = pe.getAttribute("axis_label")
        self.tw = pe.getAttribute("time_window")
        self.range = pe.getAttribute("range")
        #self.column_names = string.split(pe.getAttribute("column_names"),',')
        #self.setColumnIdentifiers(self.column_names)
        tw.reset()
        row = 0
        while 1:
            re = tw.getNextElement("row")
            if re == None: break
            data = []
            for column in range(len(self.column_names)):
                data.append(re.getAttribute(self.column_names[column]))
                self.addRow(data)
                row = row + 1
        self.expr_data = ExprData()
        self.expr_data.fromXml(pe)
        self.pre_script_data = ScriptData(None,'pre')
        self.pre_script_data.fromXml(pe)
        self.init_script_data = ScriptData(None,'init')
        self.init_script_data.fromXml(pe)
        self.comment_data = ScriptData(None,'comment')
        self.comment_data.fromXml(pe)
        self.plot_text = TextLocData()
        self.plot_text.fromXml(pe)
        self.footer = Footer()
        self.footer.fromXml(pe)
        def save(self, file):
            self.saveXml(file)
        def load(self, file):
            self.loadXml(file)
        def getItems(self):
            rc = self.getRowCount()
            cc = self.getColumnCount()
            items = []
            for row in range(rc):
                val1 = self.getValueAt(row,0)
                if val1==None: val1=""
                val2 = self.getValueAt(row,1)
                if val2==None: val2=""
                val3 = self.getValueAt(row,2)
                if val3==None: val3=""
                items.append(PlotDataItem(val1,val2,val3))
            return items
        #
    def display_graph(self, gd, simple_plot=0):
        gr = Graph();
        gr.addPlot(self.plot(gd.crvAttrs,simple_plot))
        gd._mkgraph(gr)
    def display_table(self):
        data = self.getItems()
        refs,tw,ldict = self._mkrefs(data,self.tw,0)
        if len(refs) == 0: return
        if len(refs) > 1:
            mdt = MultiDataTable(refs)
            mdt.setTitle(self.name)
        else:
            dt = DataTable(refs[0])
            dt.setTitle(self.name)
    def run_script(self):
        data = self.getItems()
        tw = self.tw
        refs,tw,ldict = self._mkrefs(data,self.tw,0,1)
    def plot(self, crvAttrs = _gd.crvAttrs, simple_plot=0):
        return self._plot(crvAttrs,simple_plot)
    def _mktw(self,tw,earray):
        st = tw.getStartTime()
        et = tw.getEndTime()
        return TimeFactory.getInstance().createTimeWindow(st,et)
    def _mk_exec_string(self, expr):
        '''
        substitute method calls for relational operators
        substitute method calls for logical operators
        '''
        import vexpr
        try:
            self._scanner = vexpr.Scanner()
            tokens = self._scanner.tokenize(expr)
            self._parser = vexpr.Parser()
            root = self._parser.parse(tokens)
            xexpr = vexpr._translate(root)
        except:
            print 'Syntax error changing %s'%expr
            print sys.exc_info()
            xexpr=expr
        return xexpr
    def show_raw_group(self):
        data = self.getItems()
        tw = self.tw
        self._mkrawrefs(data,tw)
        if self.raw_refs != None and len(self.raw_refs) > 0:
            g = Group.createGroup('raw group:'+self.name,self.raw_refs)
            GroupFrame(g)
        else:
            raise "No available references!"
    def _mkrawrefs(self,data,tw):
        # execute the script
        if self.gdata.study != None:
            exec(string.join(self.gdata.study.init_script_data.lines,'\n')) in globals(),locals()
        # execute the script
        if self.gdata != None:
            exec(string.join(self.gdata.init_script_data.lines,'\n')) in globals(),locals()
        # execute the script
        exec(string.join(self.init_script_data.lines,'\n')) in globals(),locals()
        # don't care about time window here...
        self.raw_refs = []
        current_file=''
        current_path=''
        for x in data:
            current_file=str(dynamic_eval(x.file,globals(),locals()))
            current_path=str(dynamic_eval(x.path,globals(),locals()))
            if not gfilemap.has_key(current_file):
                if string.find(current_file,'::') >=0:
                    dss_server,dss_file = tuple(string.split(current_file,'::'))
                else:
                    dss_server,dss_file = 'local',current_file
            try: 
                gfilemap[current_file] = vutils.opendss(dss_file,dss_server)
            except:
                raise "DSS File: " + current_file + " not found"
            if string.find(current_file,'::') < 0:
                if File(current_file).lastModified() > File(DSSUtil.getCatalogFilename(current_file)).lastModified():
                    gfilemap[current_file] = vutils.opendss(current_file)
                    g = gfilemap[current_file]
            # get reference to data
            refs=vutils.findpath(g,current_path)
            if refs == None or len(refs) == 0:
                print "No data found for " + current_file + " & " + current_path
            else:
                for ref in refs:
                    self.raw_refs.append(ref)
    def _mkrefs(self,data,tw,simple_plot=0,run_script_only=0):
        # execute the script
        if self.gdata.study != None:
            exec(string.join(self.gdata.study.init_script_data.lines,'\n')) in globals(),locals()
        # execute the script
        if self.gdata != None:
            exec(string.join(self.gdata.init_script_data.lines,'\n')) in globals(),locals()
        # execute the script
        exec(string.join(self.init_script_data.lines,'\n')) in globals(),locals()
        #
        try:
            if tw != None:
                tw=dynamic_eval(tw,globals(),locals())
            tw = vutils.timewindow(tw)
        except:
            tw = None
        global refs
        refs = []
        refnames = []
        #
        current_file=''
        current_path=''
        for x in data:
            current_file=dynamic_eval(x.file,globals(),locals())
            current_path=dynamic_eval(x.path,globals(),locals())
            if not gfilemap.has_key(current_file):
                if string.find(current_file,'::') >=0:
                    dss_server,dss_file = tuple(string.split(current_file,'::'))
                else:
                    if os.path.isabs(current_file):
                        dss_server,dss_file = 'local',current_file
            else:
                study_file = self.gdata.study.file
                if study_file != None:
                    local_dir = os.path.dirname(study_file)
                else:
                    local_dir = os.getcwd()
                #print 'Study file: %s, Local Dir: %s'%(study_file,local_dir)
                dss_server,dss_file = 'local',posixpath.join(local_dir,current_file)
                #print 'DSS file:%s'%dss_file
            try: 
                gfilemap[current_file] = vutils.opendss(dss_file,dss_server)
            except:
                raise "DSS File: " + current_file + " not found"
            if string.find(current_file,'::') < 0:
                if File(current_file).lastModified() > File(DSSUtil.getCatalogFilename(current_file)).lastModified():
                    gfilemap[current_file] = vutils.opendss(current_file)
                    g = gfilemap[current_file]
            # get reference to data
            ref=vutils.findpath(g,current_path)
            if ref == None or len(ref) == 0:
                raise "No data found for " + current_file + " & " + current_path
            ref = ref[0]
            refs.append(ref)
            refnames.append(x.name)
            if tw == None:
                tw = ref.getTimeWindow()
        # do operations
        if self.expr_data != None and not simple_plot:
            earray = self.expr_data.getExprs()
            names = self.expr_data.getNames()
            if len(earray) != 0:
                etw = self._mktw(tw,earray)
            else:
                etw = tw
            for i in range(len(refs)):
                rti = refs[i].getTimeInterval()
                rtw = refs[i].getTimeWindow()
                rpath = refs[i].getPathname()
                refs[i] = DataReference.create(refs[i],etw)
                if refs[i] == None:
                    nvals = (etw.getEndTime()-etw.getStartTime())/rti+1
                    refs[i] = wrap_data(RegularTimeSeries(str(rpath),str(etw.getStartTime()),\
                                      str(rti),[-901.]*nvals))
                else: # add non null to raw refs
                    pass #
                refnames[i] = string.strip(refnames[i])
                if len(refnames[i]) > 0:
                    exec_string = '%s=refs[i].getData()'%refnames[i]
                    exec(exec_string) in globals(),locals()
                else:
                    exec_string = 'a%d=refs[i].getData()'%(i+1)
                    exec(exec_string) in globals(),locals()
            # execute the script
            exec(string.join(self.pre_script_data.lines,'\n')) in globals(),locals()
            if len(earray) == 0: return refs,tw,locals()
            if run_script_only: return refs,tw,locals()
            # get the references
            global xrefs
            xrefs = []
            for j in range(len(earray)):
                x = earray[j]
                x = self._mk_exec_string(x)
                names[j] = string.strip(names[j])
                if len(names[j]) > 0:
                    exec_string = '%s=%s'%(names[j],x)
                    exec(exec_string) in globals(),locals()
                    exec_string = 'xrefs.append(%s)'%names[j]
                    exec(exec_string) in globals(),locals()
                else:
                    exec_string = 'xrefs.append(%s)'%x
                    exec(exec_string) in globals(),locals()
                #
            refs = xrefs
        # get the references
        units = self.expr_data.getUnits()
        types = self.expr_data.getTypes()
        for i in range(len(refs)):
            if hasattr(refs[i],'getData'):
                ds = refs[i].getData()
            else:
                ds = refs[i]
            ds = ds.createSlice(tw)
            if not simple_plot:
                if units[i] == None:
                    unit = ''
                else:
                    unit = string.strip(units[i])
                    unit = dynamic_eval(units[i],globals(),locals())
                if len(unit) == 0:
                    unit = ds.getAttributes().getYUnits()
                if types[i] == None:
                    type = ''
                else:
                    type = string.strip(types[i])
                    type = dynamic_eval(types[i],globals(),locals())
                if len(type) == 0:
                    type = ds.getAttributes().getYType()
                attr = DataSetAttr(DataType.REGULAR_TIME_SERIES,'',unit,'TIME',type)
                ds.setAttributes(attr)
                refs[i] = vdss.wrap_data(ds, 'calc.dss', \
                               'local', str(ds.getName()))
            #refs[i] = DataReference.create(refs[i],tw)
        return refs,tw,locals()
    def _mkPath(self, new_path, old_path):
        s1array = string.split(new_path,'/')
        s2array = string.split(old_path,'/')
        len1 = len(s1array)
        path = '/'
        if not hasattr(self,'substituter'):
            self.substituter = PathSubstituter()
        for i in range(1,7):
            if i == 4 or i == 5:
                path = path + ''
            elif i >= len1 or len(string.strip(s1array[i])) == 0:
                path = path + s2array[i]
            else:
                path = path + self.substituter.sub(s1array[i],s2array)
                path = path + '/'
        return path
    def writedss(self,dssfile):
        data = self.getItems()
        # -TP If no path defined, do not write to the dss file
        #<old_code>
        refs,tw,ldict = self._mkrefs(data,self.tw,0)
        if refs == None or len(refs) == 0: raise "No data found for writing to DSS"
        paths = None
        if self.expr_data != None:
            paths = self.expr_data.getPaths()
        count = len(refs)
        for i in range(count):
            ref = refs[i]
            path = dynamic_eval(paths[i],globals(),ldict)
            path = self._mkPath(path,ref.getPathname().toString())
            writedss(dssfile, path, ref.getData())
        #</old_code>
        #refs,tw,ldict = self._mkrefs(data,self.tw,0)
        #nrefs=[]
        #paths = None
        #if self.expr_data != None:
        #    paths = self.expr_data.getPaths()
        #cnt = len(refs)
        #for i in range(cnt):
        #    if paths[i] != '':
        #        nrefs.append(i)
        #if refs == None or len(nrefs) == 0: raise "No data found for writing to DSS"
        #paths = None
        #if self.expr_data != None:
        #    paths = self.expr_data.getPaths()
        #count = len(nrefs)
        #for i in range(count):
        #    ref = refs[nrefs[i]]
        #    path = dynamic_eval(paths[i],globals(),ldict)
        #    path = self._mkPath(path,ref.getPathname().toString())
        #    writedss(dssfile, path, ref.getData())
        # -TP 20010202
    def writetxt(self,txtfile,outf=None):
        data = self.getItems()
        refs,tw,ldict = self._mkrefs(data,self.tw,0)
        if refs == None or len(refs) == 0: raise "No data found for plot"
        paths = None
        if self.expr_data != None:
            paths = self.expr_data.getPaths()
        count = len(refs)
        created_here = 0
        if outf==None:
            created_here = 1
            outf = PrintWriter(BufferedWriter(FileWriter(txtfile)))
        for i in range(count):
            ref = refs[i]
            path = dynamic_eval(paths[i],globals(),ldict)
            # hook function: write_txt_ref(ref,path) or next two hook functions...
            path = self._mkPath(path,ref.getPathname().toString())
            # hook function: write_header_info(outf,txtfile,ref,path)
            _txt_writer.write_header_info(outf,txtfile,ref,path)
            dsi = ref.getData().getIterator()
            while not dsi.atEnd():
                el = dsi.getElement()
                # hook function: write_data(el)
                _txt_writer.write_data(outf,txtfile,ref,path,el)
                dsi.advance()
            #hook function: write_footer_info(txtfile,ref,path)
            _txt_writer.write_footer_info(outf,txtfile,ref,path)
        if created_here: outf.close()
    def _plot(self, crvAttrs, simple_plot=0):
        """
        does the plot with the given data, axis label, title
        and optional time window and difference plot
        """
        data = self.getItems()
        axisLabel = self.axisLabel
        title = self.title
        refs,tw,ldict = self._mkrefs(data,self.tw,simple_plot)
        if refs == None or len(refs) == 0: return
        #raise "No data found for plot"
        legend_array = []
        if simple_plot:
            for ref in refs:
                path = ref.getPathname()
                legend_array.append(path.getPart(Pathname.C_PART) + ' @ ' + \
                            path.getPart(Pathname.B_PART) + ' for ' + \
                            path.getPart(Pathname.F_PART))
        else:
            legend_array = self.expr_data.getLegends()
        crvs = []
        litems = []
        for i in range(len(refs)):
            attr = crvAttrs[i%len(crvAttrs)]
            legtext = str(dynamic_eval(legend_array[i],globals(),ldict))
            if isJDK2() and not attr._drawSymbol and attr._dashArray != None and len(attr._dashArray) > 1:
                crv2d = create2DCurve(refs[i],AxisAttr.BOTTOM,AxisAttr.LEFT,legtext)
                crvs.append(crv2d)
            else:
                crvs.append(CurveFactory.createCurve(refs[i],\
                                                     AxisAttr.BOTTOM, \
                                                     AxisAttr.LEFT, legtext))
                crvs[i].setAttributes(attr)
            if legend_array[i] != '':
                litems.append(LegendItem(crvs[i]))
        #
        leg = Legend()
        for item in litems:
            leg.add(item)
        #
        leg.setFontSize(self.gdata.font_data.legend)
        #
        pl = Plot()
        for crv in crvs: pl.add(crv)
        # set axis labels and font for it
        laxis = pl.getAxis(AxisAttr.LEFT);
        laxis.setAxisLabel(str(dynamic_eval(axisLabel,globals(),ldict)))
        laxis.percentMajorTickLength=0.4
        laxis.percentMinorTickLength=0.2
        #
        ttc = Class.forName("vista.graph.TickText");
        tt = laxis.getElements(ttc) [0]
        tt.setFontSize(self.gdata.font_data.laxis)
        alc = Class.forName("vista.graph.TextLine");
        axisLabel = laxis.getElements(alc)[0]
        axisLabel.setFontSize(self.gdata.font_data.laxis)
        #
        if len(string.strip(self.range)) > 0:
            rr = string.split(self.range)
            min_val = dynamic_eval(rr[0],globals(),ldict)
            max_val = dynamic_eval(rr[1],globals(),ldict)
            laxis.getTickGenerator().useDataMinMax(1)
            laxis.setDCRange(min_val, max_val)
        else:
            laxis.getTickGenerator().useDataMinMax(0)
        #
        baxis = pl.getAxis(AxisAttr.BOTTOM);
        baxis.percentMajorTickLength=0.4
        baxis.percentMinorTickLength=0.2
        ttc = Class.forName("vista.graph.TickText");
        tt = baxis.getElements(ttc) [0]
        tt.setFontSize(self.gdata.font_data.baxis)
        alc = Class.forName("vista.graph.TextLine");
        axel = baxis.getElements(alc)
        if axel != None and len(axel) > 0:
            axisLabel = baxis.getElements(alc)[0]
            axisLabel.setFontSize(self.gdata.font_data.baxis)
        #baxis.getTickGenerator().useDataMinMax(1)
        baxis.setDCRange(float(tw.getStartTime().getTimeInMinutes()), \
                 float(tw.getEndTime().getTimeInMinutes()))
        #
        pl.add(leg)
        pl.addTitle(str(dynamic_eval(title,globals(),ldict)))
        ttc = Class.forName("vista.graph.TextLine");
        tt = pl.getElements(ttc)[0]
        tt.setFontSize(self.gdata.font_data.plot_title)
        pl.addGrid(AxisAttr.LEFT)
        # add text locations
        txts = self.plot_text.getTexts()
        if txts != None and len(txts) >= 0:
            sizes = self.plot_text.getSizes()
            xs = self.plot_text.getXs()
            ys = self.plot_text.getYs()
            for i in range(len(txts)):
                label,size,x,y = str(dynamic_eval(txts[i],globals(),ldict)),
                dynamic_eval(sizes[i],globals(),ldict),dynamic_eval(xs[i],globals(),ldict),dynamic_eval(ys[i],globals(),ldict)
                pl.addLabel(label,x,y,size,Color.black)
        #
        return pl
    #
class PlotDataItem:
    """
    A class that contains the file & path from which data is extracted and its
    associated legend text
    """
    def __init__(self, name, file, path):
        self.name = name
        self.file = file
        self.path = path
    def __repr__(self):
        xstr = 'PlotDataItem: Name -> %s File -> %s Path-> %s'\
        %(str(self.name),str(self.file),str(self.path))
        return xstr
    def __str__(self):
        return __repr__(self)
    def toString(self):
        return __repr__(self)
def saveXml(file,object):
    xdoc = XmlDocument()
    xdoc.appendChild(xdoc.createElement("root"))
    root = object.toXml(xdoc)
    xdoc.getDocumentElement().appendChild(root)
    pw = PrintWriter(FileOutputStream(file))
    xdoc.write(pw)
    pw.close()
def loadXml(file,object):
    fis = FileInputStream(file)
    xdoc = XmlDocument.createXmlDocument(fis,0)
    object.fromXml(xdoc)
    fis.close()
class PathSubstituter:
    def __init__(self):
        self.m = re.compile('%[abcf]%')
        self.sub_array = ["%a%","%b%","%c%","%d%","%e%","%f%"]
        self.current_array = ['','','','','','','']
    def _subpath(self,mo):
        i = self.sub_array.index(mo.group(0))
        return self.current_array[i]
    def sub(self,str,array):
        self.current_array = array
        return self.m.sub(self._subpath,str)
#
class Xmlizer:
    def __init__(self):
        pass
    def toXml(self,obj,xdoc):
        if hasattr(obj,'__dict__'):
            xe = xdoc.createElement()
        else:
            raise "No __dict__ for object: " + str(obj)
    def fromXml(self):
        pass
#

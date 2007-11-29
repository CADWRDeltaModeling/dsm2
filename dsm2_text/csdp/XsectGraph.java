/*
    Copyright (C) 1998 State of California, Department of Water
    Resources.

    This program is licensed to you under the terms of the GNU General
    Public License, version 2, as published by the Free Software
    Foundation.

    You should have received a copy of the GNU General Public License
    along with this program; if not, contact Dr. Francis Chung, below,
    or the Free Software Foundation, 675 Mass Ave, Cambridge, MA
    02139, USA.

    THIS SOFTWARE AND DOCUMENTATION ARE PROVIDED BY THE CALIFORNIA
    DEPARTMENT OF WATER RESOURCES AND CONTRIBUTORS "AS IS" AND ANY
    EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
    IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
    PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE CALIFORNIA
    DEPARTMENT OF WATER RESOURCES OR ITS CONTRIBUTORS BE LIABLE FOR
    ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
    CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT
    OR SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA OR PROFITS; OR
    BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
    LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
    (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE
    USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH
    DAMAGE.

    For more information, contact:

    Dr. Francis Chung
    California Dept. of Water Resources
    Division of Planning, Delta Modeling Section
    1416 Ninth Street
    Sacramento, CA  95814
    916-653-5601
    chung@water.ca.gov

    or see our home page: http://wwwdelmod.water.ca.gov/
*/
package DWR.CSDP;
//import DWR.Graph.*;
//import DWR.Graph.Canvas.*;
import vista.graph.*;
import vista.set.*;
import vista.app.*;

import java.io.*;
//without this package it will compile, but you can't open a xsect graph
import org.python.core.*;

import java.awt.*;
import java.awt.event.*;
import java.util.*;
import javax.swing.*;
import javax.swing.event.*;
 import javax.swing.border.Border;
import javax.swing.BorderFactory; 
import java.net.URL;

/**
 * Plots data in cross-section view.
 *
 * @author
 * @version $Id: 
 */
public class XsectGraph extends JFrame implements ActionListener{

  public XsectGraph(CsdpFrame gui, App app, BathymetryData data, Network net, 
		    String centerlineName, int xsectNum, float thickness,
		    int colorOption){
    super("Cross-section view");
    setDefaultCloseOperation(WindowConstants.DO_NOTHING_ON_CLOSE);

    _gui = gui;
    _app = app;
    _bathymetryData = data;
    _net = net;
    Centerline centerline = _net.getCenterline(centerlineName);
    _xsect = centerline.getXsect(xsectNum);
    _thickness = thickness;
    _centerlineName = centerlineName;
    _xsectNum = xsectNum;
    //use this for Frame
    //    setLayout(new BorderLayout());
    //use this instead for JFrame
    setSize(800,600);
    getContentPane().setLayout(new BorderLayout());
    //    setBackground(Color.gray);

    _xsPropPanel.setLayout(new BoxLayout(_xsPropPanel, BoxLayout.Y_AXIS));
    _xsPropPanel.setBorder(_lineBorder);
    _xsPropPanel.add(_elevationButton); 
    _xsPropPanel.add(_elevationLabel);
    _xsPropPanel.add(_widthLabel);
    _xsPropPanel.add(_wetpLabel); 
    _xsPropPanel.add(_areaLabel); 
    _xsPropPanel.add(_hDepthLabel);

    //    _xsPropPanel.add(_dkLabel);

    //use this for Frame
    //    add("South",_xsPropPanel);
    //use this instead for JFrame
    

    _metadata = new JTextArea(_xsect.getMetadata());
    _metadataScrollPane = new JScrollPane(_metadata, 
					  JScrollPane.VERTICAL_SCROLLBAR_ALWAYS,
					  JScrollPane.HORIZONTAL_SCROLLBAR_ALWAYS);
    _metadataScrollPane.setPreferredSize(new Dimension(300,450));
    _eastPanel.setLayout(new BoxLayout(_eastPanel, BoxLayout.Y_AXIS));
    _eastPanel.add(_xsPropPanel);
    
    URL metadataIconUrl = this.getClass().getResource("images/metadataIcon.gif");
    ImageIcon _metadataIcon = new ImageIcon(metadataIconUrl);
    JLabel _metadataLabel = new JLabel(_metadataIcon,SwingConstants.CENTER);

    _eastPanel.add(_metadataLabel);
    _eastPanel.add(_metadataScrollPane);
    getContentPane().add("East",_eastPanel);

    ////_inputPanel.add("South", _xsPropPanel);
    ////add("South", _inputPanel);
    
    updateXsectProp();

    JPanel btnPanel = new JPanel();
    _xsCloseButton.setPreferredSize(_wideIconSize);
    _reverseButton.setPreferredSize(_iconSize);
    _arrowButton.setPreferredSize(_iconSize);
    _moveButton.setPreferredSize(_iconSize);
    _addButton.setPreferredSize(_iconSize);
    _insertButton.setPreferredSize(_iconSize);
    _deleteButton.setPreferredSize(_iconSize);
    _keepButton.setPreferredSize(_medIconSize);
    _restoreButton.setPreferredSize(_wideIconSize);
    //    _metadataButton.setPreferredSize(_medIconSize);

    btnPanel.setLayout(new FlowLayout());
    JPanel colorByPanel = new JPanel(new GridLayout(2,2));
    colorByPanel.setBorder(_raisedBevel);
    _colorByButtonGroup = new ButtonGroup();
    _colorByDistanceButton.setToolTipText("color points by dist. from xs line");
    _colorByYearButton.setToolTipText("color points by year");
    _colorBySourceButton.setToolTipText("color points by source");
    if(colorOption == COLOR_BY_DISTANCE) _colorByDistanceButton.setSelected(true);
    else if(colorOption == COLOR_BY_YEAR) _colorByYearButton.setSelected(true);
    else if(colorOption == COLOR_BY_SOURCE) _colorBySourceButton.setSelected(true);
    _colorByButtonGroup.add(_colorByDistanceButton);
    _colorByButtonGroup.add(_colorBySourceButton);
    _colorByButtonGroup.add(_colorByYearButton);
    _colorByDistanceButton.setSelectedIcon(_colorByDistanceIconSelected);
    _colorBySourceButton.setSelectedIcon(_colorBySourceIconSelected);
    _colorByYearButton.setSelectedIcon(_colorByYearIconSelected);
    colorByPanel.add(_colorByDistanceButton);
    colorByPanel.add(_colorBySourceButton);
    colorByPanel.add(_colorByYearButton);
    btnPanel.add(_xsCloseButton);
    btnPanel.add(colorByPanel);
    btnPanel.add(_reverseButton);
    btnPanel.add(_arrowButton);
    btnPanel.add(_moveButton);
    btnPanel.add(_addButton);
    btnPanel.add(_insertButton);
    btnPanel.add(_deleteButton);
    btnPanel.add(_keepButton);
    btnPanel.add(_restoreButton);

    btnPanel.add(_moveXsectXField);
    btnPanel.add(_moveXsectXButton);
    btnPanel.add(_moveXsectYField);
    btnPanel.add(_moveXsectYButton);

    //    btnPanel.add(_metadataButton);
    _arrowButton.setSelectedIcon(_cursorIconSelected);
    _moveButton.setSelectedIcon(_movePointIconSelected);
    _addButton.setSelectedIcon(_addPointIconSelected);
    _insertButton.setSelectedIcon(_insertPointIconSelected);
    _deleteButton.setSelectedIcon(_deletePointIconSelected);
    _xsectEditButtonGroup = new ButtonGroup();
    _xsectEditButtonGroup.add(_arrowButton);
    _xsectEditButtonGroup.add(_moveButton);
    _xsectEditButtonGroup.add(_addButton);
    _xsectEditButtonGroup.add(_insertButton);
    _xsectEditButtonGroup.add(_deleteButton);

    //use this for Frame
    //    add("North", btnPanel);
    //use this instead for JFrame
    getContentPane().add("North", btnPanel);

    _xsCloseButton.setToolTipText("close cross-section");
    _reverseButton.setToolTipText("reverse cross-section drawing  ");
    _arrowButton.setToolTipText("turn off edit mode");
    _moveButton.setToolTipText("move point  ");
    _addButton.setToolTipText("add point  ");
    _insertButton.setToolTipText("insert point  ");
    _deleteButton.setToolTipText("delete point  ");
    _restoreButton.setToolTipText("undo changes since last keep  ");
    _keepButton.setToolTipText("store changes in memory (not on disk!) ");
    _moveXsectXButton.setToolTipText("move cross-section in x dir.");
    _moveXsectYButton.setToolTipText("move cross-section in y dir.");
    //    _metadataButton.setToolTipText("view/edit metadata  ");
    _elevationButton.setToolTipText("change elevation for conveyance characteristics  ");

    createMenus();
    //    setColorByDistance();

    setCursor(CsdpFunctions._waitCursor);

    //some of this is repeated in updateGraphCanvas but it is 
    //necessary to do it here to create the oldNetworkDataSet
    //using any exisiting points in the network.
    makeNetworkDataSet();
    double[] x;
    double[] y;
    if(_networkDataSet != null){
	x = _networkDataSet.getXArray();
	y = _networkDataSet.getYArray();
	if(_oldNetworkDataSet == null)_oldNetworkDataSet = new NetworkDataSet("old",x,y);
    }
    
    updateGraphCanvas();

      Plot plot  = _graph.getPlot();
	plot.getAxis(AxisAttr.BOTTOM).setAxisLabel("Distance Along Cross-Section Line, feet");
	plot.getAxis(AxisAttr.LEFT).setAxisLabel("Elevation(NGVD), ft");
//      Plot plot  = _graph.getPlot();
//      /*
//       * The following code copied from updateGraphCanvas
//       */
//      plot.getAxis(AxisAttr.BOTTOM).setAxisLabel("Distance Along Cross-Section Line, feet");
//      plot.getAxis(AxisAttr.LEFT).setAxisLabel("Elevation(NGVD), ft");

//      TickGenerator tg = new SimpleTickGenerator();

//      plot.getAxis(AxisAttr.BOTTOM).setTickGenerator(tg);
//      plot.getAxis(AxisAttr.LEFT).setTickGenerator(tg);

//      AxisAttr aa = (AxisAttr)plot.getAxis(AxisAttr.BOTTOM).getAttributes();
//      aa._tickLocation = AxisAttr.INSIDE;
//      aa = (AxisAttr) plot.getAxis(AxisAttr.LEFT).getAttributes();
//      aa._tickLocation = AxisAttr.BOTH;

//      _graph.addGrid();

//      //use this for Frame
//      //    add("Center", _gC);
//      //use instead for JFrame
//      getContentPane().add("Center", _gC);
//      //this doesn't work right in swing--it just hides the window
//      //    enableEvents(WindowEvent.WINDOW_CLOSING);


    setCursor(CsdpFunctions._defaultCursor);
  }//constructor

  private void printGraphInfo(String a){

    try{
      System.out.println("printing graph info. "+a);
      
      System.out.println(_graph);
      System.out.println(_graph.getPlot());
      System.out.println(_graph.getPlot().getAttributes());
    }catch(java.lang.NullPointerException e){
      //      System.out.println("NULL VALUE!!!");
    }
  }


  /**
   * updates graph in xsect window
   */
  public void updateGraphCanvas(){

if(DEBUG)printGraphInfo("beginning of updateGraphCanvas");

    if(_gC != null) remove(_gC);
    // make bathymetry data set for plotting
    makeBathymetryDataSets();

if(DEBUG)printGraphInfo("1");

    //make network data set for plotting
    makeNetworkDataSet();
    double[] x;
    double[] y;
    if(_networkDataSet != null){
	x = _networkDataSet.getXArray();
	y = _networkDataSet.getYArray();
	/////	if(_oldNetworkDataSet == null)_oldNetworkDataSet = new NetworkDataSet("old",x,y);
    }

    /*
     * make new graph...Most of this code was copied from DefaultGraphBuilder
     */

    _graph = _factory.createGraph();
    _multiPlot = _factory.createMultiPlot();
    _graph.addPlot(_multiPlot);
    _multiPlot.add(_factory.createPlot());
    _graph.setBackgroundColor(Color.lightGray);
    _multiPlot.setCurrentPlot(0);
    TextLineAttr tla = new TextLineAttr();
    tla._font = new Font("Times Roman", Font.PLAIN, 16);
    tla._foregroundColor = Color.blue;
    ((GraphAttr)(_graph.getAttributes())).setTitleAttributes(tla);
    _graph.setTitle("Cross-section "+_centerlineName+"_"+_xsectNum+","+
		    " thickness="+_thickness+" ft.");

    //calculate number of bathymetry data sets.  Add 1 if there is a network data set.
    int ncurves=0;
    if(_networkDataSet != null){
  	ncurves++;
    }
    ncurves += _numBathymetryDataSets;

    _graph.setInsets(new java.awt.Insets(5,20,10,25));
    Font legendFont = new Font("Arial",Font.PLAIN,14);
    Legend legend = _factory.createLegend();
    _graph.setLegend(legend);
    
    Plot plot = _graph.getPlot();
    plot.setInsets(new java.awt.Insets(20,5,20,50));

    //now add dataReferences
    _refs = new DataReference[ncurves];
    for(int i=0; i<=_numBathymetryDataSets-1; i++){
	_refs[i] = new DefaultReference
	    ((NetworkDataSet)_bathymetryDataSets.get(_bathymetryDataSetNames.get(i)));
    }

    if(_networkDataSet != null){
	_refs[_numBathymetryDataSets] = new DefaultReference(_networkDataSet);
    }
    _info = new GraphBuilderInfo(_refs, MainProperties.getProperties());


    for(int i=0; i<_refs.length; i++){
	int xPos = _info.getXAxisPosition(_refs[i]);
	int yPos = _info.getYAxisPosition(_refs[i]);
	try{
	    DataSet ds = _refs[i].getData();
	    Curve crv = CurveFactory.createCurve(_refs[i], xPos, yPos, _info.getLegendLabel(_refs[i]));
	    plot.addCurve(crv);
	    if(_networkDataSet != null && i == _refs.length-1){
		CurveAttr c = (CurveAttr)crv.getAttributes();
		c.setDrawSymbol(true);
		c.setDrawLines(true);
		SymbolAttr networkSymbolAttr = new SymbolAttr();
		int ps = 4;
		int[] networkSymbolX = {-ps,ps,ps,-ps};
		int[] networkSymbolY = {ps,ps,-ps,-ps};
		networkSymbolAttr.setIsFilled(true);
		networkSymbolAttr.setSymbol(networkSymbolX, networkSymbolY, 4);
		c.setForegroundColor(Color.black);
		Symbol networkSymbol = new Symbol(networkSymbolAttr);
		crv.setSymbol(networkSymbol);
	    }else{
		setBathymetryCurveAttributes(i);
	    }
	    LegendItem li = _factory.createLegendItem();
	    //	li.setLegendName( _info.getLegendLabel(_refs[i]) );

	    try{
		li.setLegendName(_bathymetryDataSetNames.get(i));
	    }catch(java.lang.ArrayIndexOutOfBoundsException e){
		System.out.println("Exception caught in XsectGraph.updateGraphCanvas: "+e);
	    }
	    if(_networkDataSet!=null && i==_refs.length-1){
		li.setLegendName("cross-section points");
	    }
	    li.setCurve( crv );
	    LegendItemAttr lia = (LegendItemAttr) li.getAttributes();
	    legend.add(li);
	    lia._foregroundColor = Color.black;
	    lia.setFont(legendFont);
	}catch(DataRetrievalException dre){
	    dre.printStackTrace();
	}
    }
	
    // Scale components in layout resizing: Template file
    //    GEBorderLayout plotLayout = (GEBorderLayout) plot.getLayout();
    //    plotLayout.setScaleComponents(true);
    TextLineAttr dateAttr = new TextLineAttr();
    dateAttr._font = new java.awt.Font("Times Roman", java.awt.Font.PLAIN, 10);
    dateAttr._foregroundColor = java.awt.Color.red;
    dateAttr._resizeProportionally = true;
    dateAttr._justification = TextLineAttr.RIGHT;
    _graph.getLegend().add(new TextLine(dateAttr, new Date().toString()));
    /*
     * End of code copied from CsdpGraphBuilder
     */

    //    _gC = new GraphCanvas();
    _gC = new GECanvas(_graph);

    _graph.setInsets(new Insets(5,5,5,5));
    // add editing, listeners

if(DEBUG)printGraphInfo("2");

    XsectEditInteractor xei = new XsectEditInteractor
      (this, _xsect, _gC, _graph);
    _gC.addMouseListener(xei);
    _gC.addMouseMotionListener(xei);
    _gC.addComponentListener(new FontResizeInteractor(_gC));

if(DEBUG)printGraphInfo("3");


    PlotAttr pattr = (PlotAttr)plot.getAttributes();

    TickGenerator tg = new SimpleTickGenerator();
    
    plot.getAxis(AxisAttr.BOTTOM).setTickGenerator(tg);
    plot.getAxis(AxisAttr.LEFT).setTickGenerator(tg);
    
    AxisAttr aa = (AxisAttr)plot.getAxis(AxisAttr.BOTTOM).getAttributes();
    aa._tickLocation = AxisAttr.INSIDE;
    aa = (AxisAttr) plot.getAxis(AxisAttr.LEFT).getAttributes();
    aa._tickLocation = AxisAttr.BOTH;
    
    _graph.addGrid();
    
    //use this for Frame
    //    add("Center", _gC);
    //use instead for JFrame
    getContentPane().add("Center", _gC);
    //this doesn't work right in swing--it just hides the window
    //    enableEvents(WindowEvent.WINDOW_CLOSING);

  }//updateGraphCanvas

  /**
   * was paint(); changed to paintComponent() for conversion to swing.
   */
  public void paintComponents(Graphics ggg){
    super.paintComponents(ggg);
  }

  public void actionPerformed(ActionEvent e){
//     Component obj = (Component) e.getSource();
//     if(obj instanceof Button){
//       Button b = (Button) obj;
//       String label = b.getLabel();
//       if(label.equals("Print")){
// 	doPrint();
//       }
//     }//if obj
  }//actionPerformed

    //public void processEvent(AWTEvent evt){
//   if(evt instanceof WindowEvent){
//     if(evt.getID() == WindowEvent.WINDOW_CLOSING){
//       System.exit(0);
//     }
//   }
    //}//processEvent

  /**
   * sets value of isUpdated.  Used to tell if xsect has changed.
   */
  public void setIsUpdated(boolean b){
    _xsect.setIsUpdated(b);
  }

    /**
     * returns GECanvas (used to be GraphCanvas) object
     */
    public GECanvas getGC(){
      return _gC;
    }

//    /**
//     * puts squares at cross-section points (network data)
//     */
//    protected void setNetworkCurveAttributes(){
//      Curve networkCurve = _gC.getPlot().getCurve(_networkDataSet);
//      CurveAttr c = (CurveAttr)networkCurve.getAttributes();
//      c.setDrawSymbol(true);
//      SymbolAttr networkSymbolAttr = new SymbolAttr();
//      int ps = getPointSize();
//      int[] networkSymbolX = {-ps,ps,ps,-ps};
//      int[] networkSymbolY = {ps,ps,-ps,-ps};
//      networkSymbolAttr.setIsFilled(true);
//      networkSymbolAttr.setSymbol(networkSymbolX, networkSymbolY, 4);
//      c._color = Color.black;
//      Symbol networkSymbol = new Symbol(networkSymbolAttr);
//      networkCurve.setSymbol(networkSymbol);

//      setLegendTextBlack(_numBathymetryDataSets);
//    }//setNetworkCurveAttributes

//    /**
//     * sets color of legend text to black
//     */
//    protected void setLegendTextBlack(int index){
//      LegendItem litem = (LegendItem)_legend.getElement(index);
//      LegendItemAttr liattr = (LegendItemAttr)litem.getAttributes();
//      TextLineAttr tlattr = (TextLineAttr)liattr;
//      //    tlattr.setBackgroundColor(Color.black);
//      tlattr.setForegroundColor(Color.black);
//    }//setLegendTextBlack

  /**
   * sets attributes for plotting bathymetry
   */
  protected void setBathymetryCurveAttributes(int i){
    NetworkDataSet dataSet = (NetworkDataSet)
      (_bathymetryDataSets.get(_bathymetryDataSetNames.get(i)));
    //    Curve bathymetryCurve = _gC.getPlot().getCurve(dataSet);
    Curve bathymetryCurve = _graph.getPlot().getCurve(i);
    CurveAttr c = (CurveAttr)bathymetryCurve.getAttributes();

    //removed this line because of classCastException.  Is it necessary?
    //    ((LineElementAttr)(bathymetryCurve.getAttributes()))._backgroundColor = _gui.getColor(i);

    //    _legend.getAttributes().setForegroundColor(Color.black);

    //    setLegendTextBlack(i);

    c.setDrawSymbol(true);
    c.setDrawLines(false);
    int symbolDim = 0;
    float symbolDimf = 0.0f;
    if(getColorByDistanceMode()){

	float sd = (float)getPointSize();
	float nds = (float)_numBathymetryDataSets;
	float fi = (float)i;
	symbolDimf = (sd/2.0f * ((nds-fi)/4.0f))+1.0f;
	//	symbolDim = (getPointSize()/2 * ((_numBathymetryDataSets-i)/4))+1;
	symbolDim = (int)symbolDimf;
	if(DEBUG)System.out.println("symbol size set.  i, getPointSize(), symbolDimf, symbolDim=" + i+","+getPointSize()+","+symbolDimf+","+symbolDim);
    }else{
	symbolDim = getPointSize()/2;
    }
    SymbolAttr bathymetrySymbolAttr = new SymbolAttr();
    int[] bathymetrySymbolX = {-symbolDim,symbolDim,symbolDim,-symbolDim};
    int[] bathymetrySymbolY = {symbolDim,symbolDim,-symbolDim,-symbolDim};
    bathymetrySymbolAttr.setIsFilled(true);
    bathymetrySymbolAttr.setSymbol(bathymetrySymbolX, bathymetrySymbolY, 4);
    if(DEBUG)System.out.println
	       ("setting attributes for _bathymetryDataSetNames.get(i): color="+
		_gui.getColor(i));
    
    bathymetrySymbolAttr._foregroundColor = _gui.getColor(i);
    c._foregroundColor = _gui.getColor(i);
    Symbol bathymetrySymbol = new Symbol(bathymetrySymbolAttr);
    bathymetryCurve.setSymbol(bathymetrySymbol);
  }//setBathymetryCurveAttributes
  
    /**
     * Change the size of points in cross-section window
     */
    public void setPointSize(int size){
	if(size < 2){
	    System.out.println("Size is too small");
	}else{
	    _app.setSquareDimension(size);
	    //	    squareDimension = size;
	}
    }//setPointSize

    public int getPointSize(){
	return _app.getSquareDimension();
    }

  public void createMenus(){
    JMenuBar menubar;

    JMenu xgXsect, xgBathymetry, xgEdit;
    JMenuItem xReverse, xKeep, xRestore, xPrint, xClose;
    //, xMetadata;
    menubar = new JMenuBar();
    this.setJMenuBar(menubar);

    /*
     * Xsect menu
     */
    xgXsect = new JMenu("Xsect");
//      xgXsect.add(xReverse = new JMenuItem("Reverse Xsect"));
//      xReverse.setAccelerator
//  	(KeyStroke.getKeyStroke(KeyEvent.VK_R, ActionEvent.CTRL_MASK));
//      xgXsect.add(xKeep = new JMenuItem("Keep Xsect"));
//      xKeep.setAccelerator
//  	(KeyStroke.getKeyStroke(KeyEvent.VK_K, ActionEvent.CTRL_MASK));
//      xgXsect.add(xRestore = new JMenuItem("Restore Xsect"));
//      xRestore.setAccelerator
//  	(KeyStroke.getKeyStroke(KeyEvent.VK_E, ActionEvent.CTRL_MASK));
    xgXsect.add(xPrint = new JMenuItem("Print Xsect"));
//      xgXsect.add(xMetadata = new JMenuItem("Metadata"));
    xgXsect.add(xClose = new JMenuItem("Close"));
    xClose.setAccelerator
	(KeyStroke.getKeyStroke(KeyEvent.VK_C, ActionEvent.CTRL_MASK));
    menubar.add(xgXsect);

    /*
     * bathymetry menu
     */
    xgBathymetry = new JMenu("Bathymetry");
    JMenu cb = new JMenu("Color By");
    xgBathymetry.add(_bChangePointSize = new JMenuItem("Change Point Size"));
//      cb.add(_bColorByDistance = new JCheckBoxMenuItem("distance"));
//      cb.add(_bColorBySource   = new JCheckBoxMenuItem("source"));
//      cb.add(_bColorByYear     = new JCheckBoxMenuItem("year"));
    xgBathymetry.add(cb);
    menubar.add(xgBathymetry);

    //create and register listeners for bathymetry menu
    XsectBathymetryMenu xsectBathymetryMenu = new XsectBathymetryMenu(this);
      ItemListener bColorByDistanceListener = xsectBathymetryMenu.new XColorByDistance();
      ItemListener bColorBySourceListener = xsectBathymetryMenu.new XColorBySource();
      ItemListener bColorByYearListener = xsectBathymetryMenu.new XColorByYear();
    ActionListener bChangePointSizeListener = xsectBathymetryMenu.new BChangePointSize();
      _colorByDistanceButton.addItemListener(bColorByDistanceListener);
      _colorBySourceButton.addItemListener(bColorBySourceListener);
      _colorByYearButton.addItemListener(bColorByYearListener);
    _bChangePointSize.addActionListener(bChangePointSizeListener);

    /*
     * Edit menu
     */
      xgEdit = new JMenu("Edit");
//        xgEdit.add(_xMovePointMenuItem   = new JCheckBoxMenuItem("Move Point", false));
//        xgEdit.add(_xAddPointMenuItem    = new JCheckBoxMenuItem("Add Point", false));
//        xgEdit.add(_xInsertPointMenuItem = new JCheckBoxMenuItem("Insert Point", false));
//        xgEdit.add(_xDeletePointMenuItem = new JCheckBoxMenuItem("Delete Point", false));
      ////      menubar.add(xgEdit);

    //create and register action listener objects for the Xsect menu items
    //and for the buttons
    XsectEditMenu xsectEditMenu = new XsectEditMenu(this, _net, _app);
    ActionListener xReverseListener    = xsectEditMenu.new XReverse(_xsect);
    ActionListener xKeepListener       = xsectEditMenu.new XKeep();
    ActionListener xRestoreListener    = xsectEditMenu.new XRestore();
    ActionListener xChangeElevationListener = xsectEditMenu.new XChangeElevation();
    ActionListener xPrintListener      = xsectEditMenu.new XPrint(this);
    EventListener xCloseListener      = xsectEditMenu.new XClose();
    ItemListener xMovePointListener   = xsectEditMenu.new XMovePoint();
    ItemListener xAddPointListener    = xsectEditMenu.new XAddPoint();
    ItemListener xInsertPointListener = xsectEditMenu.new XInsertPoint();
    ItemListener xDeletePointListener = xsectEditMenu.new XDeletePoint();
    ItemListener xStopEditListener = xsectEditMenu.new XStopEdit();
    ActionListener xMoveXsectXListener = xsectEditMenu.new XMoveXsectX(_moveXsectXField,_xsect);
    ActionListener xMoveXsectYListener = xsectEditMenu.new XMoveXsectY(_moveXsectYField,_xsect);

    DocumentListener xMetadataListener   = 
	  //  	xsectEditMenu.new XMetadata(_centerlineName, _xsectNum, _xsect,_gui);
	xsectEditMenu.new XMetadata();
    _metadata.getDocument().addDocumentListener(xMetadataListener);

    //    xReverse.addActionListener(xReverseListener);
//      xKeep.addActionListener(xKeepListener);
//      xRestore.addActionListener(xRestoreListener);
    _elevationButton.addActionListener(xChangeElevationListener);
    xPrint.addActionListener(xPrintListener);
    xClose.addActionListener((ActionListener)xCloseListener);

    /////////    addWindowListener((WindowListener)xCloseListener);
    _xsCloseButton.addActionListener((ActionListener)xCloseListener);

    _moveButton.addItemListener(xMovePointListener);
    _addButton.addItemListener(xAddPointListener);
    _insertButton.addItemListener(xInsertPointListener);
    _deleteButton.addItemListener(xDeletePointListener);
    _arrowButton.addItemListener(xStopEditListener);
    
    _moveButton.addItemListener(xMovePointListener);
    _addButton.addItemListener(xAddPointListener);
    _insertButton.addItemListener(xInsertPointListener);
    _deleteButton.addItemListener(xDeletePointListener);
    _arrowButton.addItemListener(xStopEditListener);
    
    _reverseButton.addActionListener(xReverseListener);
    _restoreButton.addActionListener(xRestoreListener);
    _keepButton.addActionListener(xKeepListener);

    _moveXsectXButton.addActionListener(xMoveXsectXListener);
    _moveXsectYButton.addActionListener(xMoveXsectYListener);
    //    _metadataButton.addActionListener(xMetadataListener);
    
  }//createMenus

  /**
   * update displayed network data
   */
  public void updateNetworkDataSet(){
    int numPoints = _xsect.getNumPoints();
    double[] xArray;
    double[] yArray;
    double networkMaxX = 0.0;
    double networkMaxY = 0.0;
    double networkMinX = 0.0;
    double networkMinY = 0.0;
    double bathymetryMaxX = 0.0;
    double bathymetryMaxY = 0.0;
    double bathymetryMinX = 0.0;
    double bathymetryMinY = 0.0;
    double maxX = 0.0;
    double maxY = 0.0;
    double minX = 0.0;
    double minY = 0.0;

    if(DEBUG)System.out.println("arraylength, numpoints="+
				_networkDataSet.size()+","+numPoints);
    //    Curve networkCurve = _graph.getPlot().getCurve(_networkDataSet);
    makeNetworkDataSet();
//  //  //      Curve networkCurve = _graph.getPlot().getCurve(_numBathymetryDataSets);
//  //  //      _refs[_numBathymetryDataSets] = new DefaultReference(_networkDataSet);
//  //  //      networkCurve = CurveFactory.createCurve(_refs[_numBathymetryDataSets], xPos, yPos, "label");
//      if(networkCurve != null){
//        networkCurve.setDataSet( _networkDataSet );
//      }
    //must use getX and getY before calling next getMinimum or getMaximum because values
    //not saved.
//     if(_networkDataSet.getMinimum() != null && 
//        _networkDataSet.getMaximum() != null &&
//        _bathymetryDataSet.getMinimum() != null && 
//        _bathymetryDataSet.getMaximum() != null){

    if(_networkDataSet != null && _numBathymetryDataSets > 0){
	if(_networkDataSet !=null){
	networkMaxX = _networkDataSet.getMaxX();
	networkMaxY = _networkDataSet.getMaxY();
	networkMinX = _networkDataSet.getMinX();
	networkMinY = _networkDataSet.getMinY();
      }
      else{
	networkMaxX = CsdpFunctions.SMALL_FLOAT;
	networkMaxY = CsdpFunctions.SMALL_FLOAT;
	networkMinX = CsdpFunctions.BIG_FLOAT;
	networkMinY = CsdpFunctions.BIG_FLOAT;
      }
      NetworkDataSet dataSet = null;
      for(int i=0; i<=_numBathymetryDataSets-1; i++){
	dataSet = (NetworkDataSet)
	  (_bathymetryDataSets.get(_bathymetryDataSetNames.get(i)));
	bathymetryMaxX = Math.max(bathymetryMaxX,dataSet.getMaxX());
	bathymetryMaxY = Math.max(bathymetryMaxY,dataSet.getMaxY());
	bathymetryMinX = Math.min(bathymetryMinX,dataSet.getMinX());
	bathymetryMinY = Math.min(bathymetryMinY,dataSet.getMinY());
      }//for
      maxX = Math.max(bathymetryMaxX, networkMaxX);
      maxY = Math.max(bathymetryMaxY, networkMaxY);
      minX = Math.min(bathymetryMinX, networkMinX);
      minY = Math.min(bathymetryMinY, networkMinY);
      
      if(DEBUG)System.out.println("maxx, maxy, minx, miny="+maxX+","+maxY+","+minX+","+minY);
      _graph.getPlot().getAxis(AxisAttr.BOTTOM).setDCRange(minX,maxX);
      _graph.getPlot().getAxis(AxisAttr.LEFT).setDCRange(minY,maxY);
    }
    _restoreXsect = false;
  }//updateNetworkDataSet

  /**
   * turns off all edit modes
   */
  public void turnOffEditModes(){
//      setAllModesStatesFalse();
    setAllButtonsTrue();
  }

  /**
   * restores xsect (undo changes made since last keep)
   */
  public void restoreXsect(){
      if(_oldNetworkDataSet != null){
	  double[] x = _oldNetworkDataSet.getXArray();
	  double[] y = _oldNetworkDataSet.getYArray();

	  if(DEBUG){
	      System.out.println("##############################################################################");
	      System.out.println("oldSize, newSize="+_oldNetworkDataSet.size()+","+_networkDataSet.size());
	      System.out.println("restoring xsect: current x,y; restoring to old x,y=");
	      double[] xCurrent = _networkDataSet.getXArray();
	      double[] yCurrent = _networkDataSet.getYArray();
	      for(int i=0; i<=_oldNetworkDataSet.size()-1; i++){
		  System.out.println(CsdpFunctions.formattedOutputString((float)xCurrent[i],20,true)+","+
				     CsdpFunctions.formattedOutputString((float)yCurrent[i],20,true)+","+
				     CsdpFunctions.formattedOutputString((float)x[i],20,true)+","+
				     CsdpFunctions.formattedOutputString((float)y[i],20,true));
	      }
	      System.out.println("##############################################################################");
	  }

	  _xsect.removeAllPoints();
	  if(_oldNetworkDataSet != null && _oldNetworkDataSet.size() > 0){
	      System.out.println("adding old points back to xsect. size="+_oldNetworkDataSet.size());
	      for(int i=0; i<=_oldNetworkDataSet.size()-1; i++){
		  System.out.println("restoring: _xsect,x,y="+_xsect+","+x+","+y);
		  _xsect.addXsectPoint((float)x[i], (float)y[i]);
	      }
	  }else{
	      System.out.println("NOT ADDING POINTS TO XSECT. SIZE="+_oldNetworkDataSet.size());
	  }
	  makeNetworkDataSet();
	  _restoreXsect = true;
	  
	  System.out.println("restoring xsect");
	  //called by listener, so don't need here...
	  updateNetworkDataSet();
      }else{
	  _xsect.removeAllPoints();
	  _networkDataSet=null;







      }
  }//restoreXsect

  /**
   * keeps xsect changes, but doesn't save to file
   */
  public void keepChanges(){
    double[] x;
    double[] y;
    x = _networkDataSet.getXArray();
    y = _networkDataSet.getYArray();
    _oldNetworkDataSet = null;
    _oldNetworkDataSet = new NetworkDataSet("old",x,y);

    if(DEBUG){
	System.out.println("#####################################################");
	System.out.println("keeping changes: x, y=");
	
	for(int i=0; i<=_networkDataSet.size()-1; i++){
	    System.out.println(CsdpFunctions.formattedOutputString((float)x[i],20,true)+","+
			       CsdpFunctions.formattedOutputString((float)y[i],20,true));
	}
	System.out.println("#####################################################");
    }

    _xsect.setIsUpdated(true);
    if(DEBUG)System.out.println("_xsect.isUpdated=" + _xsect._isUpdated);

  }

//    /**
//     * adds network data set to graph
//     */
//    public void addNetworkDataSet(){
//      _gC.addData(_networkDataSet, AxisAttr.BOTTOM, AxisAttr.LEFT);
//      setNetworkCurveAttributes();
//    }

  /**
   * sets all buttons to false(inactive)
   */
  protected void setAllButtonsTrue(){
    _moveButton.setEnabled(true);
    _addButton.setEnabled(true);
    _insertButton.setEnabled(true);
    _deleteButton.setEnabled(true);
  }

  /**
   * makes data set to store network data to be plotted
   */
  protected void makeNetworkDataSet(){
    int numPoints = _xsect.getNumPoints();
    if(DEBUG)System.out.println("number of points in xsect="+numPoints);

    if(numPoints>0){
	double[] x = new double[numPoints];
	double[] y = new double[numPoints];
	for(int i=0; i<=numPoints-1; i++){
	    x[i] = _xsect.getXsectPoint(i).getStation();
	    y[i] = _xsect.getXsectPoint(i).getElevation();
	    if(DEBUG)System.out.println("x["+i+"]="+x[i]+","+"y["+i+"]="+y[i]);
	}
	_networkDataSet    = new NetworkDataSet("Network", x, y);
    }
  }//make network data set

  /**
   * Make NetworkDataSets to store bathymetry data to be plotted.  Each set will be
   * from a different year, source, or elevation range (bin) depending on plot
   * options.
   */
    protected void makeBathymetryDataSets(){
	
	int numBathymetryValues = _bathymetryData.getNumEnclosedValues();
	if(DEBUG)System.out.println("numBathymetryValues="+numBathymetryValues);
	
	int numDataSets = 0;
	float[] point = new float[2];
	int pointIndex = 0;
	int dataSetPointIndex = 0;
	String dataSetName = null;
	_numBathymetryDataSets = 0;
	float distanceBinSize = 0.0f;
	
	float[] xsectLine = _net.findXsectLineCoord(_centerlineName, _xsectNum);
	float x1 = xsectLine[CsdpFunctions.x1Index];
	float y1 = xsectLine[CsdpFunctions.y1Index];
	float x2 = xsectLine[CsdpFunctions.x2Index];
	float y2 = xsectLine[CsdpFunctions.y2Index];
	
	if(DEBUG)System.out.println("got xsectLine. x1,y1,x2,y2="+x1+","+y1+","+x2+","+y2);
	
	//find number of series (number of colors to use in graph)
	//why use number of colors?
	
	if(getColorByDistanceMode()){
	    int numBins = NUM_DISTANCE_BINS;
	    //these were the original values, but they resulted in errors.
//  	    distanceBinSize = _thickness/_gui.getNumColors();
//  	    numDataSets = _gui.getNumColors();

  	    if(_gui.getNumColors() < NUM_DISTANCE_BINS){
  		numBins=_gui.getNumColors();
  	    }
  	    distanceBinSize = _thickness/numBins;
  	    numDataSets = numBins;
	    
	    if(DEBUG)System.out.println("colordistancemode. numColors, thickness="+_gui.getNumColors()+","+_thickness);
	    if(DEBUG)System.out.println("distanceBinSize, numDataSets="+distanceBinSize+","+numDataSets);
	}//if colorByDistance
	else if(getColorBySourceMode()) numDataSets = _bathymetryData.getNumSources();
	else if(getColorByYearMode())   numDataSets = _bathymetryData.getNumYears();
	
	if(DEBUG)System.out.println("in xsectgraph: numsources="+
				    _numBathymetryDataSets);
	_numBathymetryDataSets = 0; 
	_bathymetryDataSets.clear();
	
	for(int i=0; i<=numDataSets-1; i++){
	    float binValue = 0.0f;
	    float dist = 0.0f;
	    double[] station   = new double[_bathymetryData.getNumEnclosedValues()];
	    double[] elevation = new double[_bathymetryData.getNumEnclosedValues()];
	    
	    //find name of data set(distance, year, or source)
	    if(getColorByDistanceMode()){
		binValue = distanceBinSize*(i);
		dataSetName = Float.toString(binValue) + " ft from Cross-section line";
	    }
	    else if(getColorBySourceMode()) dataSetName = _bathymetryData.getSource(i);
	    else if(getColorByYearMode()) dataSetName = 
					      Short.toString(_bathymetryData.getYear(i));

	    //beginning of new dataSet, so set index to zero.
	    dataSetPointIndex = 0;
	    for(int j=0; j<= numBathymetryValues-1; j++){
		pointIndex = _bathymetryData.getEnclosedPointIndex(j);
		float x3 = (_bathymetryData.getPoint(pointIndex))[xIndex];
		float y3 = (_bathymetryData.getPoint(pointIndex))[yIndex];
		
		if(DEBUG)System.out.println("x1x2x3y1y2y3="+x1+","+x2+","+x3+","+y1+","+y2+","+y3);
		dist = CsdpFunctions.shortestDistLineSegment(x1,x2,x3,y1,y2,y3);
		//if the distance from the point to the cross-section line is 
		//within the range defined by the binValue, add it to the dataset
		if(getColorByDistanceMode()){
		    point = _bathymetryData.getEnclosedStationElevation(j);
		    if(DEBUG)System.out.println("dist,binValue="+dist+","+binValue);
		    
		    if(binValue < dist && dist < binValue+distanceBinSize){
			station[dataSetPointIndex]   = (double)point[stationIndex];
			elevation[dataSetPointIndex] = (double)point[elevationIndex];
			dataSetPointIndex++;
		    }
		}//if colorByDistance
		else if(getColorBySourceMode()){
		    if(dist < CsdpFunctions.BIG_FLOAT &&
		       dataSetName.equals(_bathymetryData.getSource(_bathymetryData.getSourceIndex(pointIndex)))){
			point = _bathymetryData.getEnclosedStationElevation(j);
			station[dataSetPointIndex]   = (double)point[stationIndex];
			elevation[dataSetPointIndex] = (double)point[elevationIndex];
			dataSetPointIndex++;
		    }
		}//else if
		else if(getColorByYearMode()){
		    int yearIndex = _bathymetryData.getYearIndex(pointIndex);
		    short year = _bathymetryData.getYear(yearIndex);
		    if(dist < CsdpFunctions.BIG_FLOAT && dataSetName.equals(Short.toString(year))){
			point = _bathymetryData.getEnclosedStationElevation(j);
			station[dataSetPointIndex]   = (double)point[stationIndex];
			elevation[dataSetPointIndex] = (double)point[elevationIndex];
			dataSetPointIndex++;
		    }//if
		}//else if
	    }//for j:  looping through all enclosed bathymetry points
	    
	    //if any points were added to the dataset, create new arrays which are
	    //dimensioned to the number of points in the dataset.
	    if(dataSetPointIndex > 0){
		if(DEBUG)System.out.println("making data set "+dataSetName+
					    ", numPoints="+dataSetPointIndex);
		
		double[] newStation = new double[dataSetPointIndex];
		double[] newElevation = new double[dataSetPointIndex];
		for(int j=0; j<=dataSetPointIndex-1; j++){
		    newStation[j] = station[j];
		    newElevation[j] = elevation[j];
		}
		if(DEBUG)System.out.println("xsectGraph.makeBathymetryDataSets: adding element to bathymetrydatasetnames. index, value="+_numBathymetryDataSets+","+dataSetName);
		
		_bathymetryDataSets.put(dataSetName, 
					new NetworkDataSet(dataSetName,newStation,newElevation));
		_bathymetryDataSetNames.put(_numBathymetryDataSets,dataSetName);
		_numBathymetryDataSets++;
	    }else{
		if(DEBUG)System.out.println("no points added to dataset "+dataSetName);
	    }
	}//for i
	
	if(DEBUG)System.out.println("end of makeBathymetryDataSets.  numBathymetryDataSets="+_numBathymetryDataSets);
	
    }//makeBathymetryDataSets

  /**
   * change number of cross-section
   */
  public void updateXsectNum(int n){
    System.out.println("updating the xsect num to "+n);
    _xsectNum = n;
    _graph.setTitle("Cross-section "+_centerlineName+"_"+_xsectNum+","+
		 " thickness="+_thickness+" ft.");
    updateDisplay();
  }//updateXsectNum

  /**
   * repaints canvas
   */
  public void updateDisplay(){
      //needed for swing
      updateGraphCanvas();
      updateXsectProp();
      //needed for swing
      validate();
      _gC.redoNextPaint();
      //removed for conversion to swing
      //_gC.paintAll(_gC.getGraphics());
      _gC.repaint();
  }//updateDisplay

  /**
   * calculate xsectprop with new elevation
   */
  protected void updateXsectProp(){
    float width = 0.0f;
    float area = 0.0f;
    float wetp = 0.0f;
    float hDepth = 0.0f;
    //    float dk   = 0.0f;
    width = _xsect.getWidth(_xsectPropElevation);
    area  = _xsect.getArea(_xsectPropElevation);
    wetp  = _xsect.getWettedPerimeter(_xsectPropElevation);
    hDepth= _xsect.getHydraulicDepth(_xsectPropElevation);
    //dk    = _xsect.getDConveyance(_xsectPropElevation);
    _elevationLabel.setText("   Elevation, ft(NGVD)   = "+_xsectPropElevation+"   ");
    _widthLabel.setText    ("   Width, ft            =" + CsdpFunctions.twoPlaces(width)+"   ");
    _areaLabel.setText     ("   Area, square ft      = " + CsdpFunctions.twoPlaces(area)+"   ");
    _wetpLabel.setText     ("   Wetted Perimeter, ft = " + CsdpFunctions.twoPlaces(wetp)+"   ");
    _hDepthLabel.setText   
	("   HydraulicDepth, ft   = " + CsdpFunctions.twoPlaces(hDepth)+"   ");
    //    _dkLabel.setText       ("DConveyance          = " + CsdpFunctions.twoPlaces(dk));
  }

    /**
     * called when keep button pressed.
     */
    public void setChangesKept(boolean b){
	_changesKept = b;
    }

    /**
     * Only called when closing window to see if there are any changes
     * that need to be saved (in memory, not in network file).
     */
    public boolean getChangesKept(){
	return _changesKept;
    }

    public boolean getAddPointMode(){
	return _addButton.isSelected();
    }
    public boolean getInsertPointMode(){
	return _insertButton.isSelected();
    }
    public boolean getDeletePointMode(){
	return _deleteButton.isSelected();
    }
    public boolean getMovePointMode(){
	return _moveButton.isSelected();
    }
    public boolean getColorByDistanceMode(){
	return _colorByDistanceButton.isSelected();
    }
    public boolean getColorBySourceMode(){
	return _colorBySourceButton.isSelected();
    }
    public boolean getColorByYearMode(){
	return _colorByYearButton.isSelected();
    }

  protected BathymetryPlot _plotter;
  protected NetworkPlot _networkPlotter; 
  App _app;
  BathymetryData _bathymetryData;
  Network _net;
  GECanvas _gC;
  private NetworkDataSet _networkDataSet;
  private NetworkDataSet _oldNetworkDataSet;
  public static final int DATA_LENGTH = 100;
  /*
   * size of cross-section points
   */
    //  protected static int squareDimension = 2;
  protected static final int stationIndex   = 0; 
  protected static final int elevationIndex = 1; 

  Xsect _xsect;
//    protected JCheckBoxMenuItem _xMovePointMenuItem, _xAddPointMenuItem, 
//      _xInsertPointMenuItem, _xDeletePointMenuItem;
    
    URL xsCloseUrl = this.getClass().getResource("images/XSCloseButton.jpg");
    URL cursorIconUrl = this.getClass().getResource("images/ArrowButton.jpg");
    URL reverseXsectIconUrl = this.getClass().getResource("images/ReverseXsectButton.jpg");
    URL movePointIconUrl = this.getClass().getResource("images/MoveXsectPointButton.jpg");
    URL addPointIconUrl = this.getClass().getResource("images/AddXsectPointButton.jpg");
    URL insertPointIconUrl = this.getClass().getResource("images/InsertXsectPointButton.jpg");
    URL deletePointIconUrl = this.getClass().getResource("images/DeleteXsectPointButton.jpg");
    URL cursorIconSelectedUrl = this.getClass().getResource("images/ArrowButtonSelected.jpg");
    URL movePointIconSelectedUrl = this.getClass().getResource("images/MoveXsectPointButtonSelected.jpg");
    URL addPointIconSelectedUrl = this.getClass().getResource("images/AddXsectPointButtonSelected.jpg");
    URL insertPointIconSelectedUrl = this.getClass().getResource("images/InsertXsectPointButtonSelected.jpg");
    URL deletePointIconSelectedUrl = this.getClass().getResource("images/DeleteXsectPointButtonSelected.jpg");
    URL keepIconUrl = this.getClass().getResource("images/KeepButton.jpg");
    URL restoreIconUrl = this.getClass().getResource("images/RestoreButton.jpg");
    URL colorByDistanceIconUrl = this.getClass().getResource("images/ColorDistanceButton.gif");
    URL colorByDistanceIconSelectedUrl = this.getClass().getResource("images/ColorDistanceButtonSelected.gif");
    URL colorBySourceIconUrl = this.getClass().getResource("images/ColorSourceButton.gif");
    URL colorBySourceIconSelectedUrl = this.getClass().getResource("images/ColorSourceButtonSelected.gif");
    URL colorByYearIconUrl = this.getClass().getResource("images/ColorYearButton.gif");
    URL colorByYearIconSelectedUrl = this.getClass().getResource("images/ColorYearButtonSelected.gif");

    ImageIcon _xsCloseIcon = new ImageIcon(xsCloseUrl);
    ImageIcon _cursorIcon = new ImageIcon(cursorIconUrl);
    ImageIcon _reverseXsectIcon = new ImageIcon(reverseXsectIconUrl);
    ImageIcon _movePointIcon = new ImageIcon(movePointIconUrl);
    ImageIcon _addPointIcon = new ImageIcon(addPointIconUrl);
    ImageIcon _insertPointIcon = new ImageIcon(insertPointIconUrl);
    ImageIcon _deletePointIcon = new ImageIcon(deletePointIconUrl);
    ImageIcon _cursorIconSelected = new ImageIcon(cursorIconSelectedUrl);
    ImageIcon _movePointIconSelected = new ImageIcon(movePointIconSelectedUrl);
    ImageIcon _addPointIconSelected = new ImageIcon(addPointIconSelectedUrl);
    ImageIcon _insertPointIconSelected = new ImageIcon(insertPointIconSelectedUrl);
    ImageIcon _deletePointIconSelected = new ImageIcon(deletePointIconSelectedUrl);

    ImageIcon _keepIcon = new ImageIcon(keepIconUrl);
    ImageIcon _restoreIcon = new ImageIcon(restoreIconUrl);

    ImageIcon _colorByDistanceIcon = new ImageIcon(colorByDistanceIconUrl);
    ImageIcon _colorByDistanceIconSelected = new ImageIcon(colorByDistanceIconSelectedUrl);
    ImageIcon _colorBySourceIcon = new ImageIcon(colorBySourceIconUrl);
    ImageIcon _colorBySourceIconSelected = new ImageIcon(colorBySourceIconSelectedUrl);
    ImageIcon _colorByYearIcon = new ImageIcon(colorByYearIconUrl);
    ImageIcon _colorByYearIconSelected = new ImageIcon(colorByYearIconSelectedUrl);

    JRadioButton _colorByDistanceButton = new JRadioButton(_colorByDistanceIcon);
    JRadioButton _colorBySourceButton = new JRadioButton(_colorBySourceIcon);
    JRadioButton _colorByYearButton = new JRadioButton(_colorByYearIcon);

    JButton _xsCloseButton = new JButton(_xsCloseIcon);
  JButton _reverseButton = new JButton(_reverseXsectIcon);
    JRadioButton _arrowButton = new JRadioButton(_cursorIcon);
  JRadioButton _moveButton    = new JRadioButton(_movePointIcon);
  JRadioButton _addButton     = new JRadioButton(_addPointIcon);
  JRadioButton _insertButton  = new JRadioButton(_insertPointIcon);
  JRadioButton _deleteButton  = new JRadioButton(_deletePointIcon);

  JButton _restoreButton = new JButton(_restoreIcon);
  JButton _keepButton    = new JButton(_keepIcon);
    //    JButton _metadataButton = new JButton(_metadataIcon);

    ButtonGroup _colorByButtonGroup, _xsectEditButtonGroup;
    
    private static final Dimension _iconSize = new Dimension(25,25);
    private static final Dimension _medIconSize = new Dimension(33,25);
    private static final Dimension _wideIconSize = new Dimension(50,25);

  protected boolean _restoreXsect = false;
  protected float _thickness;
  protected static final boolean DEBUG = false;

  protected Hashtable _bathymetryDataSets = new Hashtable();
  protected int _numBathymetryDataSets = 0;
//    protected boolean _colorByDistance = false;
//    protected boolean _colorBySource   = false;
//    protected boolean _colorByYear     = true;
  protected ResizableStringArray _bathymetryDataSetNames = 
    new ResizableStringArray();

    JPanel _eastPanel = new JPanel();
  JPanel _xsPropPanel = new JPanel();
    JPanel _inputPanel = new JPanel();
  JButton _elevationButton = new JButton("Change elevation");
  JLabel _elevationLabel   = new JLabel();
  JLabel _widthLabel       = new JLabel();
  JLabel _areaLabel        = new JLabel();
  JLabel _wetpLabel        = new JLabel();
    JLabel _hDepthLabel    = new JLabel();
    //    JLabel _dkLabel        = new JLabel();

    //  JCheckBoxMenuItem _bColorByDistance, _bColorBySource, _bColorByYear;
  JMenuItem _bChangePointSize;
  String _centerlineName = null;
  int _xsectNum;
  protected static final int xIndex = 0;
  protected static final int yIndex = 1;
  protected float _xsectPropElevation = 0.0f;
  Legend _legend;
  CsdpFrame _gui;
  public int COLOR_BY_DISTANCE = 0;
  public int COLOR_BY_YEAR = 1;
  public int COLOR_BY_SOURCE = 2;
    protected boolean _changesKept = false;
    Border _raisedBevel = BorderFactory.createRaisedBevelBorder();
    Border _lineBorder = BorderFactory.createLineBorder(Color.black,2);

    //all new
    public JTextArea _metadata;
    private ResizableStringArray _metadataMessage;
    private JScrollPane _metadataScrollPane;

    //    DefaultGraphBuilder _dgb = new DefaultGraphBuilder();
    Graph _graph;
    private GraphFactory _factory = new DefaultGraphFactory();
    private MultiPlot _multiPlot;
    private DataReference[] _refs;
    GraphBuilderInfo _info;

    private JTextField _moveXsectXField = new JTextField("0",4);
    private JTextField _moveXsectYField = new JTextField("0",4);
    private JButton _moveXsectXButton = new JButton("dX");
    private JButton _moveXsectYButton = new JButton("dY");

    /*
     * When color by distance used, this is the maximum number of bins.
     */
    private final int NUM_DISTANCE_BINS = 10;
}//class XsectGraph

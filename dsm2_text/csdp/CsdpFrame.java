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
import vista.graph.*;
import java.awt.*;
import java.awt.event.*;
import java.util.*;
import javax.swing.*;
 import javax.swing.border.Border;
import javax.swing.BorderFactory; 
import java.lang.System;
import java.net.URL;

/**
 * Display Frame
 *
 * @author $Author: 
 * @version $Id: CsdpFrame.java,v 1.2 2002/10/21 19:58:54 btom Exp $
 */
public class CsdpFrame extends JFrame{  

public CsdpFrame(App app) {
    makeIconButtons();
  setTitle("Cross-Section Development Program Version "+CsdpFunctions.getVersion());
  //  setIconImage(Toolkit.getDefaultToolkit().
  //   createImage("DWR_Logo-1.0in.gif"));

  //only for JFrame
  setDefaultCloseOperation(WindowConstants.DO_NOTHING_ON_CLOSE);
  _app=app;

  setSize(getInitialWidth(), getInitialHeight());
  setCurrentWidth(getInitialWidth());
  setCurrentHeight(getInitialHeight());
  _dim = getSize();

  _canvas1.setPlotter(app._bathymetryPlot, _dim);

  /////  _canvas1.setBackground(Color.white);

  createGui();
  //  setColorByDepth();
  setResizable(true);
  setBackground(Color.white);
  pack();
  show();
  setDefaultColors();
}

    private void makeIconButtons(){
	URL bathUrl = this.getClass().getResource("images/FileOpenButton.jpg");
	URL propUrl = this.getClass().getResource("images/PropOpenButton.jpg");
	URL netOpenUrl = this.getClass().getResource("images/NetworkOpenButton.jpg");
	URL netSaveUrl = this.getClass().getResource("images/NetworkSaveButton.jpg");
	URL arrowUrl = this.getClass().getResource("images/ArrowButton.jpg");
	URL insertPointUrl = this.getClass().getResource("images/InsertPointButton.jpg");
	URL movePointUrl = this.getClass().getResource("images/MovePointButton.jpg");
	URL addPointUrl = this.getClass().getResource("images/AddPointButton.jpg");
	URL deletePointUrl = this.getClass().getResource("images/DeletePointButton.jpg");
	URL addXsectUrl = this.getClass().getResource("images/AddXsectButton.jpg");

	URL removeXsectUrl = this.getClass().getResource("images/RemoveXsectButton.jpg");
	URL moveXsectUrl = this.getClass().getResource("images/MoveXsectButton.jpg");

	URL viewXsectUrl = this.getClass().getResource("images/ViewXsectButton.jpg");

	URL colorUniformUrl = this.getClass().getResource("images/ColorUniformButton.gif");
	URL colorElevUrl = this.getClass().getResource("images/ColorElevButton.gif");
	URL colorSourceUrl = this.getClass().getResource("images/ColorSourceButton.gif");
	URL colorYearUrl = this.getClass().getResource("images/ColorYearButton.gif");
	URL filterSourceUrl = this.getClass().getResource("images/FilterSourceButton.gif");
	URL filterYearUrl = this.getClass().getResource("images/FilterYearButton.gif");
	URL filterLabelUrl = this.getClass().getResource("images/FilterLabel.gif");

	_fileOpenIcon = new ImageIcon(bathUrl);
	_propOpenIcon = new ImageIcon(propUrl);
	_networkOpenIcon = new ImageIcon(netOpenUrl);
	_networkSaveIcon = new ImageIcon(netSaveUrl);
	_cursorIcon = new ImageIcon(arrowUrl);
	_insertIcon = new ImageIcon(insertPointUrl);
	_moveIcon = new ImageIcon(movePointUrl);
	_addIcon = new ImageIcon(addPointUrl);
	_deleteIcon = new ImageIcon(deletePointUrl);
	_addXsectIcon = new ImageIcon(addXsectUrl);
	_removeXsectIcon = new ImageIcon(removeXsectUrl);
	_moveXsectIcon = new ImageIcon(moveXsectUrl);

	_viewIcon = new ImageIcon(viewXsectUrl);

	_colorUniformIcon = new ImageIcon(colorUniformUrl);
	_colorElevIcon = new ImageIcon(colorElevUrl);
	_colorSourceIcon = new ImageIcon(colorSourceUrl);
	_colorYearIcon = new ImageIcon(colorYearUrl);
	_filterSourceIcon = new ImageIcon(filterSourceUrl);
	_filterYearIcon = new ImageIcon(filterYearUrl);
	_filterLabelIcon = new ImageIcon(filterLabelUrl);

	URL networkCalculateUrl = this.getClass().getResource("images/NetworkCalculateButton.gif");
	URL cursorIconSelectedUrl = this.getClass().getResource("images/ArrowButtonSelected.jpg");
	URL insertPointButtonSelectedUrl = this.getClass().getResource("images/InsertPointButtonSelected.jpg");
	URL moveIconSelectedUrl = this.getClass().getResource("images/MovePointButtonSelected.jpg");
	URL addIconSelectedUrl = this.getClass().getResource("images/AddPointButtonSelected.jpg");
	URL deleteIconSelectedUrl = this.getClass().getResource("images/DeletePointButtonSelected.jpg");
	URL addXsectIconSelectedUrl = this.getClass().getResource("images/AddXsectButtonSelected.jpg");
	URL removeXsectIconSelectedUrl = this.getClass().getResource("images/RemoveXsectButtonSelected.jpg");
	URL moveXsectIconSelectedUrl = this.getClass().getResource("images/MoveXsectButtonSelected.jpg");
	URL colorUniformIconSelectedUrl = this.getClass().getResource("images/ColorUniformButtonSelected.gif");
	URL colorElevIconSelectedUrl = this.getClass().getResource("images/ColorElevButtonSelected.gif");
	URL colorSourceIconSelectedUrl = this.getClass().getResource("images/ColorSourceButtonSelected.gif");
	URL colorYearIconSelectedUrl = this.getClass().getResource("images/ColorYearButtonSelected.gif");
	URL zoomBoxIconUrl = this.getClass().getResource("images/ZoomBoxButton.jpg");
	URL zoomBoxIconSelectedUrl = this.getClass().getResource("images/ZoomBoxButtonSelected.jpg");
	URL zoomFitIconUrl = this.getClass().getResource("images/ZoomFitButton.jpg");
	URL zoomFitIconRolloverUrl = this.getClass().getResource("images/ZoomFitButtonRollover.jpg");

	_networkCalculateIcon = new ImageIcon(networkCalculateUrl);
	_cursorIconSelected = new ImageIcon(cursorIconSelectedUrl);
	_insertIconSelected = new ImageIcon(insertPointButtonSelectedUrl);
	_moveIconSelected = new ImageIcon(moveIconSelectedUrl);
	_addIconSelected = new ImageIcon(addIconSelectedUrl);
	_deleteIconSelected = new ImageIcon(deleteIconSelectedUrl);
	_addXsectIconSelected = new ImageIcon(addXsectIconSelectedUrl);
	_removeXsectIconSelected = new ImageIcon(removeXsectIconSelectedUrl);
	_moveXsectIconSelected = new ImageIcon(moveXsectIconSelectedUrl);
	_colorUniformIconSelected = new ImageIcon(colorUniformIconSelectedUrl);
	_colorElevIconSelected = new ImageIcon(colorElevIconSelectedUrl);
	_colorSourceIconSelected = new ImageIcon(colorSourceIconSelectedUrl);
	_colorYearIconSelected = new ImageIcon(colorYearIconSelectedUrl);
	_zoomBoxIcon = new ImageIcon(zoomBoxIconUrl);
	_zoomBoxIconSelected = new ImageIcon(zoomBoxIconSelectedUrl);
	_zoomFitIcon = new ImageIcon(zoomFitIconUrl);
	_zoomFitIconRollover = new ImageIcon(zoomFitIconRolloverUrl);

	_fileOpenButton    = new JButton(_fileOpenIcon);
	_propOpenButton    = new JButton(_propOpenIcon);
	_networkOpenButton    = new JButton(_networkOpenIcon);
	_networkSaveButton    = new JButton(_networkSaveIcon);
	_networkCalculateButton = new JButton(_networkCalculateIcon);
	_colorUniformButton = new JRadioButton(_colorUniformIcon);
	_colorByElevButton = new JRadioButton(_colorElevIcon,true);
	_colorBySourceButton = new JRadioButton(_colorSourceIcon);
	_colorByYearButton = new JRadioButton(_colorYearIcon);
	_filterSourceButton = new JButton(_filterSourceIcon);
	_filterYearButton = new JButton(_filterYearIcon);
	_filterLabel = new JLabel(_filterLabelIcon);

	_cursorButton    = new JRadioButton(_cursorIcon);
	_insertButton   = new JRadioButton(_insertIcon);
	_moveButton     = new JRadioButton(_moveIcon);
	_addButton      = new JRadioButton(_addIcon);
	_deleteButton   = new JRadioButton(_deleteIcon);
	_addXsectButton = new JRadioButton(_addXsectIcon);
	_removeXsectButton = new JRadioButton(_removeXsectIcon);
	_moveXsectButton = new JRadioButton(_moveXsectIcon);
	_viewXsectButton = new JRadioButton(_viewIcon);
	_zoomBoxButton = new JRadioButton(_zoomBoxIcon);
	_zoomFitButton = new JButton(_zoomFitIcon);
    }

  /**
   * makes menus and buttons; creates and registers listeners
   */
  public void createGui() {
    //editing buttons
      ////    JPanel btnPanel = new JPanel(true);
      JToolBar btnPanel = new JToolBar();
      btnPanel.setFloatable(false);

    _fileOpenButton.setPreferredSize(_wideIconSize);
    _propOpenButton.setPreferredSize(_wideIconSize);
    _networkOpenButton.setPreferredSize(_wideIconSize);
    _networkSaveButton.setPreferredSize(_wideIconSize);
    _cursorButton.setPreferredSize(_iconSize);
    _colorUniformButton.setPreferredSize(_colorByIconSize);
    _colorByElevButton.setPreferredSize(_colorByIconSize);
    _colorBySourceButton.setPreferredSize(_colorByIconSize);
    _colorByYearButton.setPreferredSize(_colorByIconSize);
    _filterSourceButton.setPreferredSize(_colorByIconSize);
    _filterYearButton.setPreferredSize(_colorByIconSize);
    _moveButton.setPreferredSize(_iconSize);
    _insertButton.setPreferredSize(_iconSize);
    _addButton.setPreferredSize(_iconSize);
    _deleteButton.setPreferredSize(_iconSize);
    _addXsectButton.setPreferredSize(_iconSize);
    _removeXsectButton.setPreferredSize(_iconSize);
    _moveXsectButton.setPreferredSize(_iconSize);
    _viewXsectButton.setPreferredSize(_iconSize);
    _networkCalculateButton.setPreferredSize(_iconSize);
    _zoomBoxButton.setPreferredSize(_iconSize);
    _zoomFitButton.setPreferredSize(_iconSize);

    _cursorButton.setSelectedIcon(_cursorIconSelected);
    _colorUniformButton.setSelectedIcon(_colorUniformIconSelected);
    _colorByElevButton.setSelectedIcon(_colorElevIconSelected);
    _colorBySourceButton.setSelectedIcon(_colorSourceIconSelected);
    _colorByYearButton.setSelectedIcon(_colorYearIconSelected);
    _moveButton.setSelectedIcon(_moveIconSelected);
    _insertButton.setSelectedIcon(_insertIconSelected);
    _addButton.setSelectedIcon(_addIconSelected);
    _deleteButton.setSelectedIcon(_deleteIconSelected);
    _addXsectButton.setSelectedIcon(_addXsectIconSelected);
    _removeXsectButton.setSelectedIcon(_removeXsectIconSelected);
    _moveXsectButton.setSelectedIcon(_moveXsectIconSelected);
    _zoomBoxButton.setSelectedIcon(_zoomBoxIconSelected);

    //rollover doesn't work?
//      _zoomFitButton.setRolloverEnabled(true);
//      _zoomFitButton.setRolloverIcon(_zoomFitIconRollover);
    //_zoomFitButton.setRolloverSelectedIcon(_zoomFitIconRollover);

    _fileOpenButton.setBorder(_raisedBevel);
    _propOpenButton.setBorder(_raisedBevel);
    _networkOpenButton.setBorder(_raisedBevel);
    _networkSaveButton.setBorder(_raisedBevel);
    _cursorButton.setBorder(_raisedBevel);
    _networkCalculateButton.setBorder(_raisedBevel);

    btnPanel.setLayout(new FlowLayout());
    btnPanel.add(_fileOpenButton);
    btnPanel.add(_propOpenButton);
    btnPanel.add(_networkOpenButton);
    btnPanel.add(_networkSaveButton);

    ButtonGroup colorByGroup = new ButtonGroup();
    colorByGroup.add(_colorUniformButton);
    colorByGroup.add(_colorByElevButton);
    colorByGroup.add(_colorBySourceButton);
    colorByGroup.add(_colorByYearButton);
    //color panel
    JPanel colorByPanel = new JPanel(new GridLayout(2,2));
    colorByPanel.setBorder(_raisedBevel);
    colorByPanel.add(_colorUniformButton);
    colorByPanel.add(_colorByElevButton);
    colorByPanel.add(_colorBySourceButton);
    colorByPanel.add(_colorByYearButton);
    //filter panel
    JPanel filterPanel = new JPanel(new GridLayout(2,1));
    JPanel filterLabelPanel = new JPanel(new GridLayout(1,1));
    JPanel filterButtonPanel = new JPanel(new GridLayout(1,2));
    filterLabelPanel.setForeground(Color.white);
    filterLabelPanel.add(_filterLabel);
    filterButtonPanel.add(_filterSourceButton);
    filterButtonPanel.add(_filterYearButton);
    filterPanel.add(filterLabelPanel);
    filterPanel.add(filterButtonPanel);
    filterPanel.setBorder(_raisedBevel);
    btnPanel.add(filterPanel);
    
    btnPanel.add(colorByPanel);
    btnPanel.add(_cursorButton);
    btnPanel.add(_moveButton);
    btnPanel.add(_insertButton);
    btnPanel.add(_addButton);
    btnPanel.add(_deleteButton);
    btnPanel.add(_addXsectButton);
    btnPanel.add(_moveXsectButton);
    btnPanel.add(_removeXsectButton);
    _centerlineEditButtonGroup = new ButtonGroup();
    _centerlineEditButtonGroup.add(_cursorButton);
    _centerlineEditButtonGroup.add(_moveButton);
    _centerlineEditButtonGroup.add(_insertButton);
    _centerlineEditButtonGroup.add(_addButton);
    _centerlineEditButtonGroup.add(_deleteButton);
    _centerlineEditButtonGroup.add(_addXsectButton);
    _centerlineEditButtonGroup.add(_moveXsectButton);
    _centerlineEditButtonGroup.add(_removeXsectButton);
    //    _centerlineEditButtonGroup.add(_invisibleRadioButton);
    btnPanel.add(_viewXsectButton);
    btnPanel.add(_zoomBoxButton);
    btnPanel.add(_zoomFitButton);
    btnPanel.add(_networkCalculateButton);

    _fileOpenButton.setToolTipText("open bathymetry file");
    _propOpenButton.setToolTipText("open properties file");
    _networkOpenButton.setToolTipText("open network file");
    _networkSaveButton.setToolTipText("save network file");
    _cursorButton.setToolTipText("select centerline/xs");
    _colorUniformButton.setToolTipText("Color bathymetry uniformly");
    _colorByElevButton.setToolTipText("Color bathymetry by elevation");
    _colorBySourceButton.setToolTipText("Color bathymetry by source");
    _colorByYearButton.setToolTipText("Color bathymetry by year");
    _filterSourceButton.setToolTipText("Filter bathymetry by source");
    _filterYearButton.setToolTipText("Filter bathymetry by year");
    _moveButton.setToolTipText("move centerline point ");
    _addButton.setToolTipText("add centerline point ");
    _insertButton.setToolTipText("insert centerline point ");
    _deleteButton.setToolTipText("delete centerline point ");
    _addXsectButton.setToolTipText("add cross-section line ");
    _removeXsectButton.setToolTipText("remove cross-section line ");
    _moveXsectButton.setToolTipText("move cross-section line ");
    _zoomBoxButton.setToolTipText("draw a box for zooming ");
    _zoomFitButton.setToolTipText("fit all data in window");
    _viewXsectButton.setToolTipText("view cross-section ");
    _networkCalculateButton.setToolTipText("Calculate Network");

//      _infoPanel = new JToolBar();
//      _infoPanel.setLayout(new GridLayout(3,4));
//      _infoPanel.setFloatable(true);
    _infoPanel = new JPanel();
    _infoPanel.setLayout(new GridLayout(3,4));
    ////_infoPanel.setOpaque(true);
    _infoPanel.add(_centerlineLabel);
    _infoPanel.add(_xsectLabel);
    _infoPanel.add(_mouseXLabel);
    _infoPanel.add(_mouseYLabel);
    _infoPanel.add(_areaLabel);
    _infoPanel.add(_wetPLabel);
    _infoPanel.add(_widthLabel);
    _infoPanel.add(_hydraulicDepthLabel);
    _infoPanel.add(_bathymetryFileLabel);
    _infoPanel.add(_networkFileLabel);
    _infoPanel.add(_landmarkFileLabel);
    _infoPanel.add(_propertiesFileLabel);
    
    menubar = new JMenuBar();
    this.setJMenuBar(menubar);

    //  add Canvas to ScrollPane to CardLayout to Frame and set its listeners;
    _p.setLayout(_cl);
    
        _sp1 = new JScrollPane(_canvas1, JScrollPane.VERTICAL_SCROLLBAR_ALWAYS,
    			   JScrollPane.HORIZONTAL_SCROLLBAR_ALWAYS);
       _sp1.setPreferredSize(new Dimension(_initialWidth,_initialHeight));
       //IMPORTANT!  SIMPLE_SCROLL_MODE or BACKINGSTORE_SCROLL_MODE
       //must be used, or redrawing will be insufficient when stuff comes
       //into viewport from outside viewport.  Backingstore uses extra memory,
       //so if an outOfMemoryError is thrown when trying to zoom in too far,
       //it won't be able to zoom back out.  Simple is slower, but uses less mem.
       _sp1.getViewport().setScrollMode(JViewport.SIMPLE_SCROLL_MODE);

//      _panelObjects.put("Plan View ScrollPane", _sp1);
    
      _p.add("Plan View ScrollPane", _sp1);
    
    _ni = new NetworkInteractor(this, _canvas1, _app);
    //    ZoomInteractor zi = new ZoomInteractor(_canvas1);
    //_canvas1.addMouseListener(zi);
    //_canvas1.addMouseMotionListener(zi);

      _canvas1.addMouseListener(_ni);
      _canvas1.addMouseMotionListener(_ni);


    //is this necessary?????????????????????????????????/
          JPanel jp = new JPanel();
          jp.setOpaque(true);
          setContentPane(jp);

	//    _legendPanel = new JPanel();
    _legendPanel = new JToolBar();
    _legendPanel.setFloatable(false);
    jp.setLayout(new BorderLayout());
    jp.add("Center", _p);
    jp.add("North", btnPanel);
    jp.add("East", _legendPanel);
    jp.add("South", _infoPanel);
    
    /*
     * File menu
     */
    cfFile = new JMenu("File");
    cfFile.add(fOpen         = new JMenuItem("Open"));
    cfFile.add(fClose        = new JMenuItem("Close"));
    cfFile.addSeparator();
    cfFile.add(fSave         = new JMenuItem("Save"));
    cfFile.add(fSaveAs       = new JMenuItem("Save As..."));
    cfFile.addSeparator();
    //   cfFile.add(fMerge        = new JMenuItem("Merge"));
    //cfFile.add(fExtract      = new JMenuItem("Extract"));
    //   cfFile.addSeparator();
    //   cfFile.add(fPrintPreview = new JMenuItem("Print Preview"));
    //   cfFile.add(fPrint        = new JMenuItem("Print..."));
    //   cfFile.add(fPrintSetup   = new JMenuItem("Print Setup"));
    //  cfFile.addSeparator();
    cfFile.add(fExit         = new JMenuItem("Exit"));

    cfFile.setMnemonic(KeyEvent.VK_F);
    fOpen.setAccelerator
	(KeyStroke.getKeyStroke(KeyEvent.VK_O, ActionEvent.CTRL_MASK));
    fOpen.setMnemonic(KeyEvent.VK_O);
    fSave.setMnemonic(KeyEvent.VK_S);
    fSaveAs.setMnemonic(KeyEvent.VK_A);
    fExit.setMnemonic(KeyEvent.VK_X);
    fExit.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_X, ActionEvent.CTRL_MASK));

    //    cfFile.add(fExit         = new JMenuItem("Exit", KeyEvent.VK_X));
    if(_addFileMenu) menubar.add(cfFile);
    
    // create and register action listener objects for the File menu items
    FileMenu fileMenu = new FileMenu(_app);
    ActionListener fOpenListener         = fileMenu.new FOpen(this);
    ActionListener fCloseListener        = fileMenu.new FClose();
    ActionListener fSaveListener         = fileMenu.new FSave();
    ActionListener fSaveAsListener       = fileMenu.new FSaveAs(this);
    //  ActionListener fMergeListener        = fileMenu.new FMerge();
    //ActionListener fExtractListener      = fileMenu.new FExtract();
    ActionListener fPrintPreviewListener = fileMenu.new FPrintPreview();
    ActionListener fPrintListener        = fileMenu.new FPrint();
    ActionListener fPrintSetupListener   = fileMenu.new FPrintSetup();
    EventListener  fExitListener          = fileMenu.new FExit(this);
    
    addWindowListener((WindowListener)fExitListener);

    //  fNew.addActionListener(fNewListener);
    fOpen.addActionListener(fOpenListener);
    fClose.addActionListener(fCloseListener);
    fSave.addActionListener(fSaveListener);
    fSaveAs.addActionListener(fSaveAsListener);
    //   fMerge.addActionListener(fMergeListener);
    //   fExtract.addActionListener(fExtractListener);
    //   fPrintPreview.addActionListener(fPrintPreviewListener);
    //   fPrint.addActionListener(fPrintListener);
    //   fPrintSetup.addActionListener(fPrintSetupListener);
    fExit.addActionListener((ActionListener)fExitListener);

    _fileOpenButton.addActionListener(fOpenListener);


    /*
     * Properties menu
     */
    cfProperties = new JMenu("Properties");
    cfProperties.add(pLoad = new JMenuItem("Load"));
    cfProperties.add(pSave = new JMenuItem("Save"));
    cfProperties.add(pSaveAs = new JMenuItem("SaveAs"));

    cfProperties.setMnemonic(KeyEvent.VK_P);
    pLoad.setMnemonic(KeyEvent.VK_L);
    pSave.setMnemonic(KeyEvent.VK_S);
    pSaveAs.setMnemonic(KeyEvent.VK_A);

    PropertiesMenu propertiesMenu = new PropertiesMenu(_app);
    ActionListener pLoadListener = propertiesMenu.new PLoad(this);
    ActionListener pSaveListener = propertiesMenu.new PSave(this);
    ActionListener pSaveAsListener = propertiesMenu.new PSaveAs(this);
    pLoad.addActionListener(pLoadListener);
    _propOpenButton.addActionListener(pLoadListener);
    pSave.addActionListener(pSaveListener);
    pSaveAs.addActionListener(pSaveAsListener);
    if(_addPropertiesMenu) menubar.add(cfProperties);

    /*
     * Display menu
     */
    cfDisplay = new JMenu("Display");
    cfDisplay.setMnemonic(KeyEvent.VK_D);
    cfDisplay.add(dParameters = new JMenuItem("Parameters"));
    dParameters.setMnemonic(KeyEvent.VK_A);
    cfDisplay.addSeparator();
    cfDisplay.add(dSource     = new JMenuItem("Filter By Source"));
    cfDisplay.add(dYear       = new JMenuItem("Filter By Year"));
    cfDisplay.addSeparator();

//      JMenu cb = new JMenu("Color By");
//      _colorByButtonGroup = new ButtonGroup();

//      _colorByButtonGroup.add
//  	(dColorUniformRadioButton = new JRadioButtonMenuItem("Uniform"));
//      _colorByButtonGroup.add
//  	(dColorByDepthRadioButton = new JRadioButtonMenuItem("Depth",true));
//      _colorByButtonGroup.add
//  	(dColorBySourceRadioButton = new JRadioButtonMenuItem("Source"));
//      _colorByButtonGroup.add
//  	(dColorByYearRadioButton = new JRadioButtonMenuItem("Year"));
//      cb.add(dColorUniformRadioButton);
//      cb.add(dColorByDepthRadioButton);
//      cb.add(dColorBySourceRadioButton);
//      cb.add(dColorByYearRadioButton);
//          cfDisplay.add(cb);
    

        JMenu fb = new JMenu("Fit By");
    _fitByButtonGroup = new ButtonGroup();
    
    _fitByButtonGroup.add
	(dFitByBathymetryMenuItem = new JRadioButtonMenuItem("Bathymetry",true));
    _fitByButtonGroup.add
	(dFitByNetworkMenuItem = new JRadioButtonMenuItem("Network"));
    _fitByButtonGroup.add
	(dFitByLandmarkMenuItem = new JRadioButtonMenuItem("Landmark"));
    fb.add(dFitByBathymetryMenuItem);
    fb.add(dFitByNetworkMenuItem);
    fb.add(dFitByLandmarkMenuItem);
    cfDisplay.add(fb);
    
    //  cfDisplay.add(dErased          = new MenuItem("Erased"));
    //  cfDisplay.addSeparator();
    cfDisplay.add(dDigitalLineGraph  = new JMenuItem
	("Digital Line Graph(Channel Outline)"));
    cfDisplay.add(dLandmarks       = new JMenuItem("Landmarks"));
    cfDisplay.add(cLandmarks       = new JMenuItem("Clear Landmarks"));
    //   cfDisplay.addSeparator();
    //   cfDisplay.add(dCreateLandmark  = new JMenuItem("Create Landmark"));
    //   cfDisplay.add(dEditLandmark    = new JMenuItem("Edit Landmark"));
    //   cfDisplay.add(dMoveLandmark    = new JMenuItem("Move Landmark"));
    //   cfDisplay.add(dDeleteLandmark  = new JMenuItem("Delete Landmark"));
    //   cfDisplay.addSeparator();
    //   cfDisplay.add(dCopyToClipboard = new JMenuItem("Copy to Clipboard"));
    if(_addDisplayMenu) menubar.add(cfDisplay);

    DisplayMenu displayMenu = new DisplayMenu(_app, _net);
    ActionListener dParametersListener  = displayMenu.new DParameters(this);
    ActionListener dDigitalLineGraphListener = displayMenu.new DDigitalLineGraph(this);
    _dLandmarksListener   = displayMenu.new DLandmarks(this);
    ActionListener dSourceListener = displayMenu.new DSource(this);
    ActionListener dYearListener  = displayMenu.new DYear(this);
    ItemListener dColorUniformListener = displayMenu.new DColorUniform(this);
    ItemListener dColorByElevListener = displayMenu.new DColorByElev(this);
    ItemListener dColorBySourceListener = displayMenu.new DColorBySource(this);
    ItemListener dColorByYearListener  = displayMenu.new DColorByYear(this);
    EventListener dFitByBathymetryListener = displayMenu.new DFitByBathymetry(this);
    EventListener dFitByNetworkListener = displayMenu.new DFitByNetwork(this);
    EventListener dFitByLandmarkListener = displayMenu.new DFitByLandmark(this);
    ActionListener dClearLandmarksListener = displayMenu.new DClearLandmark(this);
    
    dParameters.addActionListener(dParametersListener);
    dDigitalLineGraph.addActionListener(dDigitalLineGraphListener);
    dLandmarks.addActionListener(_dLandmarksListener);
    cLandmarks.addActionListener(dClearLandmarksListener);
    dSource.addActionListener(dSourceListener);
    dYear.addActionListener(dYearListener);

    _filterSourceButton.addActionListener(dSourceListener);
    _filterYearButton.addActionListener(dYearListener);

    _colorUniformButton.addItemListener(dColorUniformListener);
    _colorByElevButton.addItemListener(dColorByElevListener);
    _colorBySourceButton.addItemListener(dColorBySourceListener);
    _colorByYearButton.addItemListener(dColorByYearListener);

//      dColorUniformRadioButton.addItemListener((ItemListener)dColorUniformListener);
//      dColorByDepthRadioButton.addItemListener((ItemListener)dColorByDepthListener);
//      dColorBySourceRadioButton.addItemListener((ItemListener)dColorBySourceListener);
//      dColorByYearRadioButton.addItemListener((ItemListener)dColorByYearListener);

    dFitByBathymetryMenuItem.addItemListener((ItemListener)dFitByBathymetryListener);
    dFitByNetworkMenuItem.addItemListener((ItemListener)dFitByNetworkListener);
    dFitByLandmarkMenuItem.addItemListener((ItemListener)dFitByLandmarkListener);
    /*
     * Network menu
     */
    cfNetwork = new JMenu("Network");
    cfNetwork.add(nRead      = new JMenuItem("Read"));
    cfNetwork.add(nSave      = new JMenuItem("Save"));
    cfNetwork.add(nSaveAs    = new JMenuItem("Save As"));

    cfNetwork.setMnemonic(KeyEvent.VK_N);
    nRead.setAccelerator
	(KeyStroke.getKeyStroke(KeyEvent.VK_N, ActionEvent.CTRL_MASK));
    nRead.setMnemonic(KeyEvent.VK_R);
    nSave.setAccelerator
	(KeyStroke.getKeyStroke(KeyEvent.VK_S, ActionEvent.CTRL_MASK));
    nSave.setMnemonic(KeyEvent.VK_S);
    nSaveAs.setAccelerator
	(KeyStroke.getKeyStroke(KeyEvent.VK_A, ActionEvent.CTRL_MASK));
    nSaveAs.setMnemonic(KeyEvent.VK_A);

    cfNetwork.add(nClearNetwork = new JMenuItem("Clear Network"));
    cfNetwork.add(nExportToSEFormat = new JMenuItem("Export to Station/Elevation format"));
    cfNetwork.add(nExportTo3DFormat = new JMenuItem("Export to 3D format"));
    cfNetwork.add(nExportOptions = new JMenu("Network export options")); 
    cfNetwork.addSeparator();
    //   cfNetwork.add(nList      = new JMenuItem("List"));
    //   cfNetwork.add(nSummary   = new JMenuItem("Summary"));
    //   cfNetwork.addSeparator();
    cfNetwork.add(nCalculate = new JMenuItem("Calculate"));
    nCalculate.setMnemonic(KeyEvent.VK_C);
    if(_addNetworkMenu) menubar.add(cfNetwork);

    nExportOptions.add(noChannelLengthsOnly = new 
	JCheckBoxMenuItem("for Station/Elevation format, only export channel lengths"));
    CsdpFunctions.setChannelLengthsOnly(false);

    NetworkMenu networkMenu = new NetworkMenu(_app);
    ActionListener nReadListener          = networkMenu.new NOpen(this);
    _nSaveListener          = networkMenu.new NSave(this);
    _nSaveAsListener        = networkMenu.new NSaveAs(this);
    ActionListener nClearNetworkListener = networkMenu.new NClearNetwork(this);
    ActionListener nExportToSEFormatListener = networkMenu.new NExportToSEFormat(this);
    ActionListener nExportTo3DFormatListener = networkMenu.new NExportTo3DFormat(this);
    EventListener noChannelLengthsOnlyListener = 
	networkMenu.new NChannelLengthsOnly();
    ////ActionListener nListListener          = networkMenu.new NList();
    ////ActionListener nSummaryListener       = networkMenu.new NSummary();
    ActionListener nCalculateListener     = networkMenu.new NCalculate(this);
    
    nRead.addActionListener(nReadListener);
    nSave.addActionListener(_nSaveListener);
    nSaveAs.addActionListener(_nSaveAsListener);
    nClearNetwork.addActionListener(nClearNetworkListener);
    nExportToSEFormat.addActionListener(nExportToSEFormatListener);
    nExportTo3DFormat.addActionListener(nExportTo3DFormatListener);
    noChannelLengthsOnly.addItemListener((ItemListener)noChannelLengthsOnlyListener);
    ////nList.addActionListener(nListListener);
    ////nSummary.addActionListener(nSummaryListener);
    nCalculate.addActionListener(nCalculateListener);
    _networkOpenButton.addActionListener(nReadListener);
    _networkSaveButton.addActionListener(_nSaveListener);
    _networkCalculateButton.addActionListener(nCalculateListener);
    /*
     * Centerline menu
     */
    cfCenterline = new JMenu("Centerline");
    cfCenterline.add(cCreate    = new JMenuItem("Create"));
    cfCenterline.add(cDSMCreate = new JMenuItem("Create DSM Chan"));
    cfCenterline.add(cCursor    = new JMenuItem("Done Editing"));
    cfCenterline.add(cRemove    = new JMenuItem("Remove Centerline"));

    ////    cfCenterline.add(cRename    = new JMenuItem("Rename"));
//      cfCenterline.addSeparator();
//      cfCenterline.add(_cMovePointMenuItem   = new JCheckBoxMenuItem("Move Point"));
//      cfCenterline.add(_cInsertPointMenuItem   = new JCheckBoxMenuItem("Insert Point"));
//      cfCenterline.add(_cAddPointMenuItem    = new JCheckBoxMenuItem("Add Point"));
//      cfCenterline.add(_cDeletePointMenuItem = new JCheckBoxMenuItem("Delete Point"));
//      cfCenterline.add(_cAddXsectMenuItem = new JCheckBoxMenuItem("Add Xsect"));
//      cfCenterline.add(_cMoveXsectMenuItem   = new JCheckBoxMenuItem("Move Xsect"));  
//      cfCenterline.add(_cRemoveXsectMenuItem = new JCheckBoxMenuItem("Remove Xsect"));

    //cfCenterline.add(cRestore   = new JMenuItem("Restore"));
    //cfCenterline.add(cKeep      = new JMenuItem("Keep"));
    //  cfCenterline.addSeparator();
    //cfCenterline.add(cSplit     = new JMenuItem("Split"));
    //cfCenterline.add(cJoin      = new JMenuItem("Join"));
    //cfCenterline.addSeparator();
    //cfCenterline.add(cView      = new JMenuItem("View"));
    //cfCenterline.add(cInfo      = new JMenuItem("Info"));
    //cfCenterline.add(cList      = new JMenuItem("List"));
    //cfCenterline.add(cSummary   = new JMenuItem("Summary"));

    cfCenterline.setMnemonic(KeyEvent.VK_C);

    if(_addCenterlineMenu) menubar.add(cfCenterline);
    
    _centerlineMenu = new CenterlineMenu(this);
    ActionListener cCursorListener      = _centerlineMenu.new CCursor(this);
    ActionListener cCreateListener      = _centerlineMenu.new CCreate(_app, this);
    ActionListener cDSMCreateListener   = _centerlineMenu.new CDSMCreate
      (_DSMChannels, _app, this);
    ActionListener cRemoveListener      = _centerlineMenu.new CRemove(this);
    ////    ActionListener cRenameListener      = _centerlineMenu.new CRename();
    // ActionListener cRestoreListener     = _centerlineMenu.new CRestore();
    //  ActionListener cKeepListener        = _centerlineMenu.new CKeep();
    ActionListener cMovePointListener = _centerlineMenu.new CMovePoint();
    ActionListener cInsertPointListener = _centerlineMenu.new CInsertPoint();
    ActionListener cAddPointListener = _centerlineMenu.new CAddPoint();
    ActionListener cDeletePointListener = _centerlineMenu.new CDeletePoint();
    ActionListener cAddXsectListener = _centerlineMenu.new CAddXsect();
    ActionListener cRemoveXsectListener = _centerlineMenu.new CRemoveXsect();
    ActionListener cMoveXsectListener = _centerlineMenu.new CMoveXsect();

    cCursor.addActionListener(cCursorListener);
    cCreate.addActionListener(cCreateListener);
    cDSMCreate.addActionListener(cDSMCreateListener);
    cRemove.addActionListener(cRemoveListener);
    ////  cRename.addActionListener(cRenameListener);
    //  cRestore.addActionListener(cRestoreListener);
    //cKeep.addActionListener(cKeepListener);

    _cursorButton.addActionListener(cCursorListener);
    _moveButton.addActionListener(cMovePointListener);
    _insertButton.addActionListener(cInsertPointListener);
    _deleteButton.addActionListener(cDeletePointListener);
    _addXsectButton.addActionListener(cAddXsectListener);
    _removeXsectButton.addActionListener(cRemoveXsectListener);
    _moveXsectButton.addActionListener(cMoveXsectListener);
    //_restoreButton.addActionListener(cRestoreListener);
    //_keepButton.addActionListener(cRestoreListener);
    
    /*
     * Xsect Menu
     */
    cfXsect = new JMenu("Xsect");
    //  cfXsect.add(xAutoGen = new JMenuItem("Auto Gen"));
    //cfXsect.addSeparator();
    //  cfXsect.add(xCreate  = new JMenuItem("Create"));
    //  cfXsect.add(xPosition = new JMenuItem("Position"));
    //cfXsect.addSeparator();
    cfXsect.add(xView    = new JMenuItem("View"));
    cfXsect.add(xAdjustLength = new JMenuItem("AdjustLength"));
    //   cfXsect.add(xInfo    = new JMenuItem("Info"));
    //   cfXsect.add(xSummary = new JMenuItem("Summary"));

    cfXsect.setMnemonic(KeyEvent.VK_S);
    xView.setAccelerator
	(KeyStroke.getKeyStroke(KeyEvent.VK_V, ActionEvent.CTRL_MASK));
    xView.setMnemonic(KeyEvent.VK_V);

    if(_addXsectMenu) menubar.add(cfXsect);
    
    // create and register action listener objects for the Xsect menu items
    _xsectMenu = new XsectMenu(_app, this, _ni);
    //   ActionListener xCreateListener   = _xsectMenu.new XCreate();
    //   ActionListener xRemoveListener   = _xsectMenu.new XRemove();
    //   ActionListener xMoveListener     = _xsectMenu.new XMove();
    //   //  ActionListener xPositionListener = _xsectMenu.new XPosition();
    ActionListener xViewListener     = _xsectMenu.new XView();
    ActionListener xAdjustLengthListener = _xsectMenu.new XAdjustLength();
    //   ActionListener xInfoListener     = _xsectMenu.new XInfo();
    //   ActionListener xSummaryListener  = _xsectMenu.new XSummary();
    
    //   xCreate.addActionListener(xCreateListener);
    //   xRemove.addActionListener(xRemoveListener);
    //   xMove.addActionListener(xMoveListener);
    //   xPosition.addActionListener(xPositionListener);
    xView.addActionListener(xViewListener);
    xAdjustLength.addActionListener(xAdjustLengthListener);
    //   xInfo.addActionListener(xInfoListener);
    //   xSummary.addActionListener(xSummaryListener);
    _viewXsectButton.addActionListener(xViewListener);

    
    /*
     * Zoom menu
     */
    cfZoom = new JMenu("Zoom");
    cfZoom.add(zIn = new JMenuItem("In"));
    cfZoom.add(zOut = new JMenuItem("Out"));
    cfZoom.addSeparator();
    cfZoom.add(zPan = new JMenuItem("Pan"));
    cfZoom.add(zBox = new JMenuItem("Box"));
    cfZoom.addSeparator();
    cfZoom.add(zFit = new JMenuItem("Fit"));
    //    cfZoom.addSeparator();

    cfZoom.setMnemonic(KeyEvent.VK_Z);
    //    cfZoom.add(zFactor = new JMenuItem("Factor"));
    if(_addZoomMenu) menubar.add(cfZoom);
    
    // create and register action listener objects for the Zoom menu items
    ZoomMenu zoomMenu = new ZoomMenu(this);
    ActionListener zInListener     = zoomMenu.new ZIn();
    ActionListener zOutListener    = zoomMenu.new ZOut();
    ActionListener zPanListener    = zoomMenu.new ZPan();
    ActionListener zFitListener    = zoomMenu.new ZFit();
    ActionListener zBoxListener = zoomMenu.new ZBox();
    //    ActionListener zFactorListener = zoomMenu.new ZFactor();
    
    zIn.addActionListener(zInListener);
    zOut.addActionListener(zOutListener);
    zPan.addActionListener(zPanListener);
    zFit.addActionListener(zFitListener);
    zBox.addActionListener(zBoxListener);
    //    zFactor.addActionListener(zFactorListener);
    
    _zoomBoxButton.addActionListener(zBoxListener);
    _zoomFitButton.addActionListener(zFitListener);

    zIn.setEnabled(_addZoomWindowOption);
    zOut.setEnabled(_addZoomWindowOption);
    zPan.setEnabled(_addZoomWindowOption);
    zFit.setEnabled(_addZoomWindowOption);
    zBox.setEnabled(_addZoomWindowOption);
    
    /*
     * Tools menu
     */
    cfTools = new JMenu("Tools");
    //    cfTools.add(tOpenWaterOptionsMenu = new JMenu("Options"));

    cfTools.add(tCompareNetwork = new JMenuItem("Compare two network files"));
    cfTools.add(tCalcRect = new JMenuItem
		("Calculate Equivalent Rectangular cross-sections"));
    cfTools.add(tOpenWaterCalc = new JMenuItem("Open Water Area Calculations"));
    if(_addToolsMenu) menubar.add(cfTools);

    tCalcRect.setEnabled(false);

    cfTools.setMnemonic(KeyEvent.VK_T);

    //removed temporarily(?) options now displayed in dialog.
//      tOpenWaterOptionsMenu.add
//  	(oEchoTimeSeriesInput = new JCheckBoxMenuItem("Echo Time Series Input"));
//      tOpenWaterOptionsMenu.add
//  	(oEchoXsectInput = new JCheckBoxMenuItem("Echo Xsect Input"));
//      tOpenWaterOptionsMenu.add
//  	(oEchoToeDrainInput = new JCheckBoxMenuItem("Echo Toe Drain Input"));
//      tOpenWaterOptionsMenu.add
//  	(oPrintXsectResults = new JCheckBoxMenuItem("Print Xsect Results (long)"));
//      tOpenWaterOptionsMenu.add
//  	(oUseFremontWeir = new JCheckBoxMenuItem("Use Fremont Weir"));
//      tOpenWaterOptionsMenu.add
//  	(oUseToeDrainRestriction = new JCheckBoxMenuItem("Use toe drain restriction"));
//      cfTools.add(tOpenWaterOptionsMenu);

//      oEchoTimeSeriesInput.setSelected(getEchoTimeSeriesInput());
//      oEchoXsectInput.setSelected(getEchoXsectInput());
//      oEchoToeDrainInput.setSelected(getEchoToeDrainInput());
//      oPrintXsectResults.setSelected(getPrintXsectResults());
//      oUseFremontWeir.setSelected(CsdpFunctions.getUseFremontWeir());
//      oUseToeDrainRestriction.setSelected(CsdpFunctions.getUseToeDrainRestriction());
//      tOpenWaterCalc.setEnabled(_addOWACalcOption);
//      tOpenWaterOptionsMenu.setEnabled(_addOWACalcOption);

    tCalcRect.setEnabled(_addRectXSOption);

    _toolsMenu = new ToolsMenu(_app, this);
    ActionListener tCompareNetworkListener = _toolsMenu.new TCompareNetwork(this);
    ActionListener tCalcRectListener      = _toolsMenu.new TCalcRect(this);
    ActionListener tOpenWaterCalcListener = _toolsMenu.new TOpenWaterCalc(this);
    //removed temporarily(?) options now appear in dialog
//      EventListener oEchoTimeSeriesInputListener = _toolsMenu.new TEchoTimeSeriesInput();
//      EventListener oEchoXsectInputListener = _toolsMenu.new TEchoXsectInput();
//      EventListener oEchoToeDrainInputListener = _toolsMenu.new TEchoToeDrainInput();
//      EventListener oPrintXsectResultsListener = _toolsMenu.new TPrintXsectResults();
//      EventListener oUseFremontWeirListener = _toolsMenu.new TUseFremontWeir();
//      EventListener oUseToeDrainRestrictionListener = _toolsMenu.new TUseToeDrainRestriction();
//      oEchoTimeSeriesInput.addItemListener((ItemListener)oEchoTimeSeriesInputListener);
//      oEchoXsectInput.addItemListener((ItemListener)oEchoXsectInputListener);
//      oEchoToeDrainInput.addItemListener((ItemListener)oEchoToeDrainInputListener);
//      oPrintXsectResults.addItemListener((ItemListener)oPrintXsectResultsListener);
//      oUseFremontWeir.addItemListener((ItemListener)oUseFremontWeirListener);
//      oUseToeDrainRestriction.addItemListener((ItemListener)oUseToeDrainRestrictionListener);
    tCompareNetwork.addActionListener(tCompareNetworkListener);
    tCalcRect.addActionListener(tCalcRectListener);
    tOpenWaterCalc.addActionListener(tOpenWaterCalcListener);

    tCompareNetwork.setEnabled(_addCompareNetworkOption);
    
    /*
     * Window menu
     */
    //   cfWindow = new JMenu("Window");
    //    cfWindow.setMnemonic(KeyEvent.VK_W);
    //   cfWindow.add(wCascade      = new JMenuItem("Cascade"));
    //   cfWindow.add(wTile         = new JMenuItem("Tile"));
    //   cfWindow.add(wArrangeIcons = new JMenuItem("Arrange Icons"));
    //   cfWindow.add(wCloseAll     = new JMenuItem("Close All"));
    //   cfWindow.addSeparator();
    //   cfWindow.add(wRepaint      = new JMenuItem("Repaint"));
    //   // also show open windows
    //   menubar.add(cfWindow);
    
    cfHelp = new JMenu("Help");
    cfHelp.setMnemonic(KeyEvent.VK_H);
    //cfHelp.add(hContents  = new JMenuItem("Contents"));
    //cfHelp.add(hUsingHelp = new JMenuItem("Using Help"));
    //cfHelp.addSeparator();
    cfHelp.add(hAbout     = new JMenuItem("About CSDP"));
    if(_addHelpMenu){
	menubar.add(Box.createHorizontalGlue());
	menubar.add(cfHelp);
	menubar.add(Box.createHorizontalGlue());
    }
    HelpMenu helpMenu = new HelpMenu();
    ////    menubar.setHelpMenu(cfHelp);
    ActionListener hAboutListener = helpMenu.new HAbout(this);
    hAbout.addActionListener(hAboutListener);

    disableButtonsAndMenuItems();
  } //createGui
  
  /**
   * Creates new instance of canvas for plotting bathymetry data.
   */
  ///public void addPlanViewCanvas(String name){
  ///PlanViewCanvas can = new PlanViewCanvas(name);
  ///_pvCanvases.put(name, can);
  ///}

  /**
   * returns canvas with specified name
   */
  ///public PlanViewCanvas getPlanViewCanvas(String name){
  ///return (PlanViewCanvas)_pvCanvases.get(name);
  ///}

  /**
   * removes canvas from hashtable(close file)
   */
  ///public void removePlanViewCanvas(String name){
  ///Object value = _pvCanvases.remove(name);
  ///}

  ///  Hashtable _pvCanvases = new Hashtable();
  ///Hashtable _filenames  = new Hashtable();

    public float getZoomBoxFactor(){
      return _zoomBoxFactor;
    }//getZoomBoxFactor

      public float multiplyZoomBoxFactor(float factor){
  	_zoomBoxFactor *= factor;
  	return _zoomBoxFactor;
      }

  /**
   * sets value of factor used for Box zooming.
   */
  public void setZoomBoxFactor(float value){
    _zoomBoxFactor = value;
  }//setZoomBoxFactor
  
    public int getZoomBoxX(){
	return _zoomBoxX;
    }

    public int getZoomBoxY(){
	return _zoomBoxY;
    }

    public void setZoomBoxX(int value){
	_zoomBoxX = value;
    }

    public void setZoomBoxY(int value){
	_zoomBoxY = value;
    }

  /**
   * sets Network object
   */
  public void setNetwork(Network net){
    _net = net;
    _ni.setNetwork(net);
    _xsectMenu.setNetwork(net);
    _canvas1.setNetwork(net);
  }//setNetwork
  
  /**
   * returns handle to the network object.
   */
  public Network getNetwork(){
    return _net;
  }//getNetwork

  /**
   * save network file when quitting program (if user clicks "yes")
   */
  public void saveNetwork(){
    if(CsdpFunctions._networkFilename == null){
	_nSaveAsListener.actionPerformed(_nullActionEvent);
    }
    else{
	_nSaveListener.actionPerformed(_nullActionEvent);
    }
  }//saveNetwork

  /**
   * returns landmark object
   */
  public Landmark getLandmark(){
    Landmark landmark = null;
    //    if(_landmark != null) landmark = _landmark;
    //else{
      _dLandmarksListener.actionPerformed(_nullActionEvent);
      landmark = _landmark;
      //}
    return landmark;
  }//getLandmark

  /**
   * sets Landmark object
   */
public void setLandmark(Landmark landmark){
  _landmark = landmark;
  //_li.setLandmark(landmark);//for landmark interactor, if there is to be one.
  _canvas1.setLandmark(landmark);
  _centerlineMenu.setLandmark(landmark);
}//setLandmark

  /**
   * sets DSMChannels object
   */
  public void setDSMChannels(DSMChannels DSMChannels){
    _DSMChannels = DSMChannels;
    //  _canvas1.setLandmark(landmark);
  }//setDSMChannels

  /**
   * returns instance of bathymetry plot object
   */
  public BathymetryPlot getPlotObject(){
    return _plot;
  }

  /**
   * sets BathymetryPlot object
   */
  public void setPlotObject(BathymetryPlot plot){
    _plot = plot;
    _ni.setPlotter(plot);
  }//setPlotObject

//    /**
//     * add a canvas to the hashtable that stores all the panels contained in the frame
//     */
//    public void addXsectWindow(String name, XsectGraph xg){
//      _panelObjects.put(name, xg);
//      _p.add(name, (Canvas)_panelObjects.get(name));
//      _cl.next(_p);
//    }//addXsectWindow

    /**
     * called when user wants to stop editing
     */
    public void setStopEditingMode(){
      enableCenterlineEditButtons();
      setAllButtonsDefaultColor();
      setDefaultModesStates();

      if(getCenterlineSelected()){
    	_moveButton.setEnabled(true);
    	_insertButton.setEnabled(true);
    	_addButton.setEnabled(true);
    	_deleteButton.setEnabled(true);
    	_addXsectButton.setEnabled(true);
    	_removeXsectButton.setEnabled(true);
  	if(getXsectSelected()){
  	    _moveXsectButton.setEnabled(true);
  	    _viewXsectButton.setEnabled(true);
  	}else{
  	    _moveXsectButton.setEnabled(false);
  	    _viewXsectButton.setEnabled(false);
  	}
      }else{
  	_moveButton.setEnabled(false);
  	_insertButton.setEnabled(false);
  	_addButton.setEnabled(false);
  	_deleteButton.setEnabled(false);
  	_addXsectButton.setEnabled(false);
  	_removeXsectButton.setEnabled(false);
  	_moveXsectButton.setEnabled(false);
  	_viewXsectButton.setEnabled(false);

      }
    }//setStopEditingMode

      /**
       * called when user wants to add centerline points to 
       * the end of the set(after last pt)--only called when adding a centerline
       */
      public void setAddPointMode(){
	  _addButton.setSelected(true);
      }//setAddPointMode

  /**
   * called when user wants to zoom in without making a large canvas
   */
  public void setZoomBoxMode(boolean b){
    setAllButtonsDefaultColor();
        if(b == true){
          setDefaultModesStates();
          _zoomBoxMode = true;
          setCursor(CsdpFunctions._crosshairCursor);
        }else{
          setDefaultModesStates();
          _zoomBoxMode = false;
          setCursor(CsdpFunctions._defaultCursor);
        }
  }//setZoomBoxMode

    /**
     * toggles zoom mode when button is not clicked by user.  Allows another event to toggle mode.
     */
    public void pressZoomBoxButton(){
	_zoomBoxButton.doClick();
    }

  /**
   * called when user wants to zoom in without making a large canvas
   */
  public void toggleZoomBoxMode(){
    setAllButtonsDefaultColor();
      if(_zoomBoxMode == false){
  	setZoomBoxMode(true);
      }else{
  	setZoomBoxMode(false);
      }
  }//setZoomBoxMode

  /**
   * turns off all edit modes
   */
  public void turnOffEditModes(){
    setDefaultModesStates();
    enableCenterlineEditButtons();
    setCursor(CsdpFunctions._defaultCursor);
  }//turnOffEditModes

  /**
   * disable buttons and menu items which should not be enabled until bathymetry loaded
   */
  protected void disableButtonsAndMenuItems(){
      _cursorButton.setEnabled(false);
      _propOpenButton.setEnabled(false);
      _networkOpenButton.setEnabled(false);
      _networkSaveButton.setEnabled(false);
      _moveButton.setEnabled(false);
      _insertButton.setEnabled(false);
      _addButton.setEnabled(false);
      _deleteButton.setEnabled(false);
      _addXsectButton.setEnabled(false);
      _moveXsectButton.setEnabled(false);
      _removeXsectButton.setEnabled(false);
      _zoomBoxButton.setEnabled(false);
      _zoomFitButton.setEnabled(false);
      _viewXsectButton.setEnabled(false);
      _filterSourceButton.setEnabled(false);
      _filterYearButton.setEnabled(false);

//      _cMovePointMenuItem.setEnabled(false);
//      _cAddPointMenuItem.setEnabled(false); 
//      _cInsertPointMenuItem.setEnabled(false);
//      _cDeletePointMenuItem.setEnabled(false);
//      _cAddXsectMenuItem.setEnabled(false); 
//      _cRemoveXsectMenuItem.setEnabled(false);
//      _cMoveXsectMenuItem.setEnabled(false);

    fClose.setEnabled(false);
    fSave.setEnabled(false);
    fSaveAs.setEnabled(false);
    //fPrintPreview.setEnabled(false);
    //fPrint.setEnabled(false);
    //fPrintSetup.setEnabled(false);

    pLoad.setEnabled(false);
    pSaveAs.setEnabled(false);
    pSave.setEnabled(false);

    dParameters.setEnabled(false);
    dSource.setEnabled(false);
    dYear.setEnabled(false);
    //dColorBy.setEnabled(false);
    //dErased.setEnabled(false);
    dDigitalLineGraph.setEnabled(false);
    dLandmarks.setEnabled(false);
    cLandmarks.setEnabled(false);
    //dCreateLandmark.setEnabled(false);
    //dEditLandmark.setEnabled(false);
    //dMoveLandmark.setEnabled(false);
    //dDeleteLandmark.setEnabled(false);
    //dColorUniformRadioButton.setEnabled(false);
    //dColorByDepthRadioButton.setEnabled(false); 
    //dColorBySourceRadioButton, dColorByYearRadioButton;
    dFitByNetworkMenuItem.setEnabled(false);
    dFitByLandmarkMenuItem.setEnabled(false);

    nRead.setEnabled(false);
    nSave.setEnabled(false);
    nSaveAs.setEnabled(false);
    nExportToSEFormat.setEnabled(false);
    nExportTo3DFormat.setEnabled(false);
    //nList.setEnabled(false);nSummary.setEnabled(false);
    nClearNetwork.setEnabled(false);
    nCalculate.setEnabled(false);
    _networkCalculateButton.setEnabled(false);
    cCreate.setEnabled(false);
    cDSMCreate.setEnabled(false);
    ////    cRename.setEnabled(false); 

    tCalcRect.setEnabled(false);

    //    xMove.setEnabled(false);
    //xCreate.setEnabled(false);
    xView.setEnabled(false);
    xAdjustLength.setEnabled(false);
    //xInfo.setEnabled(false);xSummary.setEnabled(false);
    //    zFactor.setEnabled(false);
    zIn.setEnabled(false);
    zOut.setEnabled(false);
    zPan.setEnabled(false);
    zFit.setEnabled(false);
    zBox.setEnabled(false);
  }//disableButtonsAndMenuItems

  /**
   * enable buttons and menu items which should be enabled when bathymetry loaded
   */
  public void enableAfterBathymetry(){
    pSaveAs.setEnabled(true);
    pLoad.setEnabled(true);
    cfDisplay.setEnabled(true);
    cfNetwork.setEnabled(true);
    cfCenterline.setEnabled(true);
    cfZoom.setEnabled(true);
    _zoomBoxButton.setEnabled(true);
    _zoomFitButton.setEnabled(true);
    _propOpenButton.setEnabled(true);
    _filterSourceButton.setEnabled(true);
    _filterYearButton.setEnabled(true);
  
    ////    fClose.setEnabled(true);
    ////fSave.setEnabled(true);//not implemented
    fSaveAs.setEnabled(true);
    //fPrintPreview.setEnabled(true);
    //fPrint.setEnabled(true);
    //fPrintSetup.setEnabled(true);
    dParameters.setEnabled(true);
    dSource.setEnabled(true);
    dYear.setEnabled(true);
    //dColorBy.setEnabled(true);
    //dErased.setEnabled(true);
    dDigitalLineGraph.setEnabled(true);
    dLandmarks.setEnabled(true);
    //dCreateLandmark.setEnabled(true);
    //dEditLandmark.setEnabled(true);
    //dMoveLandmark.setEnabled(true);
    //dDeleteLandmark.setEnabled(true);
    //dColorUniformRadioButton.setEnabled(true);
    //dColorByDepthRadioButton.setEnabled(true); 
    //dColorBySourceRadioButton, dColorByYearRadioButton;
    nRead.setEnabled(true);
    cCreate.setEnabled(true);
    cDSMCreate.setEnabled(true);
    //    xMove.setEnabled(true);
    //xInfo.setEnabled(true);xSummary.setEnabled(true);
    //    zFactor.setEnabled(true);
    zIn.setEnabled(_addZoomWindowOption);
    zOut.setEnabled(_addZoomWindowOption);
    zPan.setEnabled(_addZoomWindowOption);
    zFit.setEnabled(_addZoomWindowOption);
    zBox.setEnabled(_addZoomWindowOption);

    _cursorButton.setEnabled(true);
    _networkOpenButton.setEnabled(true);
  }//enableAfterBathymetry

  /**
   * enable buttons and menu items which should be enabled after properties file
   * saved or loaded
   */
  public void enableAfterProperties(){
    pSave.setEnabled(true);
  }

  /**
   * enable buttons and menu items which should be enabled after network loaded
   */
  protected void enableAfterNetwork(){
    nSave.setEnabled(true);
    nSaveAs.setEnabled(true);
    nClearNetwork.setEnabled(true);
    _networkSaveButton.setEnabled(true);
    //nList.setEnabled(true);nSummary.setEnabled(true);
    nExportToSEFormat.setEnabled(true);
    nExportTo3DFormat.setEnabled(true);
    nCalculate.setEnabled(true);
    _networkCalculateButton.setEnabled(true);
    dFitByNetworkMenuItem.setEnabled(true);
    tCalcRect.setEnabled(true);
  }//enableAfterNetwork

    /**
     * disable buttons and menu items which should be disabled when network is cleared
     */
    protected void disableWhenNetworkCleared(){
	nSave.setEnabled(false);
	nSaveAs.setEnabled(false);
	nClearNetwork.setEnabled(false);
	_networkSaveButton.setEnabled(false);
	//nList.setEnabled(false);nSummary.setEnabled(false);
	nExportToSEFormat.setEnabled(false);
	nExportTo3DFormat.setEnabled(false);
	nCalculate.setEnabled(false);
	_networkCalculateButton.setEnabled(false);
	dFitByNetworkMenuItem.setEnabled(false);
	tCalcRect.setEnabled(false);
    }

    /**
     * Enable buttons and menu items which should be enabled when a network is loaded
     */
  public void enableWhenNetworkExists(){
    nSaveAs.setEnabled(true);
    nClearNetwork.setEnabled(true);
    nExportToSEFormat.setEnabled(true);
    nExportTo3DFormat.setEnabled(true);
    nCalculate.setEnabled(true);
    _networkCalculateButton.setEnabled(true);
    dFitByNetworkMenuItem.setEnabled(true);
    tCalcRect.setEnabled(true);
  }

  /**
   * enable buttons and menu items which should be enabled after landmark loaded
   */
  public void enableAfterLandmark(){
    dFitByLandmarkMenuItem.setEnabled(true);
    cLandmarks.setEnabled(true);
  }

    /**
     * disable buttons and menu items which should be disabled when landmard is cleared
     */
    public void disableWhenLandmarkCleared(){
	dFitByLandmarkMenuItem.setEnabled(false);
	cLandmarks.setEnabled(false);
    }

  /**
   * enable buttons and menu items which should be enabled after centerline selected
   */
  protected void enableAfterCenterlineSelected(){
      System.out.println("centerline selected!");

    _moveButton.setEnabled(true);
    _insertButton.setEnabled(true);
    _addButton.setEnabled(true);
    _deleteButton.setEnabled(true);
    _addXsectButton.setEnabled(true);
    if(getXsectSelected()){
	_removeXsectButton.setEnabled(true);
	_moveXsectButton.setEnabled(true);
	_viewXsectButton.setEnabled(true);
    }else{
	_removeXsectButton.setEnabled(false);
	_moveXsectButton.setEnabled(false);
	_viewXsectButton.setEnabled(false);
    }
//      _cMovePointMenuItem.setEnabled(true);
//      _cAddPointMenuItem.setEnabled(true); 
//      _cInsertPointMenuItem.setEnabled(true);
//      _cDeletePointMenuItem.setEnabled(true);
//      _cAddXsectMenuItem.setEnabled(true); 
//      _cRemoveXsectMenuItem.setEnabled(true);

    setCenterlineSelected(true);

    ////    cRename.setEnabled(true); 
  }//enableAfterCenterlineSelected

  /**
   * disable buttons and menu items which should not be enabled if not centerline 
   * selected
   */
  protected void disableIfNoCenterlineSelected(){
    setDefaultModesStates();
    setAllButtonsDefaultColor();
    _moveButton.setEnabled(false);
    _insertButton.setEnabled(false);
    _addButton.setEnabled(false);
    _deleteButton.setEnabled(false);
    _addXsectButton.setEnabled(false);
    _moveXsectButton.setEnabled(false);
    _removeXsectButton.setEnabled(false);
    _viewXsectButton.setEnabled(false);

//      _cMovePointMenuItem.setEnabled(false);
//      _cAddPointMenuItem.setEnabled(false); 
//      _cInsertPointMenuItem.setEnabled(false);
//      _cDeletePointMenuItem.setEnabled(false);
//      _cAddXsectMenuItem.setEnabled(false); 
//      _cRemoveXsectMenuItem.setEnabled(false);
//      _cMoveXsectMenuItem.setEnabled(false);
    cfXsect.setEnabled(false);

    setCenterlineSelected(false);
    ////cRename.setEnabled(false); 
  }//disableIfNoCenterlineSelected

  /**
   * enable buttons and menu items which should be enabled when xsect selected
   */
  protected void enableAfterXsectSelected(){
    cfXsect.setEnabled(true);
    _moveXsectButton.setEnabled(true);
//      _cMoveXsectMenuItem.setEnabled(true);
    xView.setEnabled(true);
    _removeXsectButton.setEnabled(true);
    _viewXsectButton.setEnabled(true);
    xAdjustLength.setEnabled(true);
    setXsectSelected(true);
  }//enableAfterXsectSelected

  /**
   * disable buttons and menu items which should not be enabled if no xsect selected
   */
  protected void disableIfNoXsectSelected(){
    _moveXsectButton.setEnabled(false);
//      _cMoveXsectMenuItem.setEnabled(false);

    cfXsect.setEnabled(false);
    xView.setEnabled(false);
    _viewXsectButton.setEnabled(false);
    xAdjustLength.setEnabled(false);
    setXsectSelected(false);
  }//disableIfNoXsectSelected

  /**
   * adds a legend (panel of buttons) which shows the colors being used to
   * plot bathymetry data
   */
  public void updateColorLegend(){
    int numButtons = 0;
    _legendPanel.setLayout(new GridLayout(numButtons, 1));
    String legendItemName = null;
    Color buttonColor = null;

    _legendPanel.removeAll();

    if(_plot != null && getColorUniform() == false){
      if(getColorByElev())       numButtons = getNumDepths();
      else if(getColorBySource()) numButtons = _plot._bathymetryData.getNumSources();
      else if(getColorByYear())   numButtons = _plot._bathymetryData.getNumYears();
      
      if(getColorByElev()) 
	headerButton.setText(_depthLegendTitle);
      else if(getColorBySource()) 
	headerButton.setText(_sourceLegendTitle);
      else if(getColorByYear())
	headerButton.setText(_yearLegendTitle);

      _legendPanel.add(headerButton);
      
      for(int i=0; i<=numButtons-1; i++){
	  if(getColorByElev())
	      legendItemName = _plot.getLegendItemName(i);
	  else if(getColorBySource())
	      legendItemName = _plot._bathymetryData.getSource(i);
	  else if(getColorByYear())
	      legendItemName = Integer.toString(_plot._bathymetryData.getYear(i));
	  buttonColor = _plot.getLegendItemColor(i);
	  //	_legendPanel.add(new Button(legendItemName));
	  JButton b = getButton(i);
	  b.setBorder(_raisedBevel);
	  b.setToolTipText("click to change color");
	  _legendPanel.add(b);
	  getButton(i).setText(legendItemName);
	  _legendPanel.getComponent(i+1).setBackground(buttonColor);
      }
    }//if plot isn't null
    _canvas1.redoNextPaint();
    _canvas1.repaint();
    //    validate();
  }//addDepthColorLegend

  /**
   * sets all buttons to true(inactive)
   */
  private void enableCenterlineEditButtons(){
    _moveButton.setEnabled(true);
    _addButton.setEnabled(true);
    _insertButton.setEnabled(true);
    _deleteButton.setEnabled(true);
    _addXsectButton.setEnabled(true);
    _removeXsectButton.setEnabled(true);
    _moveXsectButton.setEnabled(true);
    _zoomBoxButton.setEnabled(true);
  }//enableCenterlineEditButtons

  /**
   * set all buttons to the default color, which is white.
   */
  protected void setAllButtonsDefaultColor(){
    _moveButton.setBackground(Color.white);
    _addButton.setBackground(Color.white);
    _insertButton.setBackground(Color.white);
    _deleteButton.setBackground(Color.white);
    _addXsectButton.setBackground(Color.white);
    _removeXsectButton.setBackground(Color.white);
    _moveXsectButton.setBackground(Color.white);
    _zoomBoxButton.setBackground(Color.white);
  }//setAllButtonsDefaultColor

    public void setDefaultModesStates(){
	_cursorButton.setSelected(true);
	//////////	_centerlineEditButtonGroup.setSelected(_cursorButton,true);
    }

  /**
   * updates displayed value of centerline name/num and mouse position
   */
  public void updateInfoPanel(String centerlineName){
    _centerlineLabel.setText("Selected Centerline:  "+centerlineName);
  }//updateInfoPanel

  /**
   * updates displayed value of cross-section properties
   */
  public void updateInfoPanel(float area, float width, float wetp, float hd){
    float nf = 100.0f;
    if(area >=0.0f && width >=0.0f && wetp>=0.0f && hd>=0.0f){
	String sa = Float.toString( (((float)((int)(area*nf))) /nf));
	String sw = Float.toString( (((float)((int)(width*nf)))/nf));
	String sp = Float.toString( (((float)((int)(wetp*nf)))/nf));
      String shd = Float.toString( (((float)((int)(hd*nf)))/nf));
      _areaLabel.setText("Xsect Area:  "+sa);
      _widthLabel.setText("Top Width:  "+sw);
      _wetPLabel.setText("Wetted Perimeter:  "+sp);
      _hydraulicDepthLabel.setText("Hydraulic Depth:  "+shd);
    } else {
      _areaLabel.setText("Xsect Area:  ");
      _widthLabel.setText("Top Width:  ");
      _wetPLabel.setText("Wetted Perimeter:  ");
      _hydraulicDepthLabel.setText("Hydraulic Depth:  ");
    }
  }//updateInfoPanel

  /**
   * updates filename display
   */
  public void updateBathymetryFilename(String bfName){
    _bathymetryFileLabel.setText("Bathymetry Filename:  "+bfName);
  }//updateBathymetryFilename

  /**
   * updates filename display
   */
  public void updateNetworkFilename(String nfName){
    _networkFileLabel.setText("Network Filename:  "+nfName);
  }//updateNetworkFilename

  /**
   * updates filename display
   */
  public void updateLandmarkFilename(String lfName){
    _landmarkFileLabel.setText("Landmark Filename:  "+lfName);
  }//updateLandmarkFilename

  /**
   * updates filename display
   */
  public void updatePropertiesFilename(String pfName){
    _propertiesFileLabel.setText("Properties Filename:  "+pfName);
  }//updatePropertiesFilename

    /**
     * updates filename display
     */
    public void updateDigitalLineGraphFilename(String dlgFilename){
	_dlgFileLabel.setText("DLG Filename:  "+dlgFilename);
    }

    public void setDigitalLineGraph(DigitalLineGraph dlg){
	_dlg = dlg;
	_canvas1.setDigitalLineGraph(_dlg);
    }

  /**
   * updates displayed value of centerline name/num and mouse position
   */
  public void updateInfoPanel(int xsectNum){
    if(xsectNum >= 0) _xsectLabel.setText("Xsect:  "+xsectNum);
    else _xsectLabel.setText("Xsect:");
  }//updateInfoPanel

    /**
     * updates displayed value of centerline name/num and mouse position
     */
    public void updateInfoPanel(float mouseX, float mouseY){
	if(_plot != null){
	    String xLabel = "X coordinate (UTM):  ";
	    String yLabel = "Y coordinate (UTM):  ";

	    try{
		float[] bb = _plot.getBathymetryBoundaries();
		float minX = bb[CsdpFunctions.minXIndex];
		float minY = bb[CsdpFunctions.minYIndex];
		
		float x = CsdpFunctions.xPixelsToLength((int)mouseX, _plot._minSlope, minX);
		float y = CsdpFunctions.yPixelsToLength((int)mouseY, _plot._minSlope, minY,
							_canvas1.getBounds().height);
		
		//if not using zoom box....
		//  	    float x = CsdpFunctions.xPixelsToLength
		//  		((int)mouseX, _plot._minSlope, _plot._bathymetryData.getMinX());
		//  	    float y = CsdpFunctions.yPixelsToLength
		//  		((int)mouseY, _plot._minSlope, _plot._bathymetryData.getMinY(),
		//  		 _canvas1.getBounds().height);
		x -= _plot._centerX;
		y -= _plot._centerY;
		x = CsdpFunctions.feetToMeters(x);
		y = CsdpFunctions.feetToMeters(y);
		xLabel += x;
		yLabel += y;
		_mouseXLabel.setText(xLabel);
		_mouseYLabel.setText(yLabel);
	    }catch(java.lang.NullPointerException e){
		System.out.println("null pointer in CsdpFrame.updateInfoPanel: bathymetry probably not loaded yet.");
	    }
	}else{
	    _mouseXLabel.setText(Float.toString(mouseX));
	    _mouseYLabel.setText(Float.toString(mouseY));
	}
    }//updateInfoPanel

  public Color getColor(int index){
    if(getNumColors() == 0){
	if(DEBUG)System.out.println("setting default colors");
      setDefaultColors();
    }
    Color c = null;
    if(index < getNumColors()) c = (Color)(_colors.elementAt(index));
    else{
      while(index >= getNumColors()){
	_colors.addElement(_colors.elementAt(getNumColors()-1));
      }//while
      c = (Color)(_colors.elementAt(index));
    }//else
    return c;
  }//getColor

//   public static Color getColor(int index){
//     Color c = null;
//     if(index < getNumColors()) c = colorTable[index];
//     else c = colorTable[getNumColors()-1];
//     return c;
//   }

  public void setColor(int index, Color c){
    if(getNumColors() == 0) setDefaultColors();
    if(index < getNumColors()) _colors.setElementAt(c,index);
    else{
      _colors.addElement(c);
    }//else
  }//setColor

  public static int getNumColors(){
    return _colors.size();
  }

  protected void setDefaultColors(){
    _colors.addElement(new Color(255,  0,  0 )); // bright red
    _colors.addElement(new Color(214,130, 50 )); // dark orange 204,102,0
    _colors.addElement(new Color(255,204,102 )); // light orange
    _colors.addElement(new Color(210, 30, 30 )); // dark red
    _colors.addElement(new Color(255,255,  0 )); // dark yellow
    _colors.addElement(new Color(  0,255,  0 )); // dark green
    _colors.addElement(new Color(153,204,102 )); // light green
    _colors.addElement(new Color(102,255,255 )); // medium blue
    _colors.addElement(new Color(153,204,255 )); // light blue
    _colors.addElement(new Color(153,153,255 ));// light purple
    _colors.addElement(new Color(  0,102,204 )); // dark blue
//      _colors.addElement(11] = new Color(153,102,255));// medium purple
//      _colors.addElement(12] = new Color(119,119,119));// dark purple
//      _colors.addElement(13] = new Color(119,119,119));// light gray

  }//setDefaultColors

  public float getDepth(int index){
    float f = 0.0f;
    if(index < getNumDepths()) f = depthTable[index];
    else{
	f = depthTable[getNumDepths()-1];
    }
    return f;
  }

  public void setDepth(int index, float f){
    if(index < getNumColors()) depthTable[index] = f;
    else depthTable[getNumColors()-1] = f;
  }

    /**
     * returns number of depths in depth table.  used for coloring by depth
     */
    public int getNumDepths(){
	return _numDepths;
    }

  public boolean getFitByBathymetryOption(){
    return dFitByBathymetryMenuItem.isSelected();
  }
  public boolean getFitByNetworkOption(){
    return dFitByNetworkMenuItem.isSelected();
  }
  public boolean getFitByLandmarkOption(){
    return dFitByLandmarkMenuItem.isSelected();
  }

  /**
   * button that displays header information for plan view legend
   */
  public static JButton headerButton = new JButton("header");

  /**
   * returns button from array of buttons
   */
  public JButton getButton(int index){
    JButton b = null;
    if(index < NUM_BUTTONS) b = (JButton)(_buttons.elementAt(index));
    else{
      while(index >= NUM_BUTTONS){
	_buttons.addElement(new JButton());
	_legendButtonListener.addElement(new AdjustRGBColor(this,index));
	((JButton)_buttons.elementAt(index)).addActionListener
	  ((ActionListener)(_legendButtonListener.elementAt(index)));
	NUM_BUTTONS++;
      }//while
      b = (JButton)(_buttons.elementAt(index));
    }//else
    return b;
  }//getButton


    public PlanViewCanvas getPlanViewCanvas(){
	return _canvas1;
    }

  private Vector _legendButtonListener = new Vector();
  protected int NUM_BUTTONS = 0;
  private Vector _buttons = new Vector();
  /**
   * The component on which the graph is drawn.
   */
  PlanViewCanvas _canvas1 = new PlanViewCanvas(this);
  //PlanViewCanvas _canvas1 = PlanViewCanvas.getInstance();
    /**
     * might use this in the future so user can have >1 bathymetry file open
     */  
    CardLayout _cl = new CardLayout();
    JPanel _p = new JPanel(true);
    JScrollPane _sp1;
    //if you want to have more than one bathymetry file open
    //    Hashtable _panelObjects = new Hashtable();
    App _app;

    /**
     * zoom factor used to keep track of zooming in/out/box etc
     */
  protected float _zoomBoxFactor = 1.0f;

    /**
     * used to store the width in pixels of the canvas before next zoom
     */
    protected int _currentWidth;
    /**
     * used to store the height in pixels of the canvas before next zoom
     */
    protected int _currentHeight;

    /**
     * x coordinate of upper left corner of portion of canvas that is currently displayed
     */
    protected int _zoomBoxX = 0;

    /**
     * y coordinate of upper left corner of portion of canvas that is currently displayed
     */
    protected int _zoomBoxY = 0;

    /**
     * sets the current width, in pixels, of the canvas.  Used for box zooming
     */
    public void setCurrentWidth(int value){
	_currentWidth = value;
    }

    /**
     * sets the current height, in pixels, of the canvas.  Used for box zooming
     */
    public void setCurrentHeight(int value){
	_currentHeight = value;
    }

    /**
     * returns the current width, in pixels, of the canvas.  Used for box zooming
     */
    public int getCurrentWidth(){
	return _currentWidth;
    }

    /**
     * sets the current height, in pixels, of the canvas.  Used for box zooming
     */
    public int getCurrentHeight(){
	return _currentHeight;
    }

    /**
     * returns inital value of canvas width
     */
    public int getInitialWidth(){
	return _initialWidth;
    }

    /**
     * returns inital value of canvas height
     */
    public int getInitialHeight(){
	return _initialHeight;
    }

  /**
   * stores size of frame
   */
  protected Dimension _dim = null;
  protected Network _net;
  NetworkInteractor _ni;
  BathymetryPlot _plot;
  //display parameters

  protected XsectMenu _xsectMenu;
  protected CenterlineMenu _centerlineMenu;
    protected ToolsMenu _toolsMenu;
    
    public boolean getInsertPointMode(){
	return _insertButton.isSelected();
    }
    public boolean getMovePointMode(){
	return _moveButton.isSelected();
    }
    public boolean getAddPointMode(){
	return _addButton.isSelected();
    }
    public boolean getDeletePointMode(){
	return _deleteButton.isSelected();
    }
    public boolean getAddXsectMode(){
	return _addXsectButton.isSelected();
    }
    public boolean getRemoveXsectMode(){
	return _removeXsectButton.isSelected();
    }
    public boolean getMoveXsectMode(){
	return _moveXsectButton.isSelected();
    }
    public boolean getViewXsectMode(){
	return _viewXsectButton.isSelected();
    }
    public boolean getZoomBoxMode(){
	return _zoomBoxMode;
    }


  /**
   * if true, mouse click and drag will draw a rectangle for zooming
   */
    private boolean _zoomBoxMode = false;

    /**
     * Checks to see if color bathymetry uniform is selected
     */
    public boolean getColorUniform(){
	return _colorUniformButton.isSelected();
    }
    /**
     * Checks to see if color bathymetry by depth option is selected
     */
    public boolean getColorByElev(){
	return _colorByElevButton.isSelected();
    }
    /**
     * Checks to see if color bathymetry by source option is selected
     */
    public boolean getColorBySource(){
	return _colorBySourceButton.isSelected();
    }
    /**
     * Checks to see if color bathymetry by year option is selected
     */
    public boolean getColorByYear(){
	return _colorByYearButton.isSelected();
    }

    /**
     * true if a centerline selected.
     */
    private boolean _centerlineSelected = false;
    /**
     * true if a xsect selected.
     */
    private boolean _xsectSelected = false;

    private void setCenterlineSelected(boolean b){
	_centerlineSelected = b;
    }
    private boolean getCenterlineSelected(){
	return _centerlineSelected;
    }
    private void setXsectSelected(boolean b){
	_xsectSelected = b;
    }
    private boolean getXsectSelected(){
	return _xsectSelected;
    }

    Border _raisedBevel = BorderFactory.createRaisedBevelBorder();

    JRadioButton _zoomBoxButton;
    JButton _zoomFitButton;
    
    ImageIcon _fileOpenIcon;
    ImageIcon _networkOpenIcon;
    ImageIcon _networkSaveIcon;
    ImageIcon _cursorIcon;
    ImageIcon _insertIcon;
    ImageIcon _moveIcon;
    ImageIcon _addIcon;
    ImageIcon _deleteIcon;
    ImageIcon _addXsectIcon;
    ImageIcon _removeXsectIcon;
    ImageIcon _moveXsectIcon;
    ImageIcon _viewIcon;
    ImageIcon _colorUniformIcon;
    ImageIcon _colorElevIcon;
    ImageIcon _colorSourceIcon;
    ImageIcon _colorYearIcon;
    ImageIcon _networkCalculateIcon;
    ImageIcon _cursorIconSelected;
    ImageIcon _insertIconSelected;
    ImageIcon _moveIconSelected;
    ImageIcon _addIconSelected;
    ImageIcon _deleteIconSelected;
    ImageIcon _addXsectIconSelected;
    ImageIcon _removeXsectIconSelected;
    ImageIcon _moveXsectIconSelected;
    ImageIcon _colorUniformIconSelected;
    ImageIcon _colorElevIconSelected;
    ImageIcon _colorSourceIconSelected;
    ImageIcon _colorYearIconSelected;
    ImageIcon _filterYearIcon;
    ImageIcon _filterSourceIcon;
    ImageIcon _filterLabelIcon;
    ImageIcon _propOpenIcon;
    ImageIcon _zoomBoxIcon;
    ImageIcon _zoomBoxIconSelected;
    ImageIcon _zoomFitIcon;
    ImageIcon _zoomFitIconRollover;

    JButton _fileOpenButton;
    JButton _networkOpenButton;
    JButton _networkSaveButton;
    JButton _networkCalculateButton;
    JButton _propOpenButton;
    JButton _filterYearButton;
    JButton _filterSourceButton;
    JLabel _filterLabel;

    JRadioButton _colorUniformButton;
    JRadioButton _colorByElevButton;
    JRadioButton _colorBySourceButton;
    JRadioButton _colorByYearButton;
    

    /**
     * turns all centerline edit modes off
     */ 
JRadioButton _cursorButton;
    /**
     * turns insert centerline point mode on
     */
JRadioButton _insertButton;
    /**
     * turns move centerline point mode on
     */
JRadioButton _moveButton;
    /**
     * turns add centerline point mode on
     */
JRadioButton _addButton;
    /**
     * turns delete centerline point mode on
     */
JRadioButton _deleteButton;
    /**
     * turns add xsect mode on 
     */
JRadioButton _addXsectButton;
    /**
     * turns remove xsect mode on 
     */
JRadioButton _removeXsectButton;
    /**
     * turns move xsect mode on 
     */
JRadioButton _moveXsectButton;
    /**
     * turns all other modes off
     */
    //    JRadioButton _invisibleRadioButton = new JRadioButton();

JRadioButton _viewXsectButton;
    
    private static final Dimension _iconSize = new Dimension(25,25);
    private static final Dimension _wideIconSize = new Dimension(35,25);
    private static final Dimension _colorByIconSize = new Dimension(40, 15);
  //JButton _restoreButton  = new JButton("Restore");
  //JButton _keepButton     = new JButton("Keep");
//    protected JRadioButtonMenuItem _cMovePointMenuItem, _cAddPointMenuItem, 
//      _cInsertPointMenuItem, _cDeletePointMenuItem, _cAddXsectMenuItem, 
//      _cRemoveXsectMenuItem, _cMoveXsectMenuItem;

  Landmark _landmark;
    DigitalLineGraph _dlg;
  DSMChannels _DSMChannels;
  JMenuBar menubar;

  JMenu cfFile, cfProperties, cfModify, cfDisplay, cfTools, cfNetwork, cfCenterline,
    cfXsect, cfZoom, cfWindow, cfHelp;

  JMenuItem fNew, fOpen, fClose, fSave, fSaveAs, fMerge,
    fExtract, fPrintPreview, fPrint, fPrintSetup, fExit;
  JMenuItem pLoad, pSave, pSaveAs;
  JMenuItem mSource, mYear, mZSign, mErase, mRestore, mPurge, mStatus;
    JMenuItem dParameters, dSource, dYear, dColorBy, dErased, dLandmarks, cLandmarks,
      dDigitalLineGraph, dCreateLandmark, dEditLandmark, 
      dMoveLandmark, dDeleteLandmark, dCopyToClipboard;
//    JRadioButtonMenuItem dColorUniformRadioButton, dColorByDepthRadioButton, 
//        dColorBySourceRadioButton, dColorByYearRadioButton; 
JRadioButtonMenuItem dFitByBathymetryMenuItem,
    dFitByNetworkMenuItem, dFitByLandmarkMenuItem;
    ButtonGroup _colorByButtonGroup, _fitByButtonGroup, _centerlineEditButtonGroup;

    JCheckBoxMenuItem oEchoTimeSeriesInput, oEchoXsectInput, oPrintXsectResults,
      oUseFremontWeir, oUseToeDrainRestriction, oEchoToeDrainInput;
    JMenu tOpenWaterOptionsMenu, nExportOptions;
  JMenuItem tCompareNetwork, tCalcRect, tOpenWaterCalc;
  JMenuItem nRead, nSave, nSaveAs, nList, nSummary, nClearNetwork, nCalculate, 
      nExportToSEFormat, nExportTo3DFormat;
    JCheckBoxMenuItem noChannelLengthsOnly;
  JMenuItem cCursor, cCreate, cDSMCreate, cRemove;
  //JMenuItem cRemove;
    ////JMenuItem cRename; 
  //JMenuItem cMovePoint, cAddPoint, cDelPoint, 
  JMenuItem cRestore, cKeep, cSplit, cJoin, cView, cInfo, cList, cSummary;
  JMenuItem xAutoGen, xCreate, xRemove, xMove, xPosition, xView, xInfo;
    JMenuItem xSummary, xAdjustLength;
  JMenuItem zIn, zOut, zPan, zFit, zFactor, zBox;
  JMenuItem wCascade, wTile, wArrangeIcons, wCloseAll, wRepaint;
  JMenuItem hContents, hUsingHelp, hAbout;

  ActionListener _dLandmarksListener = null;
  ActionListener _nSaveListener = null;
  ActionListener _nSaveAsListener = null;
  ActionEvent _nullActionEvent = new ActionEvent(this,0,null);
  
    protected static final int COLOR_BY_DEPTH  = 0;
    protected static final int COLOR_BY_SOURCE = 1;
    protected static final int COLOR_BY_YEAR   = 2;
    protected JToolBar _legendPanel;
    protected String _depthLegendTitle  = "Elev(NGVD)";
    protected String _sourceLegendTitle = "Source";
    protected String _yearLegendTitle   = "Year";
    protected JPanel _infoPanel;
    protected JLabel _centerlineLabel = new JLabel("Selected Centerline:");
    protected JLabel _xsectLabel = new JLabel("Selected Xsect:");
    protected JLabel _mouseXLabel = new JLabel("X coordinate (UTM):");
    protected JLabel _mouseYLabel = new JLabel("Y coordinate (UTM):");
    protected JLabel _areaLabel = new JLabel("Xsect Area:");
    protected JLabel _wetPLabel = new JLabel("Wetted Perimeter:");
    protected JLabel _widthLabel = new JLabel("Top Width:");
    protected JLabel _hydraulicDepthLabel = new JLabel("Hydraulic Depth:");
    protected JLabel _bathymetryFileLabel = new JLabel("Bathymetry Filename:");
    protected JLabel _networkFileLabel = new JLabel("Network Filename:");
    protected JLabel _landmarkFileLabel = new JLabel("Landmark Filename:");
    protected JLabel _propertiesFileLabel = new JLabel("Properties Filename:");
    protected JLabel _dlgFileLabel = new JLabel("DLG Filename:");

    protected static Vector _colors = new Vector();
    private static final int _numDepths = 11;
    private final int _initialWidth = 800;
    private final int _initialHeight = 500;

  /**
   * array of depths used for coloring bathymetry data.
   */
  private static float[] depthTable;
  static{
    depthTable = new float[_numDepths];
    depthTable[0]  =  10.0f;
    depthTable[1]  =   5.0f;
    depthTable[2]  =   0.0f;
    depthTable[3]  =  -5.0f;
    depthTable[4]  = -10.0f;
    depthTable[5]  = -15.0f;
    depthTable[6]  = -20.0f;
    depthTable[7]  = -25.0f;
    depthTable[8]  = -30.0f;
    depthTable[9]  = -35.0f;
    depthTable[10] = -40.0f;
  }//depthTable

    /**
     * These are only for turning menus off.
     */
    private static final boolean _addFileMenu = true;
    private static final boolean _addPropertiesMenu = true;
    private static final boolean _addDisplayMenu = true;
    private static final boolean _addNetworkMenu = true;
    private static final boolean _addCenterlineMenu = true;
    private static final boolean _addXsectMenu = true;
    private static final boolean _addZoomMenu = true;
    private static final boolean _addToolsMenu = true;
    private static final boolean _addHelpMenu = true;

    private static final boolean _addZoomWindowOption = true;
    private static final boolean _addCompareNetworkOption = false;
    private static final boolean _addRectXSOption = true;
    private static final boolean _addOWACalcOption = true;
    private final boolean DEBUG=false;
}

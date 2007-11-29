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
import java.util.*;
import java.awt.*;
import javax.swing.*;
import java.io.*;
/**
 * Main application class
 *
 * @author
 * @version $Id: App.java,v 1.2 2002/10/21 19:58:25 btom Exp $
 */
public class App {

  public App(){
  }

  /**
   * Open ascii or binary bathymetry data file and store data in arrays
   */
  public BathymetryPlot bReadStore(JFrame gui, String directory, 
				   String filename, String filetype){
      BathymetryInput binput = null;
    _gui = (CsdpFrame)gui;
    boolean bWrite = true;
    int numLines=0;
    _gui.setCursor(CsdpFunctions._waitCursor);

    //read bathymetry file and store in BathymetryPlot object


    if(DEBUG)System.out.println("about to read file.  directory, filename, filetype=");
    if(DEBUG)System.out.println(directory+","+filename+","+filetype);


    binput = BathymetryInput.getInstance(_gui, directory, filename+"."+filetype);
    _bathymetryData = null;
    _bathymetryData = binput.readData();
    _bathymetryData.sortYearIndices();
    if(filetype.equals("prn")) _bathymetryData.sortBathymetryData();

    //plot data on canvas (BathymetryPlot)
    _bathymetryPlot=setBathymetryPlotter();
    updateNetworkPlotter();
    _gui.setPlotObject(_bathymetryPlot);
    _gui.updateColorLegend();
 
    _gui._canvas1.setPlotter(_bathymetryPlot,_gui._dim);
    _gui._canvas1.setUpdateBathymetry(true);
  
    if(_gui._canvas1._networkPlotter != null){
      if(DEBUG)System.out.println("should update network...");
      _gui._canvas1.setUpdateNetwork(true);
    }
    if(_gui._canvas1._dlgPlotter != null){
	//	_gui._canvas1.setUpdateCanvas(true);
	_gui._canvas1.setUpdateDigitalLineGraph(true);
    }

    //this tells the canvas to redraw itself.

	//removed for conversion to swing
    _gui._canvas1.redoNextPaint();
    _gui._canvas1.repaint();
    _gui.setCursor(CsdpFunctions._defaultCursor);
    if (DEBUG) displayData();
    _gui.enableAfterBathymetry();
    _gui.updateBathymetryFilename(filename+"."+filetype);

    return _bathymetryPlot;
  }//bReadStore

  /**
   * Open property data file and store data in CsdpFunctions
   */
  public void pReadStore(JFrame gui, String directory, String filename, 
			 String filetype) {
    parseFilename(filename);
    CsdpFunctions._propertiesFilename = _filename;
    CsdpFunctions._propertiesFiletype = _filetype;
    _gui = (CsdpFrame)gui;
    boolean bWrite = true;
    int numLines=0;
    _gui.setCursor(CsdpFunctions._waitCursor);

    _pinput = PropertiesInput.getInstance(_gui, directory, filename+"."+filetype);
    _pinput.readData();

    _gui.updateColorLegend();
    _gui._canvas1.setUpdateCanvas(true);
    _gui.updatePropertiesFilename(_filename+"."+_filetype);
	//removed for conversion to swing
    _gui._canvas1.redoNextPaint();
    _gui._canvas1.repaint();
    _gui.setCursor(CsdpFunctions._defaultCursor);
  }//pReadStore

    public boolean compareNetworks(JFrame gui, String nFilename1, Network net1, 
				   String nFilename2, Network net2, 
				   String outputFilename, String outputFiletype,
				   String outputDirectory){
	_gui = (CsdpFrame)gui;
	boolean success = false;
	
	NetworkCompareOutput ncOutput = 
	    NetworkCompareOutput.getInstance(nFilename1, net1, nFilename2, net2, 
					     outputDirectory, 
					     outputFilename+outputFiletype, _gui);
	success = ncOutput.writeData();

	return success;
    }

    /**
     * Calculates equivalent rectangular cross-sections for every channel
     * in the network
     */
    public void calcRect(CsdpFrame gui, Network net, 
			 String outputFilename, String outputDirectory, 
			 String dsmFilename, String dsmDirectory, 
			 String irregXsectsFilename, String irregXsectsDirectory,
			 String xsectsInpFilename, String xsectsInpDirectory){
	_gui = gui;
	DSMChannels dc = null;
	IrregularXsectsInp ixi = null;
	XsectsInp xi = null;
	
	DSMChannelsInput chanInput = DSMChannelsInput.getInstance
	    (dsmDirectory, dsmFilename);
	dc = chanInput.readData();

	IrregularXsectsInpInput ixInput = IrregularXsectsInpInput.getInstance
	    (irregXsectsDirectory, irregXsectsFilename);
	ixi = ixInput.readData();
	XsectsInpInput xiInput = XsectsInpInput.getInstance
	    (xsectsInpDirectory, xsectsInpFilename);
	xi = xiInput.readData();

	for(int i=0; i<=ixi.getNumChan()-1; i++){
	    String chanNum = ixi.getChanNum(i);
	    IrregularXsectsInp.IXIChan ixiChan = ixi.getChan(chanNum);
	    Centerline centerline = _net.getCenterline(chanNum);
	    if(centerline == null){
		_net.addCenterline(chanNum);
		centerline = _net.getCenterline(chanNum);
	    }
	    if(DEBUG)System.out.println("about to get ixiLine for chan "+chanNum);
	    if(DEBUG)System.out.println("centerline for the chan ="+centerline);

	    for(int j=0; j<=ixiChan.getNumLines()-1; j++){
		IrregularXsectsInp.IXILine ixiLine = ixiChan.getLine(j);
		if(DEBUG){
		    System.out.println("ixiLine = "+ixiLine);
		    System.out.println("distance = "+ixiLine.getDistance());
		    System.out.println("filename = "+ixiLine.getFilename());
		}
		if(ixiLine.getFilename() != null){
		    if(DEBUG)System.out.println("ixiLine="+ixiLine);
		    float distance = ixiLine.getDistance();
		    //filename includes complete path
		    String filename = ixiLine.getFilename();
		    
		    XsectInput xInput = XsectInput.getInstance(filename);
		    Xsect xsect = xInput.readData();
		    xsect.putDistAlongCenterline(distance);
		    if(DEBUG)System.out.println
				 ("set xsect dist="+xsect.getDistAlongCenterline());
		    if(DEBUG)System.out.println("about to add xsect to centerline. xsect, centerline="+xsect+","+centerline);
		    centerline.addCopiedXsect(xsect);
		}
	    }
	}

	RectXSOutput rxOutput = RectXSOutput.getInstance
	    (outputDirectory, outputFilename, net, _gui);
	rxOutput.writeData(dc, ixi, xi);
	
    }//calcRect

  /**
   * Open open water area data file and calculate
   */
    public Network openWaterAreaReadCalculate(JFrame gui,String filename, 
					      String filetype, 
					      String outputFilename, 
					      String stationFilename, 
					      String toeDrainFilename) {
	
	if(DEBUG)System.out.println("calculate called");
	Network owaNet = null;
	StationTimeSeriesData stationData = null;
	ToeDrainData toeDrainData = null;
	CsdpFunctions.setOpenWaterAreaFilename(filename);
	CsdpFunctions.setOpenWaterAreaFiletype(filetype);
	_gui = (CsdpFrame)gui;
	//    boolean bWrite = true;
	int numLines=0;
	_gui.setCursor(CsdpFunctions._waitCursor);
	
	//read time series data(flow and stage at different locations for each date)
	OpenWaterAreaInput owaInput = 
	    OpenWaterAreaInput.getInstance(filename, filetype);
	owaNet = owaInput.readData();

	System.out.println("About to read time series file.  directory, filename="+
			   stationFilename);


	//read cross-section info
	OpenWaterAreaStationInput owaSInput = 
	    OpenWaterAreaStationInput.getInstance(stationFilename);
	stationData = owaSInput.readData();

	//read toe drain info
	if(CsdpFunctions.getUseToeDrainRestriction()){
	    OpenWaterAreaToeDrainInput owaTDInput = 
		OpenWaterAreaToeDrainInput.getInstance(toeDrainFilename);
	    
	    toeDrainData = owaTDInput.readData();
	}

	//write output
	OpenWaterAreaOutput owaOutput = 
	    OpenWaterAreaOutput.getInstance(outputFilename, owaNet, _gui);
	owaOutput.writeData(stationData, toeDrainData);
	if(DEBUG)toeDrainData.printAll();

	_gui.setCursor(CsdpFunctions._defaultCursor);

	return owaNet;
    }//openWaterAreaReadCalculate

  /**
   * Display all bathymetry data on screen
   */
  public void displayData(){
    _bathymetryData.test();
  }//displayData
  
  /**
   * Saves bathymetry data as ascii(prn) or binary(cdp) file
   */
  public boolean fSave(String directory, String filename){
      boolean success = false;
    parseFilename(filename);

    System.out.println("filename="+filename);

    if(_filetype.equals(ASCII_TYPE)){ 
      BathymetryOutput aoutput = 
	BathymetryOutput.getInstance(directory, _filename, ASCII_TYPE,_bathymetryData);
      success = aoutput.writeData();
      System.out.println("Done writing ascii bathymetry data file");
    }
    else if(_filetype.equals(BINARY_TYPE)){
      BathymetryOutput boutput = 
	BathymetryOutput.getInstance(directory, _filename, BINARY_TYPE, _bathymetryData);
      success = boutput.writeData();
      System.out.println("Done writing binary bathymetry data file");
    }
    else System.out.println("filetype not defined for extension "+_filetype);
    return success;
  }  // fSave

  /**
   * write properties file
   */
  public boolean pSaveAs(String directory, String filename){
      boolean success = false;
    parseFilename(filename);
    CsdpFunctions._propertiesFilename = _filename;
    CsdpFunctions._propertiesFiletype = _filetype;
    
    if(DEBUG)System.out.println("propertiesFilename, propertiesFiletype="+CsdpFunctions._propertiesFilename+","+CsdpFunctions._propertiesFiletype);

      PropertiesOutput poutput = 
	PropertiesOutput.getInstance(_gui, directory, 
				     CsdpFunctions._propertiesFilename, 
				     CsdpFunctions._propertiesFiletype);
      success = poutput.writeData();
      System.out.println("Done writing properties file "+
			 CsdpFunctions._propertiesFilename+"."+
			 CsdpFunctions._propertiesFiletype);
    _gui.updatePropertiesFilename(_filename+"."+_filetype);
    return success;
  }//pSaveAs

  /**
   * save properties file.  return true if saved.  If user entered bad extension
   * (other than .cdn) save it anyway.
   */
  public boolean pSave(){
    boolean success = false;
    String directory = CsdpFunctions.getPropertiesDirectory().getPath();
    if(CsdpFunctions._propertiesFilename != null){
      if(CsdpFunctions._propertiesFiletype==null) 
	CsdpFunctions._propertiesFiletype = PROPERTIES_TYPE;
      PropertiesOutput poutput = PropertiesOutput.getInstance
	(_gui, directory, CsdpFunctions._propertiesFilename, 
	 CsdpFunctions._propertiesFiletype);
      success = poutput.writeData();
      System.out.println("Done writing properties file "+
			 CsdpFunctions._propertiesFilename+"."+
			 CsdpFunctions._propertiesFiletype);
    }
    return success;
  }//pSave

  /**
   * return instance of plotter object that is used by this class
   */
  public BathymetryPlot setBathymetryPlotter() {
    _bathymetryPlot = new BathymetryPlot(_gui, _bathymetryData, this);
    return _bathymetryPlot;
  }//setBathymetryPlotter
  
  /**
   * return instance of network plotter object that is used by this class
   */
  public NetworkPlot setNetworkPlotter(){
    _nPlot = new NetworkPlot(_gui, _bathymetryData, this);
    return _nPlot;
  }//setNetworkPlotter
  
  /**
    * updates BathymetryData object in NetworkPlot
    */
   public void updateNetworkPlotter(){
     if(_nPlot != null){
       _nPlot.setBathymetryData(_bathymetryData);
     }
   }//updateNetworkPlotter

  /**
   * Return instance of landmark plotter object that is used by this class
   */
  public LandmarkPlot setLandmarkPlotter(){
    _lPlot = new LandmarkPlot(_gui, _bathymetryData, this);
    return _lPlot;
  }//setLandmarkPlotter

  /**
   * Return instance of dlg plotter object that is used by this class
   */
  public DigitalLineGraphPlot setDigitalLineGraphPlotter(){
    _dlgPlot = new DigitalLineGraphPlot(_gui, _bathymetryData, this);
    return _dlgPlot;
  }//setDLGPlotter

  /**
   * read network data file
   */
  public Network nReadStore(JFrame gui, String directory, String filename){
      _gui = (CsdpFrame)gui;
    parseFilename(filename);
    CsdpFunctions._networkFilename = _filename;
    CsdpFunctions._networkFiletype = _filetype;
    NetworkInput ninput = 
      NetworkInput.getInstance(_gui, directory, _filename, _filetype);
    _net = ninput.readData();
    System.out.println("Done reading ascii network data file");
    _gui.updateNetworkFilename(_filename+"."+_filetype);

    _gui._canvas1.redoNextPaint();
    _gui._canvas1.repaint();
    return _net;
  }//nReadStore

    /**
     * remove network file from memory and display
     */
    public void clearNetwork(){
	_net=null;
	_gui.setNetwork(null);
	_gui.updateNetworkFilename(null);
	_gui.disableWhenNetworkCleared();
	_gui._canvas1.setNetworkPlotter(null);
	_gui._canvas1.setUpdateNetwork(false);
	_gui._canvas1.redoNextPaint();
	_gui._canvas1.repaint();
    }

  /**
   * read landmark data file
   */
  public Landmark lReadStore(String directory, String filename){
    parseFilename(filename);
    CsdpFunctions._landmarkFilename = _filename;
    CsdpFunctions._landmarkFiletype = _filetype;
    if(_filetype.equals(LANDMARK_TYPE)){
      LandmarkInput linput = 
	LandmarkInput.getInstance(directory, _filename+"."+_filetype);
      _landmark = linput.readData();
      System.out.println("Done reading ascii landmark data file");
    }
    else System.out.println("filetype not defined for extension "+_filetype);
    _gui.updateLandmarkFilename(_filename+"."+_filetype);
    _gui._canvas1.redoNextPaint();
    _gui._canvas1.repaint();    
    return _landmark;
  }//lReadStore

    /*
     * removes landmark from memory and from display.
     */
    public void clearLandmark(){
	_landmark = null;
	_gui.updateLandmarkFilename(null);
	_gui.setLandmark(null);
	_gui._canvas1.setLandmarkPlotter(null);
	_gui._canvas1.setUpdateLandmark(false);
	_gui.disableWhenLandmarkCleared();
	_gui._canvas1.redoNextPaint();
	_gui._canvas1.repaint();
    }

  /**
   * read Digital Line Graph data file
   */
  public DigitalLineGraph digitalLineGraphReadStore(String directory, String filename){
    parseFilename(filename);
    CsdpFunctions.setDigitalLineGraphFilename(_filename);
    CsdpFunctions.setDigitalLineGraphFiletype(_filetype);
    if(_filetype.equals(DLG_TYPE)){
      DigitalLineGraphInput dlgInput = 
	DigitalLineGraphInput.getInstance(directory, _filename+"."+_filetype);
      _dlg = dlgInput.readData();
      System.out.println("Done reading ascii Digital Line Graph data file");
    }
    else System.out.println("filetype not defined for extension "+_filetype);
    _gui.updateDigitalLineGraphFilename(_filename+"."+_filetype);
    _gui._canvas1.redoNextPaint();
    _gui._canvas1.repaint();
    return _dlg;
  }//dlgReadStore

  /**
   * read channelsInp data file
   */
  public DSMChannels chanReadStore(String directory, String filename){
    parseFilename(filename);
    CsdpFunctions._DSMChannelsFilename = _filename;
    CsdpFunctions._DSMChannelsFiletype = _filetype;
    if(_filetype.equals(DSMChannels_TYPE)){
      DSMChannelsInput chanInput = 
	DSMChannelsInput.getInstance(directory, _filename+"."+_filetype);
      _DSMChannels = chanInput.readData();
      System.out.println("Done reading ascii DSMChannels data file");
    }
    else System.out.println("filetype not defined for extension "+_filetype);
    return _DSMChannels;
  }//chanReadStore

  /**
   * save network file.  return true if saved.  If user entered bad extension
   * (other than .cdn) save it anyway.
   */
  public boolean nSave(){
    boolean success = false;
    String directory = CsdpFunctions.getNetworkDirectory().getPath();
    if(CsdpFunctions._networkFilename != null){
      if(CsdpFunctions._networkFiletype==null) 
	CsdpFunctions._networkFiletype = NETWORK_TYPE;
      NetworkOutput noutput = NetworkOutput.getInstance
	(directory, CsdpFunctions._networkFilename, 
	 CsdpFunctions._networkFiletype, _net);
      success = noutput.writeData();
      System.out.println("Done writing network file "+
			 CsdpFunctions._networkFilename+"."+
			 CsdpFunctions._networkFiletype);
    }
    return success;
  }//nSave

  /**
   * write network file
   */
  public boolean nSaveAs(String directory, String filename){
    boolean success = false;
    parseFilename(filename);
    CsdpFunctions._networkFilename = _filename;
    CsdpFunctions._networkFiletype = _filetype;
    
    if(DEBUG)System.out.println("networkFilename, networkFiletype="+CsdpFunctions._networkFilename+","+CsdpFunctions._networkFiletype);

      NetworkOutput noutput = 
	NetworkOutput.getInstance(directory, CsdpFunctions._networkFilename, 
				  CsdpFunctions._networkFiletype, _net);
      success = noutput.writeData();
      System.out.println("Done writing network file "+
			 CsdpFunctions._networkFilename+"."+
			 CsdpFunctions._networkFiletype);
    _gui.updateNetworkFilename(_filename+"."+_filetype);
    return success;
  }//nSaveAs

  /**
   * export network file to station/elevation format
   */
  public boolean nExportToSEFormat(String directory, String filename, 
				   boolean channelLengthsOnly){

    boolean success = false;
    parseFilename(filename);
    CsdpFunctions._networkFilename = _filename;
    CsdpFunctions._networkFiletype = _filetype;
    
    if(DEBUG)System.out.println("networkFilename, networkFiletype="+CsdpFunctions._networkFilename+","+CsdpFunctions._networkFiletype);

      NetworkOutput noutput = 
	NetworkOutput.getInstance(directory, CsdpFunctions._networkFilename, 
				  CsdpFunctions._networkFiletype, _net);
      noutput.setChannelLengthsOnly(channelLengthsOnly);
      success = noutput.writeData();
      System.out.println("Done writing network file in SE format"+
			 CsdpFunctions._networkFilename+"."+
			 CsdpFunctions._networkFiletype);
    _gui.updateNetworkFilename(_filename+"."+_filetype);
    return success;
  }//nExportToSEFormat

    /**
     * export network file to 3D format
     */
    public boolean nExportTo3DFormat(String directory, String filename){
	boolean success = false;
	parseFilename(filename);
	CsdpFunctions._networkFilename = _filename;
	CsdpFunctions._networkFiletype = _filetype;
	
	if(DEBUG)System.out.println("networkFilename, networkFiletype="+CsdpFunctions._networkFilename+","+CsdpFunctions._networkFiletype);
	
	NetworkOutput noutput = 
	    NetworkOutput.getInstance(directory, CsdpFunctions._networkFilename, 
				      CsdpFunctions._networkFiletype, _net);
	noutput.set3DOutput(true);
	success = noutput.writeData();
	noutput.set3DOutput(false);
	System.out.println("Done writing network file in SE format"+
			   CsdpFunctions._networkFilename+"."+
			   CsdpFunctions._networkFiletype);
	_gui.updateNetworkFilename(_filename+"."+_filetype);
	return success;
    }//nExportToSEFormat



  
  /**
   * Calculate all cross-sections in network--IS THIS USED???????????????
   */
  public void nCalculate(String dir, String chanFile, String nodeFile){
    String filename = null;
    String centerlineName = null;
    Centerline centerline;
    Xsect xsect;
    float length;
    float distAlongCenterline;
    float normalizedDist;
    
    for(int i=0; i<=_net.getNumCenterlines()-1; i++){    
      centerlineName = _net.getCenterlineName(i);
      centerline = _net.getCenterline(centerlineName);
      length = centerline.getLength();
      for(int j=0; j<=centerline.getNumXsects()-1; j++){
	xsect = centerline.getXsect(j);
	distAlongCenterline = xsect.getDistAlongCenterline();
	normalizedDist = distAlongCenterline/length;
	String s = Float.toString(normalizedDist);
	if(DEBUG)System.out.println("channel, normalized distance = "+
				    centerlineName+s);
	XsectOutput xoutput = 
	    XsectOutput.getInstance(dir, centerlineName+"_"+s.substring(0,7), 
				  "txt", j, xsect);
	xoutput.writeData();
      }//for i
    }//for j
  }//nCalculate

  /**
   * Calculate all cross-sections in network
   */
  public void nCalculate(String dir){
    String filename = null;
    String centerlineName = null;
    Centerline centerline;
    Xsect xsect;
    float length;
    float distAlongCenterline;
    float normalizedDist;
    
    for(int i=0; i<=_net.getNumCenterlines()-1; i++){    
      centerlineName = _net.getCenterlineName(i);
      centerline = _net.getCenterline(centerlineName);
      length = centerline.getLength();
      for(int j=0; j<=centerline.getNumXsects()-1; j++){
	xsect = centerline.getXsect(j);
	if(xsect.getNumPoints() > 0){
	    distAlongCenterline = xsect.getDistAlongCenterline();
	    normalizedDist = distAlongCenterline/length;
	    String s = Float.toString(normalizedDist);
	    if(s.indexOf(".",0) > 0) s = s+"0000000";
	    else s = s+".000000";
	    filename = centerlineName+"_"+s.substring(0,7);
	    XsectOutput xoutput = 
		XsectOutput.getInstance(dir, filename, XSECT_TYPE, j, xsect);
	    xoutput.writeData();
	}else{
	    System.out.println("not calculating xsect "+centerlineName+"_"+j+" because it has no points");
	}
      }//for i
    }//for j
    System.out.println("Done writing cross-section files");
  }//nCalculate

  /**
   * writes the DSM2 input file "irregular_xsects.inp"
   */
  public void writeIrregularXsectsInp(String dir){
    IrregularXsectsInpOutput ixioutput = IrregularXsectsInpOutput.getInstance(dir, _net);
    ixioutput.writeData();
  }//writeIrregularXsectsInp

  /**
   * write a landmark file which labels all the cross-sections
   */
  public void writeXsectLandmark(){
    XsectLandmarkOutput xloutput = XsectLandmarkOutput.getInstance(_net);
    xloutput.writeData();
  }//writeXsectLandmark

  /**
   * plot bathymetry and network data in cross-section view
   */
  public void viewXsect(Xsect xsect, String centerlineName, int xsectNum, 
			float thickness){
    _gui.setCursor(CsdpFunctions._waitCursor);
    Hashtable xsectDisplayData = 
      _net.findXsectDisplayRegion(centerlineName, xsectNum, thickness);
    _bathymetryData.findXsectData(xsectDisplayData);

    if(_xsectGraph.contains(centerlineName+"_"+xsectNum)){
      
    }
    else{
	if(_bathymetryData.getNumEnclosedValues() <= 0){
	    JOptionPane.showOptionDialog
		(null, "ERROR!  THERE ARE NO POINTS TO DISPLAY! TRY INCREASING THICKNESS", 
		 "ERROR! NO POINTS TO DISPLAY",
		 JOptionPane.DEFAULT_OPTION, JOptionPane.WARNING_MESSAGE, null,
		 _options, _options[0]);

	}else{
	    _xsectGraph.put(centerlineName+"_"+xsectNum, new XsectGraph
			    (_gui, this,_bathymetryData, _net, centerlineName, 
			     xsectNum, thickness, _xsectColorOption));
	    getXsectGraph(centerlineName,xsectNum).pack();
	    getXsectGraph(centerlineName,xsectNum).show();
	}
	_gui.setCursor(CsdpFunctions._defaultCursor);
    }
  }//viewXsect

  /**
   * If name exists in XsectGraph array, then rename...store first...
   */
  public void renameOpenXsectGraphs(String centerlineName, 
				    ResizableIntArray xsectIndices){
    XsectGraph graph = null;
    Hashtable sortedGraphs = new Hashtable();

    for(int i=0; i<=xsectIndices.getSize()-1; i++){
      graph = getXsectGraph(centerlineName, xsectIndices.get(i));
      if(graph != null){
	sortedGraphs.put(centerlineName+"_"+i,graph);
	graph.updateXsectNum(i);
      }
    }//for i
    _xsectGraph = sortedGraphs;
  }//renameOpenXsectGraphs

  /**
   * updates xsect display when its xsect line is moved
   */
  public void updateXsect(String centerlineName, int xsectNum){
    XsectGraph xg = getXsectGraph(centerlineName, xsectNum);
    if(xg != null){
	xg.updateGraphCanvas();
	xg.updateDisplay();
    }
  }

   /**
    * rename centerline
    */
 public String centerlineRename(Centerline c){
   //   FileDialog fd = new FileDialog("Enter new Centerline name");
   String centerlineName = null;
   // centerline names are actually stored in array in network class.  should
   // really be getting name from _net
   //   if(c.getCenterlineName() != null) fd.setFile(c.getCenterlineName());
   //fd.show();
   //String centerlineName = fd.getFile();
   return centerlineName;
 }
  
  /**
   * returns BathymetryData
   */
  public BathymetryData getBathymetryData(){
    return _bathymetryData;
  }//getBathymetryData

  /**
   * Causes all open xsect graphs to be updated.  Called when color table adjusted.
   */
  public void updateAllXsectGraphs(){
    Enumeration e = _xsectGraph.elements();
    while(e.hasMoreElements()){
      XsectGraph xg = (XsectGraph)(e.nextElement());
      xg.updateGraphCanvas();
      xg._gC.redoNextPaint();
      xg.validate();

	//removed for conversion to swing
      //      xg._gC.repaint();
    }
  }//updateAllXsectGraphs

  /**
   * redraw the xsect graph
   */
  public void updateXsectGraph(String centerlineName, int xsectNum){
    XsectGraph xg = getXsectGraph(centerlineName, xsectNum);
    if(xg != null){

//        System.out.println("about to call updateGraphCanvas");
//        System.out.println(xg._gC.getGraph());
//        System.out.println(xg._gC.getGraph().getPlot());
//        System.out.println(xg._gC.getGraph().getPlot().getAttributes());

      xg.updateGraphCanvas();
      xg._gC.redoNextPaint();
      xg.validate();
	//removed for conversion to swing
      //      xg._gC.repaint();
    }
  }

  /*
   * return network object
   */
  public Network getNetwork(){
    return _net;
  }

  /*
   * return landmark object
   */
  public Landmark getLandmark(){
    return _landmark;
  }

  public void setXsectColorOption(int value){
    _xsectColorOption = value;
  }

  /**
   * separates filename into prefix and extension
   */
  protected void parseFilename(String filename){
    int dotIndex = filename.indexOf(".",0);
    if(dotIndex >= 0){
      _filename = filename.substring(0,dotIndex);
      _filetype = filename.substring(dotIndex+1);
    }
    else{
      _filename = filename;
      _filetype = null;
    }
  }//parseFilename

  /**
   * returns XsectGraph object for specified centerlineName and xsectNum
   */
  protected XsectGraph getXsectGraph(String centerlineName, int xsectNum){
    return (XsectGraph)(_xsectGraph.get(centerlineName+"_"+xsectNum));
  }
  /**
   * removes XsectGraph object for specified centerlineName and xsectNum
   */
  protected void removeXsectGraph(String centerlineName, int xsectNum){
    _xsectGraph.remove(centerlineName+"_"+xsectNum);
  }

    public void setSquareDimension(int size){
	_squareDimension = size;
    }
  
    public int getSquareDimension(){
	return _squareDimension;
    }

  CsdpFrame _gui             = null;
  BathymetryData _bathymetryData = null;
  Network _net;
  Landmark _landmark;
  DSMChannels _DSMChannels;
  BathymetryPlot _bathymetryPlot      = null;
  NetworkPlot _nPlot        = null;
  LandmarkPlot _lPlot       = null;
    DigitalLineGraph _dlg;
    DigitalLineGraphPlot _dlgPlot        = null;
  //  XsectGraph _xsectGraph;
  Hashtable _xsectGraph = new Hashtable();

  protected String _filename = null;
  protected String _filetype = null;
  protected static final String PROPERTIES_TYPE  = "prp";
  protected static final String ASCII_TYPE       = "prn";
  protected static final String BINARY_TYPE      = "cdp";
  protected static final String NETWORK_TYPE     = "cdn";
  protected static final String LANDMARK_TYPE    = "cdl";
  protected static final String XSECT_TYPE       = "txt";
    protected static final String DLG_TYPE       = "cdo";
  protected static final String DSMChannels_TYPE = "inp";
    //  BathymetryInput _binput = null;
  PropertiesInput _pinput = null;
  protected static boolean DEBUG = false;
  public int _xsectColorOption = 0;
    public static int _squareDimension = 4;
    /**
     * used for JOptionPane
     */
    private Object[] _options = {"OK"};
    
}// class App

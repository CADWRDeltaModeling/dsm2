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
import DWR.CSDP.dialog.*;
import java.awt.event.*;
import java.awt.*;
import java.io.*;
import javax.swing.*;

/**
 * calls methods for displaying landmarks and other information
 *
 * @author
 * @version 
 */
public class DisplayMenu{

public DisplayMenu(App app, Network net) {
  _app = app;
  _net = net;
  _lOpenFilter = new CsdpFileFilter(_openExtensions, _numOpenExtensions);
  _DLGOpenFilter = new CsdpFileFilter(_openDLGExtensions, _numOpenDLGExtensions);
}

/**
 * Allow user to change display parameters
 *
 * @author
 * @version $Id: DisplayMenu.java,v 1.2 2002/10/21 19:58:54 btom Exp $
 */
public class DParameters implements ActionListener {  
    CsdpFrame _gui;
    public DParameters(CsdpFrame gui){
	_gui = gui;
    }
  /**
   * get new cross-section thickness, bathymetry point dimension from user.  
   */
public void actionPerformed(ActionEvent e) {
  _plot = _gui.getPlotObject();

  float oldThickness = CsdpFunctions._xsectThickness;
  int oldPointSize = _plot.getPointSize();
  String names[] = new String[2];
  float initValue[] = new float[2];
  names[0] = "Cross-Section Thickness, ft";
  names[1] = "Bathymetry Point Dimension, pixels";
  initValue[0] = oldThickness;
  initValue[1] = (int)oldPointSize;

  TextFieldDialog d = new TextFieldDialog(_gui, "Display Parameters", true,
					  names, initValue);
  d.show();
  String t = ((TextField)d._textFields.get(names[0])).getText();
  String pd= ((TextField)d._textFields.get(names[1])).getText();

  int newPointSize = (int)(Float.parseFloat(pd));
  float newThickness = Float.parseFloat(t);
  if(DEBUG)System.out.println("thickness, point dimension="+newThickness+","+newPointSize);
  if(newThickness != oldThickness){
    CsdpFunctions._xsectThickness=newThickness;
  }//change thickness
  if(newPointSize != oldPointSize){
    _plot.setPointSize(newPointSize);
    _gui._canvas1.setUpdateCanvas(true);
	//removed for conversion to swing
    _gui._canvas1.repaint();
  }//change point size
}

} // Class DParameters

/**
 * Originally intended to load and display USGS Digital Line Graph Files.  
 * These files are available at
 * http://bard.wr.usgs.gov/htmldir/dlg_html/hypso.html
 *
 * For now, it's designed to load the file I got from another agency...
 *
 * They are UTM zone 10 NAD 27, meters.  Only lines will be read--nodes and areas
 * (if any will be ignored.  Eventually should have selector to select/unselect
 * multiple files to load.  Then should be able to save as binary file(s).
 *
 * @author
 * @version $Id: DisplayMenu.java,v 1.2 2002/10/21 19:58:54 btom Exp $
 */
public class DDigitalLineGraph extends FileIO implements ActionListener {  
    CsdpFrame _gui;
    public DDigitalLineGraph(CsdpFrame gui){
	super(gui, _openDLGDialogMessage, _openDLGErrorMessage, _openSuccessMessage, 
	      _openFailureMessage, false, _openDLGExtensions, _numOpenDLGExtensions);
	_gui = gui;
	_jfc.setDialogTitle(_openDLGDialogMessage);
	_jfc.setApproveButtonText("Open");
	_jfc.addChoosableFileFilter(_DLGOpenFilter);
	_jfc.setFileFilter(_DLGOpenFilter);
    }

    /**
     * Called by superclass.  Uses dialog box to get filename from user.
     */
    protected String getFilename(){
	int numLines = 0;
	String filename = null;
	if(CsdpFunctions.getDigitalLineGraphDirectory() != null){
	    _jfc.setCurrentDirectory
		(CsdpFunctions.getDigitalLineGraphDirectory());
	}
	else if(CsdpFunctions.getOpenDirectory() != null){
	    _jfc.setCurrentDirectory(CsdpFunctions.getOpenDirectory());
	}
	else System.out.println("no file selected");
	_filechooserState = _jfc.showOpenDialog(_gui);
	if(_filechooserState==JFileChooser.APPROVE_OPTION){
	    filename = _jfc.getName(_jfc.getSelectedFile());
	    CsdpFunctions.setDigitalLineGraphDirectory
		(_jfc.getCurrentDirectory().getAbsolutePath()+File.separator);
	    CsdpFunctions.setOpenDirectory
		(_jfc.getCurrentDirectory().getAbsolutePath()+File.separator);
	    parseFilename(filename);
	}else if(_filechooserState == JFileChooser.CANCEL_OPTION){
	    _cancel = true;
	    filename = null;
	}else{
	    filename = null;
	}
	return filename;
    }

    /**
     * open dlg file
     */
    public boolean accessFile(){
	if(_cancel == false){
	    readFile();
	}
	_cancel = false;
	return true; //no need to tell user if failed.
    }

    protected void readFile(){
	_dlg = _app.digitalLineGraphReadStore
	    (CsdpFunctions.getDigitalLineGraphDirectory().getPath(),
	     _filename+"."+_filetype);
	_gui.setDigitalLineGraph(_dlg);
	_dlgPlot = _app.setDigitalLineGraphPlotter();

	_gui._canvas1.setDigitalLineGraphPlotter(_dlgPlot);
	_gui._canvas1.setUpdateDigitalLineGraph(true);
	//removed for conversion to swing
	//	_gui._canvas1.repaint();

	//	_gui.enableAfterDigitalLineGraph();
    }

}//class DDigitalLineGraph

/**
 * Loads and displays landmarks
 *
 * @author
 * @version $Id: DisplayMenu.java,v 1.2 2002/10/21 19:58:54 btom Exp $
 */
public class DLandmarks extends FileIO implements ActionListener {  
    CsdpFrame _gui;
  public DLandmarks(CsdpFrame gui){
    super(gui, _openDialogMessage, _openErrorMessage, _openSuccessMessage,
	  _openFailureMessage, false, _openExtensions, _numOpenExtensions);
    _gui = (CsdpFrame)gui;
    _ynd = new YesNoDialog(_gui,"A landmark file is already loaded.  Replace?",true);
    _jfc.setDialogTitle(_openDialogMessage);
    _jfc.setApproveButtonText("Open");
    _jfc.addChoosableFileFilter(_lOpenFilter);
    _jfc.setFileFilter(_lOpenFilter);
  }

  /**
   * not needed here.  Use if you want to save something before continuing.
   */
  public void warnUserIfNecessary(){
  }//warnUserIfNecessary

  /**
   * uses dialog box to get filename from user
   */
  protected String getFilename(){
    int numLines=0;
    String filename=null;
    if(CsdpFunctions.getLandmarkDirectory() != null){
	_jfc.setCurrentDirectory(CsdpFunctions.getLandmarkDirectory());
    }
    else if(CsdpFunctions.getOpenDirectory() != null){
	_jfc.setCurrentDirectory(CsdpFunctions.getOpenDirectory());
    }
    else System.out.println("no file selected");

    _filechooserState = _jfc.showOpenDialog(_gui);
    if(_filechooserState==JFileChooser.APPROVE_OPTION){
	filename = _jfc.getName(_jfc.getSelectedFile());
	CsdpFunctions.setLandmarkDirectory
	    (_jfc.getCurrentDirectory().getAbsolutePath()+File.separator);
	CsdpFunctions.setOpenDirectory
	    (_jfc.getCurrentDirectory().getAbsolutePath()+File.separator);
	parseFilename(filename);
    }else if(_filechooserState == JFileChooser.CANCEL_OPTION){
	_cancel=true;
	filename = null;
    }else{
	filename = null;
    }
    return filename;
  }//getFilename

  /**
   * open landmark file
   */
  public boolean accessFile(){
    if(_app._landmark != null){
      _ynd.show();
      if(_ynd._yes == true){
	readFile();
      }//if
    }//if
    else if(_cancel == false){
      readFile();
    }//else if
    _cancel = false;
    return true;  //no need to tell user if failed.
  }//accessFile

  protected void readFile(){
    _landmark = _app.lReadStore(CsdpFunctions.getLandmarkDirectory().getPath(), 
				_filename+"."+_filetype);
    _gui.setLandmark(_landmark);
    _lplot = _app.setLandmarkPlotter();
    
    _gui._canvas1.setLandmarkPlotter(_lplot);
    _gui._canvas1.setUpdateLandmark(true);
	//removed for conversion to swing
    //    _gui._canvas1.repaint();

    _gui.enableAfterLandmark();
  }

  YesNoDialog _ynd;
} // Class DLandmarks

    /**
     * clears landmark.
     */
    public class DClearLandmark implements ActionListener{
	CsdpFrame _gui;
	public DClearLandmark(CsdpFrame gui){
	    _gui = gui;
	}
	public void actionPerformed(ActionEvent e){
	    _app.clearLandmark();
	}
    }//class DClearLandmark

  /**
   * filters display of data by source
   *
   * @author
   * @version 
   */
  public class DSource implements ActionListener{

    BathymetryData _data = null;
    CsdpFrame _gui;
      public DSource(CsdpFrame gui){
	  _gui=gui;
      }
    public void actionPerformed(ActionEvent e) {

      _data = _app.getBathymetryData();
      int numSources = _data.getNumSources();
      ResizableStringArray source = new ResizableStringArray(numSources,1);

      for(int i=0; i<=numSources-1; i++){
	source.put(i, _data.getSource(i));
	_sourceInitState.put(i, _data.getPlotSource(i));
      }
      DataFilterCheckbox d = new DataFilterCheckbox
	((Frame)_gui, "Select source(s) to be plotted", true, source, 
	 _sourceInitState, numSources);
      d.show();
      Checkbox checkbox = null;
      boolean changed = false;
      for(int i=0; i<=numSources-1; i++){
	checkbox = (Checkbox)(d._checkboxes.get(d._names.get(i)));
	if(checkbox.getState() != _data.getPlotSource(i)){
	  _data.putPlotSource(i, checkbox.getState());
	  changed = true;
	}
      }
      if(changed){
	_gui._canvas1.setUpdateCanvas(true);
	//removed for conversion to swing--then put back because it wasn't redrawing.
	_gui._canvas1.repaint();
      }
    }//actionPerformed
  }//DSource

  /**
   * filters display of data by year
   *
   * @author
   * @version 
   */
  public class DYear implements ActionListener{
    BathymetryData _data = null;
    CsdpFrame _gui;
      public DYear(CsdpFrame gui){
	  _gui=gui;
      }
    public void actionPerformed(ActionEvent e) {

      _data = _app.getBathymetryData();
      int numYears = _data.getNumYears();
      ResizableStringArray year = new ResizableStringArray(numYears,1);
      Integer yearObject = null;
      String yearString = null;
      for(int i=0; i<=numYears-1; i++){
	  //	yearObject = new Integer(_data.getYear(i));
	  yearString = Integer.toString(_data.getYear(i));
	year.put(i, yearString);
	_yearInitState.put(i, _data.getPlotYear(i));
      }
      DataFilterCheckbox d = new DataFilterCheckbox
	((Frame)_gui, "Select year(s) to be plotted", true, year, 
	 _yearInitState, numYears);
      d.show();
      Checkbox checkbox = null;
      boolean changed = false;
      for(int i=0; i<=numYears-1; i++){
	checkbox = (Checkbox)(d._checkboxes.get(d._names.get(i)));
	if(checkbox.getState() != _data.getPlotYear(i)){
	  _data.putPlotYear(i, checkbox.getState());
	  changed = true;
	}
      }
      if(changed){
	_gui._canvas1.setUpdateCanvas(true);
	//removed for conversion to swing
	//then put back because it wasn't redrawing	
	_gui._canvas1.repaint();
      }
    }//actionPerformed
  }//DYear

  /**
   * Make all bathymetry points the same color
   */
    public class DColorUniform implements ItemListener{
	CsdpFrame _gui;
	public DColorUniform(CsdpFrame gui){
	    _gui = gui;
	}
    public void itemStateChanged(ItemEvent e){
	//      _gui.setAllColorByFalse();
	//      _gui.setColorUniform();
	_gui.updateColorLegend();
      _gui._canvas1.setUpdateBathymetry(true);
	//removed for conversion to swing
      //_gui._canvas1.repaint();
    }

  }
  /**
   * Color points by depth
   */
  public class DColorByElev implements ItemListener{
      CsdpFrame _gui;
      public DColorByElev(CsdpFrame gui){
	  _gui=gui;
      }
    public void itemStateChanged(ItemEvent e){
//        _gui.setAllColorByFalse();
//        _gui.setColorByDepth();
	_gui.updateColorLegend();
      _gui._canvas1.setUpdateBathymetry(true);
	//removed for conversion to swing
      //      _gui._canvas1.repaint();
    }
  }
  /**
   * Color points by group (source of data)
   */
  public class DColorBySource implements ItemListener{
      CsdpFrame _gui;
      public DColorBySource(CsdpFrame gui){
	  _gui = gui;
      }
    public void itemStateChanged(ItemEvent e){
//        _gui.setAllColorByFalse();
//        _gui.setColorBySource();
	_gui.updateColorLegend();
      _gui._canvas1.setUpdateBathymetry(true);
	//removed for conversion to swing
      //      _gui._canvas1.repaint();
    }
  }
  /**
   * Color points by year
   */
  public class DColorByYear implements ItemListener{
      CsdpFrame _gui;
      public DColorByYear(CsdpFrame gui){
	  _gui=gui;
      }
    public void itemStateChanged(ItemEvent e){
//        _gui.setAllColorByFalse();
//        _gui.setColorByYear();
	_gui.updateColorLegend();
      _gui._canvas1.setUpdateBathymetry(true);
	//removed for conversion to swing
      //      _gui._canvas1.repaint();
    }
  }

  /**
   * fit by bathymetry
   */
  public class DFitByBathymetry implements ItemListener{
      CsdpFrame _gui;
      public DFitByBathymetry(CsdpFrame gui){
	  _gui = gui;
      }
    public void itemStateChanged(ItemEvent e){
      if(_gui.getFitByBathymetryOption() != true){
//  	_gui.setAllFitByFalse();
	  //	_gui.setFitByBathymetry();
	_gui._canvas1.setUpdateBathymetry(true);
	_gui._canvas1.setUpdateNetwork(true);
	_gui._canvas1.setUpdateLandmark(true);
	//removed for conversion to swing
	//	_gui._canvas1.repaint();
      }//if
    }//itemStateChanged
  }//class DFitByBathymetry

  /**
   * fit by network
   */
  public class DFitByNetwork implements ItemListener{
      CsdpFrame _gui;
      public DFitByNetwork(CsdpFrame gui){
	  _gui=gui;
      }
    public void itemStateChanged(ItemEvent e){
      if(_gui.getFitByNetworkOption() != true){
//  	_gui.setAllFitByFalse();
//  	_gui.setFitByNetwork();
	_gui._canvas1.setUpdateBathymetry(true);
	_gui._canvas1.setUpdateNetwork(true);
	_gui._canvas1.setUpdateLandmark(true);
	//removed for conversion to swing
	//	_gui._canvas1.repaint();
      }//if
    }//itemStateChanged
  }//class DFitByNetwork

  /**
   * fit by Landmark
   */
  public class DFitByLandmark implements ItemListener{
      CsdpFrame _gui;
      public DFitByLandmark(CsdpFrame gui){
	  _gui=gui;
      }
    public void itemStateChanged(ItemEvent e){
      if(_gui.getFitByLandmarkOption() != true){
//  	_gui.setAllFitByFalse();
//  	_gui.setFitByLandmark();
	_gui._canvas1.setUpdateBathymetry(true);
	_gui._canvas1.setUpdateNetwork(true);
	_gui._canvas1.setUpdateLandmark(true);
	//removed for conversion to swing
	//	_gui._canvas1.repaint();
      }//if
    }//itemStateChanged
  }//class DFitByLandmark

  App _app;
  BathymetryPlot _plot;
  Network _net;
  Landmark _landmark;
  LandmarkPlot _lplot;
    DigitalLineGraph _dlg;
    DigitalLineGraphPlot _dlgPlot;
  ResizableBooleanArray _sourceInitState = new ResizableBooleanArray();
  ResizableBooleanArray _yearInitState = new ResizableBooleanArray();
  //  protected boolean _cancel = false;
  protected static final boolean DEBUG = false;
  protected static final String _openDialogMessage = "Select landmark(.cdl) file";
  protected static final String _openErrorMessage = "Only .cdl extension allowed";
  protected static final String[] _openExtensions = {"cdl"};
  protected static final int _numOpenExtensions = 1;

  protected static final String _openDLGDialogMessage = 
      "Select Channel Outline (.cdo) file";
  protected static final String _openDLGErrorMessage = "Only .cdo extension allowed";
  protected static final String[] _openDLGExtensions = {"cdo"};
  protected static final int _numOpenDLGExtensions = 1;

  //  protected static final String _saveDialogMessage = "Save Network(.cdl) file";
  //  protected static final String _saveErrorMessage = "Only .cdl extension allowed";
  //  protected static final String[] _saveExtensions = {"cdl"};
  //  protected static final int _numSaveExtensions = 1;

    //    protected static final String _saveSuccessMessage = "saved file";
    protected static final String _openSuccessMessage = "";
    //    protected static final String _saveFailureMessage = "ERROR:  couldn't save file";
    protected static final String _openFailureMessage = "ERROR:  couldn't open file";
    protected CsdpFileFilter _lOpenFilter;
    protected CsdpFileFilter _DLGOpenFilter;
    int _filechooserState = -CsdpFunctions.BIG_INT;
}//class ZoomMenu

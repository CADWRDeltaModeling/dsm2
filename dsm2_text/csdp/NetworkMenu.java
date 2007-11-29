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
import java.io.*;
import java.awt.event.*;
import java.awt.*;
import javax.swing.*;

public class NetworkMenu {

  public NetworkMenu(App app) {
    _app = app;
    _nOpenFilter=new CsdpFileFilter(_openExtensions, _numOpenExtensions);
    _nSaveFilter=new CsdpFileFilter(_saveExtensions, _numSaveExtensions);
    _nExportFilter=new CsdpFileFilter(_exportExtensions, _numExportExtensions);
    _3dNExportFilter=new CsdpFileFilter(_3dExportExtensions, _3dNumExportExtensions);
  }
/**
 * Create new file
 *
 * @author
 * @version $Id: NetworkMenu.java,v 1.2 2002/10/21 20:02:19 btom Exp $
 */
public class NOpen extends FileIO implements ActionListener {  
  NetworkPlot _nplot;
  Network _net;

  public NOpen(CsdpFrame gui){
      super(gui, _openDialogMessage, _openErrorMessage, _openSuccessMessage, 
	    _openFailureMessage, false, _openExtensions, _numOpenExtensions);
      _jfc.setDialogTitle(_openDialogMessage);
      _jfc.setApproveButtonText("Open");
      _jfc.addChoosableFileFilter(_nOpenFilter);
      _jfc.setFileFilter(_nOpenFilter);
  }

  /**
   * Option to save network before continuing.
   */
  public void warnUserIfNecessary(){
    _net = ((CsdpFrame)_gui).getNetwork();
    if(_net != null){
      if(_net.isUpdated()){
	YesNoDialog d = new YesNoDialog
	  (_gui, "Network file is not saved.  Save(y/n)?", true);
	d.show();
	if(d._yes == true) ((CsdpFrame)_gui).saveNetwork();
      }//if network has changed
    }//if net isn't null
  }//warningNeeded

    /**
     * uses dialog box to get filename from user
     */
    protected String getFilename(){
    int numLines=0;
    String filename=null;

    if(CsdpFunctions.getNetworkDirectory() != null){
	_jfc.setCurrentDirectory(CsdpFunctions.getNetworkDirectory());
    }
    else if(CsdpFunctions.getOpenDirectory() != null){
	_jfc.setCurrentDirectory(CsdpFunctions.getOpenDirectory());
    }
    else System.out.println();
    _filechooserState = _jfc.showOpenDialog(_gui);
    if(_filechooserState == JFileChooser.APPROVE_OPTION){
	filename = _jfc.getName(_jfc.getSelectedFile());
	CsdpFunctions.setNetworkDirectory
	    (_jfc.getCurrentDirectory().getAbsolutePath()+File.separator);
	CsdpFunctions.setOpenDirectory
	    (_jfc.getCurrentDirectory().getAbsolutePath()+File.separator);
	parseFilename(filename);
	CsdpFunctions.setNetworkFilename(_filename);
	CsdpFunctions.setNetworkFiletype(_filetype);
    }
    else if(_filechooserState == JFileChooser.CANCEL_OPTION){
	_cancel = true;
	filename = null;
    }else{
	filename = null;
    }
    return filename;
  }//getFilename

  public boolean accessFile(){
    _net   = _app.nReadStore(_gui,
			     CsdpFunctions.getNetworkDirectory().getPath(), 
			     CsdpFunctions.getNetworkFilename()+"."+
			     CsdpFunctions.getNetworkFiletype());
    ((CsdpFrame)_gui).setNetwork(_net); 
    _nplot = _app.setNetworkPlotter();
    ((CsdpFrame)_gui)._canvas1.setNetworkPlotter(_nplot);
    ((CsdpFrame)_gui)._canvas1.setUpdateNetwork(true);
	//removed for conversion to swing
    //    _gui._canvas1.repaint();
    
    ((CsdpFrame)_gui).enableAfterNetwork();
    return true; //no need to warn if it fails
  }//accessFile

} // NOpen

    /**
     * clear network from memory
     */
    public class NClearNetwork implements ActionListener{
	YesNoDialog _d;
	CsdpFrame _gui;
	public NClearNetwork(CsdpFrame gui){
	    _gui = gui;
	    _d = new YesNoDialog(_gui, "Network file is not saved. Save(y/n)?", true);
	}
	public void actionPerformed(ActionEvent e){
	    Network net = _gui.getNetwork();
	    if(net != null){
		if(net.isUpdated()){
		    _d.show();
		    if(_d._yes == true){
			_gui.saveNetwork();
			_app.clearNetwork();
		    }else if(_d._no == true){
			_app.clearNetwork();
		    }else{
			//do nothing
		    }
		}//if network has changed
		else if(net.isUpdated() == false){
		    _app.clearNetwork();
		}//else
	    }//if net isn't null
	    else{
		_app.clearNetwork();
	    }//else clear if network null
	}
    }

/**
 * Save network file
 *
 * @author
 * @version $Id: NetworkMenu.java,v 1.2 2002/10/21 20:02:19 btom Exp $
 */
  public class NSave extends FileSave implements ActionListener {  

    public NSave(CsdpFrame gui){
      super(gui, _saveDialogMessage, _saveErrorMessage, _saveSuccessMessage, 
	    _saveFailureMessage, true, _saveExtensions, _numSaveExtensions);
    }

        public String getCurrentFilename(){
          return CsdpFunctions.getNetworkFilename();
        }
        public String getCurrentFiletype(){
          return CsdpFunctions.getNetworkFiletype();
        }

        public void setFilenameAndType(String filename, String filetype){
          CsdpFunctions.setNetworkFilename(filename);
          CsdpFunctions.setNetworkFiletype(filetype);
        }

    public String getFilename(){
      String filename = CsdpFunctions.getNetworkFilename()+"."+
	CsdpFunctions.getNetworkFiletype();
      parseFilename(filename);
      return filename;
    }//getFilename

    public boolean accessFile(){
	return _app.nSave();
    }
    public boolean accessFile(String filename){
	return _app.nSaveAs(CsdpFunctions.getNetworkDirectory().getPath(), filename);
    }
  }// NSave

  /**
   * Save network file As
   *
   * @author
   * @version $Id: NetworkMenu.java,v 1.2 2002/10/21 20:02:19 btom Exp $
   */
  public class NSaveAs extends FileIO implements ActionListener {  
    public NSaveAs(CsdpFrame gui){
      super(gui, _saveDialogMessage, _saveErrorMessage, _saveSuccessMessage,
	    _saveFailureMessage, true, _saveExtensions, _numSaveExtensions);
      _jfc.setDialogTitle(_saveDialogMessage);
      _jfc.setApproveButtonText("Save");
      _jfc.addChoosableFileFilter(_nSaveFilter);
      _jfc.setFileFilter(_nSaveFilter);
    }

    /**
     * uses a dialog box to get filename from user
     */
    protected String getFilename(){
      int numLines=0;
      String filename=null;
      if(CsdpFunctions.getNetworkDirectory() != null){
	_jfc.setCurrentDirectory(CsdpFunctions.getNetworkDirectory());
      }

      _filechooserState = _jfc.showSaveDialog(_gui);
      if(_filechooserState==JFileChooser.APPROVE_OPTION){
	  filename = _jfc.getName(_jfc.getSelectedFile());
	  CsdpFunctions.setNetworkDirectory
	      (_jfc.getCurrentDirectory().getAbsolutePath()+File.separator);
	  parseFilename(filename);
	  _cancel = false;
	  CsdpFunctions._cancelSaveNetwork = false;
      }else if(_filechooserState == JFileChooser.CANCEL_OPTION){
	  _cancel = true;
	  CsdpFunctions._cancelSaveNetwork = true;
	  filename = null;
      }else{
	  _cancel = true;
	  CsdpFunctions._cancelSaveNetwork = true;
	  filename = null;
      }//if
      return filename;
    }//getFilename

    public boolean accessFile(){
	boolean saved = false;
	if(_cancel == false){
	    saved = _app.nSaveAs(CsdpFunctions.getNetworkDirectory().getPath(), 
				 _filename+"."+_filetype);
	    ((CsdpFrame)_gui).enableAfterNetwork();
	}else{
	    saved = false;
	}
      return saved;
    }

  } // NSaveAs


  /**
   * saves all cross-sections in network file in HEC-2 format.  
   * Name of each cross-section
   * will be channel number + distance along centerline
   *
   * @author
   * @version $Id: NetworkMenu.java,v 1.2 2002/10/21 20:02:19 btom Exp $
   */
  public class NExportToSEFormat extends FileIO implements ActionListener {  
    public NExportToSEFormat(CsdpFrame gui){
      super(gui, _exportDialogMessage, _exportErrorMessage, _exportSuccessMessage,
	    _exportFailureMessage, true, _exportExtensions, _numExportExtensions);
      _jfc.setDialogTitle(_exportDialogMessage);
      _jfc.setApproveButtonText("Export");
      _jfc.addChoosableFileFilter(_nExportFilter);
      _jfc.setFileFilter(_nExportFilter);
    }

    /**
     * uses a dialog box to get filename from user
     */
    protected String getFilename(){
      int numLines=0;
      String filename=null;
      if(CsdpFunctions.getNetworkExportDirectory() != null){
	_jfc.setCurrentDirectory(CsdpFunctions.getNetworkExportDirectory());
      }

      _filechooserState = _jfc.showSaveDialog(_gui);
      if(_filechooserState==JFileChooser.APPROVE_OPTION){
	  filename = _jfc.getName(_jfc.getSelectedFile());
	  CsdpFunctions.setNetworkExportDirectory
	      (_jfc.getCurrentDirectory().getAbsolutePath()+File.separator);
	  parseFilename(filename);
      }else if(_filechooserState == JFileChooser.CANCEL_OPTION){
	  _cancel = true;
	  filename = null;
      }else{
	  filename = null;
      }//if
      return filename;
    }//getFilename

    public boolean accessFile(){
	boolean saved = _app.nExportToSEFormat
	    (CsdpFunctions.getNetworkExportDirectory().getPath(), 
	     _filename+"."+_filetype,CsdpFunctions.getChannelLengthsOnly());
      ((CsdpFrame)_gui).enableAfterNetwork();
      return saved;
    }

  } // NExportToSEFormat

  /**
   * saves all cross-sections in network file in 3D format..  
   * Name of each cross-section
   * will be channel number + distance along centerline
   *
   * @author
   * @version $Id: NetworkMenu.java,v 1.2 2002/10/21 20:02:19 btom Exp $
   */
  public class NExportTo3DFormat extends FileIO implements ActionListener {  
    public NExportTo3DFormat(CsdpFrame gui){
      super(gui, _3dExportDialogMessage, _3dExportErrorMessage, 
	    _3dExportSuccessMessage, _3dExportFailureMessage, 
	    true, _3dExportExtensions, _3dNumExportExtensions);
      _jfc.setDialogTitle(_3dExportDialogMessage);
      _jfc.setApproveButtonText("Export");
      _jfc.addChoosableFileFilter(_3dNExportFilter);
      _jfc.setFileFilter(_3dNExportFilter);
    }

    /**
     * uses a dialog box to get filename from user
     */
    protected String getFilename(){
      int numLines=0;
      String filename=null;
      if(CsdpFunctions.getNetworkExportDirectory() != null){
	_jfc.setCurrentDirectory(CsdpFunctions.getNetworkExportDirectory());
      }

      _filechooserState = _jfc.showSaveDialog(_gui);
      if(_filechooserState==JFileChooser.APPROVE_OPTION){
	  filename = _jfc.getName(_jfc.getSelectedFile());
	  CsdpFunctions.setNetworkExportDirectory
	      (_jfc.getCurrentDirectory().getAbsolutePath()+File.separator);
	  parseFilename(filename);
      }else if(_filechooserState == JFileChooser.CANCEL_OPTION){
	  _cancel = true;
	  filename = null;
      }else{
	  filename = null;
      }//if
      return filename;
    }//getFilename

    public boolean accessFile(){
	boolean saved = _app.nExportTo3DFormat
	    (CsdpFunctions.getNetworkExportDirectory().getPath(), 
	     _filename+"."+_filetype);
      ((CsdpFrame)_gui).enableAfterNetwork();
      return saved;
    }

  } // NExportTo3DFormat



    /**
     * Changes option to print only channel lengths when exporting network data
     * to station/elevation format
     */
    public class NChannelLengthsOnly implements ItemListener{
	public void itemStateChanged(ItemEvent e){
	    if(e.getStateChange() == ItemEvent.SELECTED){
		CsdpFunctions.setChannelLengthsOnly(true);
	    }else{
		CsdpFunctions.setChannelLengthsOnly(false);
	    }
	}

    }//NChannelLengthsOnly

/**
 * Clear network from memory
 *
 * @author
 * @version $Id: NetworkMenu.java,v 1.2 2002/10/21 20:02:19 btom Exp $
 */
  ///public class NClear implements ActionListener {  
  ///App _app;
  ///CsdpFrame _gui;
  ///NetworkPlot _plot;
  ///public NClear(App app, CsdpFrame gui) {
  ///_app = app;
  ///_gui = gui;
  ///}
  ///public void actionPerformed(ActionEvent e) {
 ///}  
///} // NClear

/**
 * Calculate network file:  calculate cross-section properties for all cross-
 * sections in network and write to individual ascii files.
 *
 * @author
 * @version $Id: NetworkMenu.java,v 1.2 2002/10/21 20:02:19 btom Exp $
 */
    public class NCalculate implements ActionListener {  
	//    FileDialog _fdCalculate;
	OkDialog _okd;
	JFileChooser _xsectDirectoryChooser = new JFileChooser();
	CsdpFrame _gui;
	public NCalculate(CsdpFrame gui){
	    _gui = gui;
	    //      _fdCalculate = 
	    //new FileDialog(gui,"Calculate network: select directory for output files");
	    _xsectDirectoryChooser.setDialogTitle
		("Calculate network: select directory for output files");
	    _xsectDirectoryChooser.setApproveButtonText("Use this directory");
	    _xsectDirectoryChooser.setFileSelectionMode(JFileChooser.DIRECTORIES_ONLY);
	    _okd = new OkDialog(gui, "Cross-section files written", true);
	    _okd.setTitle("done");
	}
	
	public void actionPerformed(ActionEvent e) {
	    String filename        = null;
	    //      _fdCalculate.show();
	    
	    String calculateDirectory = null;
	    int filechooserState = -CsdpFunctions.BIG_INT;
	    
	    filechooserState = _xsectDirectoryChooser.showOpenDialog(_gui);
	    if(filechooserState == JFileChooser.APPROVE_OPTION){
		
		//      if(_fdCalculate.getDirectory() != null && _fdCalculate.getFile() != null){
		////	calculateDirectory = _fdCalculate.getDirectory();
		calculateDirectory = _xsectDirectoryChooser.
		    getCurrentDirectory().getAbsolutePath()+File.separator;
		calculateDirectory += _xsectDirectoryChooser.
		    getName(_xsectDirectoryChooser.getSelectedFile())+
		    File.separator;

		System.out.println("calculateDirectory="+calculateDirectory);

		//CsdpFunctions.setCalculateDirectory(_fdCalculate.getDirectory());
		//	if(DEBUG)System.out.println
		//   ("directory for output="+CsdpFunctions.getCalculateDirectory());
		/////	filename=_fdCalculate.getFile();
		
		//	_app.nCalculate(CsdpFunctions.getCalculateDirectory());
		_app.nCalculate(calculateDirectory);
		_app.writeIrregularXsectsInp(calculateDirectory);
		_app.writeXsectLandmark();
		_okd.show();
	    }
	}//actionPerformed
    } // NCalculate

  ////KEEP THIS CLASS--IT MIGHT BE NEEDED SOMEDAY
// //   public class NetworkFilenameFilter implements FilenameFilter{
// //     public boolean accept(File dir, String name){
// //       boolean returnValue = false;
// //       parseFilename(name);
// //       if(_filetype.equals(NETWORK_FILETYPE)){
// // 	if(DEBUG)System.out.println("match found");
// // 	returnValue = true;
// //       }
// //       else returnValue = false;
// //       return returnValue;
// //     }
// //   }//class NetworkFilenameFilter

  App _app;
  protected static final boolean DEBUG = false;

  protected static final String _openDialogMessage = "Select network(.cdn) file";
  protected static final String _openErrorMessage = "Only .cdn extension allowed";
  protected static final String[] _openExtensions = {"cdn"};
  protected static final int _numOpenExtensions = 1;

  protected static final String _saveDialogMessage = "Save Network(.cdn) file";
  protected static final String _saveErrorMessage = "Only .cdn extension allowed";
  protected static final String[] _saveExtensions = {"cdn"};
  protected static final int _numSaveExtensions = 1;

    protected static final String _saveSuccessMessage = "Saved network file";
    protected static final String _saveFailureMessage = "ERROR:  NETWORK FILE NOT SAVED!";
    protected static final String _openSuccessMessage = "";
    protected static final String _openFailureMessage = "ERROR:  couldn't open network file";

    protected static final String _exportDialogMessage = 
	"Export Network to Station/Elevation format(.se) file";
    protected static final String _exportErrorMessage = "Only .se extension allowed";
    protected static final String[] _exportExtensions = {"se"};
    protected static final int _numExportExtensions = 1;
    
    protected static final String _exportSuccessMessage = 
	"Exported network to station/elevation format";
    protected static final String _exportFailureMessage = "ERROR:   EXPORT FAILED!";

    protected static final String _3dExportDialogMessage = 
	"Export Network to 3D format(.3dn) file";
    protected static final String _3dExportErrorMessage="Only .3dn extension allowed";
    protected static final String[] _3dExportExtensions = {"3dn"};
    protected static final int _3dNumExportExtensions = 1;
    
    protected static final String _3dExportSuccessMessage = 
	"Exported network to 3D format";
    protected static final String _3dExportFailureMessage = "ERROR:   EXPORT FAILED!";

    
    CsdpFileFilter _nOpenFilter;
    CsdpFileFilter _nSaveFilter;
    CsdpFileFilter _nExportFilter;
    CsdpFileFilter _3dNExportFilter;
    
    int _filechooserState = -CsdpFunctions.BIG_INT;
} // class NetworkMenu


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
    Chung@water.ca.gov

    or see our home page: http://wwwdelmod.water.ca.gov/
*/
package DWR.CSDP;
import DWR.CSDP.dialog.*;
import java.awt.event.*;
import java.awt.*;
import java.io.*;
import javax.swing.*;

public class FileMenu{

  public FileMenu(App app){
    _app = app;
    _fOpenFilter=new CsdpFileFilter(_openExtensions, _numOpenExtensions);
    _fSaveFilter=new CsdpFileFilter(_saveExtensions, _numSaveExtensions);
  }
/**
 * Get bathymetry data filename and call functions to open, read, and display it.
 *
 * @author
 * @version $Id: FileMenu.java,v 1.2 2002/10/21 20:02:19 btom Exp $
 */
  public class FOpen extends FileIO implements ActionListener {

    public FOpen(CsdpFrame gui){
      super(gui, _openDialogMessage, _openErrorMessage, _openSuccessMessage,
	    _openFailureMessage, false, _openExtensions, _numOpenExtensions);
      _jfc.setDialogTitle(_openDialogMessage);
      _jfc.setApproveButtonText("Open");
      _jfc.addChoosableFileFilter(_fOpenFilter);
      _jfc.setFileFilter(_fOpenFilter);
    }

    /**
     * uses dialog box to get filename from user
     */
    protected String getFilename(){
    int numLines=0;
    String filename=null;
    int filechooserState = -CsdpFunctions.BIG_INT;
    if(CsdpFunctions.getBathymetryDirectory() != null){
	//      _fd.setDirectory(CsdpFunctions.getBathymetryDirectory());
	_jfc.setCurrentDirectory(CsdpFunctions.getBathymetryDirectory());
    }
    else if(CsdpFunctions.getOpenDirectory() != null){
	//      _fd.setDirectory(CsdpFunctions.getOpenDirectory());
	_jfc.setCurrentDirectory(CsdpFunctions.getOpenDirectory());
    }
    else System.out.println();
    //    _fd.show();
    //    filename = _fd.getFile();

    filechooserState = _jfc.showOpenDialog(_gui);
    if(filechooserState== JFileChooser.APPROVE_OPTION){
	filename = _jfc.getName(_jfc.getSelectedFile());
	////      CsdpFunctions.setBathymetryDirectory(_fd.getDirectory());
	////CsdpFunctions.setOpenDirectory(_fd.getDirectory());
      CsdpFunctions.setBathymetryDirectory
	  (_jfc.getCurrentDirectory().getAbsolutePath()+File.separator);
      CsdpFunctions.setOpenDirectory
	  (_jfc.getCurrentDirectory().getAbsolutePath()+File.separator);
      parseFilename(filename);
    }else if(filechooserState == JFileChooser.CANCEL_OPTION){
	_cancel = true;
	filename = null;
    }else{
	filename = null;
    }//else
    return filename;
  }//getFilename

    /**
     * called by superclass.  reads file
     */
    public boolean accessFile(){
      _plot = _app.bReadStore(_gui, CsdpFunctions.getBathymetryDirectory().getPath(),
			      _filename, _filetype);
      return true; //no need to warn user if fails
    }
    
  } // FOpenClass

  /**
   * Exit the program
   *
   * @author
   * @version $Id: FileMenu.java,v 1.2 2002/10/21 20:02:19 btom Exp $
   */
    public class FExit extends WindowAdapter implements ActionListener {  
	CsdpFrame _gui;
	public FExit(CsdpFrame gui){
	    _gui = gui;
	}
	
	public void windowClosing(WindowEvent e){
	    exitProgram();
	}
	
	public void actionPerformed(ActionEvent e) {
	    exitProgram();
	}//actionPerformed
	
	private void exitProgram(){
	    Network net = _gui.getNetwork();
	    if(net != null){
		if(net.isUpdated()){
		    YesNoDialog d = new YesNoDialog
			(_gui, "Network file is not saved.  Save(y/n)?", true);
		    d.show();
		    if(d._yes == true){
			_gui.saveNetwork();
			if(CsdpFunctions._cancelSaveNetwork == false)System.exit(0);
		    }
		    else if(d._no == true){
			System.exit(0);
		    }else{
			//do nothing
		    }
		}//if network has changed
		else if(net.isUpdated() == false){
		    System.exit(0);
		}//else
	    }//if net isn't null
	    else{
		System.exit(0);
	    }//else exit if network null
	}//exitProgram
    }//class FExit
    
  /**
   * Setup print options
   *
   * @author
   * @version $Id: FileMenu.java,v 1.2 2002/10/21 20:02:19 btom Exp $
   */
  public class FPrintSetup implements ActionListener {  
    public void actionPerformed(ActionEvent e) {
    }
  }//class FPrintSetup
  
  /**
   * print displayed data
   *
   * @author
   * @version $Id: FileMenu.java,v 1.2 2002/10/21 20:02:19 btom Exp $
   */
  public class FPrint implements ActionListener {  
    public void actionPerformed(ActionEvent e) {
    }
  }//FPrint
  
  /**
   * Preview
   *
   * @author
   * @version $Id: FileMenu.java,v 1.2 2002/10/21 20:02:19 btom Exp $
   */
  public class FPrintPreview implements ActionListener {  
    public void actionPerformed(ActionEvent e) {
    }
  }//FPrintPreview

   /**
    * Extract bathymetry data
    *
    * @author
    * @version $Id: FileMenu.java,v 1.2 2002/10/21 20:02:19 btom Exp $
    */
  //   public class FExtract extends FSaveAs implements ActionListener {  
//     public FExtract(CsdpFrame gui){
//       super(gui, _saveDialogMessage, _saveErrorMessage, _saveExtensions,
// 	    _numSaveExtensions);
//       c = gui.getExtractRegion();
//     }//constructor
//     /**
//      * called by superclass.  saves file
//      */
//     public void accessFile(){
//       if(_filename != null){
// 	_app.fSave(CsdpFunctions.getBathymetryDirectory(), 
// 		   _filename+"."+_filetype,c);
//       }//if
//     }//accessFile
//     /*
//      * stores coordinates of extract region
//      */
//     float[] c;
  //   }//FExtract

  /**
   * Save bathymetry in file with specified name
   *
   * @author
   * @version $Id: FileMenu.java,v 1.2 2002/10/21 20:02:19 btom Exp $
   */
  public class FSaveAs extends FileIO implements ActionListener {  

    public FSaveAs(CsdpFrame gui){
      super(gui, _saveDialogMessage, _saveErrorMessage, _saveSuccessMessage,
	    _saveFailureMessage, true, _saveExtensions, _numSaveExtensions);
      _jfc.setDialogTitle(_saveDialogMessage);
      _jfc.setApproveButtonText("Save");
      _jfc.addChoosableFileFilter(_fSaveFilter);
      _jfc.setFileFilter(_fSaveFilter);

    }

    /**
     * uses a dialog box to get filename from user
     */
    protected String getFilename(){
      int numLines=0;
      String filename=null;
      if(CsdpFunctions.getBathymetryDirectory() != null){
	  //	  _fd.setDirectory(CsdpFunctions.getBathymetryDirectory());
	  _jfc.setCurrentDirectory(CsdpFunctions.getBathymetryDirectory());
      }
      else System.out.println("error in FileMenu.FSaveAs:no bathymetry directory");
      ////      _fd.show();
      int filechooserState = _jfc.showSaveDialog(_gui);
      if(filechooserState == JFileChooser.APPROVE_OPTION){
	  filename=_jfc.getName(_jfc.getSelectedFile());
	  ////      filename = _fd.getFile();
	  //      CsdpFunctions.setBathymetryDirectory(_fd.getDirectory());
	  CsdpFunctions.setBathymetryDirectory
	      (_jfc.getCurrentDirectory().getAbsolutePath()+File.separator);
	  parseFilename(filename);
      }else if(filechooserState == JFileChooser.CANCEL_OPTION){
	  _cancel=true;
	  filename=null;
      }else{
	  filename=null;
      }



      return filename;
    }//getFilename
    
      /**
       * called by superclass.  saves file
       */
      public boolean accessFile(){
	  boolean success = false;
	  if(_filename != null){
	      success = _app.fSave(CsdpFunctions.getBathymetryDirectory().getPath(), 
				   _filename+"."+_filetype);
	  }//if
	  return success;
      }//accessFile
  }//FSaveAs

  /**
   * Save Bathymetry Data in file with same name.  This feature is not necessary because
   * the program currently does not allow editing of bathymetry data.
   *
   * @author
   * @version $Id: FileMenu.java,v 1.2 2002/10/21 20:02:19 btom Exp $
   */
  public class FSave implements ActionListener {  
    public void actionPerformed(ActionEvent e) {
    }
  }//FSave

  /**
   * Clear bathymetry data from memory
   *
   * @author
   * @version $Id: FileMenu.java,v 1.2 2002/10/21 20:02:19 btom Exp $
   */
  public class FClose implements ActionListener {  
    public void actionPerformed(ActionEvent e) {
    }
  }//FClose

  App _app;
  BathymetryPlot _plot;
  protected static final boolean DEBUG = false;
  protected static final String _openDialogMessage = 
    "Select bathymetry(.prn, .cdp, .cdp.gz) file";
  protected static final String _openErrorMessage = 
    "Only .prn, .cdp, .cdp.gz extensions allowed";
  protected static final String[] _openExtensions = {"prn","cdp","cdp.gz"};
  protected static final int _numOpenExtensions = 3;

  protected static final String _saveDialogMessage = "Save Bathymetry(.cdp) file";
  protected static final String _saveErrorMessage = "Only .cdp, .prn extensions allowed";
  protected static final String[] _saveExtensions = {"cdp","prn"};
  protected static final int _numSaveExtensions = 2;
    protected static final String _saveSuccessMessage = "Saved bathymetry file";
    protected static final String _saveFailureMessage = "ERROR:  BATHYMETRY FILE NOT SAVED!";
    protected static final String _openSuccessMessage = "";
    protected static final String _openFailureMessage = "ERROR: couldn't open bathymetry file";

    CsdpFileFilter _fOpenFilter;
    CsdpFileFilter _fSaveFilter;
}//class FileMenu

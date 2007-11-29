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

public class PropertiesMenu{

  public PropertiesMenu(App app){
    _app = app;
    _pOpenFilter = new CsdpFileFilter(_openExtensions, _numOpenExtensions);
    _pSaveFilter = new CsdpFileFilter(_saveExtensions, _numSaveExtensions);
  }
/**
 * Get properties filename and call functions to open, read, and display it.
 *
 * @author
 * @version $Id:
 */
  public class PLoad extends FileIO implements ActionListener {  
    public PLoad(CsdpFrame gui){
      super(gui, _openDialogMessage, _openErrorMessage, _openSuccessMessage,
	    _openFailureMessage, false, _openExtensions, _numOpenExtensions);
      _jfc.setDialogTitle(_openDialogMessage);
      _jfc.setApproveButtonText("Open");
      _jfc.addChoosableFileFilter(_pOpenFilter);
      _jfc.setFileFilter(_pOpenFilter);
    }

     /**
      * uses dialog box to get filename from user
      */
     protected String getFilename(){
     int numLines=0;
     String filename=null;
     if(CsdpFunctions.getPropertiesDirectory() != null){
	 //       _fd.setDirectory(CsdpFunctions.getPropertiesDirectory());
	 _jfc.setCurrentDirectory(CsdpFunctions.getPropertiesDirectory());
     }
     else if(CsdpFunctions.getOpenDirectory() != null){
	 //       _fd.setDirectory(CsdpFunctions.getOpenDirectory());
	 _jfc.setCurrentDirectory(CsdpFunctions.getOpenDirectory());
     }
     else System.out.println("no file selected");
     //     _fd.show();

     //     filename = _fd.getFile();
//       if(filename != null){
//         CsdpFunctions.setPropertiesDirectory(_fd.getDirectory());
//         CsdpFunctions.setOpenDirectory(_fd.getDirectory());
//         parseFilename(filename);
//         CsdpFunctions.setPropertiesFilename(_filename);
//         CsdpFunctions.setPropertiesFiletype(_filetype);
//       }
     _filechooserState = _jfc.showOpenDialog(_gui);
     if(_filechooserState == JFileChooser.APPROVE_OPTION){
	 filename = _jfc.getName(_jfc.getSelectedFile());
         CsdpFunctions.setPropertiesDirectory
	     (_jfc.getCurrentDirectory().getAbsolutePath()+File.separator);
         CsdpFunctions.setOpenDirectory
	     (_jfc.getCurrentDirectory().getAbsolutePath()+File.separator);
         parseFilename(filename);
         CsdpFunctions.setPropertiesFilename(_filename);
         CsdpFunctions.setPropertiesFiletype(_filetype);
     }
     else if(_filechooserState == JFileChooser.CANCEL_OPTION){
	 _cancel = true;
	 filename = null;
     }else{
	 filename = null;
     }
     return filename;
   }//getFilename

    /**
     * read properties file
     */
    public boolean accessFile(){
      _app.pReadStore(_gui, CsdpFunctions.getPropertiesDirectory().getPath(), 
		      _filename, _filetype);
      ((CsdpFrame)_gui).enableAfterProperties();
      return true;  //no need to warn if it fails.
    }//accessFile
    
  } // PLoad class

  /**
   * Save properties in file with specified name
   *
   * @author
   * @version $Id:
   */
  public class PSaveAs extends FileIO implements ActionListener {  
    public PSaveAs(CsdpFrame gui){
      super(gui, _saveDialogMessage, _saveErrorMessage, _saveSuccessMessage,
	    _saveFailureMessage, true, _saveExtensions, _numSaveExtensions);
      _jfc.setDialogTitle(_saveDialogMessage);
      _jfc.setApproveButtonText("Save");
      _jfc.addChoosableFileFilter(_pSaveFilter);
      _jfc.setFileFilter(_pSaveFilter);
    }

    /**
     * uses a dialog box to get filename from user
     */
    protected String getFilename(){
      int numLines=0;
      String filename=null;
      if(CsdpFunctions.getPropertiesDirectory() != null){
	  //	_fd.setDirectory(CsdpFunctions.getPropertiesDirectory());
	_jfc.setCurrentDirectory(CsdpFunctions.getPropertiesDirectory());
      }
      //      _fd.show();
      //      filename = _fd.getFile();
//        CsdpFunctions.setPropertiesDirectory(_fd.getDirectory());
//        parseFilename(filename);
//        CsdpFunctions.setPropertiesFilename(_filename);
//        CsdpFunctions.setPropertiesFiletype(_filetype);
      _filechooserState = _jfc.showSaveDialog(_gui);
      if(_filechooserState==JFileChooser.APPROVE_OPTION){
	  filename=_jfc.getName(_jfc.getSelectedFile());
	  CsdpFunctions.setPropertiesDirectory
	      (_jfc.getCurrentDirectory().getAbsolutePath()+File.separator);
	  parseFilename(filename);
	  CsdpFunctions.setPropertiesFilename(_filename);
	  CsdpFunctions.setPropertiesFiletype(_filetype);
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
      boolean success = 
	  _app.pSaveAs(CsdpFunctions.getPropertiesDirectory().getPath(), 
		       _filename+"."+_filetype);
      ((CsdpFrame)_gui).enableAfterProperties();
      return success;
    }//accessFile
  }//PSaveAs

  /**
   * Save Properties Data in file with same name
   *
   * @author
   * @version $Id:
   */
  public class PSave extends FileSave implements ActionListener {  

    public PSave(CsdpFrame gui){
      super(gui, _saveDialogMessage, _saveErrorMessage, _saveSuccessMessage,
	    _saveFailureMessage, true, _saveExtensions, _numSaveExtensions);
    }

    public String getCurrentFilename(){
      return CsdpFunctions.getPropertiesFilename();
    }
    public String getCurrentFiletype(){
      return CsdpFunctions.getPropertiesFiletype();
    }

    public void setFilenameAndType(String filename, String filetype){
      CsdpFunctions.setPropertiesFilename(filename);
      CsdpFunctions.setPropertiesFiletype(filetype);
    }

    public String getFilename(){
      String filename = CsdpFunctions.getPropertiesFilename()+"."+
	CsdpFunctions.getPropertiesFiletype();
      parseFilename(filename);
      return filename;
    }//getFilename

    public boolean accessFile(){
      boolean success = _app.pSave();
      if(DEBUG){
	if(success ==  false) System.out.println("save properties failed");
	if(success) System.out.println("save properties succeeded");
      }//if
      return success;
    }//accessFile

    public boolean accessFile(String filename){
      boolean success = _app.pSaveAs(CsdpFunctions.getPropertiesDirectory().getPath(), 
				     filename);
      return success;
    }//accessFile

  }//PSave

  App _app;
  BathymetryPlot _plot;
  protected static final boolean DEBUG = false;
  protected static final String _openDialogMessage = "Select properties(.prp) file";
  protected static final String _openErrorMessage = "Only .prp extension allowed";
  protected static final String[] _openExtensions = {"prp"};
  protected static final int _numOpenExtensions = 1;

  protected static final String _saveDialogMessage = "Save Properties(.prp) file";
  protected static final String _saveErrorMessage = "Only .prp extension allowed";
  protected static final String[] _saveExtensions = {"prp"};
  protected static final int _numSaveExtensions = 1;

    protected static final String _saveSuccessMessage = "saved properties file";
    protected static final String _saveFailureMessage = "ERROR:  UNABLE TO SAVE PROPERTIES FILE";
    protected static final String _openSuccessMessage = "";
    protected static final String _openFailureMessage = "ERROR:  couldn't open properties file";
    CsdpFileFilter _pOpenFilter;
    CsdpFileFilter _pSaveFilter;
    int _filechooserState = -CsdpFunctions.BIG_INT;
}//class PropertiesMenu

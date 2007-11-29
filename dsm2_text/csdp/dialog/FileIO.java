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
package DWR.CSDP.dialog;
import java.awt.event.*;
import java.awt.*;
import java.io.*;
import javax.swing.*;

public abstract class FileIO{

  public FileIO(JFrame gui, String dialogMessage, String errorMessage, 
		String successMessage, String failureMessage, boolean reportSuccess,
		String[] fileExtensions, int numExtensions){
    _gui = gui;

    //    _fd = new FileDialog(_gui, dialogMessage);

    _jfc = new JFileChooser();

    _errorMessage = errorMessage;
    _successMessage = successMessage;
    _reportSuccess = reportSuccess;
    _failureMessage = failureMessage;
    _fileExtensions = fileExtensions;
    _numExtensions = numExtensions;
    _errorDialog   = new OkDialog(_gui, _errorMessage, true);
    _successDialog = new OkDialog(_gui, _successMessage, true);
    _failureDialog = new OkDialog(_gui, _failureMessage, true);
  }

    /**
     * call methods to open, read, and store data files, and to plot/display data
     */
    public void actionPerformed(ActionEvent e) {
	_cancel = false;
	boolean success = false;
	String filename = null;
	while(filename == null && _cancel == false) {
	    String fname = getFilename();
	    if(_cancel == false){
		if(fname == null || fname.length() == 0){
		    //not necessary--use JFileChooser.CANCEL_OPTION in subclass
		    //		_cancel = true;
		    _errorDialog.setMessage("no file selected!");
		    _errorDialog.show();
		}else{
		    if(accept(fname) == false){
			fname = null;
			_errorDialog.setMessage(_errorMessage);
			_errorDialog.show();
		    }
		}//else
	    }//not cancelling
	    filename = fname;
	}//while

	if(filename != null && _cancel == false){
	    success = accessFile();
	    if(success == false) _failureDialog.show();
	}//if
	_cancel=false;
	if(_reportSuccess && success == true){
	    _successDialog.show();
	}
	//	}//while
    }//actionPerformed
  
  /**
   * calls the method in the application class
   */
  public abstract boolean accessFile();

    /*
     * accept is really supposed to be used by FilenameFilter, but since the
     * FilenameFilter doesn't work, I am calling it directly.
     */
    public boolean accept(String name){
      parseFilename(name);
      boolean value = false;
      if(_filetype != null){
        for(int i=0; i<=_numExtensions-1; i++){
  	if(_filetype.equals(_fileExtensions[i])) value = true;
        }//for
      }//if
      return value;
    }//accept
  
  /**
   * separates filename into prefix and extension
   */
  protected void parseFilename(String filename) throws NullPointerException{
    try{
      int dotIndex = filename.indexOf(".",0);
      if(dotIndex >= 0){
	_filename = filename.substring(0,dotIndex);
	_filetype = filename.substring(dotIndex+1);
      }
      else if(dotIndex < 0){
	_filename = null;
	_filetype = null;
      }
    } catch (Exception e) {
      System.out.println("no filename specified");
    }//catch
  }//parseFilename

  protected abstract String getFilename();

    public JFrame _gui;
    //    FileDialog _fd;
    public JFileChooser _jfc;
    
  String _errorMessage;
    String _successMessage;
    String _failureMessage;
  String[] _fileExtensions;
  int _numExtensions;
    public String _filename;
    public String _filetype;
    protected boolean _cancel = false;
    public OkDialog _errorDialog, _successDialog, _failureDialog;
    public boolean _reportSuccess;
}//class FileIO

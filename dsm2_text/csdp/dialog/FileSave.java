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
import javax.swing.JFrame;
/**
 * This is the superclass of all classes that implement a file-save feature, which
 * saves a file to the directory in/from which it was last saved/read.
 *
 * @author
 * @version $Id: FileSave.java,v 1.2 2002/10/21 20:12:30 btom Exp $
 */
public abstract class FileSave extends FileIO{

  public FileSave(JFrame gui, String dialogMessage, String errorMessage, 
		  String successMessage, String failureMessage, boolean reportSuccess,
		  String[] extensions, int numExtensions){
    super(gui, dialogMessage, errorMessage, successMessage, failureMessage, reportSuccess,
	  extensions, numExtensions);
    _reportSuccess = reportSuccess;
  }//FileSave

  public void actionPerformed(ActionEvent e){
      boolean success = false;
    String filename = getCurrentFilename();
    String filetype = getCurrentFiletype();
    String newFilename = null;
    if(filename == null && filetype == null){
      while(filename == null && _cancel == false){
	newFilename = getFilename();
	//	if(accept(newFilename) == false){
	//  if(newFilename == null) _cancel = true;
	//  newFilename = null;
	//  _okd.show();
	//}//if
	filename = newFilename;
      }//while
      if(filename != null){
	  success = accessFile(filename);
	  setFilenameAndType(filename,filetype);
      }//if
    }//if no filename
    else if(filename != null){
	success = accessFile();
	setFilenameAndType(filename,filetype);
    }//else
    //    else printErrorMessage();
    if(_reportSuccess){
	if(success == true){
	    _successDialog.show();
	}else{
	    _failureDialog.show();
	}//if
    }//if

  }//actionPerformed

  public abstract String getCurrentFilename();
  public abstract String getCurrentFiletype();
  public abstract void setFilenameAndType(String filename, String filetype);
  public abstract boolean accessFile(String filename);
  public abstract boolean accessFile();
    protected boolean reportSuccess;
  //  public abstract void printErrorMessage();
}//FileSave

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
 * This is the superclass of all classes that implement a file-open feature that
 * checks to see if a similar filetype is already open.
 *
 * @author
 * @version $Id:
 */
public abstract class FileOpen extends FileIO{

  public FileOpen(JFrame gui, String dialogMessage, String errorMessage, 
		  String successMessage, String failureMessage, boolean reportSuccess,
		  String[] extensions, int numExtensions){
    super(gui, dialogMessage, errorMessage, successMessage, failureMessage, reportSuccess,
	  extensions, numExtensions);
  }

  public void actionPerformed(ActionEvent e){
    String filename=null;
    boolean success = false;
    warnUserIfNecessary();
    //the filename filter doesn't work in 1.1.4. maybe in 1.2?
    //    fd.setFilenameFilter(((FilenameFilter)(new NetworkFilenameFilter())));

    while(filename == null && _cancel == false){
      String fname = getFilename();
      if(fname == null){
	_cancel = true;
	//      }
      //      else if(accept(fname) == false){
      //	fname = null;
      //	_okd.show();
      }
      filename = fname;
    }//while
    if(filename != null && _cancel != false){
	success = accessFile();
    }
    _cancel = false;
    
  }

  public abstract void warnUserIfNecessary();

}//class FileOpen

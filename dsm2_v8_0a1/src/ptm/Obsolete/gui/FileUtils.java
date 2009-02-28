//    Copyright (C) 1996 State of California, Department of Water
//    Resources.
//
//    Delta Simulation Model 2 (DSM2): A River, Estuary, and Land
//    numerical model.  No protection claimed in original FOURPT and
//    Branched Lagrangian Transport Model (BLTM) code written by the
//    United States Geological Survey.  Protection claimed in the
//    routines and files listed in the accompanying file "Protect.txt".
//    If you did not receive a copy of this file contact 
//    Tara Smith, below.
//
//    This program is licensed to you under the terms of the GNU General
//    Public License, version 2, as published by the Free Software
//    Foundation.
//
//    You should have received a copy of the GNU General Public License
//    along with this program; if not, contact Tara Smith, below,
//    or the Free Software Foundation, 675 Mass Ave, Cambridge, MA
//    02139, USA.
//
//    THIS SOFTWARE AND DOCUMENTATION ARE PROVIDED BY THE CALIFORNIA
//    DEPARTMENT OF WATER RESOURCES AND CONTRIBUTORS "AS IS" AND ANY
//    EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
//    IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
//    PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE CALIFORNIA
//    DEPARTMENT OF WATER RESOURCES OR ITS CONTRIBUTORS BE LIABLE FOR
//    ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
//    CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT
//    OR SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA OR PROFITS; OR
//    BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
//    LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
//    (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE
//    USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH
//    DAMAGE.
//
//    For more information about DSM2, contact:
//
//    Tara Smith
//    California Dept. of Water Resources
//    Division of Planning, Delta Modeling Section
//    1416 Ninth Street
//    Sacramento, CA  95814
//    916-653-9885
//    tara@water.ca.gov
//
//    or see our home page: http://baydeltaoffice.water.ca.gov/modeling/deltamodeling/

package DWR.DMS.PTM.gui;
import java.io.*;
import javax.swing.*;

/**
 * @author Aaron Miller
 * @version $Id: FileUtils.java,v 1.4 2000/08/07 17:34:11 miller Exp $
 */

public class FileUtils {
  JFrame frame;
  String lastDir = "./";
  //  String fileExtension;

  public static int YES_OPTION = 0;
  public static int NO_OPTION = 1;
  public static int CANCEL_OPTION = 2;

  public FileUtils(JFrame frame){
    this.frame = frame;
  }

  public String getFileInfo(String label, int type, String extension) {

    boolean replaceFile = false;
    String filename = null;
    JFileChooser dBox = new JFileChooser(lastDir);
    dBox.setLocation(200,100);
    PTMFileFilter filter = new PTMFileFilter(extension);
    dBox.addChoosableFileFilter(filter);
    dBox.setFileFilter(filter);
    dBox.setDialogType(type);
    while (! replaceFile){
      int returnVal = dBox.showDialog(frame,label);

      if (returnVal == JFileChooser.APPROVE_OPTION) {
	filename = dBox.getSelectedFile().getName();
	//	filename = checkFileExtension(filename,"xml");
	filename = checkFileExtension(filename,extension);
	lastDir = dBox.getCurrentDirectory().getAbsolutePath();
	//      System.out.println(lastDir);
	if (checkFile(filename) && type == JFileChooser.SAVE_DIALOG){
	  int retval = JOptionPane.showConfirmDialog(frame, "File Exists, Over Write?",
						     "File Exists!",JOptionPane.YES_NO_CANCEL_OPTION);
	  if (retval == 0) replaceFile = true;
	  else if(retval == 2) {
	    replaceFile = true;
	    filename = null;
	  }
	}
	else { replaceFile = true; }
	if (type == JFileChooser.OPEN_DIALOG) replaceFile = true;
      }
      else { replaceFile = true; }
    }
      System.out.println( lastDir + File.separator +  filename);
      if (filename != null) return lastDir + File.separator + filename;
      else return null;
  }
  
  private boolean checkFile(String filenm){
    File tmpfile = new File(filenm);
    return tmpfile.isFile();
  }

  public String checkFileExtension(String filename, String extension){
    String ext = null;
    String tmpFilename = filename;
    int i = filename.lastIndexOf(".");

    if (i > 0 &&  i < filename.length() - 1) {
      ext = filename.substring(i+1).toLowerCase();
      if(! ext.equalsIgnoreCase(extension)){
	tmpFilename = filename.substring(0,i+1)+extension;
      }
    }
    else {
      tmpFilename=filename+"."+extension;
    }
    return tmpFilename;
  } 
  
  public static String getExtension(File f) {
    String ext = null;
    String s = f.getName();
    int i = s.lastIndexOf('.');

    if (i > 0 &&  i < s.length() - 1) {
      ext = s.substring(i+1).toLowerCase();
    }
    return ext;
  }

  public int warnFileNotSaved(){
    int retval = JOptionPane.showConfirmDialog(frame, "The Current File Has Not Been Saved.\n"+
					       "Do You Want To Save?",
					       "Save Current File",JOptionPane.YES_NO_CANCEL_OPTION);
    return retval;
  }

}

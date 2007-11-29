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
import java.io.*;
import java.util.*;

/**
 * Write ascii and binary properties data
 *
 * @author
 * @version $Id: PropertiesOutput.java,v 1.2 2002/10/21 20:02:02 btom Exp $
 */
public abstract class PropertiesOutput {

public static final boolean DEBUG = false;

  /**
   * Make instance of subclass of PropertiesOutput
   */
public static PropertiesOutput getInstance(CsdpFrame gui, String directory, 
					   String filename, String filetype) {
  _gui = gui;
  _directory = directory;
    if((_directory.substring(_directory.length()-1,_directory.length())).
       equals(File.separator) == false){
	_directory += File.separator;
    }
  _filename = filename;
  _filetype = filetype;
  PropertiesOutput output = null;
  if (_filetype.equals(ASCII_TYPE)) {
    output = new PropertiesAsciiOutput();
  }
  else {// throw new IllegalInputFileException(msg);
    System.out.println("No properties filetype defined for "+_filetype);
    _filetype = null;
  }
  return output;
} //getInstance

  /**
   * Calls appropriate write method to write properties data
   */
public boolean writeData(){
  boolean success = false;
  open();
  success = write();
  close();
  return success;
}

  /**
   * Open file
   */
protected abstract void open();
  /**
   * write file
   */
protected abstract boolean write();
  /**
   * Close file
   */
protected abstract void close();

protected static final String ASCII_TYPE = "prp";
protected static String _filename = null; // part of filename before the first dot
protected static String _filetype = null; // filename extension (after first dot)
protected static String _directory = null;
  protected static CsdpFrame _gui;
} // PropertiesOutput

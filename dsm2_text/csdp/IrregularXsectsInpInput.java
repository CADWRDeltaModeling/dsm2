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
 * Read the DSM2 file channels.inp, which contains node/channel connectivity information
 *
 * @author
 * @version
 */
public abstract class IrregularXsectsInpInput {

  /**
   * Make instance of subclass of IrregularXsectsInpInput
   */
public static IrregularXsectsInpInput getInstance(String directory, String filename) {
  _directory = directory;
    if((_directory.substring(_directory.length()-1,_directory.length())).
       equals(File.separator) == false){
	_directory += File.separator;
    }
  int dotIndex = filename.indexOf(".",0);
  _filename = filename.substring(0,dotIndex);
  _filetype = filename.substring(dotIndex+1);
  IrregularXsectsInpInput input = null;
  if (_filetype.equals(ASCII_TYPE)) {
    input = new IrregularXsectsInpAsciiInput();
  }
  else {// throw new IllegalInputFileException(msg);
    System.out.println("No IrregularXsectsInp filetype defined for "+_filetype);
    _filetype = null;
  }
  return input;
} //getInstance

  /**
   * Calls appropriate read method to read DSMChannels data
   */
public IrregularXsectsInp readData(){
  open();
  read();
  close();
  return _data;
}

  /**
   * Open file
   */
protected abstract void open();
  /**
   * Read file
   */
protected abstract void read();
  /**
   * Close file
   */
protected abstract void close();

    /**
     * Stores a DSMChannels point
     */
    protected void storeData() {
	
	if(DEBUG)System.out.println("storing data. chan, dist, path="+
				    _chanNum+","+_dist+","+_pathname);
	
	_data.addLine(_chanNum, _dist, _pathname);
    }//storeData
    
    IrregularXsectsInp _data = new IrregularXsectsInp();
    //parsed data
    protected String _chanNum = null;
    protected float _dist = -CsdpFunctions.BIG_FLOAT;
    protected String _pathname = null;
    
    public static final boolean DEBUG = false;
    
    protected static String _filename  = null;//part of filename before the first dot
    protected static String _filetype  = null;// filename extension (after first dot)
    protected static final String ASCII_TYPE = "inp";
    protected static String _directory = null;
    protected int _numDSMChannels;

} // class IrregularXsecsInpInput

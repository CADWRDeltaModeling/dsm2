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
 * Write binary bathymetry data.
 *
 * @author
 * @version $Id: BathymetryBinaryOutput.java,v 1.2 2002/10/21 19:58:26 btom Exp $
 */

public class BathymetryBinaryOutput extends BathymetryOutput{

  FileOutputStream _outFile;       // binary output file
  DataOutputStream _binaryOut = null;
  BathymetryData _data     = null;

  /**
   * assigns data storage object to class variable
   */
  BathymetryBinaryOutput(BathymetryData data){
    _data = data;
  }

  /**
   * Open binary bathymetry output file
   */
protected void open(){
  try {
    if((_directory.substring(_directory.length()-1,_directory.length())).
       equals(File.separator) == false){
	_directory += File.separator;
    }
    _outFile   = new FileOutputStream(_directory + _filename+"."+BINARY_TYPE);
    _binaryOut = new DataOutputStream(_outFile);
  } catch(IOException e) {
    System.out.println
      ("Error ocurred while opening file "+ _directory + 
       _filename + ".cdp:" + e.getMessage());
  }
} // open

  /**
   * Write binary bathymetry data file
   */
protected boolean write(){
    boolean success = false;
  float[] point;
  try{
    _binaryOut.writeInt(_data.getNumLines());
    for (int dataNum=0; dataNum<=_data.getNumLines()-1; dataNum++) {
      point = _data.getPoint(dataNum);
      point[xIndex]=CsdpFunctions.feetToMeters(point[xIndex]);
      point[yIndex]=CsdpFunctions.feetToMeters(point[yIndex]);
      _binaryOut.writeFloat(point[xIndex]);
      _binaryOut.writeFloat(point[yIndex]);
      _binaryOut.writeFloat(point[zIndex]);
      _binaryOut.writeShort(_data.getYear(_data.getYearIndex(dataNum)));
      _binaryOut.writeUTF(_data.getSource(_data.getSourceIndex(dataNum)));
      // don't write description since it's not used--too much memory
    }
    success = true;
  } catch(IOException e) {
    System.out.println
      ("Error ocurred while writing to file " +_directory + 
       _filename+"."+BINARY_TYPE+ 
       e.getMessage());
  } // catch
  return success;
}//write
    
  /**
   * Close binary bathymetry data file
   */
protected void close(){
  System.out.println("closing binary bathymetry file");
  try{
    _binaryOut.close();
  }catch(IOException e){
    System.out.println
      ("Error ocurred while closing file "+_directory + _filename + "."+
       _filetype+":"+e.getMessage());
  }// catch
}// close

} //class BathymetryBinaryOutput

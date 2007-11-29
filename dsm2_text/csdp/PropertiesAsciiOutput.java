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

public class PropertiesAsciiOutput extends PropertiesOutput{
  FileWriter _aOutFile     = null;
  BufferedWriter _asciiOut = null;

  /**
   * Open ascii file for writing
   */
protected void open(){
  try {
    if((_directory.substring(_directory.length()-1,_directory.length())).
       equals(File.separator) == false){
	_directory += File.separator;
    }
    _aOutFile = new FileWriter(_directory + _filename+"."+ASCII_TYPE);
    _asciiOut = new BufferedWriter(_aOutFile);
  } catch(IOException e) {
    if (DEBUG) System.out.println("Directory + Filename: "+ _directory + _filename);
    if (DEBUG) System.out.println("Filetype: " + _filetype);
    System.out.println
      ("Error ocurred while opening file "+_directory + _filename + 
       _filetype + e.getMessage());
  } // catch()
}

  /**
   * write ascii properties data file
   */
protected boolean write(){
  boolean success = false;
  float[] point;
  String line = null;
  short year;
  String source = null;

  try {
    String nl=null;
//     for(int i=0; i<=CsdpFunctions.getNumColors()-1; i++){
//       line = CsdpFunctions.getColor(i).getRed()+" "+
// 	CsdpFunctions.getColor(i).getGreen()+" "+
// 	CsdpFunctions.getColor(i).getBlue();
    for(int i=0; i<=_gui.getNumColors()-1; i++){
      line = _gui.getColor(i).getRed()+" "+
	_gui.getColor(i).getGreen()+" "+
	_gui.getColor(i).getBlue();
      _asciiOut.write(line);
      _asciiOut.newLine();
    }
    success = true;
  } catch(IOException e) {
    System.out.println
      ("Error ocurred while writing file "+ _directory + _filename + "."+
       ASCII_TYPE + e.getMessage());
  } finally {
  } // catch()
  return success;
}

  /**
   * Close ascii properties data file
   */
protected void close(){
  try{
    _asciiOut.close();
  }catch(IOException e){
    System.out.println
      ("Error ocurred while closing file "+_directory + _filename+"." + 
       _filetype+":"+e.getMessage());
  }// catch
}//close

} // class PropertiesAsciiInput

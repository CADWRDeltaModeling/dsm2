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
import javax.swing.*;
/**
 * writes network comparison data to ascii file
 */
public class NetworkCompareAsciiOutput extends NetworkCompareOutput{
  FileWriter _aOutFile     = null;                // ascii input file
  BufferedWriter _asciiOut = null;
    String _nFilename1     = null;
    String _nFilename2     = null;
    Network _net1          = null;
    Network _net2          = null;
    CsdpFrame _gui;
    /**
     * used for JOptionPane
     */
    private Object[] _options = {"OK"};
    private StationTimeSeriesData _stationData = null;

    /**
     * assigns data storage object to class variable
     */
    NetworkCompareAsciiOutput(String nFilename1, Network net1, 
			      String nFilename2, Network net2, CsdpFrame gui){
	_nFilename1 = nFilename1;
	_nFilename2 = nFilename2;
	_net1 = net1;
	_net1 = net1;
	_gui = gui;
    }
    
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
	    if (DEBUG) System.out.println("Directory, Filename: " +_directory + _filename);
	    if (DEBUG) System.out.println("Filetype: " + _filetype);
	    System.out.println
		("Error ocurred while opening file "+_directory +
		 _filename + _filetype + e.getMessage());
	} // catch()
    }

    /**
     * write ascii network comparison data file
     */
    protected boolean write(){
	boolean success = false;

	_net1.sortCenterlineNames();
	_net2.sortCenterlineNames();

	try{
	    _asciiOut.write("Network file comparison"+"\n");
	    _asciiOut.write("Network1 = "+_nFilename1 +", Network2 = "+_nFilename2);
	    _asciiOut.write("---------------------------------------------------------");
	    _asciiOut.write("\n"+"\n");
	    if(_net1.getNumCenterlines() != _net2.getNumCenterlines()){
		_asciiOut.write(_nFilename1+" has "+_net1.getNumCenterlines()+"\n");
		_asciiOut.write(_nFilename2+" has "+_net2.getNumCenterlines()+"\n"+"\n");
	    }else{
		_asciiOut.write("Both networks have "+_net1.getNumCenterlines()+" centerlines"+"\n"+"\n");
	    }
	}catch(IOException e){
	    System.out.println("error occurred while writing file "+_directory+
			       _filename+"."+ASCII_TYPE+e.getMessage());
	}
	//make list of centerlines, compare.
	//CHANGE THIS LATER.
	success = true;
	return success;
    }//write

  /**
   * Close ascii bathymetry data file
   */
protected void close(){
  try{
    _asciiOut.close();
  }catch(IOException e){
    System.out.println
      ("Error ocurred while closing file "+_directory + 
       _filename+"."+_filetype+":"+e.getMessage());
  }// catch
}//close

}//class NetworkCompareAsciiOutput


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
 * writes network data to ascii file
 */
public class NetworkAsciiStationElevationOutput extends NetworkOutput{
  FileWriter _aOutFile     = null;                // ascii input file
  BufferedWriter _asciiOut = null;
  Network _net             = null;

  /**
   * assigns data storage object to class variable
   */
  NetworkAsciiStationElevationOutput(Network net){
    _net = net;
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
    _aOutFile = new FileWriter(_directory + _filename+"."+EXPORT_TYPE);
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
   * export network to station/elevation format (HEC-2) for use
   * with OpenWaterAreaCalculations
   */
protected boolean write(){
  boolean success = false;
  String line = null;
  Centerline centerline;
  CenterlinePoint cPoint;
  Xsect xsect;
  XsectPoint xPoint;
  String metadata;

  _net.sortCenterlineNames();

  try{
    String nl=null;
    String versionLine = null;
    Integer numLines=null;
    for(int i=0; i<=_net.getNumCenterlines()-1; i++){
	if(_net.getCenterlineName(i).length() <= 0){
	    System.out.println("decrementing numCenterlines because nameless centerline found");
	    _net.removeCenterline(_net.getCenterlineName(i));
	    //go back to beginning of the loop just to be sure
	    i=-1;
	}
    }

    //write centerline
    if(_channelLengthsOnly){
	_asciiOut.write("Chan Length"+"\n");
    }
    for(int i=0; i<=_net.getNumCenterlines()-1; i++){
	centerline = _net.getCenterline(_net.getCenterlineName(i));
	if(_channelLengthsOnly){
	    _asciiOut.write(CsdpFunctions.formattedOutputString
			    (_net.getCenterlineName(i), 5, true) + 
			    centerline.getLength() +"\n");

	}else{
	    if(_net.getCenterlineName(i).length() <= 0 || 
	       centerline.getNumXsects() <= 0 ||
	       centerline.getNumCenterlinePoints() <=0){
		System.out.println("not writing centerline");
	    }else{
		//write cross-section lines
		for(int k=0;k<=centerline.getNumXsects()-1;k++){
		    line = "X1    ";
		    line += _net.getCenterlineName(i) +"_";
		    _asciiOut.write(line);
		    line = null;
		    xsect = centerline.getXsect(k);
		    line = xsect.getDistAlongCenterline()+"    ";
		    //    		if(k==0){
		    //    		    line += centerline.getLength()+"   "+"\n";
		    //    		}else{
  		    line += "\n";
		    //  		}
		    _asciiOut.write(line);
		    line = null;
		    
		    
		    for(int m=0; m<=xsect.getNumPoints()-1; m++){
			xPoint = xsect.getXsectPoint(m);
			line = xPoint.getStation()+" ";
			//		  line += xPoint.getElevation()+" ";
			line += xPoint.getElevation()+"\n";
			_asciiOut.write(line);
		    }//for m
		    
		    if(k==(centerline.getNumXsects()-1)){
			//the following is to make sure that the length
			//will be in the fourth column after importing into
			//excel using an underscore delimiter
			line = "xxx_xxx_xxx_"+centerline.getLength() + "   ";
			_asciiOut.write(line);
		    }
		    
		    
		    line = null;
		}//for k
		//	  _asciiOut.newLine();
		_asciiOut.write("\n");
	    }//else if centerline has a name
	}
    }//for i
    _asciiOut.write("END"+"\n");
    success = true;
  } catch(IOException e) {
      System.out.println
	  ("Error ocurred while writing file " +_directory + 
	   _filename + "."+ASCII_TYPE + e.getMessage());
  } finally {
  } // catch()
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

}//class NetworkAsciiOutput


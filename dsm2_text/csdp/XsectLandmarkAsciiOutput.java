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
public class XsectLandmarkAsciiOutput extends XsectLandmarkOutput{
  FileWriter _aOutFile     = null;                // ascii input file
  BufferedWriter _asciiOut = null;
  Network _net             = null;
  protected static final String SPACES = "          ";
  /**
   * assigns data storage object to class variable
   */
  public XsectLandmarkAsciiOutput(Network net){
    _net = net;
  }

  /**
   * Open ascii file for writing
   */
  protected void open(){
    try {
      _aOutFile = new FileWriter(FILENAME);
      _asciiOut = new BufferedWriter(_aOutFile);
    } catch(IOException e) {
      System.out.println
	("Error ocurred while opening file "+ FILENAME + e.getMessage());
    } // catch()
  }
  
  /**
   * write ascii network data file
   */
  protected void write(){
    String line = null;
    Centerline centerline;
    Xsect xsect;
    String name = null;

    float landmarkX = 0.0f;
    float landmarkY = 0.0f;
    int numXsects = 0;
    float[] xsectLineCoord = new float[4];
    for(int i=0; i<=_net.getNumCenterlines()-1; i++){
      name = _net.getCenterlineName(i);
      centerline = _net.getCenterline(name);
      numXsects += centerline.getNumXsects();
    }
    try{
      _asciiOut.write(numXsects+"");
      _asciiOut.newLine();
      for(int i=0; i<=_net.getNumCenterlines()-1; i++){
	name = _net.getCenterlineName(i);
	centerline = _net.getCenterline(name);
	for(int j=0; j<=centerline.getNumXsects()-1; j++){
	  xsectLineCoord = _net.findXsectLineCoord(name, j);
	  xsect =centerline.getXsect(j);
	  if(xsectLineCoord[CsdpFunctions.x1Index] > 
	     xsectLineCoord[CsdpFunctions.x2Index]){
	    landmarkX = xsectLineCoord[CsdpFunctions.x1Index];
	    landmarkY = xsectLineCoord[CsdpFunctions.y1Index];
	  }
	  else{
	    landmarkX = xsectLineCoord[CsdpFunctions.x2Index];
	    landmarkY = xsectLineCoord[CsdpFunctions.y2Index];
	  }
	  landmarkX = CsdpFunctions.feetToMeters(landmarkX);
	  landmarkY = CsdpFunctions.feetToMeters(landmarkY);
	  line = landmarkX+" "+landmarkY+
	    "  0 0 "+"\"Arial"+"\" -11 0 0 0 0 0 0 0 0 0 0 0 0 "+"\""+name+"_"+j+"\"";
	  _asciiOut.write(line);	
	  _asciiOut.newLine();
	  line = null;
	}//for j
	line = null;
      }//for i
    } catch(IOException e) {
    System.out.println
      ("Error ocurred while writing file " + FILENAME + e.getMessage());
    } finally {
    } // catch()
  }//write

  /**
   * Close ascii bathymetry data file
   */
  protected void close(){
    try{
    _asciiOut.close();
    }catch(IOException e){
      System.out.println
	("Error ocurred while closing file "+ FILENAME+e.getMessage());
    }// catch
  }//close
  
}//class XsectLandmarkAsciiOutput

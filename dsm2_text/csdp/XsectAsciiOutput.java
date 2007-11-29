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
 * writes xsect data to ascii file
 */
public class XsectAsciiOutput extends XsectOutput{
  FileWriter _aOutFile     = null;                // ascii input file
  BufferedWriter _asciiOut = null;
  Xsect _xsect             = null;

  /**
   * assigns data storage object to class variable
   */
  XsectAsciiOutput(Xsect xsect){
    _xsect = xsect;
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
    if (DEBUG) System.out.println("Directory,Filename: "+_directory + _filename);
    if (DEBUG) System.out.println("Filetype: " + _filetype);
    System.out.println
      ("Error ocurred while opening file "+_directory + 
       _filename + _filetype + e.getMessage());
  } // catch()
}

  /**
   * write ascii cross-section data file
   */
protected void write(){
  float a;
  float p;
  float w;
  float rh;
  float xc;
  float zc;
  String elevation       = null;
  String area            = null;
  String wettedPerimeter = null;
  String width           = null;
  String hydraulicRadius = null;
  String xCentroid       = null;
  String zCentroid       = null;
  String sLine = null; //for appending station values
  String eLine = null; // for appending elevation values
  float[] uniqueElevations=null;
  int numUnique;
  String line=null;
  String lastElevationString = null;
  float elevationFloat = CsdpFunctions.BIG_FLOAT;
  float lastElevationFloat = CsdpFunctions.BIG_FLOAT;

  try{
    _asciiOut.write("Cross-section:  "+ _filename + "\n");
    _asciiOut.write
      ("Elev(NGVD)       A          P          W         Rh         Xc         Zc"+"\n");
    _asciiOut.write
      ("========================================================================"+"\n");
  } catch(IOException e) {
    System.out.println
      ("Error ocurred while writing headers to file " +_directory + 
       _filename + "."+ASCII_TYPE + e.getMessage());
  } finally {
  } // catch()

  uniqueElevations = _xsect.getUniqueElevations();
  numUnique = _xsect.getNumUniqueElevations();

  for(int i=numUnique-1; i>=0; i--){
    a = _xsect.getArea(uniqueElevations[i]);
    p = _xsect.getWettedPerimeter(uniqueElevations[i]);
    w = _xsect.getWidth(uniqueElevations[i]);
    rh = _xsect.getHydraulicRadius(uniqueElevations[i]);
    xc = _xsect.getXCentroid(uniqueElevations[i]);
    zc = _xsect.getZCentroid(uniqueElevations[i]);

    elevationFloat = uniqueElevations[i];
    elevation       = (lastEleven(elevationFloat,2)).substring(4,11);
    if( elevation.equals(lastElevationString) ||
	elevationFloat > lastElevationFloat ){

	System.out.println("elevationFloat, lastElevationFloat="+elevationFloat+
			   ","+lastElevationFloat);
	System.out.println
	    ("adjusting elevation for cross-section "
	     +_filename+".  Elevation="+elevation+"\n");
	elevationFloat = Float.parseFloat(elevation) - 0.02f;
	elevation = (lastEleven(elevationFloat,2)).substring(4,11);
    }
    area            = (lastEleven(a,1)).substring(1,11);
    wettedPerimeter = lastEleven(p,1);
    width           = lastEleven(w,1);
    hydraulicRadius = lastEleven(rh,1);
    if(i>0){
      xCentroid       = lastEleven(xc,1);
      zCentroid       = lastEleven(zc,1);
    }
    else if(i==0){
      xCentroid = "        0.0";
      zCentroid = "        0.0";
    }
    line = elevation+area+wettedPerimeter+width+hydraulicRadius+xCentroid+zCentroid+"\n";

    try{
      _asciiOut.write(line);
    } catch(IOException e) {
      System.out.println
	("Error ocurred while writing cross-section properties to file " +
	 _directory+ _filename + "."+ASCII_TYPE + e.getMessage());
    } finally {
    } // catch()
    lastElevationString = elevation;
    lastElevationFloat = elevationFloat;
  }//for 

  sLine = "station:  ";
  eLine = "elevation:";
  for(int i=0; i<=_xsect.getNumPoints()-1; i++){
    XsectPoint point = _xsect.getXsectPoint(i);
    sLine += point.getStation()+" ";
    eLine += point.getElevation()+" ";
  }
  try{
    _asciiOut.newLine();
    _asciiOut.write(sLine+"\n");
    _asciiOut.write(eLine+"\n");
  } catch(IOException e) {
    System.out.println
      ("Error ocurred while writing station and elevation coordinates to file "+
       _directory + _filename + "."+ASCII_TYPE + e.getMessage());
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
      ("Error ocurred while closing file "+_directory+
       _filename+"."+_filetype+":"+e.getMessage());
  }// catch
}//close

protected String lastEleven(float value, float numDecimalPlaces){
//        Float vF = new Float(value*Math.pow(10.0f,numDecimalPlaces));
//        int vFi = vF.intValue();
//        Float vFiF = new Float(vFi);
//        float vFiFf = vFiF.floatValue();
//        vFiFf /= Math.pow(10.0f,numDecimalPlaces);

      int vFi = (int)(value*Math.pow(10.0f,numDecimalPlaces));
      float vFiFf = (float)vFi;
      vFiFf /= Math.pow(10.0f,numDecimalPlaces);


  String eleven = "           ";
  int length = (eleven + vFiFf).length();
  String last= (eleven + vFiFf).substring(length-11,length);
  return last;
}

}//class XsectAsciiOutput

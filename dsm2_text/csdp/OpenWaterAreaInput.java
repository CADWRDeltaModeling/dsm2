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
 * Read open water area input data
 *
 * @author
 * @version $Id: OpenWaterAreaInput.java,v 1.2 2002/10/21 20:02:20 btom Exp $
 */
public abstract class OpenWaterAreaInput {

  Network _owaNet = new Network("openWaterArea");

  /**
   * Make instance of subclass of BathymetryInput
   */
public static OpenWaterAreaInput getInstance(String filename, String filetype){
//    _directory = directory;
//      if((_directory.substring(_directory.length()-1,_directory.length())).
//         equals(File.separator) == false){
//  	_directory += File.separator;
//      }
  _filename = filename;
  _filetype = filetype;

System.out.println("filename,filetype="+_filename+","+_filetype);

  OpenWaterAreaInput input = null;
  if (_filetype.equals(ASCII_TYPE)) {
    input = new OpenWaterAreaAsciiInput();
  }
  else {// throw new IllegalInputFileException(msg);
    System.out.println();
    _filetype = null;
  }
  return input;
} //getInstance

  /**
   * Calls appropriate read method to read Network data
   */
public Network readData(){
  open();
  read();
  close();
  return _owaNet;
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
   * copy parsed centerline values to network object
   */
protected void storeCenterline(){
  String name = _pd.centerlineName;
  _owaNet.addCenterline(name);

  for(int i=0; i<=_pd.numCenterlinePoints-1; i++){
    _owaNet.getCenterline(name).addCenterlinePoint();
    _owaNet.getCenterline(name).getCenterlinePoint(i).putX(_pd.xUTM.get(i));
    _owaNet.getCenterline(name).getCenterlinePoint(i).putY(_pd.yUTM.get(i));
  }
}

    /**
     * makes a centerline
     */
    protected Centerline storeX1Line(){
	_owaNet.addCenterline(_pd.centerlineName);
	//System.out.println(_pd.centerlineName);
	return _owaNet.getCenterline(_pd.centerlineName);
	//	System.out.println("storing centerline. name="+_pd.centerlineName);
    }
    
    /**
     * Copy parsed cross-section values to xsect
     */
    protected void storeXsectLine(Centerline centerline, int xsPointIndex){
	if(DEBUG)System.out.println("xsPointIndex="+xsPointIndex);
	
	int xsIndex = 0;  //THERE IS ONLY ONE XS/centerline for OWA calculations.
	String name = _pd.centerlineName;
	if(DEBUG)System.out.println("storing xsect data for centerline "+name);
	if(DEBUG)System.out.println("_owaNet="+_owaNet);
	
	Xsect xsect = null;

	if(centerline.getNumXsects() <= 0){
	    centerline.addXsect();
	}
	xsect = centerline.getXsect(xsIndex);

	xsect.addXsectPoint();
	xsect.getXsectPoint(xsPointIndex).putStation(_pd.station.get(xsPointIndex));
	xsect.getXsectPoint(xsPointIndex).putElevation(_pd.elevation.get(xsPointIndex));

	//System.out.println("storing.  index,station,elevation="+xsPointIndex+","+_pd.station.get(xsPointIndex)+","+_pd.elevation.get(xsPointIndex));

	
    }//storeXsectLine

  /**
   * Convert x and y values from meters(UTM) to feet
   */
protected float metersToFeet(float value){
  final float METERS_TO_FEET=3.28084f;
  return METERS_TO_FEET * value;
}

protected NetworkParsedData _pd = new NetworkParsedData();  // vector-stores 6 values

    //protected int _numLines;
    //protected float _station;
    //protected float _elevation;

protected static String _filename = null; // part of filename before the first dot
protected static String _filetype = null; // filename extension (after first dot)
protected static final String ASCII_TYPE = "owa";
protected static final String BINARY_TYPE = null;
public static final boolean DEBUG = false;


} // class NetworkInput

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
 * Read landmark data.  Landmarks are symbols with labels that are displayed on map.
 *
 * @author
 * @version
 */
public abstract class LandmarkInput {

  /**
   * Make instance of subclass of LandmarkInput
   */
public static LandmarkInput getInstance(String directory, String filename) {
  _directory = directory;
    if((_directory.substring(_directory.length()-1,_directory.length())).
       equals(File.separator) == false){
	_directory += File.separator;
    }
  int dotIndex = filename.indexOf(".",0);
  _filename = filename.substring(0,dotIndex);
  _filetype = filename.substring(dotIndex+1);
  LandmarkInput input = null;
  if (_filetype.equals(ASCII_TYPE)) {
    input = new LandmarkAsciiInput();
  }
  else {// throw new IllegalInputFileException(msg);
    System.out.println("No landmark filetype defined for "+_filetype);
    _filetype = null;
  }
  return input;
} //getInstance

  /**
   * Calls appropriate read method to read Landmark data
   */
public Landmark readData(){
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
   * Stores a landmark point
   */
  protected void storeData(int dataNumber) {
    float x = CsdpFunctions.metersToFeet(_pd.x);
    float y = CsdpFunctions.metersToFeet(_pd.y);
    String name = _pd.name;
    _data.addLandmark(name, x, y); 
  }//storeData

  /**
   * Find maximum and minimum values of x and y and store them.
   */
  protected void findMaxMin(int numLines){
    float x=0.0f;
    float y=0.0f;
    float minX = CsdpFunctions.BIG_FLOAT;
    float maxX = CsdpFunctions.SMALL_FLOAT;
    float minY = CsdpFunctions.BIG_FLOAT;
    float maxY = CsdpFunctions.SMALL_FLOAT;
    String landmarkName = null;

    if(DEBUG)System.out.println("finding max and min values.  number of points="+numLines);
  for(int i=0; i<=numLines-1; i++){
    landmarkName = _data.getLandmarkName(i);
    if(DEBUG)System.out.println("landmarkName="+landmarkName);
    x = _data.getX(landmarkName);
    y = _data.getY(landmarkName);
    if(x!=0) minX = Math.min(x,minX);
    if(x!=0) maxX = Math.max(x,maxX);
    if(y!=0) minY = Math.min(y,minY);
    if(y!=0) maxY = Math.max(y,maxY);
  }
  _data.putMinX(minX);
  _data.putMaxX(maxX);
  _data.putMinY(minY);
  _data.putMaxY(maxY);
  if(DEBUG)System.out.println("findMaxMin called. MinX,MaxX,MinY,MaxY="+_data.getMinX()+" "+_data.getMaxX()+" "+_data.getMinY()+" "+_data.getMaxY());
}//findMaxMin

  Landmark _data       = new Landmark();
  LandmarkParsedData _pd   = new LandmarkParsedData();  //vector-stores 6 values
  protected final int xIndex = 0;
  protected final int yIndex = 1;
  protected final int zIndex = 2;
  public static final boolean DEBUG = false;
  
  protected static String _filename  = null;//part of filename before the first dot
  protected static String _filetype  = null;// filename extension (after first dot)
  protected static final String ASCII_TYPE = "cdl";
  protected static String _directory = null;
  protected int _numLandmarks;
} // class LandmarkInput

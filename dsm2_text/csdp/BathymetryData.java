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
import java.util.*;
import java.awt.*;
import javax.swing.*;
/**
 * Stores bathymetry data using automatically resized arrays.
 *
 * @author
 * @version $Id: BathymetryData.java,v 1.2 2002/10/21 19:58:26 btom Exp $
 */
public class BathymetryData {
  /*
   * initial number of elements in source array
   */
  int MAX_SOURCE = 1000;
  /*
   * initial number of elements in year array
   */
  int MAX_YEAR   = 1000;
  /*
   * initial number of elements in bathymetry data point arrays
   */
    private final int NUM_POINTS = 200000;

  /*
   * how much to increament bathymetry data point arrays
   */
  private final int POINT_INCREMENT = 50000;

  
  public BathymetryData(){
    initializeVariables();
  }//constructor

  /**
   * Print all stored data to screen
   */
  public void test(){
    String line=null;
    float[] point;
    
    for (int i=0; i<=getNumLines()-1; i++){
      point = getPoint(i);
      System.out.println(point[xIndex]+" "+point[yIndex]+" "+point[zIndex]+" "+
			 getSource(getSourceIndex(i)));
    }
  }//test
  
  /**
   * returns total number of lines in the bathymetry data file
   */
  public int getNumLines(){
    return _numLines;
  }//getNumLines
  /**
   * puts total number of lines in the bathymetry data file
   */
  public void putNumLines(int value){
    _numLines = value;
  }//putNumLines
  /**
   * Increments the number of years
   */
  protected void incrementNumYears(){
    numYears++;
  }//incrementNumYears
  /**
   * Increments the number of sources
   */
  protected void incrementNumSources(){
    numSources++;
  }//incrementNumSources
  /**
   * Return array of x,y,z values
   */
  public float[] getPoint(int index){
    float point[] ={getX(index),getY(index),getZ(index)};
    return point;
  }//getPoint
  /**
   * Set array of x,y,z values
   */
  public void setPoint(int index, float x, float y, float z){
    setX(index, x);
    setY(index, y);
    setZ(index, z);
  }//setPoint
  /**
   * Set value of year
   */
  public void putYear(int index, short value){
    year.put(index, value);
    _plotYear.put(index, true);
    incrementNumYears();
  }//putYear
  /**
   * Return value of year
   */
  public short getYear(int index){
    return year.get(index);
  }//getYear

  /**
   * sets plotting option for year at specified index
   */
  public void putPlotYear(int index, boolean value){
    _plotYear.put(index, value);
  }//putPlotYear

  /**
   * gets plotting option for year at specified index
   */
  public boolean getPlotYear(int index){
    return (_plotYear.get(index)).booleanValue();
  }//getPlotYear

  /**
   * Return value of numSources
   */
  public short getNumSources(){
    return numSources;
  }//getNumSources

  /**
   * sets plotting option for source at specified index
   */
  public void putPlotSource(int index, boolean value){
    _plotSource.put(index, value);
  }//putPlotSource

  /**
   * gets plotting option for source at specified index
   */
  public boolean getPlotSource(int index){
    return (_plotSource.get(index)).booleanValue();
  }//getPlotSource

  /**
   * Return value of numYears
   */
  public short getNumYears(){
    return numYears;
  }//getNumYears

  /**
   * Return value of source
   */
  public String getSource(int index){
    return source.get(index);
  }//getSource
  /**
   * Put value of source
   */
  public void putSource(int index, String value){
    source.put(index,value);
    _plotSource.put(index, true);
    incrementNumSources();
  }//putSource
  /**
   * Set value of yearIndex
   */
  public void putYearIndex(int index, int value){
    yearIndex.put(index, value);
  }//putYearIndex
  /**
   * Return value of yearIndex
   */
  public int getYearIndex(int index){
    return yearIndex.get(index);
  }//getYearIndex

  /**
   * sorts year array, adjusts values in yearIndex array
   */
  public void sortYearIndices(){
    int left = 0;
    int right = getNumYears()-1;
    ResizableShortArray oldArray = new ResizableShortArray();

    for(int i=0; i<=getNumYears()-1; i++){
      oldArray.put(i,getYear(i));
    }//for

    ResizableShortArray newArray = CsdpFunctions.qsort(left, right, year);
    for(int i=0; i<=getNumLines()-1; i++){
      int j=0;
      while(oldArray.get(getYearIndex(i)) != newArray.get(j)) j++;
      putYearIndex(i,j);
    }//for

  }//sortYearIndices

  public ResizableIntArray getYearIndexArray(){
    return yearIndex;
  }
  /**
   * Set value of sourceIndex
   */
  public void putSourceIndex(int index, int value){
    sourceIndex.put(index, value);
  }//putSourceIndex
  /**
   * Return value of yearIndex
   */
  public int getSourceIndex(int index){
    return sourceIndex.get(index);
  }//getSourceIndex
  
  /**
   * Store min value of X
   */
  public void putMinX(float value){
    minX = value;
  }//putMinX
  /**
   * Return min value of X
   */
  public float getMinX(){
    return minX;
  }//getMinX
  /**
   * Store max value of X
   */
  public void putMaxX(float value){
    maxX = value;
  }//putMaxX
  /**
   * Return max value of X
   */
  public float getMaxX(){
    return maxX;
  }//getMaxX
  /**
   * Store min value of Y
   */
  public void putMinY(float value){
    minY = value;
  }//putMinY
  /**
   * Return min value of Y
   */
  public float getMinY(){
    return minY;
  }//getMinY
  /**
   * Store max value of Y
   */
  public void putMaxY(float value){
    maxY = value;
  }//putMaxY
  /**
   * Return max value of Y
   */
  public float getMaxY(){
    return maxY;
  }//getMaxY
  /**
   * Store min value of Z
   */
  public void putMinZ(float value){
    minZ = value;
  }//putMinZ
  /**
   * Return min value of Z
   */
  public float getMinZ(){
    return minZ;
  }//getMinZ
  /**
   * Store max value of Z
   */
  public void putMaxZ(float value){
    maxZ = value;
  }//putMaxZ
  /**
   * Return max value of Z
   */
  public float getMaxZ(){
    return maxZ;
  }//getMaxZ
  /**
   * Returns the string representation of the first five points
   */
  public String toString(){
    StringBuffer buf = new StringBuffer();
    //  int nPoints = getNumberOfDataPoints();
    int nPoints=5;
    buf.append("Number of Data Points: ").append(nPoints).append("\n");
    for(int i=0; i < Math.min(nPoints, 5); i++){
      buf.append("Point ").append(i).append(" = ").append("\n");
      buf.append(" X: ").append(getX(i)).append(", ");
      buf.append(" Y: ").append(getY(i)).append(", ");
      buf.append(" Z: ").append(getZ(i)).append(", ").append("\n");
    }
    return buf.toString();
  }//toString
  
  /**
   * sorts bathymetry data by x and y
   */
  public void sortBathymetryData(){
    System.out.println("sorting data...");
    
    x.resizeTo(getNumLines());
    y.resizeTo(getNumLines());
    z.resizeTo(getNumLines());
    yearIndex.resizeTo(getNumLines());
    sourceIndex.resizeTo(getNumLines());
    descriptionIndex.resizeTo(getNumLines());
    
    int left = 0;
    int right = getNumLines();
    qsort(left,right-1);
  } //sortBathymetryData
  
  /**
   * finds all bathymetry data points that are within the specified polygon.
   * this is used to select points to be plotted in cross-section view
   */
  public void findXsectData(Hashtable displayData){

    Polygon displayRegion = (Polygon)(displayData.get("xsectDisplayRegion"));
    
    if(DEBUG)System.out.println("polygon coordinates:");
    for(int i=0; i<=displayRegion.npoints-1; i++){ 
      if(DEBUG)System.out.println(displayRegion.xpoints[i]+","+
				  displayRegion.ypoints[i]);
    }

    float[] centerlineSegmentEndpoints = 
      (float[])(displayData.get("centerlineSegmentEndpoints"));
    Rectangle r = displayRegion.getBounds();
    Point point = new Point();
    int numValues=0;
    int i=0;
    int yearIndex = 0;
    int sourceIndex = 0;

    //find the closest point that has a y value that is below the rectangle.
    //if it's the first point (your region extends beyond the southern boundary) then i will be zero...
    i=CsdpFunctions.interpolationSearch(y,r.y,getNumLines());
    if(i>=1) i-=1;
    if(i>1 && getY(i-1) > r.y){
	System.out.println("ERROR in BathymetryData.findXsectData");
    }
    while(getY(i) < r.y && i<getNumLines()){
      i++;
    }
    float yFinal = getY(i);
    while(i-1>0 && getY(i-1) == yFinal){
	i--;
    }

    int firstIndex = i;
    while(i<getNumLines()-1 && (getY(i) > 0.0f) && (getY(i) < (r.y+r.height))){
      i++;
      if(DEBUG)System.out.println("found value; index,y, r.y, r.height= "+i+","+getY(i)+","+r.y+","+r.height);
    }
    int lastIndex = i;

    if(DEBUG)System.out.println("firstIndex, lastIndex"+firstIndex+","+lastIndex);

    for(int j=firstIndex; j<=lastIndex; j++){
      yearIndex = getYearIndex(j);
      sourceIndex = getSourceIndex(j);
      point.setLocation((int)getX(j), (int)getY(j));
      if(displayRegion.contains(point) && getPlotYear(yearIndex) && 
	 getPlotSource(sourceIndex)){
	//System.out.println("saving point");
	storeEnclosedPointIndex(numValues,j);
	//System.out.println("storing values "+getX(j)+","+getY(j)+","+getZ(j));
        numValues++;
      }//if contains point
    }//for i
    putNumEnclosedValues(numValues);
    rotateXsectData(centerlineSegmentEndpoints);
  }//findXsectData

  /**
   * Find maximum and minimum values of x,y, and z and store them.
   */
  protected void findMaxMin(int numLines){
    float x=0.0f;
    float y=0.0f;
    float z=0.0f;
    float[] point = getPoint(0);
    float minX = point[xIndex];
    float maxX = point[xIndex];
    float minY = point[yIndex];
    float maxY = point[yIndex];
    float minZ = point[zIndex];
    float maxZ = point[zIndex];
  for(int i=0; i<=numLines-1; i++){
    point = getPoint(i);
    x = point[xIndex];
    y = point[yIndex];
    z = point[zIndex];
    if(x!=0) minX = Math.min(x,minX);
    if(x!=0) maxX = Math.max(x,maxX);
    if(y!=0) minY = Math.min(y,minY);
    if(y!=0) maxY = Math.max(y,maxY);
    minZ = Math.min(z,minZ);
    maxZ = Math.max(z,maxZ);
  }
  putMinX(minX-CsdpFunctions.BORDER_THICKNESS);
  putMaxX(maxX+CsdpFunctions.BORDER_THICKNESS);
  putMinY(minY-CsdpFunctions.BORDER_THICKNESS);
  putMaxY(maxY+CsdpFunctions.BORDER_THICKNESS);
  putMinZ(minZ);
  putMaxZ(maxZ);
}//findMaxMin

  /**
   * Converts bathymetry data (which is enclosed in xsect plot region) to local 
   * coordinate system with intersection of centerline and xsect line as its origin.
   */
  protected void rotateXsectData(float[] endpoints){

    /*
     * x1, y1 are coord. of upstream centerline segment point.
     * x2, y2 are coord. of downstream centerline segment point.
     * x3, y3 are coord. of data point
     */
    float x1 = endpoints[x1Index];
    float x2 = endpoints[x2Index];
    float x3;
    float y1 = endpoints[y1Index];
    float y2 = endpoints[y2Index];
    float y3;
    float z;
    float dist;
    float sign;

    if(DEBUG)System.out.println("centerline coord:"+x1+","+y1+","+x2+","+y2);
    for(int i=0; i<=getNumEnclosedValues()-1; i++){
      x3 = getX(getEnclosedPointIndex(i));
      y3 = getY(getEnclosedPointIndex(i));
      dist = CsdpFunctions.shortestDistLine(x1,x2,x3,y1,y2,y3);

      if(DEBUG)System.out.println("x3,y3,shortestDist = "+x3+","+y3+","+dist);

      sign = getSign(x1,x2,x3,y1,y2,y3);
      putEnclosedStationElevation(i, sign*dist, getZ(getEnclosedPointIndex(i)));
    }

  }//rotateXsectData

  /**
   * return +1 if point is closer to right bank (sin will be >0)
   * return -1 if point is closer to left bank (sin will be <0)
   */
  protected int getSign(float x1, float x2, float x3, float y1, float y2, float y3){
    int s = 0;
    float theta = CsdpFunctions.getTheta(x1,x2,x3,y1,y2,y3);
    if(Math.sin(theta) >= 0) s = 1;
    if(Math.sin(theta) <  0) s = -1;
    return s;
  }//getSign

  /**
   * Get value of x
   */
protected float getX(int index){
  return x.get(index);
}
  /**
   * Set value of x
   */
protected void setX(int index, float value){
  x.put(index,value);
}
  /**
   * Return value of y
   */
protected float getY(int index){
    //    float returnValue = -CsdpFunctions.BIG_FLOAT;
//      if(index >= y.getSize()){
//  	JOptionPane.showOptionDialog
//  	    (null, "ERROR!  ATTEMPT TO ACCESS ARRAY ELEMENT THAT DOES NOT EXIST", 
//  	     "UNABLE TO COMPLY",
//  	     JOptionPane.DEFAULT_OPTION, JOptionPane.WARNING_MESSAGE, null,
//  	     _options, _options[0]);
//      }else returnValue = y.get(index);
    //    return returnValue;
    return y.get(index);
}
  /**
   * Set value of y
   */
protected void setY(int index, float value){
  y.put(index,value);
}
  /**
   * Return value of z
   */
protected float getZ(int index){
  return z.get(index);
}
  /**
   * Get value of z
   */
protected void setZ(int index, float value){
  z.put(index,value);
}
  /**
   * quicksort
   */
protected void qsort(int left, int right){
  int last=0;
  double ran=0.0;
  if(left < right){
    ran = Math.random();
    swap(x, left, left+(int)( (right-left+1) * ran));
    swap(y, left, left+(int)( (right-left+1) * ran));
    swap(z, left, left+(int)( (right-left+1) * ran));
    swap(yearIndex, left, left+(int)( (right-left+1) * ran));
    swap(sourceIndex, left, left+(int)( (right-left+1) * ran));
    swap(descriptionIndex, left, left+(int)( (right-left+1) * ran));
    last=left;
    for(int i=left+1; i<=right; i++){
      if(y.get(i) < y.get(left)){
	last++;
	swap(x, last, i); 
	swap(y, last, i); 
	swap(z, last, i); 
	swap(yearIndex, last, i); 
	swap(sourceIndex, last, i); 
	swap(descriptionIndex, last, i); 
      }//if
    }//for i
      swap(x, left, last);
      swap(y, left, last);
      swap(z, left, last);
      swap(yearIndex, left, last);
      swap(sourceIndex, left, last);
      swap(descriptionIndex, left, last);
      qsort(left, last-1);
      qsort(last+1, right);
  }//if
}//qsort

  /**
   *swap two float values in array
   */
  protected void swap(ResizableFloatArray a, int i, int j){
    float t = a.get(i);
    a.put(i,a.get(j));
    a.put(j,t);
  }//swap
  /**
   *swap two int values in array
   */
  protected void swap(ResizableIntArray a, int i, int j){
    int t = a.get(i);
    a.put(i,a.get(j));
    a.put(j,t);
  }//swap
  /**
   *swap two short values in array
   */
  protected void swap(ResizableShortArray a, int i, int j){
    short t = a.get(i);
    a.put(i,a.get(j));
    a.put(j,t);
  }//swap

  /**
   * called by contructor and called when new bathymetry loaded
   */
  public void initializeVariables(){
    x      = new ResizableFloatArray(NUM_POINTS,POINT_INCREMENT);
    y      = new ResizableFloatArray(NUM_POINTS,POINT_INCREMENT);
    //floatArray y               = new floatArray();
    z      = new ResizableFloatArray(NUM_POINTS,POINT_INCREMENT);
    yearIndex = new ResizableIntArray(NUM_POINTS,10000);
    sourceIndex = new ResizableIntArray(NUM_POINTS,10000);
    descriptionIndex  = new ResizableIntArray(NUM_POINTS,10000);
    year    = new ResizableShortArray(MAX_YEAR,5);
    source = new ResizableStringArray(MAX_SOURCE, 5);
    _plotYear = new ResizableBooleanArray(MAX_YEAR, 5);
    _plotSource=new ResizableBooleanArray(MAX_YEAR, 5);
    //description    = new String[];
    numYears   = 0;
    numSources = 0;
    _numLines   = 0;
    minX = Float.MAX_VALUE;
    maxX = Float.MIN_VALUE;
    minY = Float.MAX_VALUE;
    maxY = Float.MIN_VALUE;
  }//initialize variables

    /**
     * store number of values to be displayed in xsect view
     */
    protected void putNumEnclosedValues(int value){
      _numEnclosedValues = value;
    }

  /**
   * returns number of values to be displayed in xsect view
   */
  public int getNumEnclosedValues(){
    return _numEnclosedValues;
  }

  /**
   * stores indices of points that are to be displayed in the xsect view
   */
  protected void storeEnclosedPointIndex(int index, int value){
    _enclosedPointIndex.put(index, value);
  }

  /**
   * returns indices of points that are to be displayed in the xsect view
   */
  public int getEnclosedPointIndex(int index){
    return _enclosedPointIndex.get(index);
  }


  /**
   * stores point to be displayed in xsect view using local coord. sys.
   */
  protected void putEnclosedStationElevation(int index, 
					     float station, float elevation){
    _enclosedStation.put(index, station);
    _enclosedElevation.put(index, elevation);
  }

  /**
   * stores point to be displayed in xsect view using local coord. sys.
   */
  protected float[] getEnclosedStationElevation(int index){
    _returnPoint[stationIndex] = _enclosedStation.get(index);
    _returnPoint[elevationIndex] = _enclosedElevation.get(index);
    return _returnPoint;
  }

  protected float[] _returnPoint = new float[2];
  protected ResizableIntArray _enclosedPointIndex = new ResizableIntArray();
  protected int _numEnclosedValues = 0;
  protected ResizableFloatArray _enclosedStation = new ResizableFloatArray();
  protected ResizableFloatArray _enclosedElevation = new ResizableFloatArray();
  
  /**
   * number of unique year values in bathymetry data set
   */
  protected short numYears   = 0;
  /**
   * number of unique source values in bathymetry data set
   */
  protected short numSources = 0;
  /**
   * number of points in bathymetry data set
   */
  protected int _numLines   = 0;

  /**
   * minimum x(UTM) value
   */
  protected float minX = Float.MAX_VALUE;
  /**
   * maximum x(UTM) value
   */
  protected float maxX = Float.MIN_VALUE;
  /**
   * minimum y(UTM) value
   */
  protected float minY = Float.MAX_VALUE;
  /**
   * maximum y(UTM) value
   */
  protected float maxY = Float.MIN_VALUE;
  /**
   * minimum z(UTM) value
   */
  protected float minZ = Float.MAX_VALUE;
  /**
   * maximum y(UTM) value
   */
  protected float maxZ = Float.MIN_VALUE;


  protected final int x1Index = 0;
  protected final int y1Index = 1;
  protected final int x2Index = 2;
  protected final int y2Index = 3;
  protected static final int xIndex = 0;
  protected static final int yIndex = 1;
  protected static final int zIndex = 2;
  protected static final int stationIndex = 0;
  protected static final int elevationIndex = 1;
  protected static final boolean DEBUG = false;
    /**
     * Stores the x (west/east) coordinates of the bathymetry data.
     */
    protected ResizableFloatArray x =null;
    /**
     * Stores the y (south/north) coordinates of the bathymetry data.
     */
    protected ResizableFloatArray y = null;
  //protected floatArray y null;
    /**
     * Stores the z (elevation, NGVD) coordinates of the bathymetry data.
     */
    protected ResizableFloatArray z = null;
    /**
     * Stores the index of the year
     */
    protected ResizableIntArray yearIndex = null;
    /**
     * Stores the index of the source
     */
    protected ResizableIntArray sourceIndex = null;
    /**
     * Stores the index of the description
     */
    protected ResizableIntArray descriptionIndex  = null;
    /**
     * Stores the year that the data were collected.
     */
    protected ResizableShortArray year    = null;
    
    /**
     * Stores the source (name of agency) of the bathymetry data.
     */
    protected ResizableStringArray source = null;
    
    /**
     * true if data from this year should be plotted
     */
    protected ResizableBooleanArray _plotYear = null;
    /**
     * true if data from this source should be plotted
     */
    protected ResizableBooleanArray _plotSource = null;
    /**
     * Stores a description (cross-section name, etc.) of the bathymetry data.
     */
    //protected String[] description    = new String[];
    /**
     * used for JOptionPane
     */
    private Object[] _options = {"OK"};

}//BathymetryData

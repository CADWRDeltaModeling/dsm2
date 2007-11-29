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
import java.awt.*;
import java.util.*;

/**
 * A network contains Centerlines which can contain cross-sections
 *
 * @author
 * @version
 */
public class Network {
  public Network(String name){
    _networkName = name;
  }
  
    public boolean centerlineExists(String name){
	boolean returnValue = false;
	if(_centerlines.containsKey(name)){
	    returnValue = true;
	}else{
	    returnValue = false;
	}
	return returnValue;
    }

  /**
   * returns name of network
   */
  public String getNetworkName(){
    return _networkName;
  }
  
  /**
   * Returns number of Centerlines
   */
  public int getNumCenterlines(){
    return _numCenterlines;
  }
  
  /**
   * Stores number of Centerlines
   */
  public void putNumCenterlines(int value){
    _numCenterlines=value;
  }

  /**
   * returns Centerline object
   */
  public Centerline getCenterline(String name){
    return (Centerline)_centerlines.get(name);
  }
  
    /**
     * sort centerline names lexicographically
     */
    public void sortCenterlineNames(){
	_centerlineNames = 
	    CsdpFunctions.qsort(_centerlineNames, 0, getNumCenterlines()-1);
    }//sortCenterlineNames
    
  /**
   * print listing of all Centerline objects
   */
  public Enumeration getAllCenterlines() {
    Enumeration e;
    for(e = _centerlines.elements(); e.hasMoreElements(); ) {
      System.out.println(e.nextElement());
    }
    return e;
  }
  
  /**
   * create new Centerline object
   */
  public void addCenterline(String name) {
      //if there is already a centerline with the name, erase the old one.
      if(_centerlines.containsKey(name)){
	  removeCenterline(name);
      }
      Centerline cent = new Centerline(name);
      _centerlines.put(name, cent);
      putCenterlineName(name);
      _numCenterlines++;
  }//addCenterline

  /**
   * rename Centerline
   */
  public void renameCenterline(String oldName, String newName) {
    Centerline cent = getCenterline(oldName);
    int index = 0;
    _centerlines.put(newName, cent);
    removeCenterline(oldName);
    for(int i=0; i<=getNumCenterlines(); i++){
      if(getCenterlineName(i).equals(oldName)){
	index = i;
      }//if
    }//for
    _centerlineNames.put(index,newName);
    _numCenterlines++;
    setSelectedCenterline(cent);
    setSelectedCenterlineName(newName);
    setIsUpdated(true);
  }//renameCenterline

  /**
   * removes Centerline from hashtable
   */
  public void removeCenterline(String name){

      if(DEBUG)System.out.println("about to remove centerline: "+name);
      String centerlineName = null;
      Centerline centerline = null; 
      
      Object value = _centerlines.remove(name);
      _centerlineNames.removeElement(name);
      setIsUpdated(true);
      setSelectedCenterline(null);
      setSelectedXsect(null);
      setSelectedCenterlineName(null);
      //setSelectedXsectNum       = 0;
      _numCenterlines--;
      
  }//removeCenterline

  /**
   * prints all network data to screen
   */
  public void printNetwork(){
    System.out.println("data for network "+getNetworkName());
    System.out.println(getNumCenterlines());
    String line = null;
    Centerline centerline;
    CenterlinePoint cPoint;
    Xsect xsect;
    XsectPoint xPoint;
    
    for(int i=0; i<=getNumCenterlines()-1; i++){
      centerline = getCenterline(getCenterlineName(i));
      line = getCenterlineName(i)+" ";
      line += centerline.getNumCenterlinePoints() +" ";
      for(int j=0; j<=centerline.getNumCenterlinePoints()-1; j++){
	cPoint = centerline.getCenterlinePoint(j);
	line+=cPoint.getX()+",";
	line+=cPoint.getY()+" ";
      }//for j
      line += centerline.getNumXsects();
      System.out.println(line);
      for(int k=0; k<=getCenterline(getCenterlineName(i)).getNumXsects()-1; k++){
	xsect = centerline.getXsect(k);
	line  = xsect.getNumPoints()+" ";
	for(int m=0; m<=xsect.getNumPoints()-1; m++){
	  xPoint = xsect.getXsectPoint(m);
	  line += xPoint.getStation()+",";
	  line += xPoint.getElevation()+" ";
	}//for m
	line += xsect.getDistAlongCenterline()+" ";
	line += xsect.getXsectLineLength();
	System.out.println(line);
	line = null;
      }//for k
      System.out.println();
    }//for i
  }//printNetwork
  
  /**
   * true if network has been changed:warning will be displayed before ending
   * program
   */
  public boolean isUpdated(){
    return _isUpdated;
  }
  /**
   * set to true whenever network is changed
   */
  public void setIsUpdated(boolean value){
    _isUpdated = value;

    if(DEBUG)System.out.println("setting is updated to "+value);

  }
  
  /**
   * find 4 points that define polygon(rectangle) of data to be displayed
   * The origin of the xsect plot will be the intersection of the centerline
   * segment and the cross-section line.  Data will be displayed from left to
   * right bank, assuming that the first point in the centerline segment is
   * upstream from the second point (using the direction defined in the DSM grid) 
   */
  public Hashtable findXsectDisplayRegion(String centerlineName, int xsectNum,
					  float thickness){
    if(DEBUG)System.out.println("finding xsect display region");
    Centerline centerline = getCenterline(centerlineName);
    Xsect xsect = centerline.getXsect(xsectNum);
    Polygon xsectDisplayRegion = null;
    Hashtable returnValues = new Hashtable();
    float[] xsectEndpoints;
    float[] centerlineSegmentEndpoints;
    float distToUpperLeftVertex;
    float distToLowerLeftVertex;
    float[] vertexXRel = new float[4];
    float[] vertexYRel = new float[4];
    float[] vertexX = new float[4];
    float[] vertexY = new float[4];
    int[] vertexXInt = new int[4];
    int[] vertexYInt = new int[4];
    float xsectLineLength;
    float distAlongSegment;
    /*
     * angle of centerline
     */
    float theta;
    float thetaUpperVertex;
    float lineSegmentLength;
    float thetaLowerVertex;
    float slope;
    /*
     *   1.  find endpoints of centerline segment that contains xsect
     *   2.  find endpoints of xsect line
     *   3.  find rectangle using cross-section thickness
     */
    centerlineSegmentEndpoints=findCenterlineSegmentCoord(centerlineName, xsectNum);
    xsectEndpoints = findXsectLineCoord(centerlineName, xsectNum);
    
    if(DEBUG)System.out.println("centerlineSegmentEndpoints="+
				centerlineSegmentEndpoints[0]+" "+
				centerlineSegmentEndpoints[1]+" "+
				centerlineSegmentEndpoints[2]+" "+
				centerlineSegmentEndpoints[3]);
    /*
     * find coordinates of upstream, downstream, left, and right boundaries
     * of rectangle which is rotated so that the upstream centerline 
     * segment point is at the origin and the downstream centerline segment point
     * is on the positive y axis.  After finding relative coordinates, translate 
     * all points back to original orientation.
     */
    distAlongSegment = xsect.getDistAlongCenterline() - 
      centerlineSegmentEndpoints[distIndex];
    if(DEBUG)System.out.println("distAlongSegment="+distAlongSegment);

    xsectLineLength = xsect.getXsectLineLength();

    lineSegmentLength = CsdpFunctions.pointDist
      (centerlineSegmentEndpoints[CsdpFunctions.x1Index],
       centerlineSegmentEndpoints[CsdpFunctions.y1Index],
       centerlineSegmentEndpoints[CsdpFunctions.x2Index],
       centerlineSegmentEndpoints[CsdpFunctions.y2Index]);

    theta = CsdpFunctions.getTheta
      (centerlineSegmentEndpoints[CsdpFunctions.x1Index],
       centerlineSegmentEndpoints[CsdpFunctions.x2Index],
       centerlineSegmentEndpoints[CsdpFunctions.y1Index],
       centerlineSegmentEndpoints[CsdpFunctions.y2Index]);
    if(DEBUG)System.out.println
	       ("theta,x1x2y1y2="+theta+" "+
		centerlineSegmentEndpoints[CsdpFunctions.x1Index]+","+
		centerlineSegmentEndpoints[CsdpFunctions.x2Index]+","+
		centerlineSegmentEndpoints[CsdpFunctions.y1Index]+","+
		centerlineSegmentEndpoints[CsdpFunctions.y2Index]);
    
    xsectDisplayRegion = CsdpFunctions.findPolygon
      (centerlineSegmentEndpoints[CsdpFunctions.x1Index], 
       centerlineSegmentEndpoints[CsdpFunctions.x2Index],
       centerlineSegmentEndpoints[CsdpFunctions.y1Index], 
       centerlineSegmentEndpoints[CsdpFunctions.y2Index], 
       distAlongSegment, thickness, xsectLineLength);
    
    returnValues.put("xsectDisplayRegion",xsectDisplayRegion);
    returnValues.put("centerlineSegmentEndpoints",centerlineSegmentEndpoints);

    return returnValues;
  }//findXsectDisplayRegion

  /**
   * find coordinates of the line segment in the centerline that contains the xsect
   * also finds the distance from the first pt in the centerline to the point
   * that is upstream of the xsect
   */
  protected float[] findCenterlineSegmentCoord(String centerlineName, int xsectNum){
    float[] returnValues = new float[5];
    Centerline centerline = getCenterline(centerlineName);
    Xsect xsect = centerline.getXsect(xsectNum);
    float xsectDistAlong = xsect.getDistAlongCenterline();
    CenterlinePoint cp = null;
    CenterlinePoint cpPrevious = null;
    CenterlinePoint cpUpstream = null;
    CenterlinePoint cpDownstream = null;
    float dist = 0.0f;

    for(int i=1; i<=centerline.getNumCenterlinePoints()-1 &&
	  dist < xsectDistAlong; i++){
      cp = centerline.getCenterlinePoint(i);
      cpPrevious = centerline.getCenterlinePoint(i-1);
      dist += CsdpFunctions.pointDist
	(cpPrevious.getX(),cpPrevious.getY(),cp.getX(),cp.getY());
    }//for i
    cpUpstream = cpPrevious;
    cpDownstream = cp;
    returnValues[CsdpFunctions.x1Index] = cpUpstream.getX();
    returnValues[CsdpFunctions.y1Index] = cpUpstream.getY();
    returnValues[CsdpFunctions.x2Index] = cpDownstream.getX();
    returnValues[CsdpFunctions.y2Index] = cpDownstream.getY();
    returnValues[distIndex] = dist - CsdpFunctions.pointDist
      (cpPrevious.getX(),cpPrevious.getY(), cp.getX(), cp.getY()); 

    return returnValues;
  }//findCenterlineSegmentCoord

    public float[] find3DXsectPointCoord(String centerlineName, 
					int xsectNum, int xsectPointNum){
	float[] returnValue = new float[2];
	float[] originCoord = findXsectOriginVector(centerlineName, xsectNum);
	float xOrigin = originCoord[CsdpFunctions.x1Index];
	float yOrigin = originCoord[CsdpFunctions.y1Index];
	float xEndpoint = originCoord[CsdpFunctions.x2Index];
	float yEndpoint = originCoord[CsdpFunctions.y2Index];

	if(DEBUG){
	    System.out.println("Network.find3DXsectPointCoord");
	    System.out.println("centerlineName, xsectNum, xsectPointNum="+
			       centerlineName+","+xsectNum+","+xsectPointNum);
	    System.out.println("xOrigin, yOrigin, xEndpoint, yEndpoint="+
			       xOrigin+","+yOrigin+","+xEndpoint+","+yEndpoint);
	}

	Centerline centerline = getCenterline(centerlineName);
	Xsect xsect = centerline.getXsect(xsectNum);
	XsectPoint xsectPoint = xsect.getXsectPoint(xsectPointNum);
	float station = xsectPoint.getStation();
	float x = CsdpFunctions.stationToX(xOrigin, xEndpoint, 
					   yOrigin, yEndpoint, station);
	float y = CsdpFunctions.stationToY(xOrigin, xEndpoint, 
					   yOrigin, yEndpoint, station);
	returnValue[CsdpFunctions.x1Index] = x;
	returnValue[CsdpFunctions.y1Index] = y;
	return returnValue;
    }

    /**
     * find coordinates of intersection of cross-section line and
     * centerline.  Used for 3D network output.  Returns an array of
     * 4 values:  x1,y1 of the origin and x2,y2 of the endpoint of the
     * cross-section line that is in the positive direction.
     */
    private float[] findXsectOriginVector(String centerlineName, int xsectNum){
	Centerline centerline = getCenterline(centerlineName);
	Xsect xsect = centerline.getXsect(xsectNum);
	float dist    = xsect.getDistAlongCenterline();
	float length  = xsect.getXsectLineLength();
	
	if(DEBUG)System.out.println("inside findXsectLineCoord: dist, length="+dist+","+length);
	
	CenterlinePoint point1;
	CenterlinePoint point2;
	float x1 = 0.0f;
	float x2 = 0.0f;
	float y1 = 0.0f;
	float y2 = 0.0f;
	float xOrigin = 0.0f;
	float yOrigin = 0.0f;
	float[] returnValues = null;
	float centerlineDist = 0.0f;
	//find 2 points on either side
	int i=0;
	for(i=0; dist > centerlineDist && 
		i<=centerline.getNumCenterlinePoints()-2; i++){
	    point1 = centerline.getCenterlinePoint(i);
	    point2 = centerline.getCenterlinePoint(i+1);
	    x1 = point1.getX();
	    x2 = point2.getX();
	    y1 = point1.getY();
	    y2 = point2.getY();
	    centerlineDist += CsdpFunctions.pointDist(x1,y1,x2,y2);
	}//for i dist
	if(dist < centerlineDist) centerlineDist -= CsdpFunctions.pointDist(x1,y1,x2,y2);
	i--;
	point1 = centerline.getCenterlinePoint(i);
	point2 = centerline.getCenterlinePoint(i+1);
	x1 = point1.getX();
	x2 = point2.getX();
	y1 = point1.getY();
	y2 = point2.getY();

	returnValues = findXsectLineCoord(x1,y1,x2,y2,xsect, centerlineDist);
	//now replace x1,y1 with the coordinates of the intersection.
	xOrigin = CsdpFunctions.findXIntersection(x1, x2, 
						  returnValues[CsdpFunctions.x1Index],
						  y1, y2,
						  returnValues[CsdpFunctions.y1Index]);
	yOrigin = CsdpFunctions.findYIntersection(x1, x2, 
						  returnValues[CsdpFunctions.x1Index],
						  y1, y2,
						  returnValues[CsdpFunctions.y1Index]);
	returnValues[CsdpFunctions.x1Index] = xOrigin;
	returnValues[CsdpFunctions.y1Index] = yOrigin;

	if(DEBUG)System.out.println("coordinates of points before and after xsect x1y1x2y2="+
				    x1+","+y1+" "+x2+","+y2);
	
	return returnValues;
	
    }//findXsectOriginVector

  /**
   * find coordinates of endpoints of xsect line
   */
  public float[] findXsectLineCoord(String centerlineName, int xsectNum){
    Centerline centerline = getCenterline(centerlineName);
    Xsect xsect = centerline.getXsect(xsectNum);
    float dist    = xsect.getDistAlongCenterline();
    float length  = xsect.getXsectLineLength();

    if(DEBUG)System.out.println("inside findXsectLineCoord: dist, length="+dist+","+length);

    CenterlinePoint point1;
    CenterlinePoint point2;
    float x1 = 0.0f;
    float x2 = 0.0f;
    float y1 = 0.0f;
    float y2 = 0.0f;
    float[] returnValues = null;
    float centerlineDist = 0.0f;
    //find 2 points on either side
    int i=0;
    for(i=0; dist > centerlineDist && 
	  i<=centerline.getNumCenterlinePoints()-2; i++){
      point1 = centerline.getCenterlinePoint(i);
      point2 = centerline.getCenterlinePoint(i+1);
      x1 = point1.getX();
      x2 = point2.getX();
      y1 = point1.getY();
      y2 = point2.getY();
      centerlineDist += CsdpFunctions.pointDist(x1,y1,x2,y2);
    }//for i dist
    if(dist < centerlineDist) centerlineDist -= CsdpFunctions.pointDist(x1,y1,x2,y2);
    i--;
    point1 = centerline.getCenterlinePoint(i);
    point2 = centerline.getCenterlinePoint(i+1);
    x1 = point1.getX();
    x2 = point2.getX();
    y1 = point1.getY();
    y2 = point2.getY();
    returnValues = findXsectLineCoord(x1,y1,x2,y2,xsect, centerlineDist);

    if(DEBUG)System.out.println("coordinates of points before and after xsect x1y1x2y2="+
		       x1+","+y1+" "+x2+","+y2);
    
    return returnValues;
  }//findXsectLineCoord

  /**
   * find coordinates of xsect line
   */
  protected float[] findXsectLineCoord(float x1, float y1, float x2, float y2, 
				       Xsect xsect, float centerlineDist){
    float[] returnValues = new float[4];
    // distance of xsect from nearest upstream centerline point
    float dist       = xsect.getDistAlongCenterline() - centerlineDist;
    float length     = xsect.getXsectLineLength();
    float pi         = (float)Math.PI;
    float theta      = 0.0f; // angle of centerline segment, radians
    float thetaXsect = 0.0f; //angle of cross-section line
    //coordinates of intersection of centerline segment & xsect line
    float xi         = 0.0f;
    float yi         = 0.0f;
    
    theta = CsdpFunctions.getTheta(x1,x2,y1,y2);
    
    if(DEBUG)System.out.println
	       ("x1y1x2y2,theta="+x1+","+y1+" "+x2+","+y2+" "+theta);

    xi = x1+dist*(float)Math.cos(theta);
    yi = y1+dist*(float)Math.sin(theta);

    if(DEBUG)System.out.println("intersection coordinates x,y="+xi+","+yi);
    
    thetaXsect = theta+0.5f*pi;
    
    returnValues[CsdpFunctions.x1Index] = xi+(length/2)*(float)Math.cos(thetaXsect);
    returnValues[CsdpFunctions.y1Index] = yi+(length/2)*(float)Math.sin(thetaXsect);
    returnValues[CsdpFunctions.x2Index] = xi+(length/2)*(float)Math.cos(thetaXsect+pi);
    returnValues[CsdpFunctions.y2Index] = yi+(length/2)*(float)Math.sin(thetaXsect+pi);
    
    return returnValues;
  }//findXsectLineCoord

  /**
   * stores name of centerline
   */
  protected void putCenterlineName(String name){
    _centerlineNames.put(getNumCenterlines(),name);
  }
  
  /**
   * returns name of centerline
   */
  protected String getCenterlineName(int index){
    return _centerlineNames.get(index);
  }

  /**
   * returns instance of centerline that has been selected
   */
  public Centerline getSelectedCenterline(){
    return _selectedCenterline;
  }

  /**
   * return name of selected centerline.  Names will usually be DSM channel numbers,
   * but they don't have to be.
   */
  public String getSelectedCenterlineName(){
    return _selectedCenterlineName;
  }

  /**
   * return number of selected cross-section.  Numbers start at zero.
   */
  public int getSelectedXsectNum(){
    return _selectedXsectNum;
  }

  /**
   * returns instance of xsect that has been selected
   */
  public Xsect getSelectedXsect(){
    return _selectedXsect;
  }

  /**
   * store the name of the selected centerline.  The name will usually be a DSM 
   * channel number, but not necessarily.
   */
  public void setSelectedCenterlineName(String name){
    _selectedCenterlineName = name;
  }

  /**
   * store the number of the selected cross-section.  The numbers start at zero.
   */
  public void setSelectedXsectNum(int num){
    _selectedXsectNum = num;
  }

  /**
    * assign selected centerline to class variable
    */
  public void setSelectedCenterline(Centerline c){
    _selectedCenterline = c;
  }

  /**
    * assign selected xsect to class variable
    */
  public void setSelectedXsect(Xsect x){
    _selectedXsect = x;
  }

  /**
   * store name of selected centerline.
   */
  public void setNewCenterlineName(String name){
    _newCenterlineName = name;
  }

  public float getMinX(){
    if(_isUpdated || _maxMinFound == false) findMaxMin();
    return _minX;
  }
  public float getMaxX(){
    if(_isUpdated || _maxMinFound == false) findMaxMin();
    return _maxX;
  }
  public float getMinY(){
    if(_isUpdated || _maxMinFound == false) findMaxMin();
    return _minY;
  }
  public float getMaxY(){
    if(_isUpdated || _maxMinFound == false) findMaxMin();
    return _maxY;
  }

  public void findMaxMin(){
    _minX = CsdpFunctions.BIG_FLOAT;
    _maxX = -CsdpFunctions.BIG_FLOAT;
    _minY = CsdpFunctions.BIG_FLOAT;
    _maxY = -CsdpFunctions.BIG_FLOAT;
    Centerline centerline = null;
    CenterlinePoint cp = null;
    for(int i=0; i<=getNumCenterlines()-1; i++){
      centerline = getCenterline(_centerlineNames.get(i));
      for(int j=0; j<=centerline.getNumCenterlinePoints()-1; j++){
	cp = centerline.getCenterlinePoint(j);
	if(_minX > cp.getX()) _minX = cp.getX();
	if(_maxX < cp.getX()) _maxX = cp.getX();
	if(_minY > cp.getY()) _minY = cp.getY();
	if(_maxY < cp.getY()) _maxY = cp.getY();
      }
    }
  }//findMaxMin

  protected String _oldCenterlineName = null;
  protected String _newCenterlineName = null;
  /**
   * centerline that has been selected by the user
   */
  protected Centerline _selectedCenterline;
  /**
   * xsect that has been selected by the user
   */
  protected Xsect _selectedXsect;
  /**
   * name of centerline that has been selected
   */
  protected String _selectedCenterlineName;
  /**
   * number of xsect that has been selected.  The numbers start with zero, with
   * xsect number zero being the closest xsect to the upstream end.
   */
  protected int _selectedXsectNum;
  /**
   * the number of centerlines in the network
   */
  protected int _numCenterlines;
  /**
   * the name of the network.  no use for this now; currently only one instance of
   * the Network class is allowed.
   */
  protected String _networkName=null;
  /**
   * stores all Centerline objects
   */
  protected Hashtable _centerlines = new Hashtable();
  /**
   * stores all centerline names
   */
  protected ResizableStringArray _centerlineNames = new ResizableStringArray();
  /**
   * turn on to print debugging statements
   */
  protected static final boolean DEBUG = false; 
  /**
   * true if any change has been made to the network or anything it contains
   * This value is checked when exiting program, loading new network, etc.
   */
  protected boolean _isUpdated = false;
  protected boolean _maxMinFound = false;
  /**
   * array index of 
   */
  protected final int distIndex = 4;
  protected float _maxX;
  protected float _minX;
  protected float _maxY;
  protected float _minY;
} // class Network

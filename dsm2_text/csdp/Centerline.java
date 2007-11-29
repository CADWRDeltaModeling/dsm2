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
 * A centerline contains centerline points and cross-sections.  Centerline points
 * are stored in order.
 *
 * @author
 * @version
 */
public class Centerline{
  public Centerline(String name){
    _centerlineName = name;
  }
  /**
   * returns number of points in the centerline
   */
  public int getNumCenterlinePoints(){
    return _numCenterlinePoints;
  }
  
    /**
     * Returns the normalized distance, which is the distance from the upstream
     * end of the centerline to the cross-section divided by the total length
     * of the centerline.
     */
    public float getNormalizedDist(Xsect xsect){
	return xsect.getDistAlongCenterline()/getLength();
    }

  /**
   * calculates length of centerline
   */
  public float getLength(){
    float length=0.0f;
    float x1;
    float y1;
    float x2;
    float y2;
    for(int i=0; i<=getNumCenterlinePoints()-2; i++){
	x1 = getCenterlinePoint(i).getX();
	y1 = getCenterlinePoint(i).getY();
	x2 = getCenterlinePoint(i+1).getX();
	y2 = getCenterlinePoint(i+1).getY();
	length += CsdpFunctions.pointDist(x1,y1,x2,y2);
    }
    return length;
  }
  
  /**
   * returns centerline point object at specified index.
   */
  public CenterlinePoint getCenterlinePoint(int index){
    return ((CenterlinePoint)_centerlinePoints.elementAt(index));
  }
  
  /**
   * create new CenterlinePoint object, add to _centerlinePoints vector.
   */
  public void addCenterlinePoint(){
    CenterlinePoint point = new CenterlinePoint();
    _centerlinePoints.addElement(point);
    _numCenterlinePoints++;
    _maxMinCalled = false;
  }
  
  /**
   * create new CenterlinePoint object, add to _centerlinePoints vector.
   */
  public void addCenterlinePoint(float x, float y){
    CenterlinePoint point = new CenterlinePoint();
    _centerlinePoints.addElement(point);
    point.putX(x);
    point.putY(y);
    _numCenterlinePoints++;
    _maxMinCalled = false;
  }

  /**
   * insert new CenterlinePoint object, add to _centerlinePoints vector.
   */
  public void insertCenterlinePoint(float x, float y){
    int numPoints = getNumCenterlinePoints();
    CenterlinePoint point;
    CenterlinePoint point1;
    CenterlinePoint point2;
    float x1;
    float x2;
    float y1;
    float y2;
    float minDist = CsdpFunctions.BIG_FLOAT;
    float dist;
    int minDistIndex = CsdpFunctions.BIG_INT;
    CenterlinePoint newPoint = new CenterlinePoint();
    newPoint.putX(x);
    newPoint.putY(y);
    for(int i=0; i<=numPoints-2; i++){
      point1 = getCenterlinePoint(i);
      point2 = getCenterlinePoint(i+1);
      x1 = point1.getX();
      y1 = point1.getY();
      x2 = point2.getX();
      y2 = point2.getY();
      
      dist = CsdpFunctions.shortestDistLineSegment(x1, x2, x, y1, y2, y);
      if(dist < minDist){
	minDist = dist;
	minDistIndex = i;
      }
    }//for i      
    _centerlinePoints.insertElementAt(newPoint, minDistIndex+1);
    _numCenterlinePoints++;
  }//insertCenterlinePoint
  
  /**
   * delete CenterlinePoint
   */
  public void deleteCenterlinePoint(float x, float y){
    int minDistIndex = getNearestPointIndex(x,y);
    _centerlinePoints.removeElementAt(minDistIndex);
    _numCenterlinePoints--;
  }

  /**
   * sort array that stores all of the xsects in this centerline.  sort by distance
   * from upstream end.  If an XsectGraph is open for this xsect, rename the graph.
   */
  public ResizableIntArray sortXsectArray(){
      int numXsects = getNumXsects();
    float[] distances = new float[numXsects];
    float[] unsortedDistances = new float[numXsects];
    float[] sortedDistances = new float[numXsects];
    ResizableIntArray xsectIndices = new ResizableIntArray(1,1);
    int left = 0;
    int right = numXsects-1;
    Vector sortedXsects = new Vector();
    Xsect xsect = null;
    for(int i=0; i<=right; i++){
      xsect = getXsect(i);
      distances[i] = xsect.getDistAlongCenterline();
      unsortedDistances[i] = distances[i];
      if(DEBUG)System.out.println("i,unsortedDistances[i]="+i+","+unsortedDistances[i]);
    }//for

    sortedDistances = CsdpFunctions.qsort(distances, left, right);

    for(int i=0; i<=right; i++){
        int index=0;
        while(unsortedDistances[i] != sortedDistances[index]) index++;
        xsectIndices.put(i,index);
	if(DEBUG)System.out.println("xsectIndices: i,value="+i+","+index);
    }//for
    
    for(int i=0; i<=right; i++){
	xsect=getXsect(xsectIndices.get(i));
	xsect.putDistAlongCenterline(sortedDistances[i]);
	sortedXsects.addElement(xsect);
    }
    _xsects = sortedXsects;

    return xsectIndices;
  }//sortXsectArray

  /**
   * returns point that is closest to specified coord.
   */
  protected int getNearestPointIndex(float x, float y){
    CenterlinePoint point;
    float minDist = CsdpFunctions.BIG_FLOAT;
    int minDistIndex = CsdpFunctions.BIG_INT;
    float dist;
    float x1;
    float y1;
    for(int i=0; i<=getNumCenterlinePoints()-1; i++){
      point = getCenterlinePoint(i);
      x1 = point.getX();
      y1 = point.getY();
      dist = (float)CsdpFunctions.pointDist(x, y, x1, y1);
      if(DEBUG)System.out.println("e.x, e.y, point#, x1, y1, dist"+
			 x+","+y+" "+i+" "+x1+","+y1+" "+dist); 
      if(dist < minDist){
	minDist = dist;
	minDistIndex = i;
      }//if dist
    }//for i
      return minDistIndex;
  }//getNearestPoint

  /**
   * reverse order of points
   */
  public void reverseOrder(){
    Vector temp = new Vector();
    for(int i=0; i<=getNumCenterlinePoints()-1; i++){
      temp.addElement(getCenterlinePoint(getNumCenterlinePoints()-1-i));
    }   
    for(int i=0; i<=getNumCenterlinePoints()-1; i++){
      _centerlinePoints.setElementAt(temp.elementAt(i),i);
    }
  }

  /**
   * returns number of cross-sections in the centerline
   */
  public int getNumXsects(){
    return _numXsects;
  }

    public int getNumXsectsWithPoints(){
	int returnValue = 0;
	for(int i=0; i<=getNumXsects()-1; i++){
	    if(getXsect(i).getNumPoints() > 0){
		returnValue++;
	    }
	}
	return returnValue;
    }

  /**
   * add a cross-section
   */
  public void addXsect(){    
    _numXsects++;
    Xsect xsect = new Xsect();
    _xsects.addElement(xsect);
  }

  /**
   * remove a cross-section
   */
  public void removeXsect(int index){
    if(_xsects.size() > 0){
      _numXsects--;
      _xsects.removeElementAt(index);    


    }
  }//removeXsect

  /**
   * insert a cross-section at specified index
   */
  public void addXsectAt(int index){
    _numXsects++;
    Xsect xsect = new Xsect();
    _xsects.insertElementAt(xsect, index);
  }

  /**
   * returns xsect object at specified index
   */
  public Xsect getXsect(int index){    
    return (Xsect)_xsects.elementAt(index);
  }

    /**
     * returns elevation of lowest point in the centerline.
     */
    public float getMinimumElevation(){
	Xsect xsect = null;
	float minElev = CsdpFunctions.BIG_FLOAT;
	getAllXsects();

	for(int i=0; i<=_numCombinedXsects-1; i++){
	    xsect = (Xsect)(_allXsects.get(i));
	    if(xsect.getMinimumElevation() < minElev){    
		minElev = xsect.getMinimumElevation();
	    }
	}
	return minElev;
    }//getMinimumElevation

//      /**
//       * Calculate volume of centerline for specified elevation
//       */    


//      public float getVolume(float elev){
//  	float totalVolume = 0.0f;
//  	Xsect xsect1 = null;
//  	Xsect xsect2 = null;
//  	float distance1 = -CsdpFunctions.BIG_FLOAT;
//  	float distance2 = -CsdpFunctions.BIG_FLOAT;
//  	float area1 = -CsdpFunctions.BIG_FLOAT;
//  	float area2 = -CsdpFunctions.BIG_FLOAT;

//  	//estimate volume of upstream portion of channel(between
//  	//upstream node and first cross-section).

//  	if(getNumXsects() > 0){
//  	    xsect1 = getXsect(0);
//  	    distance1 = xsect1.getDistAlongCenterline();
//  	    area1 = xsect1.getArea(elev);
//  	    totalVolume = distance1 * area1;
//  	    for(int j=1; j<=getNumXsects()-2; j++){
//  		xsect1 = getXsect(j);
//  		xsect2 = getXsect(j+1);
//  		distance1 = xsect1.getDistAlongCenterline();
//  		distance2 = xsect2.getDistAlongCenterline();
//  		area1 = xsect1.getArea(elev);
//  		area2 = xsect2.getArea(elev);
//  		totalVolume += Math.abs( (distance2-distance1) *0.5f* (area2-area1) );
//  	    }
//  	    //estimate volume of downstream portion of channel
//  	    //(between last cross-section and downstream node).
//  	    xsect2 = getXsect(getNumXsects()-1);
//  	    distance2 = Math.abs(getLength() - xsect2.getDistAlongCenterline());
//  	    area2 = xsect2.getArea(elev);
//  	    totalVolume += area2 * distance2;
//  	}else{
//  	    totalVolume = 0.0f;
//  	}	

//  	return totalVolume;
//      }

    /** 
     * make array of all xsects(including copies, if any)
     * if distances match, eliminate the ones that aren't copied and if copies
     * override each other, keep the most recent.
     */
    private void getAllXsects(){
	_numCombinedXsects = 0;
	_allXsects.removeAllElements();
	for(int i=0; i<=getNumXsects()-1; i++){
	    Xsect xsect = getXsect(i);
	    if(xsect.getNumPoints() > 0){
		_allXsects.insertElementAt(xsect, _numCombinedXsects);
		_numCombinedXsects++;
	    }else{
		if(DEBUG)System.out.println("calculating average area");
		System.out.println("not using cross-section "+getCenterlineName()+"_"+i+" because it has no points");
	    }
	}
	for(int j=0; j<=getNumCopiedXsects()-1; j++){
	    _allXsects.insertElementAt(getCopiedXsect(j), _numCombinedXsects);
	    _numCombinedXsects++;
	}
	for(int k=0; k<=getNumRectXsects()-1; k++){
	    _allXsects.insertElementAt
		(getRectXsect(k), _numCombinedXsects);
	    _numCombinedXsects++;

	    if(DEBUG)System.out.println("inserting rectxs for centerline "+getCenterlineName());
	    if(DEBUG)System.out.println("area at 0 ft ngvd="+getRectXsect(k).getArea(0.0f));
	}

	//sort all xsects by distAlongCenterline
	CsdpFunctions.qsort(_allXsects, 0, _numCombinedXsects-1);
    }

    /**
     * Calculate weighted average of cross-sectional areas.
     */
    public float getAverageArea(float elev, float chanLength){
	float averageArea = -CsdpFunctions.BIG_FLOAT;
	Xsect xsect1 = null;
	Xsect xsect2 = null;
	float normalizedDistance1 = -CsdpFunctions.BIG_FLOAT;
	float normalizedDistance2 = -CsdpFunctions.BIG_FLOAT;
	float area1 = -CsdpFunctions.BIG_FLOAT;
	float area2 = -CsdpFunctions.BIG_FLOAT;
	float normalizedDistLast = -CsdpFunctions.BIG_FLOAT;

	getAllXsects();

	//estimate volume of upstream portion of channel(between
	//upstream node and first cross-section).
	if(_numCombinedXsects > 0){
	    float normalizedDistFirst = -CsdpFunctions.BIG_FLOAT;
	    xsect1 = (Xsect)(_allXsects.elementAt(0));
	    normalizedDistFirst = getNormalizedDist(xsect1);
	    //if it's a copied cross-section, the distAlongCenterline is actually
	    //the normalized dist.
	    if(Float.isNaN(normalizedDistFirst) || normalizedDistFirst < 0.0f
	       || normalizedDistFirst > CsdpFunctions.BIG_FLOAT) {
		normalizedDistFirst = xsect1.getDistAlongCenterline();
	    }
	    area1 = xsect1.getArea(elev);
	    averageArea = area1*normalizedDistFirst;

	    if(_numCombinedXsects == 1){
		xsect1 = (Xsect)(_allXsects.elementAt(0));
		averageArea = xsect1.getArea(elev);
	    }
	    else if(_numCombinedXsects >= 2){
		for(int j=0; j<=_numCombinedXsects-2; j++){
		    xsect1 = (Xsect)(_allXsects.elementAt(j));
		    xsect2 = (Xsect)(_allXsects.elementAt(j+1));
		    normalizedDistance1 = getNormalizedDist(xsect1);
		    normalizedDistance2 = getNormalizedDist(xsect2);

		    //This assumes that if all cross-sections are rectangular,
		    //there will be exactly 2 cross-sections
		    if(getNumRectXsects() == _numCombinedXsects){
			normalizedDistance1 = 0.0f;
			normalizedDistance2 = 1.0f;
		    }

		    if(Float.isNaN(normalizedDistance1) || normalizedDistance1<0.0f
		       || normalizedDistance1 > CsdpFunctions.BIG_FLOAT){
			normalizedDistance1 = xsect1.getDistAlongCenterline();
		    }
		    if(Float.isNaN(normalizedDistance2) || normalizedDistance2<0.0f
		       || normalizedDistance2 > CsdpFunctions.BIG_FLOAT){
			normalizedDistance2 = xsect2.getDistAlongCenterline();
		    }
		    area1 = xsect1.getArea(elev);
		    area2 = xsect2.getArea(elev);

		    averageArea += Math.abs((area2+area1) *0.5f*
					    (normalizedDistance2-normalizedDistance1));
		}//for j
	    }//if 2 or more cross-sections
	    //estimate volume of downstream portion of channel
	    //(between last cross-section and downstream node).

	    xsect2 = (Xsect)(_allXsects.elementAt(_numCombinedXsects-1));

	    normalizedDistLast = getNormalizedDist(xsect2);
	    if(Float.isNaN(normalizedDistLast) || normalizedDistLast < 0.0f
	       || normalizedDistLast > CsdpFunctions.BIG_FLOAT){
		normalizedDistLast = xsect2.getDistAlongCenterline();
	    }
	    if(getNumRectXsects() == _numCombinedXsects) normalizedDistLast = 1.0f;

	    area2 = xsect2.getArea(elev);
	    averageArea += area2 * (1.0f - normalizedDistLast);
	}else{
	    System.out.println("No xs found; setting average area to zero");
	    averageArea = 0.0f;
	}	

	return averageArea;
    }//getAverageArea

    /**
     * Calculate weighted average of cross-sectional widths.
     */
    public float getAverageWidth(float elev, float chanLength){
	float averageWidth = -CsdpFunctions.BIG_FLOAT;
	Xsect xsect1 = null;
	Xsect xsect2 = null;
	float normalizedDistance1 = -CsdpFunctions.BIG_FLOAT;
	float normalizedDistance2 = -CsdpFunctions.BIG_FLOAT;
	float width1 = -CsdpFunctions.BIG_FLOAT;
	float width2 = -CsdpFunctions.BIG_FLOAT;
	float normalizedDistLast = -CsdpFunctions.BIG_FLOAT;

	getAllXsects();
	//estimate volume of upstream portion of channel(between
	//upstream node and first cross-section).

	if(_numCombinedXsects > 0){

	    float normalizedDistFirst = -CsdpFunctions.BIG_FLOAT;
	    xsect1 = (Xsect)(_allXsects.elementAt(0));
	    normalizedDistFirst = getNormalizedDist(xsect1);
	    if(Float.isNaN(normalizedDistFirst) || normalizedDistFirst < 0.0f
	       || normalizedDistFirst > CsdpFunctions.BIG_FLOAT){
		normalizedDistFirst = xsect1.getDistAlongCenterline();
	    }
	    width1 = xsect1.getWidth(elev);
	    averageWidth = width1*normalizedDistFirst;

	    if(_numCombinedXsects == 1){
		xsect1 = (Xsect)(_allXsects.elementAt(0));
		averageWidth = xsect1.getWidth(elev);
	    }
	    else if(_numCombinedXsects >= 2){
		for(int j=0; j<=_numCombinedXsects-2; j++){
		    xsect1 = (Xsect)(_allXsects.elementAt(j));
		    xsect2 = (Xsect)(_allXsects.elementAt(j+1));
		    normalizedDistance1 = getNormalizedDist(xsect1);
		    normalizedDistance2 = getNormalizedDist(xsect2);
		    if(getNumRectXsects() == _numCombinedXsects){
			normalizedDistance1 = 0.0f;
			normalizedDistance2 = 1.0f;
		    }
		    if(Float.isNaN(normalizedDistance1) || normalizedDistance1<0.0f
		       || normalizedDistance1 > CsdpFunctions.BIG_FLOAT){
			normalizedDistance1 = xsect1.getDistAlongCenterline();
		    }
		    if(Float.isNaN(normalizedDistance2) || normalizedDistance2<0.0f
		       || normalizedDistance2 > CsdpFunctions.BIG_FLOAT){
			normalizedDistance2 = xsect2.getDistAlongCenterline();
		    }
		    width1 = xsect1.getWidth(elev);
		    width2 = xsect2.getWidth(elev);
		    
		    averageWidth += Math.abs((width2+width1) *0.5f*
					    (normalizedDistance2-normalizedDistance1));

		}
	    }//if 2 or more cross-sections
	    //estimate volume of downstream portion of channel
	    //(between last cross-section and downstream node).
	    xsect2 = (Xsect)(_allXsects.elementAt(_numCombinedXsects-1));
	    normalizedDistLast = getNormalizedDist(xsect2);
	    if(Float.isNaN(normalizedDistLast) || normalizedDistLast < 0.0f
	       || normalizedDistLast > CsdpFunctions.BIG_FLOAT){
		normalizedDistLast = xsect2.getDistAlongCenterline();
	    }
	    if(getNumRectXsects() == _numCombinedXsects) normalizedDistLast = 1.0f;
	    width2 = xsect2.getWidth(elev);
	    averageWidth += width2 * (1.0f - normalizedDistLast);
	}else{
	    averageWidth = 0.0f;
	}	

	return averageWidth;
    }//getAverageWidth

    /**
     * Calculate weighted average of cross-sectional wettedPerimeters.
     */
    public float getAverageWettedPerimeter(float elev, float chanLength){
	float averageWettedPerimeter = -CsdpFunctions.BIG_FLOAT;
	Xsect xsect1 = null;
	Xsect xsect2 = null;
	float normalizedDistance1 = -CsdpFunctions.BIG_FLOAT;
	float normalizedDistance2 = -CsdpFunctions.BIG_FLOAT;
	float wettedPerimeter1 = -CsdpFunctions.BIG_FLOAT;
	float wettedPerimeter2 = -CsdpFunctions.BIG_FLOAT;
	float normalizedDistLast = -CsdpFunctions.BIG_FLOAT;

	getAllXsects();
	//estimate volume of upstream portion of channel(between
	//upstream node and first cross-section).
	//	if(getNumXsects() > 0){
	if(_numCombinedXsects >0){

	    
	    float normalizedDistFirst = -CsdpFunctions.BIG_FLOAT;

	    xsect1=(Xsect)(_allXsects.elementAt(0));

	    normalizedDistFirst = getNormalizedDist(xsect1);
	    if(Float.isNaN(normalizedDistFirst) || normalizedDistFirst < 0.0f
	       || normalizedDistFirst > CsdpFunctions.BIG_FLOAT){
		normalizedDistFirst = xsect1.getDistAlongCenterline();
	    }
	    wettedPerimeter1 = xsect1.getWettedPerimeter(elev);
	    averageWettedPerimeter = wettedPerimeter1*normalizedDistFirst;

	    if(_numCombinedXsects == 1){
		xsect1 = (Xsect)(_allXsects.elementAt(0));
		averageWettedPerimeter = xsect1.getWettedPerimeter(elev);
	    }
	    else if(_numCombinedXsects >= 2){
		for(int j=0; j<=_numCombinedXsects-2; j++){
		    xsect1 = (Xsect)(_allXsects.elementAt(j));
		    xsect2 = (Xsect)(_allXsects.elementAt(j+1));

		    normalizedDistance1 = getNormalizedDist(xsect1);
		    normalizedDistance2 = getNormalizedDist(xsect2);
		    if(getNumRectXsects() == _numCombinedXsects){
			normalizedDistance1 = 0.0f;
			normalizedDistance2 = 1.0f;
		    }
		    if(Float.isNaN(normalizedDistance1) || normalizedDistance1<0.0f
		       || normalizedDistance1 > CsdpFunctions.BIG_FLOAT){
			normalizedDistance1 = xsect1.getDistAlongCenterline();
		    }
		    if(Float.isNaN(normalizedDistance2) || normalizedDistance2<0.0f
		       || normalizedDistance2 > CsdpFunctions.BIG_FLOAT){
			normalizedDistance2 = xsect2.getDistAlongCenterline();
		    }
		    wettedPerimeter1 = xsect1.getWettedPerimeter(elev);
		    wettedPerimeter2 = xsect2.getWettedPerimeter(elev);
		    
		    averageWettedPerimeter += 
			Math.abs((wettedPerimeter2+wettedPerimeter1) *0.5f*
				 (normalizedDistance2-normalizedDistance1));

		}
	    }//if 2 or more cross-sections
	    //estimate volume of downstream portion of channel
	    //(between last cross-section and downstream node).
	    xsect2 = (Xsect)(_allXsects.elementAt(_numCombinedXsects-1));

	    normalizedDistLast = getNormalizedDist(xsect2);
	    if(Float.isNaN(normalizedDistLast) || normalizedDistLast < 0.0f
	       || normalizedDistLast > CsdpFunctions.BIG_FLOAT){
		normalizedDistLast = xsect2.getDistAlongCenterline();
	    }
	    if(getNumRectXsects() == _numCombinedXsects) normalizedDistLast = 1.0f;
	    wettedPerimeter2 = xsect2.getWettedPerimeter(elev);
	    averageWettedPerimeter += wettedPerimeter2 * (1.0f - normalizedDistLast);
	}else{
	    averageWettedPerimeter = 0.0f;
	}

	return averageWettedPerimeter;
    }//getAverageWettedPerimeter


    /**
     * returns width weighted hydraulic depth using all xs in centerline.
     */
    public float getWidthWeightedHydraulicDepth(float elev){
	float wwhd = 0.0f;
        float sumOfWidths = 0.0f;
	float sumOfWidthTimesHD = 0.0f;
	float width = -CsdpFunctions.BIG_FLOAT;
	float hd = -CsdpFunctions.BIG_FLOAT;
	Xsect xsect = null;

	for(int i=0; i<=getNumXsects()-1; i++){
	    xsect = getXsect(i);
	    width = xsect.getWidth(elev);
	    hd = xsect.getHydraulicDepth(elev);
	    sumOfWidths += width;
	    sumOfWidthTimesHD += width * hd;
	}
	wwhd = sumOfWidthTimesHD / sumOfWidths;
	if(wwhd < 0) System.out.println("ERROR in Centerline.getWidthWeightedHydraulic Depth:  value="+wwhd);

	return wwhd;
    }

    public Xsect getCopiedXsect(int index){
	return (Xsect)(_copiedXsects.elementAt(index));
    }

    public Xsect getRectXsect(int index){
	return (Xsect)(_rectXsects.elementAt(index));
    }

    /**
     * Adds a cross-section that is copied from another centerline
     * Distance along centerline should already be set in the xsect object.
     */
    public void addCopiedXsect(Xsect xsect){
	_numCopiedXsects++;
	_copiedXsects.addElement(xsect);
    }

//      /**
//       * adds all cross-sections that should be copied
//       */
//      public void addCopiedXsects(Network net, IrregularXsectsInp ixi){
//  	IrregularXsectsInp.IXIChan ixiChan = null;
//  	String centerlineName = getCenterlineName();
//  	Centerline otherCenterline = null;
//  	StringTokenizer t = null;
//  	String lastToken = null;
//  	String nextToLastToken = null;
	
//  	if(ixi.chanExists(centerlineName)){

//  	    System.out.println("inside Centerline.addCopiedXsects.  found copy for centerline "+centerlineName);

//  	    ixiChan = ixi.getChan(centerlineName);
//  	    for(int i=0; i<=ixiChan.getNumLines()-1; i++){
//  		//delimiters=tab, space, forward slash, backslash
//  		t = new StringTokenizer(ixiChan.getLine(i).getFilename(),
//  					"\t \057\134");
//  		for(int j=0; j<=t.countTokens()-1; j++){
//  		    nextToLastToken = lastToken;
//  		    lastToken = t.nextToken();
//  		}



//  		//DON't need otherCenterline!


//  		if(centerlineName.equals(nextToLastToken) == false){
//  		    otherCenterline = net.getCenterline(nextToLastToken);
//  		}else{
//  		    otherCenterline = this;
//  		}
//  		for(int k=0; k<=otherCenterline.getNumXsects()-1; k++){
//  		    t = new StringTokenizer(lastToken,".");
//  		    for(int m=0; m<=t.countTokens()-1; m++){
//  			nextToLastToken = lastToken;
//  			lastToken=t.nextToken();
//  		    }//for m
//  		    if(otherCenterline.getNormalizedDist
//  		       (otherCenterline.getXsect(k)) ==
//  		       Float.parseFloat(nextToLastToken)){
//  			addCopiedXsect(otherCenterline.getXsect(k));
//  		    }//if
//  		}//for k
//  	    }//for i
//  	}//if
//      }//addCopiedXsects

    /**
     * Adds rectangular cross-sections using dimensions from xsects.inp file.
     * Only used for rectangular cross-section calculation.
     */
    public void addRectangularXsects(DSMChannels dsmChannels, XsectsInp xi){
	String centerlineName = getCenterlineName();
	int xsect1 = dsmChannels.getXsect1(centerlineName);
	int dist1  = dsmChannels.getDist1(centerlineName);
	int xsect2 = dsmChannels.getXsect2(centerlineName);
	int dist2  = dsmChannels.getDist2(centerlineName);

	if(DEBUG)System.out.println("addRectangularXsects for centerline "+getCenterlineName()+":  dist1, dist2="+dist1+","+dist2);

//  	String x1 = new Integer(xsect1).toString();
//  	String d1 = new Integer(dist1).toString();
//  	String x2 = new Integer(xsect2).toString();
//  	String d2 = new Integer(dist2).toString();

	String x1 = Integer.toString(xsect1);
	String d1 = Integer.toString(dist1);
	String x2 = Integer.toString(xsect2);
	String d2 = Integer.toString(dist2);

	float width1 = xi.getWidth(x1);
	float botelv1 = xi.getBotelv(x1);
	float width2 = xi.getWidth(x2);
	float botelv2 = xi.getBotelv(x2);

	if(DEBUG)System.out.println("adding rect xs.  width1,width2,botelv1,botelv2="+
			   width1+","+width2+","+botelv1+","+botelv2);
	_numRectXsects+=2;
	Xsect xs1 = new Xsect();
	Xsect xs2 = new Xsect();
	xs1.putDistAlongCenterline(0.0f);
	xs1.addXsectPoint(0.0f, 100.0f);
	xs1.addXsectPoint(0.0f, botelv1);
	xs1.addXsectPoint(width1, botelv1);
	xs1.addXsectPoint(width1, 100.0f);

	xs2.putDistAlongCenterline(0.0f);
	xs2.addXsectPoint(0.0f, 100.0f);
	xs2.addXsectPoint(0.0f, botelv2);
	xs2.addXsectPoint(width2, botelv2);
	xs2.addXsectPoint(width2, 100.0f);

	_rectXsects.addElement(xs1);
	_rectXsects.addElement(xs2);
	
    }
    
    public int getNumCopiedXsects(){
	return _numCopiedXsects;
    }

    public int getNumRectXsects(){
	return _numRectXsects;
    }
  
  /**
   * returns name of centerline
   */
  public String getCenterlineName(){
    return _centerlineName;
  }
  
  /**
   * sets name of centerline
   */
  public void setCenterlineName(String name){
    _centerlineName = name;
  }
  
  /**
   * returns minimum value of X
   */
  private float getMinX(){return _minX;}
  /**
   * returns maximum value of X
   */
  private float getMaxX(){return _maxX;}
  /**
   * returns minimum value of Y
   */
  private float getMinY(){return _minY;}
  /**
   * returns maximum value of Y
   */
  private float getMaxY(){return _maxY;}
  
  private int _numCenterlinePoints;
  private Vector _centerlinePoints = new Vector();
  private Vector _xsects = new Vector();
  private String _centerlineName = null;
  private float _maxX=CsdpFunctions.SMALL_FLOAT;
  private float _minX=CsdpFunctions.BIG_FLOAT;
  private float _maxY=CsdpFunctions.SMALL_FLOAT;
  private float _minY=CsdpFunctions.BIG_FLOAT;
  private boolean _maxMinCalled = false;
  private static final boolean DEBUG = false;

    /**
     * number of irregular xsects
     */ 
    private int _numXsects = 0;
    private int _numRectXsects = 0;
    private Vector _rectXsects = new Vector();
    private Vector _copiedXsects = new Vector();
    private int _numCopiedXsects = 0;

  Rectangle _r;
    private int _numCombinedXsects = 0;
    private Vector _allXsects = new Vector();
}//class Centerline

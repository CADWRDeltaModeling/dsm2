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
import DWR.CSDP.semmscon.UseSemmscon;
import java.awt.*;
import java.util.*;
import java.io.*;


/**
 * Global functions and parameters for CSDP
 */
public class CsdpFunctions{

  /**
   * finds the distance between two points
   */
  public static float pointDist(float x1, float y1, float x2, float y2){
    return (float)(Math.sqrt( (x1-x2)*(x1-x2) + (y1-y2)*(y1-y2) ));
  }//pointDist

  /**
   * finds the distance between two points
   */
  public static int pointDist(int x1, int y1, int x2, int y2){
    float x1f = (float)x1;
    float x2f = (float)x2;
    float y1f = (float)y1;
    float y2f = (float)y2;
    float p = pointDist(x1f, y1f, x2f, y2f);
    return (int)p;
  }//pointDist

    /**
     * Convert station (xsect point horizontal coordinate in relative coord. sys)
     * to UTM, using x1, x2 as origin and x2, y2 as other point in cross-section 
     * line.  Used for 3D network output.
     */
    public static float stationToX(float x1, float x2, float y1, float y2, 
				   float station){
	//use imaginary horizontal line with same y value as first point to find theta
	float theta = getTheta(x1, x2, y1, y2);
	return x1 + station*(float)Math.cos(theta);
    }

    /**
     * Convert station (xsect point horizontal coordinate in relative coord. sys)
     * to UTM, using x1, x2 as origin and x2, y2 as other point in cross-section 
     * line.  Used for 3D network output.
     */
    public static float stationToY(float x1, float x2, float y1, float y2, 
				   float station){
	//use imaginary horizontal line with same y value as first point to find theta
	float theta = getTheta(x1, x2, y1, y2);
	if(DEBUG){
	    System.out.println("got theta.  x1,x2,y1,y2,station="+x1+","+x2+","+
			       ","+y1+","+y2+","+station);
	    System.out.println("theta="+theta);
	}
	return y1 + station*(float)Math.sin(theta);
    }

  /**
   * find x coordinate of intersection of 2 perpendicular lines. 1st line is defined
   * by (x1,y2) and (x2,y2) and the second line is defined by (x3,y3) and the
   * intersection.
   */
  public static float findXIntersection(float x1, float x2, float x3, 
					float y1, float y2, float y3){
    float xIntersect;
    if(x1 == x2) xIntersect = x1;
    else{
      float slope1 = ((y1-y2)/(x1-x2));
      xIntersect = (x3 + slope1*(y3-y1+slope1*x1)) / (slope1*slope1 + 1);
    }
    return xIntersect;
  }//findXIntersection

  /**
   * find y coordinate of intersection of 2 perpendicular lines. 1st line is defined
   * by (x1,y2) and (x2,y2) and the second line is defined by (x3,y3) and the
   * intersection
   */
  public static float findYIntersection(float x1, float x2, float x3, 
					float y1, float y2, float y3){
    float xIntersect;
    float yIntersect;
    if(x1 == x2) yIntersect = y3;
    else{
      float slope1 = ((y1-y2)/(x1-x2));
      xIntersect = (x3 + slope1*(y3-y1+slope1*x1)) / (slope1*slope1 + 1);
      yIntersect = slope1*(xIntersect - x1) + y1;
    }
    return yIntersect;
  }//findYIntersection

  /**
   * find the shortest distance between a line(infinite length) and a point.  
   * Points 1 and 2 are the line points, point 3 is the point.
   */
  public static float shortestDistLine(float x1, float x2, float x3, 
				       float y1, float y2, float y3){
    float xIntersect = findXIntersection(x1,x2,x3,y1,y2,y3);
    float yIntersect = findYIntersection(x1,x2,x3,y1,y2,y3);
    return (float)(Math.sqrt( Math.pow((xIntersect-x3),2) + 
			      Math.pow((yIntersect-y3),2) ));
  }//shortestDistLine

  /**
   * Find the shortest distance between a line segment (finite length) and a point.
   * Return maximum value if angle between line segment and line connecting the
   * point and either end of the line segment is > 90 degrees.
   * Points 1 and 2 are the line segment endpoints, point 3 is the point.  
   * polygonWidth is the width of the rectangular polygon which will be used to see 
   * if the selected point is close enough to the line segment.
   */
  public static float shortestDistLineSegment(float x1, float x2, float x3, 
					      float y1, float y2, float y3){
    float dist;
    float width;
    float xIntersect = findXIntersection(x1,x2,x3,y1,y2,y3);
    float yIntersect = findYIntersection(x1,x2,x3,y1,y2,y3);
    float theta = getTheta(x1,x2,y1,y2);

    if(DEBUG)System.out.println("shortestDistLineSegment x1x2x3y1y2y3="+x1+","+x2+","+x3+","+y1+","+y2+","+y3);

    //the polygon is a square region.  the length of a side is equal to the length
    //of the centerline segment
    width = pointDist(x1,y1,x2,y2);
    Polygon p = findPolygon(x1,x2,y1,y2,width);
    for(int i=0; i<=3; i++){
      if(DEBUG)System.out.println("polygon coordinates: point "+i+": x,y="+
				  p.xpoints[i]+","+p.ypoints[i]);
    }
    if(p.contains((int)x3,(int)y3)){
      dist = (float)(Math.sqrt( Math.pow((xIntersect-x3),2) + 
				Math.pow((yIntersect-y3),2) ));
    }
    else{
      dist = BIG_FLOAT;
    }
    return dist;
  }//shortestDistLineSegment

  /**
   * find the shortest distance between a line segment (finite length) and a point.
   * Return maximum value if angle between line segment and line connecting the
   * point and either end of the line segment is > 90 degrees.
   * Points 1 and 2 are the line segment endpoints, point 3 is the point.  
   * polygonWidth is the width of the rectangular polygon which will be used to see 
   * if the selected point is close enough to the line segment.
   */
  public static float shortestDistLineSegment(float x1, float x2, float x3, 
					      float y1, float y2, float y3,
					      float width){
    float dist = 0.0f;
    float xIntersect = findXIntersection(x1,x2,x3,y1,y2,y3);
    float yIntersect = findYIntersection(x1,x2,x3,y1,y2,y3);
    float theta = getTheta(x1,x2,y1,y2);

    if(DEBUG)System.out.println("shortestDistLineSegment x1x2x3y1y2y3="+
				x1+","+x2+","+x3+","+y1+","+y2+","+y3);

    //the polygon is a square region.  the length of a side is equal to the length
    //of the centerline segment
    Polygon p = findPolygon(x1,x2,y1,y2,width);
    for(int i=0; i<=3; i++){
      if(DEBUG)System.out.println("polygon coordinates: point "+i+": x,y="+
				  p.xpoints[i]+","+p.ypoints[i]);
    }
    if(p.contains((int)x3,(int)y3)){
      dist = (float)(Math.sqrt( Math.pow((xIntersect-x3),2) + 
				Math.pow((yIntersect-y3),2) ));
    }
    else{
      dist = BIG_FLOAT;
    }
    return dist;
  }//shortestDistLineSegment


  /**
   * convert feet to meters
   */
  public static float feetToMeters(float value){
    return value*0.3048f;
  }

  /**
   * convert meters to feet
   */
  public static float metersToFeet(float value){
    return value/0.3048f;
  }

  /**
   * convert pixels to UTM(meters) in x direction.
   *    xPixels is the value to be converted
   *    minSlope is the factor that is used to convert UTM to pixels.
   *    minX is the minimum west-east bathymetry coordinate
   */
  public static float xPixelsToLength(int xPixels, float minSlope, float minX){
    return ( (xPixels-1.0f)/(minSlope) )+minX;
  }//xPixelsToUTM

  /**
   * convert pixels to UTM(meters) in y direction
   *    yPixels is the value to be converted
   *    minSlope is the factor that is used to convert UTM to pixels.
   *    minY is the minimum north-south bathymetry coordinate
   *    height is the height of the frame in pixels
   */
  public static float yPixelsToLength(int yPixels, float minSlope, 
				   float minY, float height){
    return ( -(yPixels-height)/(minSlope) )+minY;
  }//yPixelsToUTM


  /**
   * Convert meters to pixels for plotting
   *    x is the value to be converted to pixels
   *    minSlope is the factor that is used to convert UTM to pixels.
   *    minX is the minimum west-east bathymetry coordinate
   */
  public static int xUTMToPixels(float minX, float minSlope, float x) {
    return (int)((minSlope)*(x-minX)+1);
  }

  /**
   * Convert meters to pixels for plotting
   *    y is the value to be converted to pixels
   *    minSlope is the factor that is used to convert UTM to pixels.
   *    minY is the minimum north-south bathymetry coordinate
   *    height is the height of the frame in pixels
   */
  public static int yUTMToPixels(float minY, float minSlope, float height, 
				 float y) {
      return (int)(-((minSlope)*(y-minY))+height);
  }

  /**
   * calculate angle in radians of centerline segment with respect to another.
   * The angle is measured from the line defined by points 1 and 2 to the line
   * defined by points 1 and 3.  Angles are measured in the counter clockwise
   * direction.
   */
  public static float getTheta(float x1, float x2, float x3, 
			       float y1, float y2, float y3){

    float theta  = 0.0f;
    float theta2 = 0.0f;
    float theta3 = 0.0f;
    
    theta3 = getTheta(x1, x3, y1, y3);
    theta2 = getTheta(x1, x2, y1, y2);
    theta = theta2 - theta3;
    
    return theta;
  }//getTheta
  
  /**
   * Calculates the angle of a line in radians defined by points 1 and 2, with 
   * point 1 at the origin.  The atan function alone is not sufficient because 
   * it only returns values in the range of -pi/2 < theta < pi/2.
   */
  public static float getTheta(float x1, float x2, float y1, float y2){
    float theta=0.0f;
    float slope=0.0f;
    float pi = (float)Math.PI;
    
    if(x2==x1){

      if(y2 > y1) theta = pi/2.0f;
      if(y2 < y1) theta = 3.0f*pi/2.0f;
    }
    else{
      slope = (y2-y1)/(x2-x1);
      if(DEBUG)System.out.println
		 ("tan(theta),x1,y1,x2,y2="+slope+" "+x1+","+y1+" "+x2+","+y2);
      theta = (float)Math.atan(slope);
      //if point 2 is to the left of point1, then it's in the 2nd or 3rd quadrants
      //and must add pi radians to angle
      if (x2<x1) theta+=pi;
    }
    return theta;
  }//getTheta

  /**
   * find rectangular polygon which is defined by a line segment a width, and a 
   * length.  The line segment defines the length of the rectangle; the width 
   * defines the width.
   */
  public static Polygon findPolygon(float x1, float x2, float y1, float y2,
				    float distAlong, float thickness, float width){
    if(DEBUG)System.out.println("in the first method: x1x2y1y2, distalong, thickness="+
		       x1+","+x2+","+y1+","+y2+" "+distAlong+","+thickness);

    float theta = getTheta(x1,x2,y1,y2);
    float xRectangleCenter = x1+distAlong*(float)Math.cos(theta);
    float yRectangleCenter = y1+distAlong*(float)Math.sin(theta);
    float xFirstPoint = xRectangleCenter - 0.5f*thickness*(float)Math.cos(theta);
    float yFirstPoint = yRectangleCenter - 0.5f*thickness*(float)Math.sin(theta);
    float xSecondPoint = xRectangleCenter + 0.5f*thickness*(float)Math.cos(theta);
    float ySecondPoint = yRectangleCenter + 0.5f*thickness*(float)Math.sin(theta);
    if(DEBUG)System.out.println("theta,x1x2y1y2="+
				theta+" "+x1+","+x2+","+y1+","+y2);
    if(DEBUG)System.out.println
	       ("firstpt x,y secondpt x,y="+xFirstPoint+","+yFirstPoint+","+
		xSecondPoint+","+ySecondPoint);

    return findPolygon(xFirstPoint, xSecondPoint, yFirstPoint, ySecondPoint, width);
  }//findPolygon

  /**
   * find rectangular polygon which is defined by a line segment and a width.  The
   * line segment defines the length of the rectangle; the width defines the width.
   */
  public static Polygon findPolygon(float x1, float x2, float y1, float y2, 
				    float width){
    float rectangleLength = pointDist(x1,y1,x2,y2);
    float theta  = getTheta(x1,x2,y1,y2);

    if(DEBUG)System.out.println("in the second method:");
    if(DEBUG)System.out.println("theta="+theta);
    if(DEBUG)System.out.println("x1x2y1y2="+x1+","+x2+" "+y1+","+y2);
    /*
     * angle of line connecting center of polygon and upper right vertex
     */
    float thetaUpperRightVertex=0.0f;
    /*
     * angle of line connecting center of polygon and upper left vertex
     */
    float thetaLowerRightVertex=0.0f;
    float distToUpperRightVertex=0.0f;
    float distToLowerRightVertex=0.0f;

    /*
     * coordinates of vertices of rectangular polygon; measured relative to 
     * coordinate system that is rotated and translated such that middle of one end
     * of the rectangle is at (0,0) and the middle of the other end is on the 
     * positive y axis.
     * indices:  0 = lower left, 1 = upper left, 2 = upper right, 3 = lower right
     */
    float[] vertexXRel = new float[4];
    float[] vertexYRel = new float[4];
    /*
     * coordinates of vertices of rectangular polygon.
     */
    float[] vertexX  = new float[4];
    float[] vertexY  = new float[4];
    int[] vertexXInt = new int[4];
    int[] vertexYInt = new int[4];

    //find vertex points
    vertexXRel[0] = -0.5f * width;
    vertexXRel[1] = -0.5f * width;
    vertexXRel[2] =  0.5f * width;
    vertexXRel[3] =  0.5f * width;
    vertexYRel[0] = 0.0f;
    vertexYRel[1] = rectangleLength;
    vertexYRel[2] = rectangleLength;
    vertexYRel[3] = 0.0f;
    if(DEBUG)System.out.println
	       ("Relative X Vertices:"+ vertexXRel[0]+" " + vertexXRel[1]+" " +
		vertexXRel[2]+" " + vertexXRel[3]); 
    if(DEBUG)System.out.println
	       ("Relative Y Vertices:"+ vertexYRel[0]+" " + vertexYRel[1]+" " +
		vertexYRel[2]+" " + vertexYRel[3]); 
    Polygon p =null;
    
    thetaUpperRightVertex  = getTheta(0.0f, vertexXRel[2], 0.0f, vertexYRel[2]);
    distToUpperRightVertex = pointDist(0.0f, 0.0f, vertexXRel[2], vertexYRel[2]);
    thetaLowerRightVertex  = getTheta(0.0f, vertexXRel[3], 0.0f, vertexYRel[3]);
    distToLowerRightVertex = pointDist(0.0f, 0.0f, vertexXRel[3], vertexYRel[3]);
    float thetaLowerLeftVertex = getTheta(0.0f, vertexXRel[0], 0.0f, vertexYRel[0]);
    float thetaUpperLeftVertex = getTheta(0.0f, vertexXRel[1], 0.0f, vertexYRel[1]);

     if(DEBUG)System.out.println("theta upper,lower="+thetaUpperRightVertex+","+
 		       thetaLowerRightVertex);
     if(DEBUG)System.out.println("dist to upper, lower="+distToUpperRightVertex+","+
 		       distToLowerRightVertex);
    //rotate
     if(DEBUG)System.out.println("after rotation, before translation");
     if(DEBUG)System.out.println("theta, thetalrv, thetaurv="+theta+","+
			thetaLowerRightVertex+","+thetaUpperRightVertex);

     /*
      * theta is the angle of the rotated y axis (of the local coordinate sys)
      * wrt the x axis of the global coord. sys.  theta-pi/2 is the angle of the
      * rotated x axis wrt the x axis of the global coord sys.
      */

     vertexX[0]=distToLowerRightVertex*
       (float)Math.cos(theta+thetaLowerLeftVertex-Math.PI/2.0f);
     vertexX[1]=distToUpperRightVertex*
       (float)Math.cos(theta+thetaUpperLeftVertex-Math.PI/2.0f);
     vertexX[2]=distToUpperRightVertex*
       (float)Math.cos(theta+thetaUpperRightVertex-Math.PI/2.0f);
     vertexX[3]=distToLowerRightVertex*
       (float)Math.cos(theta+thetaLowerRightVertex-Math.PI/2.0f);

     vertexY[0]=distToLowerRightVertex*
       (float)Math.sin(theta+thetaLowerLeftVertex-Math.PI/2.0f);
     vertexY[1]=distToUpperRightVertex*
       (float)Math.sin(theta+thetaUpperLeftVertex-Math.PI/2.0f);
     vertexY[2]=distToUpperRightVertex*
       (float)Math.sin(theta+thetaUpperRightVertex-Math.PI/2.0f);
     vertexY[3]=distToLowerRightVertex*
       (float)Math.sin(theta+thetaLowerRightVertex-Math.PI/2.0f);
     if(DEBUG)System.out.println("vertexX =" + vertexX[0]+" "+vertexX[1]+" "+
				 vertexX[2]+" "+vertexX[3]);
     if(DEBUG)System.out.println("vertexY =" + vertexY[0]+" "+vertexY[1]+" "+
				 vertexY[2]+" "+vertexY[3]);

    //translate
    vertexX[0] += x1;
    vertexX[1] += x1;
    vertexX[2] += x1;
    vertexX[3] += x1;

    vertexY[0] += y1;
    vertexY[1] += y1;
    vertexY[2] += y1;
    vertexY[3] += y1;

    for(int i=0; i<=3; i++){
      vertexXInt[i] = (int)vertexX[i];
      vertexYInt[i] = (int)vertexY[i];
      _polygon.addPoint(vertexXInt[i],vertexYInt[i]);
    }
      _polygon = new Polygon(vertexXInt, vertexYInt, 4);
    return _polygon;
  }//findPolygon

    /**
     * Interpolation search
     */
    public static int interpolationSearch(ResizableFloatArray array, int n, int arraySize){
	int low=0;
	int high=arraySize-1;
	int mid=0;
	//	System.out.println("about to start interpolation search:");
	//	System.out.println("n,array.get(low),array.get(high)="+n+","+array.get(low)+","+array.get(high));
	while(n>=array.get(low) && n<=array.get(high)){
	    //	    System.out.println("interpolationSearch: searching...");
	    mid = low+(int)Math.abs(Math.floor((n-array.get(low))*(high-low)/
						   (array.get(high)-array.get(low))));
	    if(n == array.get(mid)){
		return mid;
	    }else{
		if(n<array.get(mid)){
		    high=mid-1;
		}else{
		    low=mid+1;
		}
	    }
	}
	return mid;
    }

  /**
   * quicksort.  left is the index of the first element in the array;
   * right is the index of the last
   */
  public static float[] qsort(float[] array, int left, int right){
    int last=0;
    double ran=0.0;
    if(left < right){
      ran = Math.random();
      swap(array, left, left+(int)( (right-left+1) * ran));
      last=left;
      for(int i=left+1; i<=right; i++){
	if(array[i] < array[left]){
	  last++;
	  swap(array, last, i); 
	}//if
      }//for i
      swap(array, left, last);
      qsort(array, left, last-1);
      qsort(array, last+1, right);
    }//if
    return array;
  }//qsort

  /**
   * swap two float values in array.  used by quicksort
   */
  protected static void swap(float[] array, int i, int j){
    float t  = array[i];
    array[i] = array[j];
    array[j] = t;
  }

  /**
   * quicksort for vectors.  left is the index of the first element in the array;
   * right is the index of the last
   */
  public static Vector qsort(Vector xsects, int left, int right){
    int last=0;
    double ran=0.0;
    if(left < right){
      ran = Math.random();
      swap(xsects, left, left+(int)( (right-left+1) * ran));
      last=left;
      for(int i=left+1; i<=right; i++){
	  if(((Xsect)(xsects.elementAt(i))).getDistAlongCenterline() < 
	     ((Xsect)(xsects.elementAt(left))).getDistAlongCenterline()){
	      //	if(array[i] < array[left]){
	      last++;
	      swap(xsects, last, i); 
	  }//if
      }//for i
      swap(xsects, left, last);
      qsort(xsects, left, last-1);
      qsort(xsects, last+1, right);
    }//if
    return xsects;
  }//qsort

  /**
   * swap two float values in array.  used by quicksort
   */
  protected static void swap(Vector array, int i, int j){
      Xsect t = (Xsect)(array.elementAt(i));
      array.setElementAt(array.elementAt(j), i);
      array.setElementAt(t, j);

//      float t  = array[i];
//      array[i] = array[j];
//      array[j] = t;
  }



  /**
   * quicksort for Strings.  left is the index of the first element in
   * array; right is the index of the last.
   */
    public static ResizableStringArray qsort(ResizableStringArray array, int left, int right){
	int last=0;
	double ran=0.0;

	int iValue = -CsdpFunctions.BIG_INT;
	int leftValue = -CsdpFunctions.BIG_INT;

	if(left < right){
	    ran = Math.random();
	    swap(array, left, left+(int)( (right-left+1) * ran));
	    last=left;
	    for(int i=left+1; i<=right; i++){
		//	if(array[i] < array[left]){
		boolean iValueParsable = true;
		boolean leftValueParsable = true;
		try{
		    iValue = Integer.parseInt(array.get(i),10);
		}catch (NumberFormatException e){
		    iValueParsable = false;
		    if(DEBUG)System.out.println("iValue not parsable:  "+array.get(i));
		}
		try{
		    leftValue = Integer.parseInt(array.get(left),10);
		}catch(NumberFormatException e){
		    leftValueParsable = false;
		    if(DEBUG)System.out.println("leftValue not parsable:  "+array.get(left));
		}
		//if both values are numbers
		if(iValueParsable && leftValueParsable){
		    //then swap if the ith value is less then the left value
		    if(iValue < leftValue){
			last++;
			swap(array, last, i); 
		    }//if
		    //else if the ith value is a number but the left isn't
		}else if(iValueParsable && leftValueParsable == false){
		    last++;
		    swap(array, last, i);
		    //else if they're both not numbers
		}else if(iValueParsable == false && leftValueParsable == false){
		    //then swap if ith value is lex. less then left value
		    if(array.get(i).compareTo(array.get(left)) < 0){
			last++;
			swap(array, last, i); 
		    }
		    //else if ith value is not number and left value is
		}else if(iValueParsable == false && leftValueParsable){
		    //do nothing, because numbers should go before text
		}

	    }//for i
	    swap(array, left, last);
	    qsort(array, left, last-1);
	    qsort(array, last+1, right);
	}//if
	return array;
    }//qsort

    /**
     * swap two Strings in array.  used by quicksort
     */
    protected static void swap(ResizableStringArray array, int i, int j){
	String t  = array.get(i);
	//	array.get(i) = array.get(j);
	//	array.get(j) = t;
	array.put(i,array.get(j));
	array.put(j,t);
    }

  /**
   * rounds value off (or truncates?) to two decimal places
   */
  public static float twoPlaces(float f){
    int i = (int)(f*100.0f);
    return ((float)(i))/100.0f;
  }//twoPlaces

  /**
   * converts a 2 digit hex value to integer
   */
  public static int hexToInt(char h1, char h2) {
    int hBase=16;
    int h1int=0;
    int h2int=0;
    if      (h2=='0') h2int=0; 
    else if (h2=='1') h2int=1; 
    else if (h2=='2') h2int=2; 
    else if (h2=='3') h2int=3; 
    else if (h2=='4') h2int=4; 
    else if (h2=='5') h2int=5; 
    else if (h2=='6') h2int=6; 
    else if (h2=='7') h2int=7; 
    else if (h2=='8') h2int=8; 
    else if (h2=='9') h2int=9; 
    else if (h2=='A') h2int=10; 
    else if (h2=='B') h2int=11; 
    else if (h2=='C') h2int=12; 
    else if (h2=='D') h2int=13; 
    else if (h2=='E') h2int=14; 
    else if (h2=='F') h2int=15; 
    
    if      (h1=='0') h1int =  0*hBase; 
    else if (h1=='1') h1int =  1*hBase; 
    else if (h1=='2') h1int =  2*hBase; 
    else if (h1=='3') h1int =  3*hBase; 
    else if (h1=='4') h1int =  4*hBase; 
    else if (h1=='5') h1int =  5*hBase; 
    else if (h1=='6') h1int =  6*hBase; 
    else if (h1=='7') h1int =  7*hBase; 
    else if (h1=='8') h1int =  8*hBase; 
    else if (h1=='9') h1int =  9*hBase; 
    else if (h1=='A') h1int = 10*hBase; 
    else if (h1=='B') h1int = 11*hBase; 
    else if (h1=='C') h1int = 12*hBase; 
    else if (h1=='D') h1int = 13*hBase; 
    else if (h1=='E') h1int = 14*hBase; 
    else if (h1=='F') h1int = 15*hBase; 

    return h1int+h2int;
  }//hexToInt

  /**
   * interpolate between 2 points to find x value for given y value (y3)
   */
  public static float interpX(float x1, float x2, float y1, float y2, float y){
    return -( (y2-y)*((x2-x1)/(y2-y1)) - x2);
  }

  /**
   * interpolate between 2 points to find y value for given x value (x3)
   */
  public static float interpY(float x1, float x2, float y1, float y2, float x){
      return (x-x1)*( (y2-y1)/(x2-x1) ) + y1;
  }


  /**
   * returns version number
   */
  public static String getVersion(){
    return _version;
  }

  /**
   * get name of current bathymetry file directory.  _bathymtryDirectory stores 
   * the name of the last directory accessed.
   */
  public static File getBathymetryDirectory(){
    return _bathymetryDirectory;
  }

  /**
   * get name of current landmark directory.  _landmarkDirectory stores the name 
   * of the last directory accessed.
   */
  public static File getLandmarkDirectory(){
    return _landmarkDirectory;
  }

  /**
   * get name of current network directory.  _networkDirectory stores the name 
   * of the last directory accessed.
   */
  public static File getNetworkDirectory(){
    return _networkDirectory;
  }

    /**
     * get name of current digital line graph directory.  _digitalLineGraphDirectory
     * store the name of the last directory accessed.
     */
    public static File getDigitalLineGraphDirectory(){
	return _digitalLineGraphDirectory;
    }

  /**
   * get name of current network export directory.  _networkExportDirectory 
   * stores the name of the last directory accessed.
   */
  public static File getNetworkExportDirectory(){
    return _networkExportDirectory;
  }

  /**
   * get name of current properties directory.  _propertiesDirectory stores the 
   * name of the last directory accessed.
   */
  public static File getPropertiesDirectory(){
    return _propertiesDirectory;
  }

  /**
   * get name of current open water area calc directory.  _openWaterAreaDirectory stores the 
   * name of the last directory accessed.
   */
  public static File getOpenWaterAreaDirectory(){
    return _openWaterAreaDirectory;
  }

  /**
   * returns the last directory accessed when opening a file
   */
  public static File getOpenDirectory(){
    return _openDirectory;
  }


  /**
   * returns name of current DigitalLineGraph file.  _digitalLineGraphFilename 
   * stores the name of the last dlg filename accessed.
   */
  public static String getDigitalLineGraphFilename(){
      return _digitalLineGraphFilename;
  }

    /**
     * stores name of current bathymetry directory.  _bathymetry directory stores 
     * the name of the last directory accessed.
     */
  public static void setBathymetryDirectory(File d){
    _bathymetryDirectory = d;
  }

    /**
     * store name of current digital line graph directory.  _digitalLineGraphDirectory
     * store the name of the last directory accessed.
     */
    public static void setDigitalLineGraphDirectory(File name){
	_digitalLineGraphDirectory = name;
    }

  /**
   * stores name of current landmark directory.  _landmarkDirectory stores the 
   * name of the last directory accessed.
   */
  public static void setLandmarkDirectory(File d){
    _landmarkDirectory = d;
  }

  /**
   * stores name of current network directory.  _networkDirectory stores the name 
   * of the last directory accessed.
   */
  public static void setNetworkDirectory(File d){
    _networkDirectory = d;
  }

  /**
   * stores name of current network export directory.  _networkExportDirectory stores the name 
   * of the last directory accessed.
   */
  public static void setNetworkExportDirectory(File d){
    _networkExportDirectory = d;
  }

  /**
   * stores name of current properties directory.  _propertiesDirectory stores the 
   * name of the last directory accessed.
   */
  public static void setPropertiesDirectory(File d){
    _propertiesDirectory = d;
  }

  /**
   * stores name of current open water area calc directory.  _openWaterAreaDirectory 
   * stores the name of the last directory accessed.
   */
  public static void setOpenWaterAreaDirectory(File d){
    _openWaterAreaDirectory = d;
  }

  /**
   * sets the last directory accessed when opening a file
   */
  public static void setOpenDirectory(File dir){
    _openDirectory = dir;
  }

    /**
     * stores name of current bathymetry directory.  _bathymetry directory stores 
     * the name of the last directory accessed.
     */
  public static void setBathymetryDirectory(String d){
    _bathymetryDirectory = new File(d);
  }

    /**
     * store name of current digital line graph directory.  _digitalLineGraphDirectory
     * store the name of the last directory accessed.
     */
    public static void setDigitalLineGraphDirectory(String name){
	_digitalLineGraphDirectory = new File(name);
    }

  /**
   * stores name of current landmark directory.  _landmarkDirectory stores the 
   * name of the last directory accessed.
   */
  public static void setLandmarkDirectory(String d){
    _landmarkDirectory = new File(d);
  }

  /**
   * stores name of current network directory.  _networkDirectory stores the name 
   * of the last directory accessed.
   */
  public static void setNetworkDirectory(String d){
    _networkDirectory = new File(d);
  }

  /**
   * stores name of current network export directory.  _networkExportDirectory stores the name 
   * of the last directory accessed.
   */
  public static void setNetworkExportDirectory(String d){
    _networkExportDirectory = new File(d);
  }

  /**
   * stores name of current properties directory.  _propertiesDirectory stores the 
   * name of the last directory accessed.
   */
  public static void setPropertiesDirectory(String d){
    _propertiesDirectory = new File(d);
  }

  /**
   * stores name of current open water area calc directory.  _openWaterAreaDirectory 
   * stores the name of the last directory accessed.
   */
  public static void setOpenWaterAreaDirectory(String d){
    _openWaterAreaDirectory = new File(d);
  }

  /**
   * sets the last directory accessed when opening a file
   */
  public static void setOpenDirectory(String dir){
    _openDirectory = new File(dir);
  }



  /**
   * stores name of current DigitalLineGraph file.  _digitalLineGraphFilename 
   * stores the name of the last directory accessed.
   */
  public static void setDigitalLineGraphFilename(String d){
    _digitalLineGraphFilename = d;
  }

  /**
   * stores name of current DigitalLineGraph filetype
   */
  public static void setDigitalLineGraphFiletype(String d){
    _digitalLineGraphFiletype = d;
  }

  /**
   * returns filetype of current DigitalLineGraph file.
   */
  public static String getDigitalLineGraphFiletype(){
      return _digitalLineGraphFiletype;
  }

  /**
   * stores name of network file
   */
  public static void setNetworkFilename(String f){
    _networkFilename = f;
  }

  /**
   * returns name of network file
   */
  public static String getNetworkFilename(){
    return _networkFilename;
  }

  /**
   * stores network filename extension
   */
  public static void setNetworkFiletype(String f){
    _networkFiletype = f;
  }

  /**
   * returns network filename extension
   */
  public static String getNetworkFiletype(){
    return _networkFiletype;
  }

  /**
   * returns name of properties file
   */
  public static String getPropertiesFilename(){
    return _propertiesFilename;
  }

  /**
   * sets name of properties file
   */
  public static void setPropertiesFilename(String f){
    _propertiesFilename = f;
  }

  /**
   * stores properties filename extension
   */
  public static String getPropertiesFiletype(){
    return _propertiesFiletype;
  }

  /**
   * stores properties filename extension
   */
  public static void setPropertiesFiletype(String f){
    _propertiesFiletype = f;
  }

    /**
     * returns name of openWaterArea file
     */
    public static String getOpenWaterAreaFilename(){
      return _openWaterAreaFilename;
    }

    /**
     * sets name of openWaterArea file
     */
    public static void setOpenWaterAreaFilename(String f){
      _openWaterAreaFilename = f;
    }

    /**
     * stores openWaterArea filename extension
     */
    public static String getOpenWaterAreaFiletype(){
      return _openWaterAreaFiletype;
    }

    /**
     * stores openWaterArea filename extension
     */
    public static void setOpenWaterAreaFiletype(String f){
      _openWaterAreaFiletype = f;
    }

  /**
   * quicksort
   */
  public static ResizableShortArray qsort(int left, int right, 
					  ResizableShortArray a){
    int last=0;
    double ran=0.0;
    if(left < right){
      ran = Math.random();
      swap(a, left, left+(int)( (right-left+1) * ran));
      last=left;
      for(int i=left+1; i<=right; i++){
	if(a.get(i) < a.get(left)){
	  last++;
	  swap(a, last, i); 
	}//if
      }//for i
      swap(a, left, last);
      qsort(left, last-1,a);
      qsort(last+1, right,a);
    }//if
    return a;
  }//qsort
  
  /**
   *swap two float values in array
   */
  public static void swap(ResizableFloatArray a, int i, int j){
    float t = a.get(i);
    a.put(i,a.get(j));
    a.put(j,t);
  }//swap
  /**
   *swap two int values in array
   */
  protected static void swap(ResizableIntArray a, int i, int j){
    int t = a.get(i);
    a.put(i,a.get(j));
    a.put(j,t);
  }//swap
  /**
   *swap two short values in array
   */
  protected static void swap(ResizableShortArray a, int i, int j){
    short t = a.get(i);
    a.put(i,a.get(j));
    a.put(j,t);
  }//swap

      /**
       * converts float to a string with spaces at the end--length is spcified.
       */
      public static String formattedOutputString(float f, int fieldWidth, 
  					       boolean leftJustify){
  	//dont truncate exponent!
  	String returnString = Float.toString(f);
  	int length = returnString.length();
  	String rjString = "";

  	/*
  	 * If the number has more digits then the specified length, then
  	 * change the requested length to the number of digits.
  	 */
  	if(length > fieldWidth) fieldWidth = length;
  	if(leftJustify){
  	    for(int i=0; i<=fieldWidth-1; i++){
  		returnString += " ";
  	    }
  	    returnString = returnString.substring(0,fieldWidth)+" ";
  	}else{
  	    for(int i=0; i<=fieldWidth-1; i++){
  		rjString += " ";
  	    }
  	    rjString += returnString;
  	    returnString = rjString;
  	    //System.out.println("returnString before = "+returnString);
  	    returnString = returnString.substring
  		(returnString.length() - fieldWidth);
  	    //System.out.println("returnString after = "+returnString);
  	}

  	return returnString;
      }

      /**
       * converts int to a string with spaces at the end--length is spcified.
       */
      public static String formattedOutputString(int ii, int fieldWidth,
  					       boolean leftJustify){
	  //  	String returnString = (new Integer(ii)).toString();
	String returnString = Integer.toString(ii);
  	int length = returnString.length();
  	String rjString = "";

  	if(length > fieldWidth) fieldWidth = length;
  	if(leftJustify){
  	    for(int i=0; i<=fieldWidth-1; i++){
  		returnString += " ";
  	    }
  	    returnString = returnString.substring(0,fieldWidth)+" ";
  	}else{
  	    for(int i=0; i<=fieldWidth-1; i++){
  		rjString += " ";
  	    }
  	    rjString += returnString;
  	    returnString = rjString;
  //  	    System.out.println("returnString before = "+returnString);
  //  	    System.out.println("returnString.length(), fieldwidth="+returnString.length()+","+fieldWidth);
  	    returnString = returnString.substring
  		(returnString.length() - fieldWidth);
  	    //	    System.out.println("returnString after = "+returnString);
  	}
  	return returnString;
      }

    /**
     * converts string to a string with spaces at the end--length is spcified.
     */
    public static String formattedOutputString(String s, int fieldWidth,
					       boolean leftJustify){
	String returnString = s;
	int length = returnString.length();
	String rjString = "";

	if(length > fieldWidth) fieldWidth = length;
	if(leftJustify){
	    for(int i=0; i<=fieldWidth-1; i++){
		returnString += " ";
	    }
	    returnString = returnString.substring(0,fieldWidth)+" ";
	}else{
	    for(int i=0; i<=fieldWidth-1; i++){
		rjString += " ";
	    }
	    rjString += returnString;
	    returnString = rjString;
	    //   System.out.println("returnString before = "+returnString);
	    returnString = returnString.substring
		(returnString.length() - fieldWidth);
	    //	    System.out.println("returnString after = "+returnString);
	}
	return returnString;
    }

    /**
     * for open water area calc. (Yolo Bypass study)
     */
    public static void setEchoTimeSeriesInput(boolean b){
	_echoTimeSeriesInput = b;
    }
    /**
     * for open water area calc. (Yolo Bypass study)
     */
    public static boolean getEchoTimeSeriesInput(){
	return _echoTimeSeriesInput;
    }
    /**
     * for open water area calc. (Yolo Bypass study)
     */
    public static void setEchoXsectInput(boolean b){
	_echoXsectInput = b;
    }
    /**
     * for open water area calc. (Yolo Bypass study)
     */
    public static boolean getEchoXsectInput(){
	return _echoXsectInput;
    }
    /**
     * for open water area calc. (Yolo Bypass study)
     */
    public static void setEchoToeDrainInput(boolean b){
	_echoToeDrainInput=b;
    }
    /**
     * for open water area calc. (Yolo Bypass study)
     */
    public static boolean getEchoToeDrainInput(){
	return _echoToeDrainInput;
    }
    /**
     * for open water area calc. (Yolo Bypass study)
     */
    public static void setPrintXsectResults(boolean b){
	_printXsectResults = b;
    }
    /**
     * for open water area calc. (Yolo Bypass study)
     */
    public static boolean getPrintXsectResults(){
	return _printXsectResults;
    }
    /**
     * for open water area calc. (Yolo Bypass study)
     */
    public static boolean getChannelLengthsOnly(){
	return _channelLengthsOnly;
    }
    /**
     * for open water area calc. (Yolo Bypass study)
     */
    public static void setChannelLengthsOnly(boolean b){
	_channelLengthsOnly = b;
    }

    /**
     * For Yolo Bypass Calculations
     */
    public static void setUseFremontWeir(boolean b){
	_useFremontWeir = b;
    }
    /**
     * For Yolo Bypass Calculations
     */
    public static boolean getUseFremontWeir(){
	return _useFremontWeir;
    }
    /**
     * returns elevation of top of Fremont Weir.  Used only for Yolo Bypass 
     * calculations (see StationTimeSeriesData).
     */
    public static float getFremontWeirElevation(){
	return _fremontWeirElevation;
    }
    /**
     * For Yolo Bypass Calculations
     */
    public static void setUseToeDrainRestriction(boolean b){
	_useToeDrainRestriction = b;
    }
    /**
     * For Yolo Bypass Calculations
     */
    public static boolean getUseToeDrainRestriction(){
	return _useToeDrainRestriction;
    }

    /**
     * returns index of last character that isn't a tab or a space
     */
    public static int lastNonblank(String s){
	int returnValue = 0;
	for(int i=0; i<=s.length()-1; i++){
	    if(s.charAt(i) != '\t' && s.charAt(i) != ' ') returnValue = i;
	}
	return returnValue;
    }

    /**
     * returns index of last character that isn't the specified character
     */
    public static int lastIndexOf(String s, char c){
	int returnValue = 0;
	for(int i=0; i<=s.length()-1; i++){
	    if(s.charAt(i) != c) returnValue = i;
	}
	return returnValue;
    }

    /**
     * replaces a string that occurs within a string with another string
     */
    public static String replaceString(String theString, 
				       String oldString, String newString){
	int lastIndex = CsdpFunctions.BIG_INT;
	String returnString = "";
	String oldReturnString = "";
	
	int stringLength = theString.length();
	while(theString.indexOf(oldString) >= 0){
	    returnString += theString.substring(0, theString.indexOf(oldString));
	    returnString += newString;
	    theString = theString.substring(theString.indexOf(oldString) +
					    oldString.length());

	    if(DEBUG) System.out.println("theString, returnString="+theString+","+returnString);
	}//while the string still contains the delimiter (oldString)
	returnString += theString;

	// this doesn't work with multi-character delimiters! if they
	// ever make a tokenizer that will accept multi-character delimiters,
	// then this code could replace the while loop above
//    	StringTokenizer t = new StringTokenizer(theString, oldString);
//    	String token = null;

//      	while(t.hasMoreTokens()){
//      	    token = t.nextToken();
//      	    returnString += token + newString;
//      	}

	return returnString;
    }

  /**
   * debugging statements printed if true
   */
  public static final boolean DEBUG = false;
  /**
   * large float value-used to find minimum value
   */
  public static final float BIG_FLOAT  = 9.9e30f;
  /**
   * small float value-used to find maximum value
   */
  public static final float SMALL_FLOAT = -1.0E30f;
  /**
   * large int value-used to find minimum value
   */
  public static final int BIG_INT = (int)1E10;
  /**
   * large int value-used to find minimum value
   */
  public static final short BIG_SHORT = (short)1E10;
  /**
   * defines region which contains bathymetry points to be displayed in xsect view
   */
  protected static Polygon _polygon = new Polygon(); 
  /**
   * index of x coordinate of first point-used when storing coordinates for 2 points
   */
  public static final int x1Index = 0;
  /**
   * index of y coordinate of first point- use when storing coordinates for 2 points
   */
  public static final int y1Index = 1;
  /**
   * index of x coordinate of 2nd point  - use when storing coordinates for 2 points
   */
  public static final int x2Index = 2;
  /**
   * index of y coordinate of 2nd point  - use when storing coordinates for 2 points
   */
  public static final int y2Index = 3;
  /**
   * index of minimum x value-used when calculating min and max values
   */
  public static final int minXIndex = 0;
  /**
   * index of minimum y value-used when calculating min and max values
   */
  public static final int minYIndex = 1;
  /**
   * index of maximum x value-used when calculating min and max values
   */
  public static final int maxXIndex = 2;
  /**
   * index of maximum y value-used when calculating min and max values
   */
  public static final int maxYIndex = 3;
  /**
   * name of directory accessed when opening bathymetry file.  used for saving 
   */
  protected static File _bathymetryDirectory = null;
  /**
   * name of directory accessed when opening network file.  used for saving 
   */
  protected static File _networkDirectory = null;
  /**
   * name of directory accessed when exporting network file.  used for saving 
   */
  protected static File _networkExportDirectory = null;
  /**
   * name of directory accessed when opening landmark file.  used for saving 
   */
  protected static File _landmarkDirectory = null;
  /**
   * name of directory accessed when opening DigitalLineGraphFile.
   */
  protected static File _DLGDirectory = null;
  /**
   * name of directory accessed when opening properties file.  used for saving 
   */
  protected static File _propertiesDirectory = null;
  /**
   * name of directory accessed when opening file containing information to be used for
   * open water area calculations. 
   */
  protected static File _openWaterAreaDirectory = null;
  /**
   * name of directory accessed when opening file of any type.  default directory
   * for next open command 
   */
  protected static File _openDirectory = null;

    /**
     *
     */
    protected static File _digitalLineGraphDirectory = null;

  /**
   * default value of cross-section thickness (perpendicualar to screen in cross-
   * section view).  determines amount of data to be displayed in xsect view.
   */
  protected static float _xsectThickness = 1000.0f;
  /**
   * number of pixels between bathymetry data and canvas edges
   */
  protected static float BORDER_THICKNESS = 1000.0f;

    public static boolean getWarnZoom(){
	return _warnZoom;
    }
    public static void setWarnZoom(boolean b){
	_warnZoom = b;
    }

    protected static String _digitalLineGraphFilename = null;
    protected static String _digitalLineGraphFiletype = null;
  protected static String _landmarkFilename = null;
  protected static String _landmarkFiletype = null;
  protected static String _DSMChannelsFilename = null;
  protected static String _DSMChannelsFiletype = null;
  protected static String _networkFilename = null;
  protected static String _networkFiletype = null;

  protected static String _propertiesFilename = null;
  protected static String _propertiesFiletype = null;
    protected static String _openWaterAreaFilename = null;
    protected static String _openWaterAreaFiletype = null;
  public static float CHARACTER_TO_PIXELS = 300.0f/44.0f;
  //  private static Vector _buttons = new Vector();
  //  protected static int NUM_BUTTONS = 0;
  //  protected static Vector _colors = new Vector();
    private static boolean _echoTimeSeriesInput = true;
    private static boolean _echoXsectInput = false;
    private static boolean _printXsectResults = false;
    private static boolean _echoToeDrainInput = false;
    private static boolean _channelLengthsOnly = false;

    private static boolean _useFremontWeir = true;
    private static final float _fremontWeirElevation = 33.5f;
    private static boolean _useToeDrainRestriction = true;

//      private static float _zoomFactor=1.0f;
//      public static float getZoomFactor(){
//  	return _zoomFactor;
//      }
//      public static void setZoomFactor(float z){
//  	_zoomFactor=z;
//      } 
//      private static float _oldZoomFactor=1.0f;
//      public static float getOldZoomFactor(){
//  	return _oldZoomFactor;
//      }
//      public static void setOldZoomFactor(float z){
//  	_oldZoomFactor=z;
//      } 
    
    private static boolean _warnZoom = true;
    public static final Cursor _waitCursor = new Cursor(Cursor.WAIT_CURSOR);
    public static final Cursor _defaultCursor = new Cursor(Cursor.DEFAULT_CURSOR);
    public static final Cursor _crosshairCursor = new Cursor(Cursor.CROSSHAIR_CURSOR);
    public static final Cursor _handCursor = new Cursor(Cursor.HAND_CURSOR);
    public static boolean _cancelSaveNetwork = true;

    /**
     * the minimum x coordinate (data coordinates) of the current plot region.
     */
    public static float minXPlot;
    /**
     * the maximum x coordinate (data coordinates) of the current plot region.
     */
    public static float maxXPlot;
    /**
     * the minimum y coordinate (data coordinates) of the current plot region.
     */
    public static float minYPlot;
    /**
     * the maximum y coordinate (data coordinates) of the current plot region.
     */
    public static float maxYPlot;

    /**
     * version number-displayed at top of frame
     */
    private static final String _version = "2.31";

    /*
     * bathymetry metadata-use as default: utm zone 10 nad 83 meters and 
     * ngvd 1929 U.S. survey feet
     */
    public static String getHDatum(){return _hDatum;}
    public static void setHDatum(String s){_hDatum=s;}
    public static int getHZone(){return _hZone;}
    public static void setHZone(int h){_hZone=h;}
    public static String getHUnits(){return _hUnits;}
    public static void setHUnits(String h){_hUnits = h;}
    public static String getVDatum(){return _vDatum;}
    public static void setVDatum(String v){_vDatum=v;}
    public static String getVUnits(){return _vUnits;}
    public static void setVUnits(String v){_vUnits=v;}

    

    private static String _hDatum = "UTMNAD27";
    private static int _hZone = 10;
    private static String _hUnits = "meters";
    private static String _vDatum = "NGVD29";
    private static String _vUnits = "ussurveyfeet";
    
}//class CsdpFunctions

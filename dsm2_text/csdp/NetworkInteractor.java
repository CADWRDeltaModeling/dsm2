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
//import DWR.Graph.*;
import vista.graph.*;
import java.awt.*;
import java.awt.event.*;
import java.util.*;

/**
 * This class handles interaction of mouse and mouse motion.
 *
 * @author
 * @version
 */

public class NetworkInteractor extends ElementInteractor{

public NetworkInteractor(CsdpFrame gui, PlanViewCanvas can, App app){
  _gui = gui;
  _can = can;
  _app = app;
}

  /**
   * Invoked when the mouse has been clicked on a component.

   */
  public void mouseClicked(MouseEvent e){
    if(DEBUG)System.out.println("Mouse Clicked at ( "
				  + e.getX() + ", " + e.getY() + " )");
    setInitialPoint(e.getX(), e.getY());
    if(DEBUG)System.out.println
	       ("mouse clicked:  setting initial point "+e.getX()+","+e.getY());
  }//mouseClicked

    /**
     * Invoked when a mouse button has been pressed.
     * this selects the nearest centerline.
     */
    public void mousePressed(MouseEvent e){
	setInitialPoint(e.getX(), e.getY());
	Xsect xsect = null;
	if(DEBUG)System.out.println
		     ("mouse pressed:  setting initial point "+e.getX()+","+e.getY());
	
	_nPlotter = _can._networkPlotter;
	if(_nPlotter != null){
	    _minSlope = _nPlotter._minSlope;
	    _height   = _nPlotter._height;
	    _centerX  = _nPlotter._centerX;
	    _centerY  = _nPlotter._centerY;
	    if(DEBUG)System.out.println("NetworkInteractor: centerX,centerY="+
					_centerX+","+_centerY);
	}//if _nPlotter is null
	if(_bathymetryPlot != null && _bathymetryPlot._bathymetryData != null){
	    //      _minX = _bathymetryPlot._bathymetryData.getMinX();
	    //      _minY = _bathymetryPlot._bathymetryData.getMinY();
	    float[] bb = _bathymetryPlot.getBathymetryBoundaries();
	    _minX = bb[CsdpFunctions.minXIndex];
	    _minY = bb[CsdpFunctions.minYIndex];
	}
	if(_gui.getAddPointMode()) addPoint();
	else if(_gui.getInsertPointMode()) insertPoint();
	else if(_gui.getDeletePointMode()) deletePoint();
	else if(_gui.getAddXsectMode())addXsect();
	else if(_gui.getMoveXsectMode()) moveXsect();
	else if(_gui.getZoomBoxMode()){
	    _drawDragRect = true;
	    _previouslyDoubleBuffered = _can.isDoubleBuffered();
	    if (! _previouslyDoubleBuffered) _can.setDoubleBuffered(true);
	    
	    Rectangle r = _can.getBounds();
	    _gCImage = _can.createImage(r.width, r.height);
	}//else if zoomBoxMode
	// if no modes set, select centerline and/or cross-section line
	else if(_gui.getMovePointMode() == false){
	    Centerline centerline;
	    int numCenterlinesFound = 0;
	    ResizableStringArray cFoundName = new ResizableStringArray();
	    ResizableIntArray cFoundDist = new ResizableIntArray();
	    float dist = 0.0f;
	    int minDist = BIG_INT;
	    int selectIndex=0;
	    boolean pointSelected = false;
	    float x1Meters = 0.0f;
	    float y1Meters = 0.0f;
	    float x2Meters = 0.0f;
	    float y2Meters = 0.0f;
	    //    _nPlotter = _can._networkPlotter;
	    //select centerline and/or cross-section line      
	    if(_nPlotter != null && _net != null){
		for(int i=0; i<=_net.getNumCenterlines()-1; i++){
		    centerline = _net.getCenterline(_net.getCenterlineName(i));
		    pointSelected=false;
		    if(DEBUG)System.out.println
				 ("trying to select centerline "+_net.getCenterlineName(i));
		    minDist = BIG_INT;
		    for(int j=0; j<=centerline.getNumCenterlinePoints()-2; j++){
			if(DEBUG)System.out.println("centerline segment "+j);
			x1Meters = centerline.getCenterlinePoint(j).getX();
			y1Meters = centerline.getCenterlinePoint(j).getY();
			x2Meters = centerline.getCenterlinePoint(j+1).getX();
			y2Meters = centerline.getCenterlinePoint(j+1).getY();
			if(DEBUG)System.out.println
				     ("x1,x2,y1,y2,selectedX, selectedY="+x1Meters+","+x2Meters+","+y1Meters+","+y2Meters+","+_centerX+CsdpFunctions.xPixelsToLength(e.getX(),_minSlope,_minX)+","+_centerY+CsdpFunctions.yPixelsToLength(e.getY(),_minSlope,_minY,_height));
			
			if(_nPlotter._useZoomBox){
			    dist=CsdpFunctions.shortestDistLineSegment
				(x1Meters,x2Meters,
				 CsdpFunctions.xPixelsToLength(e.getX(),_minSlope,_minX),
				 y1Meters, y2Meters,CsdpFunctions.yPixelsToLength
				 (e.getY(),_minSlope,_minY,_height));
			}else{
			    dist=CsdpFunctions.shortestDistLineSegment
				(x1Meters,x2Meters,
				 CsdpFunctions.xPixelsToLength(e.getX(),_minSlope,_minX)-_centerX,
				 y1Meters, y2Meters,CsdpFunctions.yPixelsToLength
				 (e.getY(),_minSlope,_minY,_height)-_centerY);
			}
			if(DEBUG)System.out.println("shortest dist to line segment="+dist);
			if(dist < CsdpFunctions.BIG_FLOAT){
			    pointSelected = true;
			    if(DEBUG)System.out.println
					 ("before min function: minDist, (int)dist="+
					  minDist+","+(int)dist);
			    minDist = Math.min(minDist, (int)dist);
			    if(DEBUG)System.out.println("after min function: minDist, (int)dist="+
							minDist+","+(int)dist);	  
			}//if 
		    }//for j
		    if(DEBUG)System.out.println("minDist="+minDist);
		    //if centerline is close enough to clicked point, save name and distance 
		    //from point.
		    if(pointSelected == true){
			cFoundDist.put(numCenterlinesFound,minDist);
			cFoundName.put(numCenterlinesFound,_net.getCenterlineName(i));
			numCenterlinesFound++;
		    }//if
		}//for i
		//loop through all centerlines found and pick closest one
		minDist = BIG_INT;
		if(DEBUG)System.out.println("looping through possible centerlines to find closest");
		for(int j=0; j<=numCenterlinesFound-1; j++){
		    centerline = _net.getCenterline(cFoundName.get(j));
		    if(DEBUG)System.out.println("centerline, dist="+cFoundName.get(j)+","+cFoundDist.get(j));
		    if(cFoundDist.get(j) < minDist){
			selectIndex = j;
			minDist = cFoundDist.get(j);
		    }//if
		}//for j
		
		/*
		 * select closest centerline
		 */
		if(numCenterlinesFound > 0){
		    if(DEBUG)System.out.println("selected centerline "+
						cFoundName.get(selectIndex));
		    centerline = _net.getCenterline(cFoundName.get(selectIndex));
		    _net.setSelectedCenterlineName(cFoundName.get(selectIndex));
		    _net.setSelectedCenterline(_net.getCenterline
					       (_net.getSelectedCenterlineName()));
		    _net.setSelectedXsectNum(0);
		    _net.setSelectedXsect(null);
		    _gui.enableAfterCenterlineSelected();
		    _gui.disableIfNoXsectSelected();
		    _net.setNewCenterlineName(cFoundName.get(selectIndex));
		    _gui.updateInfoPanel(cFoundName.get(selectIndex));
		    //removed for conversion to swing
		    
		    _can.redoNextPaint();
		    _can.repaint();
		    /*
		     * If user clicks on same centerline, select xsect line
		     */
		    ////    if(_newCenterlineName.equals(_oldCenterlineName)){
		    //select xsect:  loop through all in centerline and see which is closest
		    xsect = null;
		    boolean xsectSelected = false;
		    ResizableIntArray xFoundNum = new ResizableIntArray();
		    ResizableIntArray xFoundDist = new ResizableIntArray();
		    int numXsectFound = 0;
		    
		    minDist = BIG_INT;
		    for(int k=0; k<=centerline.getNumXsects()-1; k++){
			xsectSelected = false;
			xsect = centerline.getXsect(k);
			float[] xsectLine = _net.findXsectLineCoord(_net.getSelectedCenterlineName(), k);
			if(_nPlotter._useZoomBox){
			    dist = CsdpFunctions.shortestDistLineSegment
				(xsectLine[x1Index],xsectLine[x2Index],
				 CsdpFunctions.xPixelsToLength(e.getX(),_minSlope,_minX),
				 xsectLine[y1Index],xsectLine[y2Index],
				 CsdpFunctions.yPixelsToLength(e.getY(),_minSlope,_minY,_height));
			}else{
			    dist = CsdpFunctions.shortestDistLineSegment
				(xsectLine[x1Index],xsectLine[x2Index],
				 CsdpFunctions.xPixelsToLength(e.getX(),_minSlope,_minX)-_centerX,
				 xsectLine[y1Index],xsectLine[y2Index],
				 CsdpFunctions.yPixelsToLength(e.getY(),_minSlope,_minY,_height)-_centerY);
			}
			if(DEBUG)System.out.println("trying to select xsect line: dist,x1x2x3y1y2y3="+dist+","+xsectLine[x1Index]+","+xsectLine[x2Index]+","+_centerX+CsdpFunctions.xPixelsToLength(e.getX(),_minSlope,_minX)+","+xsectLine[y1Index]+","+xsectLine[y2Index]+","+_centerY+CsdpFunctions.yPixelsToLength(e.getY(),_minSlope,_minY,_height));
			if(dist < CsdpFunctions.BIG_FLOAT){
			    xsectSelected = true;
			    minDist = Math.min(minDist, (int)dist);
			    
			    xFoundNum.put(numXsectFound, k);
			    xFoundDist.put(numXsectFound, (int)dist);
			    numXsectFound++;
			}//if searchBox
		    }//for k
		    
		    System.out.println();
		    for(int m=0; m<=numXsectFound-1; m++){
			if(DEBUG)System.out.println("m, xFoundDist, minDist="+m+","+xFoundDist.get(m)+","+minDist);
			if(xFoundDist.get(m) <= minDist){
			    selectIndex = xFoundNum.get(m);
			}//if xFoundDist
		    }//for m
		    /*
		     *select closest xsect line
		     */
		    if(numXsectFound > 0){
			System.out.println("select xsect "+selectIndex);
			xsect = centerline.getXsect(selectIndex);
			_net.setSelectedXsectNum(selectIndex);
			
			_net.setSelectedXsect(centerline.getXsect(_net.getSelectedXsectNum()));
			_gui.enableAfterXsectSelected();
			_gui.updateInfoPanel(_net.getSelectedXsectNum());
			_gui.updateInfoPanel(xsect.getArea(0.0f),xsect.getWidth(0.0f),
					     xsect.getWettedPerimeter(0.0f),
					     xsect.getHydraulicDepth(0.0f));
			//removed for conversion to swing
			_can.redoNextPaint();
			_can.repaint();
		    }//if xsect found
		    else{
			_gui.updateInfoPanel(-CsdpFunctions.BIG_INT);
			_gui.updateInfoPanel(-CsdpFunctions.BIG_INT,-CsdpFunctions.BIG_INT,
					     -CsdpFunctions.BIG_INT,-CsdpFunctions.BIG_INT);
		    }
		    ////    }//if it's the same centerline
		    //added for conversion to swing
		    _gui._canvas1.setUpdateNetwork(true);
		}//if found > 0 centerlines
		else{
		    _net.setSelectedCenterlineName(null);
		    _net.setSelectedCenterline(null);
		    _net.setSelectedXsectNum(-CsdpFunctions.BIG_INT);
		    _net.setSelectedXsect(null);
		    _gui.disableIfNoCenterlineSelected();
		    _gui.disableIfNoXsectSelected();
		    _gui.updateInfoPanel("");
		    _gui.updateInfoPanel(-CsdpFunctions.BIG_INT);
		    _gui.updateInfoPanel(-CsdpFunctions.BIG_INT, -CsdpFunctions.BIG_INT,
					 -CsdpFunctions.BIG_INT, -CsdpFunctions.BIG_INT);
		    _net.setNewCenterlineName(cFoundName.get(selectIndex));
		    
		    //removed for conversion to swing
		    _can.redoNextPaint();
		    _can.repaint();
		}//else select no centerline or xsect
		
		_net._oldCenterlineName = _net._newCenterlineName;
	    }//if _nPlotter != null
	    if(_gui.getRemoveXsectMode()){
		_net.getSelectedCenterline().removeXsect(_net.getSelectedXsectNum());
		_gui.updateInfoPanel(_net.getSelectedCenterlineName());
		_gui.updateInfoPanel(_net.getSelectedXsectNum());
		//	_gui.setRemoveXsectMode();
		
		_gui.turnOffEditModes();
	    }
	}//else
    }//mousePressed
  
  /**
   * Sets initial point of zoom rectangle region
   */
  public void setInitialPoint(int x, int y){
    _xi = x;
    _yi = y;
  }
  
  /**
   * Sets final point of zoom rectangle region. This would be
   * the point diagonally opposite the initial point
   */
  public void setFinalPoint(int x, int y){
    _xf = x;
    _yf = y;
    constructRectangle();
  }

  /**
   * Invoked when component has been moved.
   */    
  public void mouseMoved(MouseEvent e){
      if(DEBUG)System.out.println("Component Event: " + e.toString());
    _gui.updateInfoPanel(e.getX(),e.getY());
  }

  
  /**
   * add point to the end (usually downstream end) of centerline
   */
  protected void addPoint(){
    float xDataCoord;
    float yDataCoord;
    Centerline centerline;
    if(_net.getSelectedCenterlineName() != null){
      if(DEBUG)System.out.println("adding centerline point");
      centerline = _net.getCenterline(_net.getSelectedCenterlineName());
      if(_nPlotter._useZoomBox){
	  xDataCoord = CsdpFunctions.xPixelsToLength(_xi,_minSlope,_minX);
	  yDataCoord = CsdpFunctions.yPixelsToLength(_yi,_minSlope,_minY,_height);
      }else{
	  xDataCoord = CsdpFunctions.xPixelsToLength(_xi,_minSlope,_minX)-_centerX;
	  yDataCoord = CsdpFunctions.yPixelsToLength(_yi,_minSlope,_minY,_height)-_centerY;
      }
      centerline.addCenterlinePoint(xDataCoord, yDataCoord);
      _gui._canvas1.setUpdateNetwork(true);
      //removed for conversion to swing
	_gui._canvas1.redoNextPaint();
        _gui._canvas1.repaint();
        _net.setIsUpdated(true);
    }//if centerlineName not null
  }//addPoint

  /**
   * move centerline point
   */
  protected void movePoint(){
      System.out.println("NetworkInteractor.movePoint called");
    float xDataCoordInitial = -CsdpFunctions.BIG_FLOAT;
    float yDataCoordInitial = -CsdpFunctions.BIG_FLOAT;
    float xDataCoordFinal   = -CsdpFunctions.BIG_FLOAT;
    float yDataCoordFinal   = -CsdpFunctions.BIG_FLOAT;
    Centerline centerline = null;
    int minDistIndex = -CsdpFunctions.BIG_INT;
    float distToNearestPoint = 0.0f;
    CenterlinePoint point = null;
    if(_net.getSelectedCenterlineName() != null){
      System.out.println("moving centerline point");
      centerline = _net.getCenterline(_net.getSelectedCenterlineName());

      if(DEBUG)System.out.println("xi, yi, xf, yf="+_xi+","+ _yi+" "+_xf+","+_yf);

      if(_nPlotter._useZoomBox){
	  xDataCoordInitial = 
	      CsdpFunctions.xPixelsToLength(_xi,_minSlope,_minX);
	  yDataCoordInitial = 
	      CsdpFunctions.yPixelsToLength(_yi,_minSlope,_minY,_height);
	  xDataCoordFinal = 
	      CsdpFunctions.xPixelsToLength(_xf,_minSlope,_minX);
	  yDataCoordFinal = 
	      CsdpFunctions.yPixelsToLength(_yf,_minSlope,_minY,_height);
      }else{
	  xDataCoordInitial = 
	      CsdpFunctions.xPixelsToLength(_xi,_minSlope,_minX)-_centerX;
	  yDataCoordInitial = 
	      CsdpFunctions.yPixelsToLength(_yi,_minSlope,_minY,_height)-_centerY;
	  xDataCoordFinal = 
	      CsdpFunctions.xPixelsToLength(_xf,_minSlope,_minX)-_centerX;
	  yDataCoordFinal = 
	      CsdpFunctions.yPixelsToLength(_yf,_minSlope,_minY,_height)-_centerY;
      }
      
      minDistIndex = centerline.getNearestPointIndex(xDataCoordInitial, 
						     yDataCoordInitial);
      point = centerline.getCenterlinePoint(minDistIndex);
      distToNearestPoint = CsdpFunctions.pointDist
	(xDataCoordInitial, yDataCoordInitial,point.getX(), point.getY());
      if(distToNearestPoint < MAX_SEARCH_DIST){
	point.putX(xDataCoordFinal);
	point.putY(yDataCoordFinal);
	_gui._canvas1.setUpdateNetwork(true);
	//removed for conversion to swing
	//	
	_gui._canvas1.redoNextPaint();
	_gui._canvas1.repaint();
//    _gui._canvas1.validate();
      }
    }//if centerlineName not null
    _net.setIsUpdated(true);
}//movePoint

  /**
   * insert point between two existing points in a centerline
   */
  protected void insertPoint(){
    float xDataCoord;
    float yDataCoord;
    Centerline centerline;
    if(_net.getSelectedCenterlineName() != null){
      if(DEBUG)System.out.println("inserting centerline point");
      centerline = _net.getCenterline(_net.getSelectedCenterlineName());
      if(_nPlotter._useZoomBox){
	  xDataCoord = CsdpFunctions.xPixelsToLength(_xi,_minSlope,_minX);
	  yDataCoord = CsdpFunctions.yPixelsToLength(_yi,_minSlope,_minY,_height);
      }else{
	  xDataCoord = CsdpFunctions.xPixelsToLength(_xi,_minSlope,_minX)-_centerX;
	  yDataCoord = CsdpFunctions.yPixelsToLength(_yi,_minSlope,_minY,_height)-_centerY;
      }
      centerline.insertCenterlinePoint(xDataCoord, yDataCoord);
      _gui._canvas1.setUpdateNetwork(true);
      //removed for conversion to swing
      _gui._canvas1.redoNextPaint();
      _gui._canvas1.repaint();
    }//if centerlineName not null
    _net.setIsUpdated(true);
  }//insertPoint

  /**
   * delete point in centerline
   */
  protected void deletePoint(){
    float xDataCoord;
    float yDataCoord;
    Centerline centerline;
    Xsect xsect;
    if(_net.getSelectedCenterlineName() != null){
      if(DEBUG)System.out.println("deleting centerline point");
      centerline = _net.getCenterline(_net.getSelectedCenterlineName());
      if(_nPlotter._useZoomBox){
	  xDataCoord = CsdpFunctions.xPixelsToLength(_xi,_minSlope,_minX);
	  yDataCoord = CsdpFunctions.yPixelsToLength(_yi,_minSlope,_minY,_height);
      }else{
	  xDataCoord = CsdpFunctions.xPixelsToLength(_xi,_minSlope,_minX)-_centerX;
	  yDataCoord = CsdpFunctions.yPixelsToLength(_yi,_minSlope,_minY,_height)-_centerY;
      }
      centerline.deleteCenterlinePoint(xDataCoord, yDataCoord);
      if(centerline.getNumCenterlinePoints() <= 0){
	_net.removeCenterline(_net.getSelectedCenterlineName());
	_gui.disableIfNoCenterlineSelected();
      }
      for(int i=centerline.getNumXsects()-1; i>0; i--){
	xsect = centerline.getXsect(i);
	if(xsect.getDistAlongCenterline() > centerline.getLength() ||
	   xsect.getDistAlongCenterline() < 0){
	  centerline.removeXsect(i);
	}
      }
      _gui._canvas1.setUpdateNetwork(true);
      //removed for conversion to swing
      _gui._canvas1.redoNextPaint();
      _gui._canvas1.repaint();
    }//if centerlineName not null
    _net.setIsUpdated(true);
  }

  /**
   * move a cross-section along its centerline
   */
  protected void moveXsect(){
    if(DEBUG)System.out.println("moving xsect");
    Centerline centerline = _net.getSelectedCenterline();
    String centerlineName = centerline.getCenterlineName();
    float minDist = CsdpFunctions.BIG_FLOAT;
    float dist  =0.0f;
    int xsectNum = _net.getSelectedXsectNum();
    Xsect xsect = _net.getSelectedXsect();
    Vector xsectPoints = null;
    int numPoints = 0;
    float x1 = 0.0f;
    float x2 = 0.0f;
    float y1 = 0.0f;
    float y2 = 0.0f;
    float xDataCoord = -CsdpFunctions.BIG_FLOAT;
    float yDataCoord = -CsdpFunctions.BIG_FLOAT;
    int minDistIndex = 0;

    if(_nPlotter._useZoomBox){
	xDataCoord = CsdpFunctions.xPixelsToLength(_xi,_minSlope,_minX);
	yDataCoord = CsdpFunctions.yPixelsToLength(_yi,_minSlope,_minY,_height);
    }else{
	xDataCoord = CsdpFunctions.xPixelsToLength(_xi,_minSlope,_minX)-_centerX;
	yDataCoord = CsdpFunctions.yPixelsToLength(_yi,_minSlope,_minY,_height)-_centerY;
    }

    if(xsect != null){
      xsectPoints = xsect.getAllPoints();
      numPoints = xsect.getNumPoints();
    }

    //removing by number: sometimes number changes.
    for(int i=0; i<=centerline.getNumCenterlinePoints()-2; i++){
      x1 = centerline.getCenterlinePoint(i).getX();
      y1 = centerline.getCenterlinePoint(i).getY();
      x2 = centerline.getCenterlinePoint(i+1).getX();
      y2 = centerline.getCenterlinePoint(i+1).getY();
      dist = CsdpFunctions.shortestDistLineSegment
	(x1,x2,xDataCoord,y1,y2,yDataCoord,MAX_XSECT_LINE_LENGTH);
      if(dist < minDist){
	minDist = dist;
	minDistIndex = i;
      }
    }//for i

    if(DEBUG)System.out.println("minDist, CsdpFunctions.BIG_FLOAT "+minDist+","+CsdpFunctions.BIG_FLOAT);

    if(minDist < CsdpFunctions.BIG_FLOAT){
      centerline.removeXsect(xsectNum);
      addXsect();
      if(xsectPoints != null) xsect.putAllPoints(numPoints, xsectPoints);
    }
    //      centerline.sortXsects();
    _net.setIsUpdated(true);
    ////the following method isn't working yet.
    ////_app.updateXsectGraph(centerlineName, xsectNum);
    _gui.setAllButtonsDefaultColor();

    _gui.setDefaultModesStates();

    _app.updateXsect(centerlineName, xsectNum);
  }//moveXsect
  
  /**
   * add cross-section line to existing centerline
   */
  protected void addXsect(){
    int selectedXsectNum = 0;
    Xsect xsect = null;
    float xDataCoord = 0.0f;
    float yDataCoord = 0.0f;
    Centerline centerline = null;
    /*
     * Minimum perpendicular distance from the point on which the mouse was pressed to
     * a centerline segment
     */
    float minDist = CsdpFunctions.BIG_FLOAT;
    /*
     * Index of first centerline point in the centerline segment that is closest to the point
     * on which the mouse was pressed
     */
    int minDistIndex = CsdpFunctions.BIG_INT;
    float x1    = 0.0f;
    float y1    = 0.0f;
    float x2    = 0.0f;
    float y2    = 0.0f;
    float dist  = 0.0f;
    float theta = 0.0f;
    float xi    = 0.0f;
    float yi    = 0.0f;
    float cumDist = 0.0f;
    ResizableIntArray xsectIndices = new ResizableIntArray();
    if(_net.getSelectedCenterlineName() != null){
      if(DEBUG)System.out.println("adding cross-section");
      centerline = _net.getCenterline(_net.getSelectedCenterlineName());
      if(_nPlotter._useZoomBox){
	  xDataCoord = CsdpFunctions.xPixelsToLength(_xi,_minSlope,_minX);
	  yDataCoord = CsdpFunctions.yPixelsToLength(_yi,_minSlope,_minY,_height);
      }else{
	  xDataCoord = CsdpFunctions.xPixelsToLength(_xi,_minSlope,_minX)-_centerX;
	  yDataCoord = CsdpFunctions.yPixelsToLength(_yi,_minSlope,_minY,_height)-_centerY;
      }
      if(DEBUG)System.out.println
		 ("trying to add xsect to centerline "+
		  _net.getSelectedCenterlineName()+", x,y initial point="+
		  xDataCoord+","+yDataCoord);

      //loop through all centerline segments; find minimum perpendicular distance
      //and index of first point of line segment that has minimum perpendicular dist
      for(int i=0; i<=centerline.getNumCenterlinePoints()-2; i++){
	x1 = centerline.getCenterlinePoint(i).getX();
	y1 = centerline.getCenterlinePoint(i).getY();
	x2 = centerline.getCenterlinePoint(i+1).getX();
	y2 = centerline.getCenterlinePoint(i+1).getY();
	dist = CsdpFunctions.shortestDistLineSegment
	  (x1,x2,xDataCoord,y1,y2,yDataCoord,MAX_XSECT_LINE_LENGTH);
	if(DEBUG)System.out.println("line segment, shortest dist, x1x2x3y1y2y3="+
				    i+","+dist+","+x1+","+x2+","+xDataCoord+","+y1+
				    ","+y2+","+yDataCoord);
	if(dist < minDist){
	  minDist = dist;
	  minDistIndex = i;
	}//if
      }//for i

      if(DEBUG)System.out.println("minDistIndex, min dist="+minDistIndex+","+minDist);
      if(minDist < CsdpFunctions.BIG_FLOAT){
	x1 = centerline.getCenterlinePoint(minDistIndex).getX();
	y1 = centerline.getCenterlinePoint(minDistIndex).getY();
	x2 = centerline.getCenterlinePoint(minDistIndex+1).getX();
	y2 = centerline.getCenterlinePoint(minDistIndex+1).getY();
	theta = CsdpFunctions.getTheta(x1,x2,y1,y2);
	xi=CsdpFunctions.findXIntersection(x1,x2,xDataCoord,y1,y2,yDataCoord);
	yi=CsdpFunctions.findYIntersection(x1,x2,xDataCoord,y1,y2,yDataCoord);
	
	if(DEBUG)System.out.println("Intersection coord:"+xi+","+yi);
	
	//find dist from first point in centerline to first point in centerline 
	//segment that contains the xsect
	cumDist = 0.0f;
	if(DEBUG)System.out.println("minDistIndex="+minDistIndex);
	for(int i=0; i<=minDistIndex-1 && minDistIndex > 0; i++){
	  x1 = centerline.getCenterlinePoint(i).getX();
	  y1 = centerline.getCenterlinePoint(i).getY();
	  x2 = centerline.getCenterlinePoint(i+1).getX();
	  y2 = centerline.getCenterlinePoint(i+1).getY();
	  cumDist += CsdpFunctions.pointDist(x1,y1,x2,y2);
	  if(DEBUG)System.out.println("increasing cumDist:"+cumDist);
	}//for i
	
	x1 = centerline.getCenterlinePoint(minDistIndex).getX();
	y1 = centerline.getCenterlinePoint(minDistIndex).getY();
	cumDist += CsdpFunctions.pointDist(x1,y1,xi,yi);
	
	if(DEBUG)System.out.println("cumDist="+cumDist);
	
	//find index of last xsect that is closer to first point in centerline.
	//	Xsect xsect = null;
	int lastIndex = 0;
	int numXsects = 0;
	if(minDist < CsdpFunctions.BIG_FLOAT){
	  numXsects = centerline.getNumXsects();
	  if(numXsects == 0){
	    centerline.addXsect();
	    xsect = centerline.getXsect(0);
	  }
	  else{
	    for(int i=0; i<=numXsects-1; i++){
	      xsect = centerline.getXsect(i);
	      if(xsect.getDistAlongCenterline() < cumDist) lastIndex = i;
	    }
	    if(DEBUG)System.out.println("lastindex = "+lastIndex);
	    centerline.addXsectAt(lastIndex+1);
	    xsect = centerline.getXsect(lastIndex+1);
	  }//else

	  xsect.putDistAlongCenterline(cumDist);
	  xsect.putXsectLineLength(minDist*2.0f);

	  //sort
	  xsectIndices = centerline.sortXsectArray();
	  _app.renameOpenXsectGraphs(_net.getSelectedCenterlineName(), 
				     xsectIndices);

	  _gui._canvas1.setUpdateNetwork(true);
	  //removed for conversion to swing
	  _gui._canvas1.redoNextPaint();
	  _gui._canvas1.repaint();

	  if(numXsects == 0) selectedXsectNum = lastIndex;
	  else selectedXsectNum = lastIndex+1;

	  selectedXsectNum = xsectIndices.get(selectedXsectNum);

	  if(DEBUG)System.out.println("selected xsect number ="+
				      selectedXsectNum);

	  _net.setSelectedXsectNum(selectedXsectNum);
	  _net.setSelectedXsect(xsect);
	  _gui.enableAfterXsectSelected();

	  _gui.updateInfoPanel(_net.getSelectedXsectNum());
	}//if
      }//if minDist < BIG_FLOAT
    }//if a centerline has been selected
    _net.setIsUpdated(true);
    _gui.setAllButtonsDefaultColor();
    _gui.setDefaultModesStates();
  }//addXsect

  /**
   * find midpoint of xsect line, which is the intersection of the xsect line and 
   * the centerline.  Arguments are coordinates of xsect line.
   */
  protected int[] findIntersection(int x1, int y1, int x2, int y2){
    int[] returnValues = new int[2];
    float x1float = (float)x1;
    float y1float = (float)y1;
    float x2float = (float)x2;
    float y2float = (float)y2;

    float slope = ( (y2float-y1float)/(x2float-x1float) );
    float x = x1float + 0.5f*(x2float-x1float);
    float y = slope*(x - x1float) + y1float;
    returnValues[x1Index] = (int)x;
    returnValues[y1Index] = (int)y;
    return returnValues;
  }//findIntersection

  /**
   * Invoked when a mouse button has been released on a component.
   */
  public void mouseReleased(MouseEvent e){
    setFinalPoint(e.getX(), e.getY());
    if(DEBUG)System.out.println("mouse released:  final point="+e.getX()+","+e.getY());
    _nPlotter = _can._networkPlotter;
    if(_nPlotter != null){
      _minSlope = _nPlotter._minSlope;
      _height = _nPlotter._height;
      _centerX = _nPlotter._centerX;
      _centerY = _nPlotter._centerY;
    }
    if(_bathymetryPlot != null && _bathymetryPlot._bathymetryData != null){
//        _minX = _bathymetryPlot._bathymetryData.getMinX();
//        _minY = _bathymetryPlot._bathymetryData.getMinY();
      float[] bb = _bathymetryPlot.getBathymetryBoundaries();
      _minX = bb[CsdpFunctions.minXIndex];
      _minY = bb[CsdpFunctions.minYIndex];
    }
    if(_gui.getMovePointMode()) movePoint();
    if(_gui.getZoomBoxMode()) {
	_drawDragRect = false;
	if(_zoomRect.width <= 5 || _zoomRect.height <= 5) return;
	_can.zoomInOut(_zoomRect);
	_gui.setZoomBoxMode(false);
    }//if zoom mode
    
    _mouseDragged = false;
  }//mouseReleased

  /**
   * Constructs a rectangle from a set of diagonally opposite points.  Stores
   * rectangle in a stack.
   */
  protected void constructRectangle(){
    _zoomRect.x = Math.min(_xi, _xf);
    _zoomRect.y = Math.min(_yi, _yf);
    _zoomRect.width = Math.abs(_xi - _xf);
    _zoomRect.height = Math.abs(_yi - _yf);

    //    System.out.println("constructing rectangle.  _xi, _xf, _yi, _yf="+_xi+","+_xf+","+_yi+","+_yf);
    
  }//contructRectangle
  
  /**
   * Invoked when the mouse enters a component.
   */
  public void mouseEntered(MouseEvent e){
    if (DEBUG) System.out.println("Mouse Entered at ( " 
				  + e.getX() + ", " + e.getY() + " )");
  }
  
  /**
   * Invoked when the mouse exits a component.
   */
  public void mouseExited(MouseEvent e){
    if (DEBUG) System.out.println("Mouse Exited at ( " 
				  + e.getX() + ", " + e.getY() + " )");
  }
  /**
   * Invoked when a mouse button is pressed on a component and then 
   * dragged.  Mouse drag events will continue to be delivered to
   * the component where the first originated until the mouse button is
   * released (regardless of whether the mouse position is within the
   * bounds of the component).
   */
  public void mouseDragged(MouseEvent e){
    if(_drawDragRect) _mouseDragged = true;
    if(_gui.getZoomBoxMode()){
      Graphics g = _gCImage.getGraphics();
      Rectangle bounds = _can.getBounds(); bounds.x=0; bounds.y=0;
      ////      g.setClip(bounds);
      g.clipRect(0,0,bounds.width,bounds.height);

      //for changing to Swing
      //      g.drawImage(_can.getGraphicElementImage(),0,0,null);
      g.drawImage(_can.getBackgroundImage(),0,0,null);
      
      setFinalPoint(e.getX(), e.getY());
      Rectangle r = _zoomRect;
      g.setColor(_zoomRectColor);
      g.drawRect(r.x, r.y, r.width, r.height);
      _can.getGraphics().drawImage(_gCImage, 0, 0, null);
    }// if zoomBox Mode
  }//mouseDragged

  /**
   * Invoked when component has been moved.
   */    
  public void componentMoved(ComponentEvent e){
    if (DEBUG) System.out.println("Component Event: " + e.toString());
  }
  
  /**
   * sets Network object
   */
  public void setNetwork(Network net){
    _net = net;
  }
  /**
   * sets BathymetryPlot object
   */
  public void setPlotter(BathymetryPlot plot){
    _bathymetryPlot = plot;
  }

  /**
   * Initial point's x value
   */
  protected int _xi = 0;
  /**
   * Initial point's y value
   */
  protected int _yi = 0;
  /**
   * Final point's x value
   */
  protected int _xf = 0;
  /**
   * Final point's y value
   */
  protected int _yf = 0;
  public static final boolean DEBUG = false;
  PlanViewCanvas _can;
  Network _net;
  CsdpFrame _gui;
  BathymetryPlot _bathymetryPlot;
  //  protected boolean _centerlineSelected = false;
  NetworkPlot _nPlotter;
  /*
   * number of pixels to search for centerline
   */
  protected int SELECT_RANGE=30;
  protected static final int BIG_INT = (int)1E10;
  protected static final int x1Index = 0;
  protected static final int y1Index = 1;
  protected static final int x2Index = 2;
  protected static final int y2Index = 3;
  protected boolean _mouseDragged = false;
  protected static final float MAX_XSECT_LINE_LENGTH = 1.0e6f;
  protected float _minSlope = 0.0f;
  protected float _minX = 0.0f;
  protected float _minY = 0.0f;
  protected float _height = 0.0f;
  /**
   * centerX is the value in meters to add to all x coordinates so that the data
   * will be centered in the window in the x direction
   */
  float _centerX = 0.0f;
  /**
   * centerY is the value in meters to add to all y coordinates so that the data
   * will be centered in the window in the y direction
   */
  float _centerY = 0.0f;
  float MAX_SEARCH_DIST = 1000.0f;
  
  protected boolean _drawDragRect = true;
  protected boolean _previouslyDoubleBuffered = true;
  protected Image _gCImage;
  protected Rectangle _zoomRect = new Rectangle(0,0,0,0);
  protected Color _zoomRectColor = Color.black;
  protected App _app;
}//NetworkInteractor

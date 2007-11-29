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
import java.awt.*;
import java.awt.event.*;
/**
 * Plot Data on PlanViewCanvas
 *
 * @author
 * @version $Id:
 */
public abstract class PlanViewPlot {
  
  public PlanViewPlot(CsdpFrame gui, BathymetryData data, App app) {
    _gui = gui;
    _bathymetryData = data;
    _app = app;
  }//constructor
  
  /**
   * finds boundaries of plot region and center of region, then calls 
   * plot method in subclass
   */
  public void plotData(Graphics2D g, 
		       Rectangle bounds, Rectangle zb, boolean uzb){
    _zoomBox = zb;
    _useZoomBox = uzb;
    _width = bounds.width;
    _height = bounds.height;
    //float plotBoundaries[] = findMinSlope();
    //    findCenterX(bounds, plotBoundaries);
    //findCenterY(bounds, plotBoundaries);

    if(DEBUG)System.out.println("PlanViewPlot.plotData():width,height,centerX, centerY="+_width+","+_height+","+_centerX+","+_centerY);

    drawPoints(g, bounds, getBathymetryBoundaries());
  }//plotData

  public void plotBathymetryData(Graphics2D g, 
		       Rectangle bounds, Rectangle zb, boolean uzb){
    _zoomBox = zb;
    _useZoomBox = uzb;
    _width = bounds.width;
    _height = bounds.height;
    float plotBoundaries[] = findMinSlope();
    setBathymetryBoundaries(plotBoundaries);
    findCenterX(bounds, plotBoundaries);
    findCenterY(bounds, plotBoundaries);

    if(DEBUG)System.out.println("PlanViewPlot.plotData():width,height,centerX, centerY="+_width+","+_height+","+_centerX+","+_centerY);

    drawPoints(g, bounds, plotBoundaries);
  }//plotData

    public void setBathymetryBoundaries(float[] bathymetryBoundaries){
	_bathymetryBoundaries=bathymetryBoundaries;
    }
    public float[] getBathymetryBoundaries(){
	return _bathymetryBoundaries;
    }

  /**
   * Plot data points, top view. Points are plotted as square rectangles; 
   * requested size of points will determine number of pixels used for 
   * each square.
   */
  protected abstract void drawPoints(Graphics2D g, Rectangle Bounds, 
				     float[] dataBoundaries);
  
    /**
     * find x value (feet) to add to every x coordinate to center data 
     * in x dir.  The reason for centering data:  if the data set is 
     * very narrow, its display size will be adjusted to fit in the 
     * window and it will be centered in the horizontal direction.  If 
     * the data set is very wide, it will be centered in the vertical 
     * direction.  
     *
     *If using a zoom window, centerX should be the 
     * horizontal distance from the center of the data to the center 
     * of the zoom window.
     */
    private void findCenterX(Rectangle bounds, float[] plotBoundaries){
  	/**
  	 * the minimum x(UTM) coordinate of the bathymetry data
  	 */
  	float minXdata = CsdpFunctions.BIG_FLOAT;
  	/**
  	 * the maximum x(UTM) coordinate of the bathymetry data
  	 */
  	float maxXdata = -CsdpFunctions.BIG_FLOAT;

	/**
	 * the width (pixels) of the plot region
	 */ 
	int width  = -CsdpFunctions.BIG_INT;
	/**
	 * the width of the plot region(feet, assuming the bathymetry 
	 * data coordinates are in feet) of the plot region
	 */
	float widthFeet = -CsdpFunctions.BIG_FLOAT;
	/**
	 * The width of the data set in feet
	 */
	float dataWidthFeet = -CsdpFunctions.BIG_FLOAT;

	width = bounds.width;
	
	minXdata = plotBoundaries[CsdpFunctions.minXIndex];
	maxXdata = plotBoundaries[CsdpFunctions.maxXIndex];

	widthFeet = CsdpFunctions.xPixelsToLength(width, _minSlope, minXdata)-
	    CsdpFunctions.xPixelsToLength(0, _minSlope, minXdata);
	dataWidthFeet = maxXdata - minXdata;

	_centerX = (widthFeet - dataWidthFeet)/2.0f;
    }//findCenterX
    
    /**
     * find y value (feet) to add to every y coordinate to center data in y dir.
     * The reason for centering data:  if the data set is very narrow, its display
     * size will be adjusted to fit in the window and it will be centered in the 
     * horizontal direction.  If the data set is very wide, it will be centered 
     * in the vertical direction.
     *
     * If using a zoom window, centerY should be the vertical distance from the center
     * of the data to the center of the zoom window.
     */
  private void findCenterY(Rectangle bounds, float[] plotBoundaries){
    float minYdata = CsdpFunctions.BIG_FLOAT;
    float maxYdata = -CsdpFunctions.BIG_FLOAT;
    int height = CsdpFunctions.BIG_INT;

    height = bounds.height;
    

    minYdata = plotBoundaries[CsdpFunctions.minYIndex];
    maxYdata = plotBoundaries[CsdpFunctions.maxYIndex];
    
    /*
     * The actual height of the canvas in feet?
     */
    float heightFeet = CsdpFunctions.yPixelsToLength(0, _minSlope, minYdata, height)-
      CsdpFunctions.yPixelsToLength(height,_minSlope,minYdata,height);
    /*
     * The height of the data in feet
     */
    float dataHeightFeet = maxYdata - minYdata;
    _centerY = (heightFeet - dataHeightFeet)/2.0f;
    //    System.out.println("PlanViewPlot.findCenterX():  _useZoomBox = false. centerY="+_centerY);
  }//findCenterY

  /**
   * find the factor that will be used to convert length to pixels.  The factor
   * will be calculated in the horizontal and vertical directions, and the min
   * value will be used.
   * find coordinates of the four corners of the region that will be plotted on
   * the canvas.
   */
  private float[] findMinSlope(){
    float d[] = new float[4];

    if(_gui.getFitByBathymetryOption()){
	d[CsdpFunctions.minXIndex] = _bathymetryData.getMinX();
	d[CsdpFunctions.maxXIndex] = _bathymetryData.getMaxX();
	d[CsdpFunctions.minYIndex] = _bathymetryData.getMinY();
	d[CsdpFunctions.maxYIndex] = _bathymetryData.getMaxY();
    }
    else if(_gui.getFitByNetworkOption()){
	if(_net == null) _net = _app.getNetwork();
	d[CsdpFunctions.minXIndex] = _net.getMinX();
	d[CsdpFunctions.maxXIndex] = _net.getMaxX();
	d[CsdpFunctions.minYIndex] = _net.getMinY();
	d[CsdpFunctions.maxYIndex] = _net.getMaxY();
    }
    else if(_gui.getFitByLandmarkOption()){
	if(_landmark == null) _landmark = _app.getLandmark();
	d[CsdpFunctions.minXIndex] = _landmark.getMinX();
	d[CsdpFunctions.maxXIndex] = _landmark.getMaxX();
	d[CsdpFunctions.minYIndex] = _landmark.getMinY();
	d[CsdpFunctions.maxYIndex] = _landmark.getMaxY();
    }
    else System.out.println("error in BathymetryPlot: no fit option selected");

    float minX = d[CsdpFunctions.minXIndex];
    float maxX = d[CsdpFunctions.maxXIndex];
    float minY = d[CsdpFunctions.minYIndex];
    float maxY = d[CsdpFunctions.maxYIndex];
    float oldMaxX = maxX;
    float oldMinX = minX;
    float oldMaxY = maxY;
    float oldMinY = minY;

    if(DEBUG)System.out.println("PlanViewPlot.findMinSlope(): oldMinX, oldMaxX, oldMinY, oldMaxY="+
				oldMinX+","+oldMaxX+","+oldMinY+","+oldMaxY);

    if(_useZoomBox){
	System.out.println("findMinSlope:_centerx, centery="+_centerX+","+_centerY);
	//reset plot boundaries to area contained within zoom box
	//_width=width of canvas in pixels
	//_height=height of canvas in pixels
	//oldMaxX-oldMinX = width of data (in data units)
	float minSlopeOld = _minSlope;
  	_minSlope = Math.min((_width*_width/_zoomBox.width)/(oldMaxX-oldMinX), 
  			     (_height*_height/_zoomBox.height)/(oldMaxY-oldMinY));

  	//adjust zoombox:  if its short and wide, make it taller.
  	//If it's tall and narrow, make it wider.
  	if(_width/_zoomBox.width <= _height/_zoomBox.height){
    	  //adjust requested height because the zoombox is short and wide
    	  float newZoomBoxHeight = _zoomBox.width * (_height/_width);
    	  float newZoomBoxY = _zoomBox.y - (0.5f*Math.abs(newZoomBoxHeight - _zoomBox.height));
	  float newZoomBoxMinY = _zoomBox.y + (0.5f*Math.abs(newZoomBoxHeight - _zoomBox.height));

	  d[CsdpFunctions.minYIndex] = 
	      CsdpFunctions.yPixelsToLength((int)(newZoomBoxMinY+_zoomBox.height),
					    minSlopeOld,oldMinY,_height);
	  d[CsdpFunctions.maxYIndex] = 
	      CsdpFunctions.yPixelsToLength((int)(newZoomBoxY), 
					    minSlopeOld,oldMinY,_height);

    	  d[CsdpFunctions.minXIndex] = 
	      oldMinX + (_zoomBox.x/_width) * (oldMaxX-oldMinX);
    	  d[CsdpFunctions.maxXIndex] = 
	      oldMinX + ((_zoomBox.x+_zoomBox.width)/_width) * (oldMaxX-oldMinX);

	  System.out.println("short and wide: minx,maxx,miny,maxy="+d[CsdpFunctions.minXIndex]+","+
			     d[CsdpFunctions.maxXIndex]+","+
			     d[CsdpFunctions.minYIndex]+","+
			     d[CsdpFunctions.maxYIndex]);
  	}else{
	    //adjust requested width because the zoombox is tall and narrow
	    float newZoomBoxWidth = (_zoomBox.height/_height) * _width;
	    float newZoomBoxX = _zoomBox.x - (0.5f*Math.abs(newZoomBoxWidth - _zoomBox.width));
	    float newZoomBoxMaxX = _zoomBox.x + (0.5f*Math.abs(newZoomBoxWidth - _zoomBox.width));

	    d[CsdpFunctions.minXIndex] = oldMinX + ((newZoomBoxX)/_width) * (oldMaxX-oldMinX);
	    d[CsdpFunctions.maxXIndex] = oldMinX + ((newZoomBoxMaxX+_zoomBox.width)/_width) * (oldMaxX-oldMinX);
	    
	    d[CsdpFunctions.minYIndex] = CsdpFunctions.yPixelsToLength
		((int)(_zoomBox.y+_zoomBox.height), minSlopeOld, oldMinY, _height);
	    d[CsdpFunctions.maxYIndex] = CsdpFunctions.yPixelsToLength
		((int)(_zoomBox.y), minSlopeOld, oldMinY, _height);
	    System.out.println("tall and narrow: minx,maxx,miny,maxy="+d[CsdpFunctions.minXIndex]+","+
			       d[CsdpFunctions.maxXIndex]+","+
			       d[CsdpFunctions.minYIndex]+","+
			       d[CsdpFunctions.maxYIndex]);
	}
    } else {
	_minSlope = Math.min(_width/(maxX-minX), _height/(maxY-minY));
    }

    if(_minSlope < 0) System.out.println
			("Error in BathymetryPlot: _minSlope<0="+_minSlope);
    return d;
  }//findMinSlope

    /**
     * The factor which will be used to convert data coordinates to pixels.  
     * It's calculated in the x and y directions and the minimum value is
     * used so the data will fit in the window.
     */
    protected static float _minSlope;
    /*
     * The Width of the canvas in pixels
     */
    protected static float _width;
    /*
     * The height of the canvas in pixels
     */
    protected static float _height;
    /**
     * centerX is the value in feet to add to all x coordinates so that the data
     * will be centered in the window in the x direction
     */
    protected static float _centerX = 0.0f;
    /**
     * centerY is the value in feet to add to all y coordinates so that the data
     * will be centered in the window in the y direction
     */
    protected static float _centerY = 0.0f;
    //  protected int _colorTableInterval = 0;
    protected Network _net;
    protected Landmark _landmark;
    protected App _app;
    protected CsdpFrame _gui = null;
    protected static BathymetryData _bathymetryData = null;
    protected static final boolean DEBUG = false;


    //do these need to be static?
  protected Rectangle _zoomBox;
  protected boolean _useZoomBox = false;

    private static float[] _bathymetryBoundaries;
} // Class BathymetryPlot
  

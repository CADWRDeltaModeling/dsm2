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
//    import DWR.Graph.*;
//    import DWR.Graph.Canvas.*;
import vista.graph.*;

import java.awt.*;
import java.awt.event.*;
  /**
   * This class handles interaction of mouse and mouse motion with
   * Graph class and its components.
   * @author
   * @version
   */
public class XsectEditInteractor extends ElementInteractor{
  /**
   * for debugging
   */
public static final boolean DEBUG = false;
  /**
   * constructor
   */
    ///public XsectEditInteractor(XsectGraph xg, Xsect xsect, ElementContext gC, Graph[] graphs){
public XsectEditInteractor(XsectGraph xg, Xsect xsect, GECanvas gC, Graph graph){
  _xsectGraph = xg;
  _xsect = xsect;
  _gC = gC;
  _plot = ((Graph)_gC.getGraphicElement()).getPlot();
  _graph = graph;
  //  _zoom = new Zoom( (Graph) _gC.getGraphicElement());
}

  /**
   *
   */
  public void mouseClicked(MouseEvent e){
    setInitialPoint(e.getX(), e.getY());
//      if(_xsectGraph.getAddPointMode()) addPoint();
//      else if(_xsectGraph.getInsertPointMode()) insertPoint();
//      else if(_xsectGraph.getDeletePointMode()) deletePoint();
  } 


  /**
   * Invoked when a mouse button has been pressed on a component.
   * This sets the initial point of the zoom region
   */
    public void mousePressed(MouseEvent e){
	setInitialPoint(e.getX(), e.getY());
	if(_xsectGraph.getAddPointMode()) addPoint();
	else if(_xsectGraph.getInsertPointMode()) insertPoint();
	else if(_xsectGraph.getDeletePointMode()) deletePoint();

	//   _plot = ((Graph) _gC.getGraphicElement()).getPlot();
	//   _drawDragRect = true;
	
	//   _previouslyDoubleBuffered = _gC.isDoubleBuffered();
	//   if (!_previouslyDoubleBuffered) _gC.setDoubleBuffered(true);
	
	//   Rectangle r = _gC.getBounds();
	//   _gCImage = _gC.createImage(r.width, r.height);
	
    }
  
  /**
   * Invoked when a mouse button has been released on a component.
   * If mouse was dragged this marks the diagonally opposite point
   * to the initial point of the zoom region. Then region is zoomed 
   * into.
   * <p>
   * If mouse was clicked without dragging then zoom out is done to 
   * the previous zoom state.
   */
public void mouseReleased(MouseEvent e){
  ////   if (_mouseDragged) {
//     _drawDragRect = false;
    
     setFinalPoint(e.getX(), e.getY());
     if(_xsectGraph.getMovePointMode()) movePoint();


//     if(_zoomRect.width == 0 || _zoomRect.height == 0) return;
//     _zoom.zoomIn(_zoomRect);
    
//     //    g.setColor(previousColor);
    
//     _gC.redoNextPaint();
//     //    _gC.setDoubleBuffered(_previouslyDoubleBuffered);
//     _gC.repaint();

//     _mouseDragged = false;

//     if (_zoom.zoomOut()){

//       _gC.redoNextPaint();
//       //      _gC.setDoubleBuffered(_previouslyDoubleBuffered);
//       _gC.repaint();

       _mouseDragged = false;
//     }
       ////   }
}
  
  /**
   * Invoked when a mouse button is pressed on a component and then 
   * dragged.  
   * As mouse is dragged a rectangle showing the currently selected 
   * zoom region is displayed as a rectangle. To achieve good 
   * performance double buffering was used, however there seems to be
   * a bug in JDK 1.1.2 which does not draw the complete image when
   * a complicated drawing is done. This affects multiple curve plot.
   */
public void mouseDragged(MouseEvent e){
//   if(_xsectGraph._movePointMode){
//     //search for nearest point and grab
//   }

//   if ( _drawDragRect ){
     _mouseDragged = true;
    
//     Graphics g = _gCImage.getGraphics();
//     Rectangle bounds = _gC.getBounds(); bounds.x=0; bounds.y=0;
//     g.setClip(bounds);
//     g.drawImage(_gC.getGraphicElementImage(), 0, 0, null);

//     setFinalPoint(e.getX(), e.getY());
//     Rectangle r = _zoomRect;
//     //    Color previousColor = g.getColor();
//     g.setColor(_zoomRectColor);
//     g.drawRect(r.x, r.y, r.width, r.height);
//     _gC.getGraphics().drawImage(_gCImage, 0, 0, null);
//   }
}
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
  //  constructRectangle();
}

  /**
   * moves the nearest point in xsect
   */
  protected void movePoint(){
    double xDataCoordInitial;
    double yDataCoordInitial;
    double xDataCoordFinal;
    double yDataCoordFinal;
    XsectPoint point;
    int minDistIndex;
    int numPoints = _xsect.getNumPoints();
    if(DEBUG)System.out.println("moving xsect point");
    xDataCoordInitial = (_graph.getPlot().
  			 getAxis(AxisAttr.BOTTOM)).getScale().scaleToDC(_xi);
    yDataCoordInitial = (_graph.getPlot().
  			 getAxis(AxisAttr.LEFT)).getScale().scaleToDC(_yi);
    xDataCoordFinal   = (_graph.getPlot().
  			 getAxis(AxisAttr.BOTTOM)).getScale().scaleToDC(_xf);
    yDataCoordFinal   = (_graph.getPlot().
  			 getAxis(AxisAttr.LEFT)).getScale().scaleToDC(_yf);

    minDistIndex = _xsect.getNearestPointIndex((float)xDataCoordInitial, 
					       (float)yDataCoordInitial);
    point = _xsect.getXsectPoint(minDistIndex);

    point.putStation((float)xDataCoordFinal);
    point.putElevation((float)yDataCoordFinal);
    //    _xsectGraph.updateNetworkDataSet();
    _xsectGraph.updateDisplay();
    _xsect.setIsUpdated(true);
  }//movePoint

  /**
   * deletes the nearest point in xsect
   */
  protected void deletePoint(){
    double xDataCoord;
    double yDataCoord;
    int numPoints = _xsect.getNumPoints();
    if(DEBUG)System.out.println("deleting xsect point");
//      xDataCoord = (((GraphCanvas)_gC).getPlot().
//  		  getAxis(AxisAttr.BOTTOM)).getScale().scaleToDC(_xi);
//      yDataCoord = (((GraphCanvas)_gC).getPlot().
//  		  getAxis(AxisAttr.LEFT)).getScale().scaleToDC(_yi);

    xDataCoord = (_graph.getPlot().
		  getAxis(AxisAttr.BOTTOM)).getScale().scaleToDC(_xi);
    yDataCoord = (_graph.getPlot().
		  getAxis(AxisAttr.LEFT)).getScale().scaleToDC(_yi);
    _xsect.deleteXsectPoint(xDataCoord, yDataCoord);
    //    _xsectGraph.updateNetworkDataSet();
    _xsectGraph.updateDisplay();
    _xsect.setIsUpdated(true);
  }//deletePoint

  /**
   * inserts a point between the nearest two points in xsect
   */
  protected void insertPoint(){
    double xDataCoord;
    double yDataCoord;
    int numPoints = _xsect.getNumPoints();
    if(DEBUG)System.out.println("inserting xsect point");
    xDataCoord = (_graph.getPlot().
		  getAxis(AxisAttr.BOTTOM)).getScale().scaleToDC(_xi);
    yDataCoord = (_graph.getPlot().
		  getAxis(AxisAttr.LEFT)).getScale().scaleToDC(_yi);
    _xsect.insertXsectPoint(xDataCoord, yDataCoord);
    //    _xsectGraph.updateNetworkDataSet();
    _xsectGraph.updateDisplay();
    _xsect.setIsUpdated(true);
  }

  /**
   * adds a point after the last point in xsect
   */
  protected void addPoint(){
    double xDataCoord;
    double yDataCoord;
    if(DEBUG)System.out.println("adding xsect point");
    xDataCoord = (_graph.getPlot().
		  getAxis(AxisAttr.BOTTOM)).getScale().scaleToDC(_xi);
    yDataCoord = (_graph.getPlot().
		  getAxis(AxisAttr.LEFT)).getScale().scaleToDC(_yi);
    _xsect.addXsectPoint((float)xDataCoord, (float)yDataCoord);
    //if there are no points, create net networkdataset
    //    if(_xsectGraph._gC.getPlot().getCurve(_xsectGraph._networkDataSet) == null){
    //NOT SURE IF THIS IS THE RIGHT CURVE....
    System.out.println("num curves,num bathymetry datasets="+_graph.getPlot().getNumberOfCurves()+","+
		       _xsectGraph._numBathymetryDataSets);

    //    _xsectGraph.updateNetworkDataSet();
    _xsectGraph.updateDisplay();
    _xsectGraph.updateXsectProp();
    _xsect.setIsUpdated(true);
  }//addPoint

  /**
   * Buffers image so as to avoid flickering while selecting zoom region
   */
protected Image _gCImage;
  /**
   * Graph canvas
   */
    ///protected ElementContext _gC;
protected GECanvas _gC;
  /**
   * Stores flag to indicate if double buffering was being used before
   */
protected boolean _previouslyDoubleBuffered = false;
  /**
   * Flag to indicate zoom region selection is in progress.
   */
protected boolean _drawDragRect = true;  
  /**
   * Flag to indicate whether mouse was dragged after mouse button was pressed.
   */
protected boolean _mouseDragged = false;
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
  /**
   * Current zooming region
   */
protected Rectangle _zoomRect = new Rectangle(0,0,0,0);
  /**
   * color used to draw the zoom rectangle
   */
protected Color _zoomRectColor = Color.black;
  /**
   * Plot object
   */
protected Plot _plot;
/**
 * XsectGraph object
 */
protected XsectGraph _xsectGraph;
protected Xsect _xsect;
    private Graph _graph;
}

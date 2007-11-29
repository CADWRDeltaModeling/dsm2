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
 * Plot Network Data on screen
 *
 * @author
 * @version $Id: NetworkPlot.java,v 1.2 2002/10/21 20:02:20 btom Exp $
 */
public class NetworkPlot extends PlanViewPlot{
    
    public NetworkPlot (CsdpFrame gui, BathymetryData data, App app) {
	super(gui, data, app);
    }

    /**
     * update bathymetry data
     */
    public void setBathymetryData(BathymetryData data){
	if(DEBUG)System.out.println("setting bathymetry data object in networkplot. minx,miny="
				    +data.getMinX()+","+data.getMinY());
	_bathymetryData = data;
    }
    
    /**
     * sets network object
     */
    public void setNetwork(Network net){
	_net = net;
    }
    
    /**
     * Plot network data, top view.
     */
    public void drawPoints(Graphics2D g, Rectangle bounds, float[] dataBoundaries){
	g.setColor(Color.black);
	Centerline centerline;
	CenterlinePoint centerlinePoint;
	CenterlinePoint nextCenterlinePoint;
	Xsect xsect;
	float height = bounds.height;
	
  	float minX = dataBoundaries[CsdpFunctions.minXIndex];
  	float minY = dataBoundaries[CsdpFunctions.minYIndex];

	System.out.println("plotNetwork: minX, minY, minSlope, height="+minX+","+minY+","+_minSlope+","+height);
	
	float[] xy;
	int x1Index = 0;
	int y1Index = 1;
	int x2Index = 2;
	int y2Index = 3;
	String centerlineName;
	
	if(DEBUG)System.out.println("NetworkPlot: centerX, centerY="+_centerX+","+_centerY);
	
	for(int c=0; c<=(_net.getNumCenterlines()-1); c++) {
	    if(DEBUG)System.out.println
			 ("networkplot: numcenterlines="+_net.getNumCenterlines());
	    centerlineName = _net.getCenterlineName(c);
	    centerline = _net.getCenterline(centerlineName);
	    // plot centerline
	    if(DEBUG)System.out.println("centerlineName, centerline object="+centerlineName+","+centerline);
	    for(int p=0; p<=centerline.getNumCenterlinePoints()-2; p++) {
		centerlinePoint = centerline.getCenterlinePoint(p);
		nextCenterlinePoint = centerline.getCenterlinePoint(p+1);
		if(_useZoomBox){
		    _x1Pixels = CsdpFunctions.xUTMToPixels(minX,_minSlope, centerlinePoint.getX());
		    _x2Pixels = CsdpFunctions.xUTMToPixels(minX,_minSlope, nextCenterlinePoint.getX());
		    _y1Pixels = CsdpFunctions.yUTMToPixels(minY,_minSlope, height, centerlinePoint.getY());
		    _y2Pixels = CsdpFunctions.yUTMToPixels(minY,_minSlope, height, nextCenterlinePoint.getY());
		}else{
		    _x1Pixels = CsdpFunctions.xUTMToPixels(minX,_minSlope,
							   centerlinePoint.getX()+_centerX);
		    _x2Pixels = CsdpFunctions.xUTMToPixels(minX,_minSlope,
							   nextCenterlinePoint.getX()+_centerX);
		    _y1Pixels = CsdpFunctions.yUTMToPixels(minY,_minSlope, height,
							   centerlinePoint.getY()+_centerY);
		    _y2Pixels = CsdpFunctions.yUTMToPixels(minY,_minSlope, height,
							   nextCenterlinePoint.getY()+_centerY);
		}
		
		if(DEBUG)System.out.println
			     ("centerline: x1,y1,x2,y2(meters)="+
			      centerlinePoint.getX()+_centerX+","+
			      centerlinePoint.getY()+_centerY+" "+
			      nextCenterlinePoint.getX()+_centerX+","+
			      nextCenterlinePoint.getY()+_centerY);
		if(DEBUG)System.out.println
			     ("centerline: x1,y1,x2,y2(pixels)="+
			      _x1Pixels+","+_y1Pixels+" "+_x2Pixels+","+_y2Pixels);
		
		g.drawLine((int)_x1Pixels,(int)_y1Pixels,(int)_x2Pixels,(int)_y2Pixels);
	    } // for p
	    // draw squares on points if centerline is selected
	    if(DEBUG)System.out.println("net="+_net);
	    if(DEBUG)System.out.println("centerlineName, selectedcenterlinename="+centerlineName+","+_net.getSelectedCenterlineName());
	    if(centerlineName.equals(_net.getSelectedCenterlineName())){
		if(DEBUG)System.out.println("match found");
		for(int p=0; p<=centerline.getNumCenterlinePoints()-1; p++){
		    centerlinePoint = centerline.getCenterlinePoint(p);
		    
		    if(_useZoomBox){
			_x1Pixels = CsdpFunctions.xUTMToPixels(minX,_minSlope,
							       centerlinePoint.getX());
			_y1Pixels = CsdpFunctions.yUTMToPixels(minY,_minSlope,height,
							       centerlinePoint.getY());
		    }else{
			_x1Pixels = CsdpFunctions.xUTMToPixels(minX,_minSlope,
							       centerlinePoint.getX()+_centerX);
			_y1Pixels = CsdpFunctions.yUTMToPixels(minY,_minSlope,height,
							       centerlinePoint.getY()+_centerY);
		    }
		    
		    g.drawRect((int)_x1Pixels-POINT_DIMENSION/2, 
			       (int)_y1Pixels-POINT_DIMENSION/2,
			       POINT_DIMENSION, POINT_DIMENSION);
		    //if upstream point, draw a "U"; if downstream point, draw a "D"
		    if(p==0) g.drawString("U",(int)_x1Pixels, (int)_y1Pixels);
		    else if(p==centerline.getNumCenterlinePoints()-1){
			g.drawString("D",(int)_x1Pixels,(int)_y1Pixels);
		    }
		}//for p
	    }//if centerline is selected
	    // plot xsect line
	    for(int xs=0; xs<=centerline.getNumXsects()-1; xs++){
		xsect=centerline.getXsect(xs);
		//xy=findXsectLineCoord(centerline, xsect, xs);
		if(DEBUG)System.out.println("number of xsect = "+centerline.getNumXsects());
		if(DEBUG)System.out.println("centerlineName, xs#="+centerlineName+","+xs);
		xy = _net.findXsectLineCoord(centerlineName, xs);
		if(_useZoomBox){
		    _x1Pixels = CsdpFunctions.xUTMToPixels(minX,_minSlope,xy[x1Index]);
		    _x2Pixels = CsdpFunctions.xUTMToPixels(minX,_minSlope,xy[x2Index]);
		    _y1Pixels = CsdpFunctions.yUTMToPixels(minY,_minSlope,height, xy[y1Index]);
		    _y2Pixels = CsdpFunctions.yUTMToPixels(minY,_minSlope,height, xy[y2Index]);
		}else{
		    _x1Pixels = CsdpFunctions.xUTMToPixels(minX,_minSlope,xy[x1Index]+_centerX);
		    _x2Pixels = CsdpFunctions.xUTMToPixels(minX,_minSlope,xy[x2Index]+_centerX);
		    _y1Pixels = CsdpFunctions.yUTMToPixels(minY,_minSlope,height,
							   xy[y1Index]+_centerY);
		    _y2Pixels = CsdpFunctions.yUTMToPixels(minY,_minSlope,height,
							   xy[y2Index]+_centerY);
		    
		}
		
		if(DEBUG)System.out.println("Cross-section line coord: x1,y1,x2,y2="+
					    xy[x1Index]+_centerX+","+xy[y1Index]+_centerX+" "+
					    xy[x2Index]+_centerY+","+xy[y2Index]+_centerY);
		if(DEBUG)System.out.println("Cross-section line coord: x1,y1,x2,y2="+
					    _x1Pixels+","+_y1Pixels+" "+
					    _x2Pixels+","+_y2Pixels);
		
		g.drawLine((int)_x1Pixels,(int)_y1Pixels,(int)_x2Pixels,(int)_y2Pixels);
		if(xsect == _net.getSelectedXsect()){
		    g.drawRect((int)_x1Pixels-POINT_DIMENSION/2, 
			       (int)_y1Pixels-POINT_DIMENSION/2,
			       POINT_DIMENSION, POINT_DIMENSION);
		    g.drawRect((int)_x2Pixels-POINT_DIMENSION/2, 
			       (int)_y2Pixels-POINT_DIMENSION/2,
			       POINT_DIMENSION, POINT_DIMENSION);
		}//if xsect
	    } // for xs
	} // for c
    }//plot
    
  protected static final int POINT_SIZE = 1;// size of displayed data point (square)
  protected float _x1Pixels; // coordinates of centerline points converted to pixels
  protected float _y1Pixels;
  protected float _x2Pixels;
  protected float _y2Pixels;
  Network _net = null;
  protected int POINT_DIMENSION = 2;
  protected static final boolean DEBUG = false;
} // class NetworkPlot

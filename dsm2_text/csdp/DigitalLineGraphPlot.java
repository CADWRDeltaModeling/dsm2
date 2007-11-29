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
 * Plot DigitalLineGraph Data on screen
 *
 * @author
 * @version $Id: DigitalLineGraphPlot.java,v 1.2 2002/10/21 19:58:54 btom Exp $
 */
public class DigitalLineGraphPlot extends PlanViewPlot{

    /**
     * sets value of _bathymetryData in superclass
     */
    public DigitalLineGraphPlot (CsdpFrame gui, BathymetryData data, App app) {
	super(gui, data, app);
    }

  /**
   * sets dlg object
   */
  public void setDigitalLineGraph(DigitalLineGraph dlg){
    _dlg = dlg;
  }

    /**
     * Plot dlg data, top view.
     */
    public void drawPoints(Graphics2D g, Rectangle bounds, float[] dataBoundaries){
	g.setColor(Color.black);
	//    String name = null;
	float height = bounds.height;
	float minX = dataBoundaries[CsdpFunctions.minXIndex];
	float minY = dataBoundaries[CsdpFunctions.minYIndex];
	float x = -CsdpFunctions.BIG_FLOAT;
	float y = -CsdpFunctions.BIG_FLOAT;
	float x2 = -CsdpFunctions.BIG_FLOAT;
	float y2 = -CsdpFunctions.BIG_FLOAT;
	int xPixels = -CsdpFunctions.BIG_INT;
	int yPixels = -CsdpFunctions.BIG_INT;
	int xPixels2 = -CsdpFunctions.BIG_INT;
	int yPixels2 = -CsdpFunctions.BIG_INT;
	DigitalLineGraphLine line = null;
	DigitalLineGraphPoint point = null;
	DigitalLineGraphPoint point2 = null;
	
	System.out.println("in DigitalLineGraphPlot: centerx, centery="+_centerX+","+_centerY);
	
	//    for(int i=0; i<=_landmark.getNumLandmarks()-1; i++){
	//line numbers in the dlg file start at 1
	for(int i=0; i<=_dlg.getNumLines()-1; i++){
	    if(DEBUG)System.out.println("DigitalLineGraphPlot.drawPoints: numlines="+_dlg.getNumLines());
	    //      name = _dlg.getDigitalLineGraphName(i);
	    line = _dlg.getLine(i);
	    if(DEBUG)System.out.println("line number "+i+"="+line);
	    for(int j=0; j<=line.getNumPoints()-2; j++){
		point = line.getPoint(j);
		point2= line.getPoint(j+1);
		x = (point.getX());
		y = (point.getY());
		x2= (point2.getX());
		y2= (point2.getY());
		if(DEBUG)System.out.println("x, y, minx, miny="+x+","+y+" "+minX+","+minY);
		if(_useZoomBox){
		    xPixels = CsdpFunctions.xUTMToPixels(minX, _minSlope, x);
		    yPixels = CsdpFunctions.yUTMToPixels(minY, _minSlope, height, y);
		    xPixels2 = CsdpFunctions.xUTMToPixels(minX, _minSlope, x2);
		    yPixels2 = CsdpFunctions.yUTMToPixels(minY, _minSlope, height, y2);
		}else{
		    xPixels = CsdpFunctions.xUTMToPixels(minX, _minSlope, x + _centerX);
		    yPixels = CsdpFunctions.yUTMToPixels(minY, _minSlope, height, y + _centerY);
		    xPixels2 = CsdpFunctions.xUTMToPixels(minX, _minSlope, x2 + _centerX);
		    yPixels2 = CsdpFunctions.yUTMToPixels(minY, _minSlope, height, y2 + _centerY);
		}
		
		g.drawLine((int)xPixels,(int)yPixels,(int)xPixels2,(int)yPixels2);
	    }
	    
	    if(DEBUG)System.out.println("x, y="+xPixels+","+yPixels);
	    
	}
    }//plot digital line graph
    
  protected static final int POINT_SIZE = 1;// size of displayed data point (square)
  protected float _x1Pixels; // coordinates of centerline points converted to pixels
  protected float _y1Pixels;
  protected float _x2Pixels;
  protected float _y2Pixels;
  DigitalLineGraph _dlg = null;
  protected int POINT_DIMENSION = 2;
} // class DigitalLineGraphPlot

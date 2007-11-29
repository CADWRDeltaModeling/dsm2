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
 * Plot Landmark Data on screen
 *
 * @author
 * @version $Id: LandmarkPlot.java,v 1.2 2002/10/21 20:02:19 btom Exp $
 */
public class LandmarkPlot extends PlanViewPlot{

  /**
   * sets value of _bathymetryData in superclass
   */
public LandmarkPlot (CsdpFrame gui, BathymetryData data, App app) {
  super(gui, data, app);
}

  /**
   * sets landmark object
   */
  public void setLandmark(Landmark landmark){
    _landmark = landmark;
  }

    /**
     * Plot landmark data, top view.
     */
    public void drawPoints(Graphics2D g, Rectangle bounds, float[] dataBoundaries){
	g.setColor(Color.black);
	String name = null;
	float height = bounds.height;
	float minX = dataBoundaries[CsdpFunctions.minXIndex];
	float minY = dataBoundaries[CsdpFunctions.minYIndex];
	
	float x;
	float y;
	int xPixels;
	int yPixels;
	
	if(DEBUG)System.out.println("in LandmarkPlot: centerx, centery="+_centerX+","+_centerY);
	
	for(int i=0; i<=_landmark.getNumLandmarks()-1; i++){
	    name = _landmark.getLandmarkName(i);
	    x = (_landmark.getX(name));
	    y = (_landmark.getY(name));
	    
	    if(_useZoomBox){
		xPixels = CsdpFunctions.xUTMToPixels(minX, _minSlope, x);
		yPixels = CsdpFunctions.yUTMToPixels(minY, _minSlope, height, y);
	    }else{
		xPixels = CsdpFunctions.xUTMToPixels(minX, _minSlope, x + _centerX);
		yPixels = CsdpFunctions.yUTMToPixels(minY, _minSlope, height, y + _centerY);
	    }
	    
	    if(DEBUG)System.out.println("x, y="+xPixels+","+yPixels);
	    g.drawRect((int)xPixels-POINT_DIMENSION/2, 
		       (int)yPixels-POINT_DIMENSION/2,
		       POINT_DIMENSION, POINT_DIMENSION);
	    g.drawString(name, xPixels, yPixels);
	}
    }//plot landmark
    
    protected static final int POINT_SIZE = 1;// size of displayed data point (square)
    protected float _x1Pixels; // coordinates of centerline points converted to pixels
    protected float _y1Pixels;
    protected float _x2Pixels;
    protected float _y2Pixels;
    Landmark _landmark = null;
    protected int POINT_DIMENSION = 2;
} // class LandmarkPlot

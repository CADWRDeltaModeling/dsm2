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
import DWR.CSDP.dialog.*;
import java.awt.event.*;
import java.awt.*;
import java.io.*;
import javax.swing.*;
/**
 * calls methods for zooming 
 *
 * @author
 * @version $Id: ZoomMenu.java,v 1.2 2002/10/21 20:01:44 btom Exp $
 */
public class ZoomMenu{
    public ZoomMenu(CsdpFrame gui){
	_gui = gui;
    }
    /**
     * Zooms in by a factor of 2(result will be half as wide and high)
     */
    public class ZIn implements ActionListener{
	public ZIn(){

	}
	public void actionPerformed(ActionEvent e){
	    System.out.println("ZIn");
	    PlanViewCanvas can = _gui._canvas1;
	    float newZBFactor = _gui.multiplyZoomBoxFactor(0.5f);
	    int width = (int)((float)can.getSize().width * newZBFactor);
	    int height = (int)((float)can.getSize().height * newZBFactor);

	    if(DEBUG)System.out.println("before zooming: width, height="+width+","+height);

	    /**
	     * x coordinate of the point in the canvas that should be at top left corner
	     * = location of upper left corner before zooming + difference in x coordinates
	     * (x after zooming - x before zooming)
	     */
	    int x = _r.getLocation().x + (int)(((float)_gui.getCurrentWidth() - 
						(float)width)/2.0f);
	    /**
	     * y coordinate of the point in the canvas that should be at top left corner
	     * = location of upper left corner before zooming + difference in y coordinates
	     * (y after zooming - y before zooming)
	     */
	    int y = _r.getLocation().y + (int)(((float)_gui.getCurrentHeight() - 
						(float)height)/2.0f);

	    System.out.println("x and y set.  rectangle x, rectangle width, new width="+
			       _r.getLocation().x+","+_gui.getCurrentWidth()+","+width);
	    System.out.println("rectangle y, rectangle height, new height="+
			       _r.getLocation().y+","+_gui.getCurrentHeight()+","+height);

	    System.out.println("rectangle coordinates="+x+","+y);
	    System.out.println("about to zoom in.  width,height="+width+","+height);

	    _r.setSize(width, height);
	    _r.setLocation(x,y);
	    _gui._canvas1.zoomInOut(_r);
	}//actionPerformed
    }//class ZIn

    /**
     * Zooms out by a factor of 2 (result will be twice as wide and high)
     */

    public class ZOut implements ActionListener{
	public ZOut(){

	}
	public void actionPerformed(ActionEvent e){
	    System.out.println("ZOut");
	    PlanViewCanvas can = _gui._canvas1;
	    float newZBFactor = _gui.multiplyZoomBoxFactor(2.0f);
	    int width = (int)((float)can.getSize().width * newZBFactor);
	    int height = (int)((float)can.getSize().height * newZBFactor);
	    int x = _r.getLocation().x + (int)(((float)_gui.getCurrentWidth() - 
						(float)width)/2.0f);
	    int y = _r.getLocation().y + (int)(((float)_gui.getCurrentHeight() - 
						(float)height)/2.0f);
	    if(x<0) x=0;
	    if(y<0) y=0;

	    System.out.println("x and y set.  rectangle x, rectangle width, new width="+
			       _r.getLocation().x+","+_gui.getCurrentWidth()+","+width);
	    System.out.println("rectangle y, rectangle height, new height="+
			       _r.getLocation().y+","+_gui.getCurrentHeight()+","+height);
	    System.out.println("rectangle coordinates="+x+","+y);
	    System.out.println("about to zoom in.  width,height="+width+","+height);

	    _r.setSize(width, height);
	    _r.setLocation(x,y);
	    _gui._canvas1.zoomInOut(_r);
	}//actionPerformed
    }//class ZOut

    /**
     * Pan--user specifies initial point and offset
     */

    public class ZPan implements ActionListener{
	public ZPan(){

	}
	public void actionPerformed(ActionEvent e){
	    System.out.println("Pan");

	}

    }
    /**
     * return to original view
     */

    public class ZFit implements ActionListener{
	public ZFit(){

	}
	public void actionPerformed(ActionEvent e){
	    System.out.println("Fit");
	    int width = _gui.getInitialWidth();
	    int height = _gui.getInitialHeight();
	    int x = 0;
	    int y = 0;
	    _gui.setZoomBoxFactor(1.0f);
	    _gui.setZoomBoxX(x);
	    _gui.setZoomBoxY(y);
	    _gui.setCurrentWidth(width);
	    _gui.setCurrentHeight(height);
	    _r.setSize(width,height);
	    _r.setLocation(x,y);
	    _gui._canvas1.zoomInOut(_r);
	}

    }

  /**
   * Allow user to draw rectangle for zoom box mode.  uses less memory
   * than zoom factor(below)
   *
   * @author
   * @version
   */
  public class ZBox implements ActionListener{

    public void actionPerformed(ActionEvent e){
	System.out.println("zoom box");
	_gui.toggleZoomBoxMode();
    }//actionPerformed

  }//class ZWindow

//    /**
//     * Get zoom factor from user and use to redraw window
//     *
//     * @author
//     * @version $Id: ZoomMenu.java,v 1.2 2002/10/21 20:01:44 btom Exp $
//     */
//    public class ZFactor implements ActionListener {  
    
//      float _nf=0.0f;
//      final int _numScrollBars = 1;
//      float[] _zoomFactor;
//      ScrollbarDialog _s;
//        YesNoDialog _ynd = new YesNoDialog(_gui, "WARNING", true, 
//  					 "WARNING:  high zoom factors can cause"+"OutOfMemoryErrors."+
//  					 System.getProperty("line.separator")+
//  					 "If you get an OutOfMemoryError, zoom out.  Show message again?");
//      /**
//       * assign instances of application and gui classes to class variables
//       */
//      public ZFactor() {
//        _zoomFactor = new float[1];
//        _zoomFactor[0] = CsdpFunctions.getZoomFactor();
//        _s = new ScrollbarDialog
//  	(_gui,"Select zoom factor(Large values may not work. Resize frame after adjusting.)",true,
//  	 _numScrollBars,_zoomFactor,MIN_ZOOM,MAX_ZOOM,FACTOR);
//      }
    
//      /**
//       * get new zoom factor from user
//       */
//      public void actionPerformed(ActionEvent e) {
//  	if(CsdpFunctions.getWarnZoom()){
//  	    _ynd.reset();
//  	    _ynd.show();
//  	    if(_ynd._yes){
//  		CsdpFunctions.setWarnZoom(true);
//  	    }else if(_ynd._no){
//  		CsdpFunctions.setWarnZoom(false);
//  	    }else{
//  		CsdpFunctions.setWarnZoom(true);
//  	    }
//  	}
	
//  	getValueAndZoom();
//      }//actionPerformed

//        /**
//         * you can't catch an error, so this doesn't really work...
//         * but it will zoom in if the factor isn't too big.
//         */
//        public void getValueAndZoom(){
//  	  CsdpFunctions.setOldZoomFactor(CsdpFunctions.getZoomFactor());
//  	  _s.updateScrollbar(0,(int)CsdpFunctions.getZoomFactor());
//  	  _s.show();
//  	  _nf = _s.getScrollbarValue(0);
	  
//  	  if(_nf != CsdpFunctions.getZoomFactor()){
//  	      if(DEBUG)System.out.println("new zoom factor="+_nf);
//  	      CsdpFunctions.setZoomFactor(_nf);
//  	      _gui._canvas1.setUpdateCanvas(true);
//  	      _gui._canvas1.resetSize();
//  	      System.out.println("setting new zoomfactor="+_nf);
//  	      //removed for conversion to swing
//  	      _gui._canvas1.repaint();
//  	  }//if zoom factor changed
//        }//getValueAndZoom
//    } // Class ZFactor
  
  App _app;
  CsdpFrame _gui;
  BathymetryPlot _plot;
//      /**
//       * Minimum zoom factor
//       */
//    protected static final int MIN_ZOOM = 1;
//      /**
//       * Maximum zoom factor
//       */
//    protected static final int MAX_ZOOM = 10;
//      /**
//       * factor used by ScrollbarDialog
//       */
//    protected static final int FACTOR   = 10;
  protected static final boolean DEBUG = false;
    protected Rectangle _r = new Rectangle(0,0);
}//class ZoomMenu

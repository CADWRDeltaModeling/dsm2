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
 * calls methods for editing and viewing cross-sections 
 *
 * @author
 * @version $Id:
 */
public class XsectMenu{
  public XsectMenu(App app, CsdpFrame gui, NetworkInteractor ni){
    _app = app;
    _gui = gui;
    _ni = ni;
    _okd = new OkDialog(_gui, "You are already viewing that xsect!", true);
    ////_okd = new OkDialog(_gui, "Only one cross-section window may be open", true);
  }

  public void setNetwork(Network net){
    _net = net;
  }

  OkDialog _okd;
  App _app;
  CsdpFrame _gui;
  NetworkInteractor _ni;
  Network _net;
/**
 * View selected cross-section
 *
 * @author
 * @version $Id: 
 */
public class XView implements ActionListener {  

    public void actionPerformed(ActionEvent e){
	if(DEBUG)System.out.println("net="+_net);
	Xsect xsect = _net.getSelectedXsect();
	String centerlineName = _net.getSelectedCenterlineName();
	int xsectNum = _net.getSelectedXsectNum();
	//    _gui.addXsectWindow("Xsect", new XsectGraph());
	if(DEBUG)System.out.println("about to call app.viewxsect: app, xsect="+_app+","+xsect);
	System.out.println(centerlineName+"_"+xsectNum);
	if(_app._xsectGraph.containsKey(centerlineName+"_"+xsectNum)){
	    _okd.show();
	    //((XsectGraph)(_app._xsectGraph.get(centerlineName+"_"+xsectNum))).show();
	} 
	else {
	    _app.viewXsect(xsect, centerlineName, xsectNum, 
			   CsdpFunctions._xsectThickness);
	}//if 
    }//viewXsect
} // class XView

    /**
     * Adjust length of cross-section line
     *
     * @author
     * @version $Id: 
     */
    public class XAdjustLength implements ActionListener {  
	ScrollbarDialog _s;
	float _length;
	float _newLength;
	public XAdjustLength(){
	    int min = 0;
	    int max = 3;
	    int factor = 10;
	    int numScrollbars = 1;
	    float[] initialValue = new float[1];
	    initialValue[0] = 1;
	    
	    _s = new ScrollbarDialog
		(_gui,"Select Factor for adjusting cross-section line length",
		 true, numScrollbars, initialValue,min,max,factor);
	}

	public void actionPerformed(ActionEvent e){
	    Xsect xsect = _net.getSelectedXsect();
	    _length = xsect.getXsectLineLength();
	    _newLength = -CsdpFunctions.BIG_FLOAT;
	    _s.show();
	    float adjustmentFactor = _s.getScrollbarValue(0);
	    _newLength = _length*adjustmentFactor;


	    if(DEBUG)System.out.println
			 ("length, adjustmentFactor, newlength="+_length+","+
			  adjustmentFactor+","+_newLength);

	    if(_length == _newLength){
	    }else{
		xsect.putXsectLineLength(_newLength);
		_gui.getPlanViewCanvas().redoNextPaint();
	//removed for conversion to swing
		_gui.getPlanViewCanvas().repaint();
		_net.setIsUpdated(true);
	    }
	}
	

    }//XAdjustLength
protected static final boolean DEBUG = false;

}//class XsectMenu

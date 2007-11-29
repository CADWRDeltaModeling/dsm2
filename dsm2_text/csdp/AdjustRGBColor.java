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
import java.util.*;
import javax.swing.*;
import javax.swing.colorchooser.*;

/**
 * Adjust color of legend button.  When button is clicked, a dialog with 3
 * scrollbars appears, allowing user to adjust RGB values.
 *
 * @author
 * @version $Id: AdjustRGBColor.java,v 1.2 2002/10/21 19:58:25 btom Exp $
 */

public class AdjustRGBColor implements ActionListener{

  public AdjustRGBColor(CsdpFrame gui, int buttonNumber){
    _gui = gui;
    float[] oldColorValues = new float[3];
    _buttonNumber = buttonNumber;
    //    oldColorValues[0] = CsdpFunctions.getColor(buttonNumber).getRed();
    //oldColorValues[1] = CsdpFunctions.getColor(buttonNumber).getGreen();
    //oldColorValues[2] = CsdpFunctions.getColor(buttonNumber).getBlue();
    oldColorValues[0] = _gui.getColor(buttonNumber).getRed();
    oldColorValues[1] = _gui.getColor(buttonNumber).getGreen();
    oldColorValues[2] = _gui.getColor(buttonNumber).getBlue();
    ////     _s = new ScrollbarDialog(_gui,"Select new values for RGB Color",
    ////		     true,_numScrollBars,oldColorValues,
    ////		     MIN_VAL,MAX_VAL,FACTOR);

     Color c = new Color((int)oldColorValues[0],
			 (int)oldColorValues[1],(int)oldColorValues[2]);

     _jcc = new JColorChooser(new DefaultColorSelectionModel(c));
  }//adjustRGBColor

  /**
   * called when legend button is pressed
   */
  public void actionPerformed(ActionEvent e){
    float[] oldColorValues = new float[3];
    float[] newColorValues = new float[3];

    //    _s.updateColor(_buttonNumber);
    //    _s.show();
    Color newColor = null;

    _jcc.setColor(_gui.getColor(_buttonNumber));
    newColor = _jcc.showDialog(_gui,"Select a new color",_gui.getColor(_buttonNumber));

    if(newColor != null){
	oldColorValues[0] = _gui.getColor(_buttonNumber).getRed();
	oldColorValues[1] = _gui.getColor(_buttonNumber).getGreen();
	oldColorValues[2] = _gui.getColor(_buttonNumber).getBlue();
	
	
	newColorValues[0] = newColor.getRed();
	newColorValues[1] = newColor.getGreen();
	newColorValues[2] = newColor.getBlue();

	////    for(int i=0; i<=_numScrollBars-1; i++){
	////      newColorValues[i] = _s.getScrollbarValue(i);
	////    }
	
	//    CsdpFunctions.setColor(_buttonNumber,null);
	//CsdpFunctions.setColor(_buttonNumber, 
	_gui.setColor(_buttonNumber,null);
	_gui.setColor(_buttonNumber, 
		      new Color((int)newColorValues[0],
				(int)newColorValues[1],
				(int)newColorValues[2]));
	if((oldColorValues[0] != newColorValues[0]) ||
	   (oldColorValues[1] != newColorValues[1]) ||
	   (oldColorValues[2] != newColorValues[2])){
	    _gui.updateColorLegend();
	    //problem:  causes null pointer when more than one graph open....
	    //      _gui._app.updateAllXsectGraphs();
	    _gui._canvas1.setUpdateCanvas(true);
	    //removed for conversion to swing
	    //      _gui._canvas1.repaint();
	    for(int i=0; i<=_numScrollBars-1; i++){
		oldColorValues[0] = newColorValues[0];
	    }
	}//if the new color is different
    }//if newColor isn't null
  }//actionPerformed
  /**
   * instance of CsdpFrame
   */
  protected CsdpFrame _gui;
  /**
   * RGB values of current color.  compare to new values to see if changed
   */
  ////  protected float[] _oldColorValues;
  /**
   * new RGB values.  compare to old values to see if changed
   */
  protected float[] _newColorValues;
  /**
   * number of button that was pressed--also number of color in table to change
   */
  protected int _buttonNumber;
  /**
   * minimum scrollbar value
   */
  protected final int MIN_VAL = 0;
  /**
   * maximum scrollbar value
   */
  protected final int MAX_VAL = 255;
  /**
   * higher value makes finer scrollbar adjustments
   */
  protected final int FACTOR  = 1;
  /**
   * instance of ScrollbarDialog class
   */
    ////  protected ScrollbarDialog _s;
  /**
   * number of scrollbars in the ScrollbarDialog
   */
  protected final int _numScrollBars = 3;


    protected JColorChooser _jcc;

}//class AdjustRGBColor
//test...--file icon isn't red...

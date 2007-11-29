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
/**
 * scroll bar to define a value.  factor is used to determine and number of decimal places.
 *
 * @author
 * @version 
 */
public class ScrollbarDialog extends Dialog implements AdjustmentListener,
						       ActionListener {
  public ScrollbarDialog(Frame parent, String title, boolean modal, int numBars, 
			 float[] initialValue, int min, int max, int factor){
    super(parent, title, modal);
    _factor = factor;
    _frame = (CsdpFrame)parent;
    _numBars = numBars;
    _sbValueLabel = new Label[_numBars];
    _sb = new Scrollbar[_numBars];
    _initialValue = initialValue;
    AdjustmentListener scrollbarListener = this;
    for(int i=0; i<=numBars-1; i++){
	//      _sbValueLabel[i] = new Label((new Float(initialValue[i])).toString());
      _sbValueLabel[i] = new Label(Float.toString(initialValue[i]));
      _sb[i] = new Scrollbar(Scrollbar.HORIZONTAL,(int)(initialValue[i]*_factor),
			     max*_factor/5,min,(max*_factor)+max*_factor/5);
      add(_sbValueLabel[i]);
      add(_sb[i]);
      _sb[i].addAdjustmentListener(scrollbarListener);
    }
    ActionListener okListener = this;
    Button okButton = new Button("ok");
    add(okButton);
    okButton.addActionListener(okListener);

    if(isOdd(_numBars) && _numBars >= 3){
      _previewButton = new Button();
      _previewButton.setBackground(new Color((int)_initialValue[0],
					     (int)_initialValue[1],
					     (int)_initialValue[2]));
      add(_previewButton);
    }

    setSize(_dialogWidth,_dialogHeight+_barHeight*_numBars);
    setLayout(new GridLayout(2+numBars-1,2));
  }//constructor

  public Insets getInsets() {
    return new Insets(30,10,10,10);
  }

  public void adjustmentValueChanged(AdjustmentEvent e){
    float value;
    for(int i=0; i<=_numBars-1; i++){
      value = (float)(_sb[i].getValue())/_factor;
      if(DEBUG)System.out.println("changing adjustment value to "+value);
      //      _sbValueLabel[i].setText((new Float(value)).toString());
      _sbValueLabel[i].setText(Float.toString(value));
    }
    if(_numBars >= 3)
      _previewButton.setBackground(new Color((int)getScrollbarValue(0),
					     (int)getScrollbarValue(1),
					     (int)getScrollbarValue(2)));
  }//adjustmentValueChanged

  public void updateColor(int buttonNumber){

//     int value0 = CsdpFunctions.getColor(buttonNumber).getRed();
//     int value1 = CsdpFunctions.getColor(buttonNumber).getGreen();
//     int value2 = CsdpFunctions.getColor(buttonNumber).getBlue();
    int value0 = _frame.getColor(buttonNumber).getRed();
    int value1 = _frame.getColor(buttonNumber).getGreen();
    int value2 = _frame.getColor(buttonNumber).getBlue();

//      Float value0Object = new Float(value0);
//      Float value1Object = new Float(value1);
//      Float value2Object = new Float(value2);

    String value0String = Float.toString((float)value0);
    String value1String = Float.toString((float)value1);
    String value2String = Float.toString((float)value2);
    float value0Float = Float.parseFloat(value0String);
    float value1Float = Float.parseFloat(value1String);
    float value2Float = Float.parseFloat(value2String);
    
    _initialValue[0] = value0Float;
    _initialValue[1] = value1Float;
    _initialValue[2] = value2Float;
    _sbValueLabel[0].setText(value0String);
    _sbValueLabel[1].setText(value1String);
    _sbValueLabel[2].setText(value2String);

//      _initialValue[0] = value0Object.floatValue();
//      _initialValue[1] = value1Object.floatValue();
//      _initialValue[2] = value2Object.floatValue();

//      _sbValueLabel[0].setText(value0Object.toString());
//      _sbValueLabel[1].setText(value1Object.toString());
//      _sbValueLabel[2].setText(value2Object.toString());
    setScrollbarValue(0,value0);
    setScrollbarValue(1,value1);
    setScrollbarValue(2,value2);

    _previewButton.setBackground(new Color(value0,value1,value2));
  }//updateColor

  public void actionPerformed(ActionEvent e){
    dispose();
  }//actionPerformed

    public void updateScrollbar(int index, int value){
	setScrollbarValue(index, value);
	float floatValue = (float)_sb[index].getValue()/(float)_factor;
	_sbValueLabel[index].setText(Float.toString(floatValue));
    }

  /**
   * returns the value of a scrollbar.  (not the value label)
   */
  public float getScrollbarValue(int index){
    return (float)_sb[index].getValue()/(float)_factor;
  }

  /**
   * sets the value of a scrollbar.  (not the value label)
   */
  public void setScrollbarValue(int index, int value){
    _sb[index].setValue(value*_factor);
  }

  /**
   * returns true if the value is odd
   */
  protected boolean isOdd(int value){
    boolean returnValue;
    int value1 = (int)(((float)value)/2.0f);
    int value2 = value;
    if(value1 == value2) returnValue = false;
    else returnValue = true;
    return returnValue;
  }

  protected static final boolean DEBUG = false;
  //  Frame _frame;
  CsdpFrame _frame;
  Scrollbar[] _sb;
  Label[] _sbValueLabel=null;
  /*
   * Use a higher scrollbar to make finer adjustments
   */
  int _factor;
  int _numBars;
  Button _previewButton;
  int _dialogWidth = 600;
  int _dialogHeight = 150;
  int _barHeight = 50;
  float[] _initialValue;
}//ScrollbarDialog

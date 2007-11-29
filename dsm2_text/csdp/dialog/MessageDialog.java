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
package DWR.CSDP.dialog;
import java.awt.*;
import java.awt.event.*;
import javax.swing.*;

/**
 * a dialog box with a text field to get input from user
 */
public class MessageDialog extends JDialog implements ActionListener{
    private static final boolean DEBUG = false;
    JTextArea ta;
    JFrame _f;
    JButton _okButton = new JButton("OK");
    int _numLines = 0;
    JScrollPane _sp;
    //  ResizableStringArray _resizableMessage;
    String _stringMessage;
    boolean _editable;
    int _maxStringLength;
//    MessageDialog(JFrame parent, String title, ResizableStringArray message, 
//  		int numLines, boolean modal, boolean editable) {
//      super(parent, title, modal);
//      _resizableMessage = message;
//      _numLines = numLines;
//      _f = parent;
//      _editable = editable;
//      configure(_editable);
//    }

    public MessageDialog(JFrame parent, String title, String message, 
			 boolean modal, boolean editable, int maxStringLength, int numLines) {
	super(parent, title, modal);
	_stringMessage = message;
	_f = parent;
	_editable = editable;
	_maxStringLength = maxStringLength;
	_numLines = numLines;
	configure(_editable);
	if(DEBUG)System.out.println("maxStringLength, numLines="+_maxStringLength+","+_numLines);
    }

    public void updateMessage(String updatedMessage){
	ta.setText(updatedMessage);
    }

//      public void updateMessage(ResizableStringArray updatedMessage){
//  	_resizableMessage = updatedMessage;
//  	_numLines = _resizableMessage.getSize();
//  	configure(_editable);
//      }

//      public ResizableStringArray getMessage(){
//  	return _resizableMessage;
//      }

  public void configure(boolean editable){
      //    setLayout(new BorderLayout(10,10));
    getContentPane().setLayout(new BorderLayout(10,10));
    setBackground(Color.white);

    ta = new JTextArea();
    ta.setEditable(editable);
    String line = " ";

    ta.setText(_stringMessage);

//      for(int i=0; i<=_numLines-1; i++){
//        ta.append(_resizableMessage.get(i));
//        ta.append("\n");
//        if(_resizableMessage.get(i) != null){
//  	  maxStringLength = Math.max(maxStringLength, _message.get(i).length());
//        }
//      }
    ta.setColumns(_maxStringLength + 5);
    ta.setRows(_numLines);

    //    add("Center", ta);
    //    add("South", _okButton);
    ////    _sp.setLayout(new ScrollPaneLayout());
    _sp = new JScrollPane(ta,
			  JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED,
			  JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);
    _sp.getViewport().setScrollMode(JViewport.SIMPLE_SCROLL_MODE);
    getContentPane().add("Center", _sp);
    //    getContentPane().add("Center", ta);
    getContentPane().add("South", _okButton);
    ActionListener okListener = this;
    _okButton.addActionListener(okListener);

System.out.println("size set to" +
		   (int)((float)(_maxStringLength)*(300.0f/44.0f) + 50.0f)+","+
		   (int)((float)(_numLines)*(700.0f/44.0f)+100.0f));

 System.out.println("maxStringLength,numLines="+_maxStringLength+","+_numLines);
    setSize((int)((float)(_maxStringLength)*(300.0f/44.0f) + 50.0f),
	    (int)((float)(_numLines)*(700.0f/44.0f)+100.0f));
    requestFocus();
  }//configure

    public String getMessage(){
	return ta.getText();
    }

  public Insets getInsets() {
    return new Insets(30,10,10,10);
  }

  public void actionPerformed(ActionEvent e){
    //    setVisible(false);
    dispose();
  }
}//class MessageDialog


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
 * a dialog box with Yes, No, and Cancel buttons
 */
public class YesNoDialog extends JDialog {
  JFrame _f;
  JButton _yesButton    = new JButton("Yes");
  JButton _noButton     = new JButton("No");
  JButton _cancelButton = new JButton("Cancel");
    private String _message = null;

    public YesNoDialog(JFrame parent, String title, boolean modal) {
	super(parent, title, modal);
	
	_f = parent;
	configure(title.length());
    }//constructor

    public YesNoDialog(JFrame parent, String title, boolean modal, int messageLength){
	super(parent, title, modal);
	_f = parent;
	configure(messageLength);
    }
    
    public YesNoDialog(JFrame parent, String title, boolean modal, String message) {
	super(parent, title, modal);
	
	_f = parent;
	_message = message;
	configure(title.length());
    }//constructor

    public void reset(){
	_yes = false;
	_no = false;
	_cancel = false;
    }

  public void configure(int titleLength){
    getContentPane().setLayout(new BorderLayout(10,10));
    setBackground(Color.white);
    if(_message != null){
	JTextArea messageLabel = new JTextArea(_message);
	Font f = new Font("Arial", Font.BOLD, 16);
	messageLabel.setEditable(false);
	messageLabel.setBackground(Color.lightGray);
	messageLabel.setFont(f);
	getContentPane().add("North", messageLabel);
    }
    JPanel btnPanel = new JPanel();
    btnPanel.setLayout(new FlowLayout());
    btnPanel.add(_yesButton);
    btnPanel.add(_noButton);
    btnPanel.add(_cancelButton);
    getContentPane().add("Center", btnPanel);
    ActionListener yesListener    = new SetYes(this);
    ActionListener noListener     = new SetNo(this);
    ActionListener cancelListener = new SetCancel(this);
    _yesButton.addActionListener(yesListener);
    _noButton.addActionListener(noListener);
    _cancelButton.addActionListener(cancelListener);
    int maxLength = 0;
    if(_message !=null){
	maxLength = Math.max(titleLength, _message.length());
    }else{
	maxLength = titleLength;
    }
    setSize((int)(50.0f+(float)maxLength*CHARACTER_TO_PIXELS),150);
    requestFocus();
  }//configure

    //what is this for???????
//    public Insets getInsets() {
//      return new Insets(30,10,10,10);
//    }//getInsets

  public class SetYes implements ActionListener{
    YesNoDialog _ynd = null;
    public SetYes(YesNoDialog ynd){
      _ynd = ynd;
    }//constructor
    public void actionPerformed(ActionEvent e){
      _ynd._yes = true;
      //setVisible(false);
      dispose();
    }//actionPerformed
  }//class SetYes

  public class SetNo implements ActionListener{
    YesNoDialog _ynd = null;
    public SetNo(YesNoDialog ynd){
      _ynd = ynd;
    }
    public void actionPerformed(ActionEvent e){
      _ynd._no = true;
      //setVisible(false);
      dispose();
    }
  }//class SetNo

  public class SetCancel implements ActionListener{
    YesNoDialog _ynd = null;
    public SetCancel(YesNoDialog ynd){
      _ynd = ynd;
    }
    public void actionPerformed(ActionEvent e){
      _ynd._cancel = true;
      //      setVisible(false);
      dispose();
    }
  }//class SetCancel

  public boolean _yes    = false;
  public boolean _no     = false;
  public boolean _cancel = false;
    public static final float CHARACTER_TO_PIXELS = 300.0f/44.0f;
  
}//class YesNoDialog


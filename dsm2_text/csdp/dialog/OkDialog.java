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

/**
 * a dialog box with a message and an ok button
 */
public class OkDialog extends Dialog {
  Frame _f;
  Button _okButton = new Button("Ok");
  Button _messageButton = null;
    public OkDialog(Frame parent, String title, boolean modal) {
	super(parent, "message", modal);
	_messageButton = new Button(title);
	_messageButton.setBackground(Color.lightGray);
	_f = parent;
	configure(title);
    }//constructor

  /**
   * changes message displayed in top button
   */
  public void setMessage(String m){
    _messageButton.setLabel(m);
  }

  public void configure(String title){
    setLayout(new BorderLayout(10,10));
    setBackground(Color.white);
    Panel btnPanel = new Panel();
    btnPanel.setLayout(new GridLayout(2,1));
    btnPanel.add(_messageButton);
    btnPanel.add(_okButton);
    add("Center", btnPanel);
    ActionListener okListener = new SetOk(this);
    _okButton.addActionListener(okListener);
    //set width of frame to be a little wider than the title string
    setSize((int)((float)(title.length())*(CHARACTER_TO_PIXELS) + 
		  50.0f),150);
    requestFocus();
  }//configure

  public Insets getInsets() {
    return new Insets(30,10,10,10);
  }//getInsets

  public class SetOk implements ActionListener{
    OkDialog _okd = null;
    public SetOk(OkDialog okd){
      _okd = okd;
    }
    public void actionPerformed(ActionEvent e){
      _okd._ok = true;
      //setVisible(false);
      dispose();
    }
  }//class SetCancel

  public boolean _ok = false;
    public static final float CHARACTER_TO_PIXELS = 300.0f/44.0f;
}//class OkDialog


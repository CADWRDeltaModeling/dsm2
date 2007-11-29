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
import java.util.*;
/**
 * dialog with multiple labels and text fields
 *
 * @author
 * @version $Id: TextFieldDialog.java,v 1.2 2002/10/21 20:12:30 btom Exp $
 */
public class TextFieldDialog extends Dialog implements ActionListener {
  
  Frame _frame;
  public TextFieldDialog(Frame parent, String title, boolean modal,
			 String[] names, float[] initValue){
    super(parent, title, modal);
    _frame = parent;
    _names = names;
    _numNames = names.length;
    GridLayout layout = new GridLayout(_numNames+1, 1);
    setLayout(layout);
    Button _okButton = new Button("ok");

    for(int i=0; i<=_numNames-1; i++){
	//      String index = (new Integer(i)).toString();
	String index = Integer.toString(i);
      add(new Label(names[i]));
      _textFields.put(names[i], new TextField
		      ( Float.toString(initValue[i]), FIELD_WIDTH));
      add((TextField)(_textFields.get(names[i])));
      if(DEBUG)System.out.println(names[i]);
    }
    add(_okButton);
    validate();
    doLayout();
    ActionListener okButtonListener = this;
    _okButton.addActionListener(okButtonListener);
    setSize(500,150);
  }//constructor

  public Insets getInsets() {
    return new Insets(30,10,10,10);
  }

  public void actionPerformed(ActionEvent e){
    dispose();
  }

  int _numNames = 0;
  public String[] _names;
  public Hashtable _textFields = new Hashtable();
  protected static final int FIELD_WIDTH = 10;
  protected static final boolean DEBUG = false;
}//TextFieldDialog

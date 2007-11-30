//    Copyright (C) 1996 State of California, Department of Water
//    Resources.
//
//    Delta Simulation Model 2 (DSM2): A River, Estuary, and Land
//    numerical model.  No protection claimed in original FOURPT and
//    Branched Lagrangian Transport Model (BLTM) code written by the
//    United States Geological Survey.  Protection claimed in the
//    routines and files listed in the accompanying file "Protect.txt".
//    If you did not receive a copy of this file contact Dr. Paul
//    Hutton, below.
//
//    This program is licensed to you under the terms of the GNU General
//    Public License, version 2, as published by the Free Software
//    Foundation.
//
//    You should have received a copy of the GNU General Public License
//    along with this program; if not, contact Dr. Paul Hutton, below,
//    or the Free Software Foundation, 675 Mass Ave, Cambridge, MA
//    02139, USA.
//
//    THIS SOFTWARE AND DOCUMENTATION ARE PROVIDED BY THE CALIFORNIA
//    DEPARTMENT OF WATER RESOURCES AND CONTRIBUTORS "AS IS" AND ANY
//    EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
//    IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
//    PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE CALIFORNIA
//    DEPARTMENT OF WATER RESOURCES OR ITS CONTRIBUTORS BE LIABLE FOR
//    ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
//    CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT
//    OR SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA OR PROFITS; OR
//    BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
//    LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
//    (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE
//    USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH
//    DAMAGE.
//
//    For more information about DSM2, contact:
//
//    Dr. Paul Hutton
//    California Dept. of Water Resources
//    Division of Planning, Delta Modeling Section
//    1416 Ninth Street
//    Sacramento, CA  95814
//    916-653-5601
//    hutton@water.ca.gov
//
//    or see our home page: http://wwwdelmod.water.ca.gov/

package DWR.DMS.PTM.behave;
import javax.swing.*;
import java.awt.*;
import java.awt.event.*;

/**
 * This class creates a field that contains 
 * a label, text field and a combo box. <br>
 * 
 * @author Aaron Miller
 * @version $Id: BehaviorField.java,v
 */

public class BehaviorField extends JPanel {

  /**
   * creates a new Field containing a label, text field and combo box.
   *
   * @param name Label name.
   * @param type Unit type: volume, time ...
   *
   */

  public BehaviorField (String name, int type) {
    JPanel panel1, panel2, panel3;
    panel1 = new JPanel(false);
    panel2 = new JPanel(false);
    panel3 = new JPanel(false);
    //    panel = new JPanel(false);
    label = new JLabel(name);
    field = new JTextField(NUM_COL);

    //    panel.setLayout(new GridLayout(1,3));
    setLayout(new GridLayout(1,3));
    //    setLayout(new FlowLayout());

    setComboBox(type);

    panel1.add(label, BorderLayout.EAST);
    panel2.add(field, BorderLayout.CENTER);
    panel3.add(units, BorderLayout.WEST);
    
    //    panel.add(panel1);
    //    panel.add(panel2);
    //    panel.add(panel3);

    add(panel1);
    add(panel2);
    add(panel3);

    //    add(panel);
  }

  /**
    *  gets position of combo box which can be further translated
    */
  public String getComboPosition(){
    return  new String().valueOf(units.getSelectedIndex());
  }

  /**
    *  sets position of combo box
    */
  public void setComboPosition(String value){
    if (value != null && value != ""){
      int tmp = (int) new Integer(value).intValue();
      units.setSelectedIndex(tmp);
    }
  }

  /**
    *  sets the type of combo box: volume, time, mortality etc.
    */
  public void setComboBox(int type){
    String [] tmp = {};
    if (type == unitData.TIME) tmp = unitData.time;
    else if (type == unitData.VEL) tmp = unitData.velocity;
    else if (type == unitData.MORT) tmp = unitData.mortality;
    units = new JComboBox(tmp);
  }

  /**
    *  returns the contents of this text field
    */
  public String getFieldText(){
    return field.getText().trim();
  }
  
  /**
    *  fills the text box and sets the combo box
    */
  public void setFieldText(String value, String units){
    String tmp = value;
    try {
      field.setText(tmp);
    } catch (Exception e) {}
    setComboPosition(units);
  }

  /**
    *  JPanel containing JTextfield, JLabel, and JComboBox
    */
  JPanel panel;

  /**
    *  editable text field
    */
  JTextField field;

  /**
    *  text field label
    */
  JLabel label;

  /**
    *  lists available units
    */
  JComboBox units;

  /**
    *  textfield width
    */
  int NUM_COL = 10;

  /**
    *  contains unit conversion tables
    */
  static Units unitData;
  
}

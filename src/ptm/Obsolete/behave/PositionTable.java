//    Copyright (C) 1996 State of California, Department of Water
//    Resources.
//
//    Delta Simulation Model 2 (DSM2): A River, Estuary, and Land
//    numerical model.  No protection claimed in original FOURPT and
//    Branched Lagrangian Transport Model (BLTM) code written by the
//    United States Geological Survey.  Protection claimed in the
//    routines and files listed in the accompanying file "Protect.txt".
//    If you did not receive a copy of this file contact 
//    Tara Smith, below.
//
//    This program is licensed to you under the terms of the GNU General
//    Public License, version 2, as published by the Free Software
//    Foundation.
//
//    You should have received a copy of the GNU General Public License
//    along with this program; if not, contact Tara Smith, below,
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
//    Tara Smith
//    California Dept. of Water Resources
//    Division of Planning, Delta Modeling Section
//    1416 Ninth Street
//    Sacramento, CA  95814
//    916-653-9885
//    tara@water.ca.gov
//
//    or see our home page: http://baydeltaoffice.water.ca.gov/modeling/deltamodeling/

package DWR.DMS.PTM.behave;
import javax.swing.*;
import javax.swing.table.*;
import javax.swing.border.*;
import java.awt.*;
import java.util.*;
import java.awt.event.*;

/**
 * @author Aaron Miller
 * @version $Id: PositionTable.java,v 1.5 2000/08/07 17:05:08 miller Exp $
 */

public class PositionTable extends JPanel{

  JPanel tablePanel;
  JTable table;
  JScrollPane scroll;
  JLabel label;
  Vector rowData;
  Vector columnData;
  TitledBorder border;
  String columns [] = {"Chan Lower","Chan Upper","Start Time","End Time"};
  String rows [] = {"","","",""};
  private static int NUM_ROWS = 4;
  DefaultTableModel dm;

  public PositionTable(String name) {
    label = new JLabel(name);
    border = new TitledBorder(name);
    columnData = new Vector();
    rowData = new Vector();
    tablePanel = new JPanel();
    tablePanel.setLayout(new BorderLayout());

    fillSimpleVector(columnData, columns);
    fillComplexVector(rowData, rows);
    
    dm = new DefaultTableModel();
    dm.setDataVector(rowData, columnData);

    createTable();
    scroll = new JScrollPane( table );

    tablePanel.add(scroll, BorderLayout.CENTER);
    tablePanel.setBorder(border);
    setLayout(new BorderLayout());
    add(tablePanel, BorderLayout.CENTER);

  }

  public void createTable(){
    table = new JTable( dm ) {
      protected JTableHeader createDefaultTableHeader() {
	return new GroupableHeader(columnModel);
      }
    };
    TableColumnModel cm = table.getColumnModel();
    ColumnGroup g_loc = new ColumnGroup("Channel Location");
    g_loc.add(cm.getColumn(0));
    g_loc.add(cm.getColumn(1));
    ColumnGroup g_time = new ColumnGroup("Time Span");
    g_time.add(cm.getColumn(2));
    g_time.add(cm.getColumn(3));
    GroupableHeader header = (GroupableHeader)table.getTableHeader();
    header.addColumnGroup(g_loc);
    header.addColumnGroup(g_time);
  }

  public Vector getTableContents(){
    return dm.getDataVector();
  }

  public void setTableContents(Vector vector){
    if(vector.size() != 0){
      dm.setDataVector(vector,columnData);
      createTable();
    }
  }

  public void fillSimpleVector (Vector vector, String value[]){
    for (int i = 0; i < value.length; i++) {
      vector.addElement(value[i]);
    }
  }

  public void fillComplexVector (Vector vector, String value[]){
    Vector tmp;
    for (int j = 0; j < NUM_ROWS; j++){
      tmp = new Vector();
      for (int i = 0; i < value.length; i++) {
	tmp.addElement(value[i]);
      }
      vector.addElement(tmp);
    }
  }
}


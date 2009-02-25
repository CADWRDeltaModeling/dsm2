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
import java.util.*;
import java.awt.*;
import javax.swing.*;
import javax.swing.table.*;

/**
 * @author Aaron Miller
 * @version $Id: ColumnGroup.java,v 1.2.6.1 2005/11/21 23:43:07 eli2 Exp $
 */

public class ColumnGroup {
  protected TableCellRenderer renderer;
  protected Vector v;
  protected String text;
  protected int margin=0;

  public ColumnGroup(String text) {
    this(null,text);
  }

  public ColumnGroup(TableCellRenderer renderer,String text) {
    if (renderer == null) {
      this.renderer = new DefaultTableCellRenderer() {
	public Component getTableCellRendererComponent(JTable table, Object value,
                         boolean isSelected, boolean hasFocus, int row, int column) {
	  JTableHeader header = table.getTableHeader();
	  if (header != null) {
	    setForeground(header.getForeground());
	    setBackground(header.getBackground());
	    setFont(header.getFont());
	  }
          setHorizontalAlignment(JLabel.CENTER);
          setText((value == null) ? "" : value.toString());
	  setBorder(UIManager.getBorder("TableHeader.cellBorder"));
	  return this;
        }
      };
    } else {
      this.renderer = renderer;
    }
    this.text = text;
    v = new Vector();
  }

  public void add(Object obj) {
    if (obj == null) { return; }
    v.addElement(obj);
  }


  public Vector getColumnGroups(TableColumn c, Vector g) {
    g.addElement(this);
    if (v.contains(c)) return g;
    Enumeration e = v.elements();
    while (e.hasMoreElements()) {
      Object obj = e.nextElement();
      if (obj instanceof ColumnGroup) {
        Vector groups =
          (Vector)((ColumnGroup)obj).getColumnGroups(c,(Vector)g.clone());
        if (groups != null) return groups;
      }
    }
    return null;
  }

  public TableCellRenderer getHeaderRenderer() {
    return renderer;
  }

  public void setHeaderRenderer(TableCellRenderer renderer) {
    if (renderer != null) {
      this.renderer = renderer;
    }
  }

  public Object getHeaderValue() {
    return text;
  }

  public Dimension getSize(JTable table) {
    Component comp = renderer.getTableCellRendererComponent(
        table, getHeaderValue(), false, false,-1, -1);
    int height = comp.getPreferredSize().height;
    int width  = 0;
    Enumeration e = v.elements();
    while (e.hasMoreElements()) {
      Object obj = e.nextElement();
      if (obj instanceof TableColumn) {
        TableColumn aColumn = (TableColumn)obj;
        width += aColumn.getWidth();
        width += margin;
      } else {
        width += ((ColumnGroup)obj).getSize(table).width;
      }
    }
    return new Dimension(width, height);
  }

  public void setColumnMargin(int margin) {
    this.margin = margin;
    Enumeration e = v.elements();
    while (e.hasMoreElements()) {
      Object obj = e.nextElement();
      if (obj instanceof ColumnGroup) {
        ((ColumnGroup)obj).setColumnMargin(margin);
      }
    }
  }
}


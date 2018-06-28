/*
    Copyright (C) 1996-2000 State of California, Department of 
    Water Resources.

    DSM2-PTM : Delta Simulation Model 2 - Particle Tracking Model module.
        Maintained by: Aaron Miller
    California Dept. of Water Resources
    Division of Planning, Delta Modeling Section
    1416 Ninth Street
    Sacramento, CA 95814
    (916)-653-4603
    knam@water.ca.gov

    Send bug reports to knam@water.ca.gov

    This program is licensed to you under the terms of the GNU General
    Public License, version 2, as published by the Free Software
    Foundation.

    You should have received a copy of the GNU General Public License
    along with this program; if not, contact Tara Smith, below,
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

    For more information about PTM, contact:

    Tara Smith
    California Dept. of Water Resources
    Division of Planning, Delta Modeling Section
    1416 Ninth Street
    Sacramento, CA  95814
    916-653-9885
    tara@water.ca.gov

    or see our home page: http://baydeltaoffice.water.ca.gov/modeling/deltamodeling/

    Send bug reports to knam@water.ca.gov or call (916)-653-7552

*/
package DWR.DMS.PTM.tools.image;

import javax.swing.*;
import java.util.*;
import java.awt.event.*;
import java.awt.*;
import java.io.*;
/**
 * @author Aaron Miller
 * @version $Id: LocationWriter.java,v 1.2 2000/08/07 17:15:20 miller Exp $
 * 
 */

public class LocationWriter {
  int x = 0;
  int y = 0;
  ArrayList stations;
  JFrame dialog;
  //  JDialog dialog;
  JTextField txName, txLocation;
  JButton btnOk,btnCancel;
  ImageDisplayer parent;
  JLabel position;
  public LocationWriter(ImageDisplayer parent){
    this.parent = parent;
    stations = new ArrayList();
    openDialog();
  }
  
  public void setLocation(int x, int y){
    Point loc = parent.getLocationOnScreen();
    this.x = x;
    this.y = y;
    clearFields();
    setLabelValues();
    dialog.setLocation((int)loc.getX()+x-25,(int)loc.getY()+y-25);
    dialog.setVisible(true);
  }

  public void drawLocations(Graphics g){
    int leg = 10;
    StringTokenizer str;
    String tmpStr;
    for(int i = 0; i < stations.size(); i++){
      str = new StringTokenizer((String)stations.get(i),"\t");
      tmpStr = str.nextToken();
      tmpStr = str.nextToken();
      int xpos = new Integer(str.nextToken()).intValue();
      int ypos = new Integer(str.nextToken()).intValue();
      g.setColor(Color.blue);
      g.fillOval(xpos-(leg/2),ypos-(leg/2),leg,leg);
      g.drawString(tmpStr,xpos+leg,ypos);
    }
  }

  private void setLabelValues(){
    position.setText("x = "+x+",   y = "+y);
  }

  private void clearFields(){
    txName.setText("");
    txLocation.setText("");
  }

  private void storeLocation(){
    String value = txLocation.getText()+"\t"+txName.getText()+"\t"+x+"\t"+y+"\t\n";
    stations.add(value);
    clearFields();
    dialog.setVisible(false);
  }

  public void openLocations(String filename){
    BufferedReader reader = null;
    String line;
    StringTokenizer str;
    StringBuffer buff = null;
    try{
      reader = new BufferedReader(new FileReader(filename));
      line = reader.readLine();
      while (line != null){
	str = new StringTokenizer(line);
	int numtokens = str.countTokens();
	if (! line.startsWith("#") && ! line.equals("")){
	  if (numtokens < 4) throw new IOException("Corrupted Line [ "+line+" ]");
	  else if(numtokens > 4){
	    buff = new StringBuffer(str.nextToken()+"\t");
	    for (int i = 0; i < numtokens - 3; i++){
	      buff.append(str.nextToken()+" ");
	    }
	    buff.append("\t"+str.nextToken()+"\t");
	    buff.append(str.nextToken()+"\t\n");
	  }
	  else{
	    buff = new StringBuffer(str.nextToken()+"\t");
	    for (int i = 1; i < numtokens; i++){
	      buff.append(str.nextToken()+"\t");
	    }
	    buff.append("\n");
	  }
	if (buff != null) stations.add(buff.toString());  
	}
	if (line != null) line = reader.readLine();
      }
    }catch(IOException e){System.out.println(e);}
  }

  public void saveLocations(String filename){
    BufferedWriter writer = null;
    try{
      writer = new BufferedWriter(new FileWriter(filename));
      writer.write("# File Auto Generated\n");
      writer.write("# [node] [name] [x pos] [y pos]\n");

      for(int i = 0; i < stations.size(); i++){
	writer.write((String)stations.get(i));
      }
      writer.flush();
      writer.close();
    }catch(IOException e){System.out.println(e);}
  }

  public ArrayList getArrayList(){
    return stations;
  }

  public void openDialog(){
    JLabel header = new JLabel("Please Enter Information for this Location");
    position = new JLabel("x = ?, y = ?");
    JLabel lbName = new JLabel("Location Name");
    JLabel lbLocation = new JLabel("DSM Grid Node");
    JPanel mainPanel = new JPanel();
    dialog = new JFrame("Location Information");
    //    dialog = new JDialog();
    txName = new JTextField(15);
    txLocation = new JTextField(15);

    btnOk = new JButton("OK");
    btnOk.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent evt){
	storeLocation();
	parent.upDate();
      }
    });

    btnCancel = new JButton("CANCEL");
    btnCancel.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent evt){
	clearFields();
	dialog.setVisible(false);
      }
    });
    
    GridBagConstraints gbc = new GridBagConstraints();
    mainPanel.setLayout(new GridBagLayout());
    
    gbc.ipadx = 2;
    gbc.ipady = 2;
    gbc.insets = new Insets(5,5,5,5);
    gbc.weightx = 0;
    gbc.weighty = 0;

    gbc.gridwidth = 2;
    gbc.gridx = 0;
    gbc.gridy = 0;
    gbc.anchor = GridBagConstraints.WEST;
    mainPanel.add(header,gbc);

    gbc.gridx = 0;
    gbc.gridy = 1;
    gbc.anchor = GridBagConstraints.WEST;
    mainPanel.add(position,gbc);

    gbc.gridwidth = 1;
    gbc.anchor = GridBagConstraints.CENTER;

    gbc.gridx = 0;
    gbc.gridy = 2;
    mainPanel.add(lbName,gbc);

    gbc.gridx = 1;
    gbc.gridy = 2;
    mainPanel.add(txName,gbc);

    gbc.gridx = 0;
    gbc.gridy = 3;
    mainPanel.add(lbLocation,gbc);
    
    gbc.gridx = 1;
    gbc.gridy = 3;
    mainPanel.add(txLocation,gbc);

    gbc.gridx = 0;
    gbc.gridy = 4;
    gbc.anchor = GridBagConstraints.EAST;
    mainPanel.add(btnOk,gbc);

    gbc.gridx = 1;
    gbc.gridy = 4;
    gbc.anchor = GridBagConstraints.WEST;
    mainPanel.add(btnCancel,gbc);

    dialog.getContentPane().add(mainPanel);
    dialog.pack();
  }

  public static void main(String [] args){
    LocationWriter lw = new LocationWriter(null);
  }

}

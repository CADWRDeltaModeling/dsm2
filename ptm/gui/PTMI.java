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

package DWR.DMS.PTM.gui;
import DWR.DMS.PTM.*;
import java.awt.*;
import javax.swing.*;
import java.awt.event.*;
import java.io.*;
/**
 * @author Aaron Miller
 * @version $Id: PTMI.java,v 1.2 2000/08/07 17:07:55 miller Exp $
 * 
 */
public class PTMI extends JFrame implements ActionListener {

  static final String VERSION = "1.0";
  PTMI parent;
  FileMenu fileMenu;
  EditMenu editMenu;
  //  IconMenu iconMenuBar;
  ToolBar toolBarMenu;
  Display mainDisplay;
  Thread ptm;
  JPanel contentPane;
  //  boolean modelRunning;
  public PTMI() {
    super("Particle Tracking Model Interface v"+VERSION);
    System.out.println("Particle Tracking Model Interface v"+VERSION);
    parent = this;


    //    setSize(800,600);

    JMenuBar menuBar = new JMenuBar();
    fileMenu = new FileMenu(parent);
    menuBar.add(fileMenu);

    editMenu = new EditMenu(parent);
    menuBar.add(editMenu);

    setJMenuBar(menuBar);

//     iconMenuBar = new IconMenu(parent);
//     getContentPane().add(iconMenuBar, BorderLayout.NORTH);
    contentPane = new JPanel();
    contentPane.setLayout(new BorderLayout());
    contentPane.setPreferredSize(new Dimension(800, 600));
    setSize(800,600);

    toolBarMenu = new ToolBar(parent);
    contentPane.add(toolBarMenu, BorderLayout.NORTH);

    mainDisplay = new Display(parent);
    contentPane.add(mainDisplay, BorderLayout.CENTER);

    addWindowListener(new WindowAdapter() {
      public void windowClosing(WindowEvent e) {
	System.exit(0);}
    });
    setContentPane(contentPane);
    pack();
    setVisible(true);
    //    show();
    //    mainDisplay.addTextPane();
  }

  public void actionPerformed(ActionEvent e){

    String event = e.getActionCommand();

    if (event.equals("File:New"))  System.out.println("new file");
    else if (event.equals("File:Save")) System.out.println("save file");
    else if (event.equals("File:Load")) System.out.println("load file");
    else if (event.equals("File:Quit")) System.exit(0);
    else if (event.equals("Run:PTM")){
      RunPTM runPTM = new RunPTM(mainDisplay);
      ptm = new Thread(runPTM);
      ptm.start();
    }
  }
  
  public static void main (String args[]){
    PTMI frame = new PTMI();
    //    frame.setVisible(true);
    //frame.pack();
  }
  
}

//    Copyright (C) 1996, 2009 State of California, Department of Water
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
import java.awt.*;
import java.awt.event.*;

/**
 * @author Aaron Miller
 * @version $Id: PhaseGUI.java,v 1.5.6.1 2006/04/04 18:16:26 eli2 Exp $
 */

public class PhaseGUI implements FocusListener {

  JDialog frame;
  JPanel mainPanel, tabPanel, buttonPanel;
  PhysicalPanel physical;
  TimePanel time;
  PositionPanel position;
  FlowPanel flow;
  StagePanel stage;
  //  QualityPanel quality;
  JTabbedPane tabbedPane;
  JButton doneBtn, cancelBtn;
  Phase origPhase;//, changedPhase;
  boolean phase_closed = false;

  public PhaseGUI(JFrame parent, Phase phase) {
    origPhase = phase;
    frame = new JDialog(parent,"Behavior Center for "+phase.getName(),true);
    mainPanel = new JPanel(false);
    tabPanel = new JPanel(false);
    buttonPanel = new JPanel(false);
    tabbedPane = new JTabbedPane();

    physical = new PhysicalPanel(origPhase.getPhysical());
    physical.setLayout(new GridLayout(1,1));
    tabbedPane.addTab("Physical",null,physical,"Give Physical Behavior");

    position = new PositionPanel(origPhase.getPosition());
    position.setLayout(new GridLayout(1,1));
    tabbedPane.addTab("Time",null,position,"Give Position Behavior");

    flow = new FlowPanel(origPhase.getFlow());
    flow.setLayout(new GridLayout(1,1));
    tabbedPane.addTab("Flow",null,flow,"Give Flow Behavior");

    stage = new StagePanel(origPhase.getStage());
    //  stage.setLayout(new GridLayout(1,1));
    tabbedPane.addTab("stage",null,stage,"Give Flow Behavior");
    
    tabPanel.setLayout(new GridLayout(1,1));
    tabPanel.add(tabbedPane);

    doneBtn = new JButton("DONE");
    cancelBtn = new JButton("CANCEL");

    doneBtn.addActionListener( new ActionListener(){
      public void actionPerformed(ActionEvent evt){
	System.out.println("DONE");
	physical.setParams();
	position.setParams();
	flow.setParams();
	stage.setParams();
	frame.setVisible(false);
	phase_closed = true;
      }
  });

    cancelBtn.addActionListener( new ActionListener(){
      public void actionPerformed(ActionEvent evt){
	System.out.println("CANCEL");
	frame.setVisible(false);
	phase_closed = true;
      }
  });

    doneBtn.addFocusListener(this);
    cancelBtn.addFocusListener(this);
    
    buttonPanel.setLayout(new FlowLayout());
    buttonPanel.add(doneBtn);
    buttonPanel.add(cancelBtn);

    mainPanel.setLayout(new BorderLayout());
    mainPanel.add(tabPanel, BorderLayout.CENTER);
    mainPanel.add(buttonPanel, BorderLayout.SOUTH);

    frame.addWindowListener(new WindowAdapter() {
      public void windowClosing(WindowEvent e) {
      	  frame.setVisible(false);
		  phase_closed = true;
      	}
    });

    frame.getContentPane().add(mainPanel);

    Point position = parent.getLocation();
    frame.setLocation(position);
    frame.setSize(500,350);
    frame.setVisible(true);
  }

  public boolean phaseClosed(){
    return phase_closed;
  }

  public void focusGained (FocusEvent evt) { 
      System.out.println("Gained");
      //      physical.testNumeric(physical.currentField);
      if (physical.focusable) {
	//	  new FocusEvent(currentField,FocusEvent.FOCUS_GAINED);
	physical.currentField.selectAll();
	physical.currentField.requestFocus();
      }
  } 

  public void focusLost (FocusEvent evt) {
      System.out.println("Lost");
  } 
}

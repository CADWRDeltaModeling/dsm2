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
import DWR.DMS.PTM.gui.*;
import javax.swing.*;
import java.awt.*;
import java.awt.event.*;
import com.sun.xml.tree.XmlDocument;
import com.sun.xml.tree.TreeWalker;
import org.w3c.dom.Element;
import java.io.*;
import java.util.*;

/**
 * @author Aaron Miller
 * @version $Id: mainGUI.java,v 1.7.6.1 2006/04/04 18:16:27 eli2 Exp $
 */

public class mainGUI implements MouseListener, MouseMotionListener{
  

  JFrame frame;
  JPanel mainPanel, listPanel, listBtnPanel, addPanel, buttonPanel, btnAddPanel;
  JButton editBtn, addBtn, removeBtn, doneBtn, cancelBtn, saveBtn, loadBtn;
  JList phaseList;
  Hashtable phases;
  Vector phaseId;
  int _id;
  JTextField addText;
  FileUtils fileHandler;
  PhaseGUI phaseGUI;
  String tmpName;
  int nameIndex, posIndex;
  boolean fileChanged;

  
  public mainGUI() {
    try {
        UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName());
      } 
         catch (Exception e) { 
      }
  	phases = new Hashtable();
    phaseId = new Vector();
    frame = new JFrame("Behavior Center");
    mainPanel = new JPanel(false);
    listPanel = new JPanel(false);
    listBtnPanel = new JPanel(false);
    buttonPanel = new JPanel(false);
    addPanel = new JPanel(false);
    btnAddPanel = new JPanel(false);
    addText = new JTextField(20);
    fileHandler = new FileUtils(frame);
    
    phaseList = new JList(phaseId);
    phaseList.setFixedCellHeight(16);
    phaseList.setFixedCellWidth(60);
    phaseList.setVisibleRowCount(4);
    phaseList.addMouseListener(this);
    phaseList.addMouseMotionListener(this);

    listPanel.setLayout(new GridLayout(1,1));
    listPanel.add(phaseList);

    editBtn = new JButton("EDIT");
    addBtn = new JButton("ADD");
    removeBtn = new JButton("REMOVE");
    doneBtn = new JButton("DONE");
    cancelBtn = new JButton("CANCEL");
    saveBtn = new JButton("SAVE");
    loadBtn = new JButton("LOAD");

    editBtn.addActionListener( new ActionListener(){
      public void actionPerformed(ActionEvent evt){
	//	System.out.println("EDIT");
	editPhase();
      }
    });

    addBtn.addActionListener( new ActionListener(){
      public void actionPerformed(ActionEvent evt){
	//	System.out.println("ADD");	
	addPhase();
      }
    });

    removeBtn.addActionListener( new ActionListener(){
      public void actionPerformed(ActionEvent evt){
	//	System.out.println("REMOVE");
	removePhase();
      }
    });

    doneBtn.addActionListener( new ActionListener(){
      public void actionPerformed(ActionEvent evt){
	closeDown();
      }
    });

    cancelBtn.addActionListener( new ActionListener(){
      public void actionPerformed(ActionEvent evt){
	//	System.out.println("CANCEL");
	System.exit(0);
      }
    });

    saveBtn.addActionListener( new ActionListener(){
      public void actionPerformed(ActionEvent evt){
	//	System.out.println("SAVE");
	boolean saved = initSave();
      }
    });

    loadBtn.addActionListener( new ActionListener(){
      public void actionPerformed(ActionEvent evt){
	//	System.out.println("LOAD");
	String filename = fileHandler.getFileInfo("Open",JFileChooser.OPEN_DIALOG,"bhv");
	if (filename != null) {
	  try {
	    load(filename);
	  } catch (IOException e) {System.out.println(e); }
	}
      }
    });

    addText.addActionListener( new ActionListener(){
      public void actionPerformed(ActionEvent evt){
	if ( evt.getModifiers() == 0 ){
	  //	  System.out.println("return");
	  addPhase();
	}
      }
    });

    listBtnPanel.setLayout(new FlowLayout());
    listBtnPanel.add(editBtn);
    listBtnPanel.add(removeBtn);
    
    buttonPanel.setLayout(new FlowLayout());
    buttonPanel.add(saveBtn);
    buttonPanel.add(loadBtn);
    buttonPanel.add(doneBtn);
    buttonPanel.add(cancelBtn);

    addPanel.setLayout(new FlowLayout());
    addPanel.add(addText);
    addPanel.add(addBtn);

    btnAddPanel.setLayout(new BorderLayout());
    btnAddPanel.add(addPanel, BorderLayout.NORTH);
    btnAddPanel.add(buttonPanel, BorderLayout.SOUTH);

    mainPanel.setLayout(new BorderLayout());
    mainPanel.add(listPanel, BorderLayout.CENTER);
    mainPanel.add(listBtnPanel, BorderLayout.EAST);
    mainPanel.add(btnAddPanel, BorderLayout.SOUTH);

    frame.addWindowListener(new WindowAdapter() {
      public void windowClosing(WindowEvent e) {closeDown();}
    });

    frame.getContentPane().add(mainPanel);
    frame.setSize(400,300);
    frame.setVisible(true);

  }

  public void removePhase() {
    int index = phaseList.getSelectedIndex();
    phases.remove(phaseId.elementAt(index));

    System.out.println(phaseId.size());
    phaseId.removeElementAt(index);
    System.out.println(phaseId.size());

    phaseList.setListData(phaseId);
    phaseList.setSelectedIndex(index);
    
  }


  public void addPhase() {
    String name = null;
    try {
      name = new String(addText.getText()).trim();
    } catch (Exception e) { }
    if (! doesNameExist(name)) {
      if (name != null && name.length() != 0) {
	addText.setText(null);
	Phase newPhase = new Phase(name);
	phaseId.addElement(name);
	phases.put(phaseId.lastElement(),newPhase);
	phaseList.setListData(phaseId);
	phaseList.setSelectedIndex(phaseId.indexOf(name));
	editPhase();
      }
    }
    else { 
      addText.selectAll();
    }
  }

  public void editPhase() {
    String name;
    name = (String) phaseList.getSelectedValue();
    Phase newPhase;
    newPhase = (Phase) phases.get(name);
    //    new PhaseGUI(newPhase);
    if (phaseGUI == null || phaseGUI.phaseClosed()) 
      phaseGUI = new PhaseGUI(frame, newPhase);
    fileChanged = true;
  }

  public void setId(int id){
    _id = id;
  }

  public String getId(){
    return new String().valueOf(_id);
  }

  public int getNumberPhases(){
    return phases.size();
  }

  public boolean doesNameExist(String name) {
    boolean alert = false;
    for ( int i = 0; i < phaseId.size(); i++ ) {
      if (name.equals(phaseId.elementAt(i))) {
	alert = true;
	  JOptionPane.showMessageDialog(null,"This Name Already Exists!",
					"ERROR", JOptionPane.ERROR_MESSAGE);	
      }
    }
    return alert;
  }

  public boolean initSave(){
    boolean success = false;
    String filename = fileHandler.getFileInfo("Save",JFileChooser.SAVE_DIALOG,"bhv");
    if (filename != null) {
      try {
	save(filename);
	fileChanged = false;
	success = true;
      } catch (IOException e) { }
    }
    return success;
  }

  public void save(String filename) throws IOException {
    XmlDocument doc = new XmlDocument();
    this.toXml(doc);
    Writer writer = new FileWriter(filename);
    doc.write(writer); 
    writer.flush();
    writer.close();
    System.out.println("Saved");
  }

  public void load(String filename) throws IOException{
    XmlDocument doc = null;
    try {
      doc = XmlDocument.createXmlDocument(new FileInputStream(filename),false);
    }catch(Exception e){ System.out.println(e); }
    this.fromXml(doc.getDocumentElement());
    phaseList.setListData(phaseId);    
  }

  public void fromXml(Element element){
    TreeWalker walker = new TreeWalker(element);
    setId (new Integer (element.getAttribute("id")).intValue());
    int numBehaviors = new Integer (element.getAttribute("num_behaviors")).intValue();
    walker.reset();
    Phase newPhase;
    String phaseName;
    for ( int i=0; i<numBehaviors; i++){
      Element phaseElement = walker.getNextElement("PHASE");
      phaseName = phaseElement.getAttribute("name");
      if ( phaseElement == null ) //break;
	System.out.println("Error in fromXml");
      
      newPhase = new Phase(phaseName);
      // create vector name lookup 
      phaseId.insertElementAt(phaseName,i);
      // fill new Phase will values
      newPhase.fromXml(phaseElement);
      // store in a Hashtable
      phases.put(phaseId.elementAt(i),newPhase);
    }
  }

  public void toXml(XmlDocument doc) {
    Element element = doc.createElement("PARTICLE");
    element.setAttribute("id",getId());
    element.setAttribute("num_behaviors",new String().valueOf(getNumberPhases()));
    Phase retPhase;
    System.out.println("Numer = "+getNumberPhases());
    for( int i = 0; i<getNumberPhases(); i++) {
      retPhase = (Phase) phases.get(phaseId.elementAt(i));
      retPhase.toXml(doc, element);
    }
    doc.appendChild(element);
  }

  public void mouseClicked (MouseEvent evt){ 
    if (evt.getClickCount() > 1) editPhase(); 
  }
  public void mouseEntered (MouseEvent evt){}
  public void mouseExited  (MouseEvent evt){}
  public void mousePressed (MouseEvent evt){
    nameIndex = phaseList.getSelectedIndex();
    posIndex = nameIndex;
    if (nameIndex >= 0)
      tmpName = (String)phaseId.elementAt(nameIndex);
  }
  public void mouseReleased(MouseEvent evt){}
  public void mouseDragged (MouseEvent evt){
    int index = phaseList.getSelectedIndex();
    if (index != posIndex){
      phaseId.removeElementAt(nameIndex);
      phaseId.insertElementAt(tmpName,phaseList.getSelectedIndex());
      nameIndex = phaseList.getSelectedIndex();
      posIndex = nameIndex;
      fileChanged = true;
      //      posIndex = index;
    }
  }
  public void mouseMoved   (MouseEvent evt){}


  public void closeDown(){
    if (fileChanged){
      int retval = fileHandler.warnFileNotSaved();
      if (retval == fileHandler.YES_OPTION){
	if (initSave()) System.exit(0);
      }
      else if (retval == fileHandler.NO_OPTION){
	System.exit(0);
      }
    }
    else
      System.exit(0);
  }

  public static void main(String args []) {
    new mainGUI();
  }

}

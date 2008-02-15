package DWR.DMS.PTM.gui;

import javax.swing.*;

public class FileMenu extends JMenu{
  
  PTMI parent;

  JMenuItem newItem,loadItem,saveItem,quitItem;

  public FileMenu(PTMI main){
    super("File");
    parent = main;

    // File Menu Creation
    newItem = new JMenuItem("New",'N');
    newItem.addActionListener(parent);
    newItem.setActionCommand("File:New");
    add(newItem);
    loadItem = new JMenuItem("Load",'L');
    loadItem.addActionListener(parent);
    loadItem.setActionCommand("File:Load");
    add(loadItem);
    saveItem = new JMenuItem("Save",'S');
    saveItem.addActionListener(parent);
    saveItem.setActionCommand("File:Save");
    add(saveItem);
    addSeparator();
    quitItem = new JMenuItem("Quit",'Q');
    quitItem.addActionListener(parent);
    quitItem.setActionCommand("File:Quit");
    add(quitItem);
    setMnemonic('F');
    initialize();
  }

  public void initialize(){
    newItem.setEnabled(true);
    loadItem.setEnabled(true);
    saveItem.setEnabled(false);
    quitItem.setEnabled(true);
  }
}

package DWR.DMS.PTM.gui;

import javax.swing.*;

public class EditMenu extends JMenu{
  
  PTMI parent;

  JMenuItem copyItem,cutItem,pasteItem;

  public EditMenu(PTMI main){
    super("Edit");
    parent = main;

    // File Menu Creation
    cutItem = new JMenuItem("Cut");
    cutItem.addActionListener(parent);
    cutItem.setActionCommand("Edit:Cut");
    add(cutItem);
    copyItem = new JMenuItem("Copy");
    copyItem.addActionListener(parent);
    copyItem.setActionCommand("Edit:Copy");
    add(copyItem);
    pasteItem = new JMenuItem("Paste");
    pasteItem.addActionListener(parent);
    pasteItem.setActionCommand("Edit:Paste");
    add(pasteItem);
    setMnemonic('E');
    initialize();
  }

  public void initialize(){
    cutItem.setEnabled(false);
    copyItem.setEnabled(false);
    pasteItem.setEnabled(false);
  }
}

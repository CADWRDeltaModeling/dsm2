
package DWR.DMS.PTM.gui;
import javax.swing.*;
import java.awt.*;


public class ToolBar extends JToolBar {

  PTMI parent;
  JButton saveFile, newFile, openFile;
  JButton copySelection, runPTM;
  ImageIcon saveIcon, newIcon, openIcon;
  ImageIcon copyIcon, runIcon;
  //  BevelBorder border;
  //  EtchedBorder etcBorder;

  static Dimension iconSize = new Dimension(35,35);

  public ToolBar (PTMI main) {
    parent = main;

    newIcon = new ImageIcon("images/file-new_u.gif","New File");
    newFile = new JButton(newIcon);
    initButton(newFile, "File:New", true, "New File");

    openIcon = new ImageIcon("images/file-open_u.gif","Open File");
    openFile = new JButton(openIcon);
    initButton(openFile, "File:Open", true, "Open File");

    saveIcon = new ImageIcon("images/file-save_u.gif","Save File");
    saveFile = new JButton(saveIcon);
    initButton(saveFile, "File:Save", false, "Save File");

    copyIcon = new ImageIcon("images/edit-copy_u.gif","Copy");
    copySelection = new JButton(copyIcon);
    initButton(copySelection, "Edit:Copy", false, "Copy");

    runIcon = new ImageIcon("images/tb-run_u.gif","Run");
    runPTM = new JButton(runIcon);
    initButton(runPTM, "Run:PTM", true, "Run PTM");

  }

  private void initButton(JButton but, String command, boolean enabled, String tip) {
    but.addActionListener(parent);
    but.setActionCommand(command);
    //    but.setBorder(border);
    but.setEnabled(enabled);
    but.setPreferredSize(iconSize);
    but.setToolTipText(tip);
    this.add(but);
  }
}

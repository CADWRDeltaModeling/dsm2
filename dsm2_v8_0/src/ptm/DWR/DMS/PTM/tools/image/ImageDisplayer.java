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
import java.awt.*;
import java.awt.print.*;
import java.awt.image.*;
import java.awt.event.*;
import javax.swing.*;
import java.io.*;
import com.sun.image.codec.jpeg.*;
/**
 * @author Aaron Miller
 * @version $Id: ImageDisplayer.java,v 1.3.6.1 2006/04/04 18:16:29 eli2 Exp $
 * 
 */

public class ImageDisplayer implements MouseListener, Printable{
  JMenuBar menuBar;
  JMenu menu;
  JMenuItem menuItem;
  JCheckBoxMenuItem checkBoxMenuItem;
  boolean printXY = false;
  boolean fileAltered = false;
  ImagePanel imagePanel;
  MediaTracker tracker;
  JScrollPane scroll;
  JPanel mainPanel;
  JFrame frame;
  LocationWriter locWriter = null;
  ImageDisplayer display = this;
  public ImageDisplayer (String imageFile) {
    this(imageFile, "");
  }

  public ImageDisplayer (String imageFile, String inputFile) {
    frame = new JFrame("ImageDisplayer");
    mainPanel = new JPanel();
    BufferedReader input = null;
    if(inputFile != null && inputFile != ""){
      try{
	input = new BufferedReader(new FileReader(inputFile));
      }catch(Exception ex){System.out.println(ex);}
    }

    Image image = Toolkit.getDefaultToolkit().getImage(imageFile);

    imagePanel = new ImagePanel(image, input);
    imagePanel.addMouseListener(this);
    scroll = new JScrollPane(imagePanel);
    scroll.setPreferredSize(new Dimension(580,740));


    menuBar = new JMenuBar();
    frame.setJMenuBar(menuBar);
    menu = new JMenu("File");
    menuBar.add(menu);

    menuItem = new JMenuItem("Save Loc File");
    menuItem.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent evt) {
	saveLocationFile();
      }
    });
    menu.add(menuItem);

    menuItem = new JMenuItem("Load Loc File");
    menuItem.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent evt) {
	String tmpStr = "./station.loc";
	try{
	  JFileChooser chooser = new JFileChooser(new File(tmpStr));
	  int returnVal = chooser.showOpenDialog(frame);
	  if (returnVal == JFileChooser.APPROVE_OPTION) {
	    tmpStr = chooser.getSelectedFile().getName();
	    if (locWriter == null) {
	      locWriter = new LocationWriter(display);
	      imagePanel.setLocationWriter(locWriter);
	    }
	    locWriter.openLocations(tmpStr);
	  }
	}catch(Exception e){System.out.println(e);}
	upDate();
      }
    });
    menu.add(menuItem);
    
    menuItem = new JMenuItem("Print");
    menuItem.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent evt) {
	System.out.println("print");
	try{
	  print();
	} catch (Exception e){System.out.println(e);}
      }
    });
    menu.add(menuItem);

    menuItem = new JMenuItem("Export");
    menuItem.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent evt) {
	String tmpStr = "./Image.jpg";
	try{
	  JFileChooser chooser = new JFileChooser(new File(tmpStr));
	  int returnVal = chooser.showSaveDialog(frame);
	  if (returnVal == JFileChooser.APPROVE_OPTION) {
	    tmpStr = chooser.getSelectedFile().getName();
	    if (tmpStr.lastIndexOf(".") > 0 && 
		tmpStr.substring(tmpStr.lastIndexOf(".")).toLowerCase().equals(".jpg") ||
		tmpStr.substring(tmpStr.lastIndexOf(".")).toLowerCase().equals(".jpeg")) {
	      exportImage(tmpStr);
	    }
	    else if (tmpStr.lastIndexOf(".") > 0) {
	      exportImage((tmpStr.substring(0,tmpStr.lastIndexOf(".")))+".jpg");
	    }
	    else {
	      exportImage(tmpStr+".jpg");
	    }
	  }
	}catch(Exception e){
	  System.out.println(e);
	}
      }
    });

    menu.add(menuItem);
    menuItem = new JMenuItem("Quit");
    menuItem.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent evt) {closeDown();}
    });
    menu.add(menuItem);

    menu = new JMenu("Action");
    menuBar.add(menu);
    checkBoxMenuItem = new JCheckBoxMenuItem("Show Coordinates");
    checkBoxMenuItem.addItemListener(new ItemListener() {
      public void itemStateChanged(ItemEvent evt) {
	if (checkBoxMenuItem.getState()){
	  printXY = true;
	}
	else{
	  printXY = false;
	}
      }
    });
    menu.add(checkBoxMenuItem);

    menuItem = new JMenuItem("Resize");
    menuItem.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent evt) {
	scroll.setPreferredSize(new Dimension(imagePanel.getWidth(), imagePanel.getHeight()));
	frame.pack();}
    });
    menu.add(menuItem);

    frame.addWindowListener(new WindowAdapter() {
      public void windowClosing(WindowEvent e) {closeDown();}
    });

    mainPanel.setLayout(new BorderLayout());
    mainPanel.add(scroll, BorderLayout.CENTER);
    frame.getContentPane().add(mainPanel);
    frame.pack();
    frame.setVisible(true);
  }

  public void closeDown(){
    if(fileAltered){
      int option = JOptionPane.showConfirmDialog(null, "Do You Want To Save Location File?",
						 "Save Changes", JOptionPane.YES_NO_OPTION);
      if (option == JOptionPane.YES_OPTION){
	saveLocationFile();
      }
    }
    System.exit(0);
  }

  public void exportImage(String imageFile) throws IOException {
    frame.pack();
    FileOutputStream out = new FileOutputStream(imageFile);
    Graphics2D gra;
    System.out.println("export");
    Image newImage;
    newImage = imagePanel.createImage(imagePanel.getSize().width,imagePanel.getSize().height);
    gra = (Graphics2D) newImage.getGraphics();
    imagePanel.paintAll(gra);
    JPEGImageEncoder encoder = JPEGCodec.createJPEGEncoder(out);
    encoder.encode((BufferedImage)newImage);  
    out.flush();
    out.close();
  }

  public void hideApp(){
    frame.setVisible(false);
  }

  public void upDate(){
    imagePanel.repaint();
  }
  
  public Point getLocationOnScreen(){
    return frame.getLocationOnScreen();
  }

  public void print() throws PrinterException {
    PrinterJob job = PrinterJob.getPrinterJob();
    PageFormat format = job.pageDialog(job.defaultPage());
    job.setPrintable(this,format);
    if (job.printDialog()) job.print();
  }

  public int print(Graphics g, PageFormat format, int pagenum) {
    if (pagenum > 0) return Printable.NO_SUCH_PAGE;
    Graphics2D g2 = (Graphics2D) g;
    g2.translate(format.getImageableX(), format.getImageableY());
    imagePanel.paint(g2);
    return Printable.PAGE_EXISTS;
  }

  public void saveLocationFile(){
    if(locWriter == null){System.out.println("no file to save");}
    else{
      String tmpStr = "./station.loc";
      try{
	JFileChooser chooser = new JFileChooser(new File(tmpStr));
	int returnVal = chooser.showSaveDialog(frame);
	if (returnVal == JFileChooser.APPROVE_OPTION) {
	  tmpStr = chooser.getSelectedFile().getName();
	  locWriter.saveLocations(tmpStr);
	  fileAltered = false;
	}
      }catch(Exception e){System.out.println(e);}
    }
  }

  public void mouseClicked(MouseEvent evt){
    if (printXY){
      int x = evt.getX();
      int y = evt.getY();
      System.out.println("x = "+x+" y = "+y);
      if (locWriter == null) {
	locWriter = new LocationWriter(display);
	imagePanel.setLocationWriter(locWriter);
      }
      fileAltered = true;
      locWriter.setLocation(x, y);
    }
  }
  public void mouseEntered(MouseEvent evt){}
  public void mouseExited(MouseEvent evt){}
  public void mousePressed(MouseEvent evt){}
  public void mouseReleased(MouseEvent evt){}

  public static void main(String[] args) {
    if (args.length == 2) {
      new ImageDisplayer(args [0], args [1]);
    }
    else if (args.length == 1) {
      new ImageDisplayer(args [0]);
    }
    else {
      System.out.println("USEAGE: java ImageDisplayer [ImageFile] [PlotInputFile (optional)]");
    }
  }
}

class ImagePanel extends JPanel {
  Image image;
  BufferedReader input;
  GraphicComponents comps;
  MediaTracker tracker;
  LocationWriter locWriter = null;

  public ImagePanel(Image image, BufferedReader input) {
    this.image = image;
    this.input = input;

    tracker = new MediaTracker(this);
    tracker.addImage(image, 0);
    try { 
      tracker.waitForID(0);
    } catch (InterruptedException e) {
      return;
    }
    if (input != null) comps = new GraphicComponents(input);
    setPreferredSize(new Dimension(image.getWidth(this),image.getHeight(this)));
  }

  public void paintComponent(Graphics g) {
    super.paintComponent(g); //paint background

    //Draw image at its natural size first.
    g.drawImage(image, 0, 0, this); //85x62 image
    if (input != null) comps.drawComponents(g);
    if (locWriter != null) locWriter.drawLocations(g);
  }

  public void setLocationWriter(LocationWriter locWriter){
    this.locWriter = locWriter;
  }
    
}

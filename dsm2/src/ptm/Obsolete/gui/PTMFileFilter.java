package DWR.DMS.PTM.gui;
import java.io.File;
import javax.swing.*;
import javax.swing.filechooser.*;

public class PTMFileFilter extends FileFilter {
  String fileExtension;

  public PTMFileFilter(String fileExtension) {
    this.fileExtension = fileExtension;
  }
    
  // Accept all directories and all gif, jpg, or tiff files.
  public boolean accept(File f) {
    if (f.isDirectory()) {
      return true;
    }

    //    String extension = Utils.getExtension(f);
    String extension = FileUtils.getExtension(f);
    if (extension != null) {
      //      if (extension.equals(Utils.xml)){		
      if (extension.equals(fileExtension)){		
	return true;
      } 
      else {
	return false;
      }
    }
	
    return false;
  }
  
  // The description of this filter
  public String getDescription() {
    return fileExtension;
    //    return "XML";
  }
}

import com.sun.install.*;
import com.sun.install.products.*;
import com.sun.wizards.core.*;
import com.sun.wizards.registry.*;
import com.sun.wizards.panels.*;
import com.sun.wizards.tasks.*;
import java.util.*;
import java.io.*;

/**  
  * Builder for ptm installer
  */
public class PTMBuilder extends InstallArchiveWriter{
  /**
    * Creates a blank PTMBuilder
    */
  public PTMBuilder(){
  }
  /**
    *  Creates the client panel tree. The actual tree building takes
    * place in the superclass (InstallArchiveWriter).  This method
    * simply calls into other methods that gather user input in order
    * to customize the installer.  All custom builders will want to
    * override this method to create their own customized installer.
    */
  protected void createClientTree(){
    super.createClientTree();
    setProductName("PTM"); // product name
    setDefaultDirectory("[userDir]/ptm"); // default directory
    String sdkBaseDirectory = "../"; // base directory
    Hashtable forcedClassPermissions = new Hashtable();
    // force permissions exectuable for these scripts on unix
    forcedClassPermissions.put("bin" + File.separator + 
			       "ptm",
			       "rx");
    // required components
    //    addComponent(new Msg("Readme"),  sdkBaseDirectory, "0Readme.txt", true, true, null);
    addComponent(new Msg("FAQ"),  sdkBaseDirectory, "FAQ.txt", true, true, null);
    addComponent(new Msg("Copyright"), sdkBaseDirectory, "COPYRIGHT", true, true, null);
    addComponent(new Msg("Core Classes"), sdkBaseDirectory, "lib", true, true, forcedClassPermissions);
    addComponent(new Msg("Core Executables"), sdkBaseDirectory, "bin", true, true, forcedClassPermissions);
    // optional components
    addComponent(new Msg("Sample Setup"), sdkBaseDirectory, "sample", true, false, forcedClassPermissions);
    //    addComponent(new Msg("API Documentation"), sdkBaseDirectory, "docs", true, false, forcedClassPermissions);
    //    addComponent(new Msg("Scripts"), sdkBaseDirectory, "scripts", true, false, forcedClassPermissions);
    //    addComponent(new Msg("Change Log"),  sdkBaseDirectory, "Changes.txt", true, true, null);
    // desktop icons
    DesktopUnit desktop = new DesktopUnit();
    desktop.createDesktopItem(getProductName(), "PTM", "{InstallLocation}/bin/ptm.bat", null);
    addComponent(desktop);
      
    /*
     * Set the InstallShield image into the wizard
     */
    setImage("com.sun.install.install");

    /*
     * Could set a baseline image with this line, if desired
     * setBaselineImage("com.sun.wizards.baseline");
     */
      
    //      setBaselineImage(dwrlogo);
    /*
     * Set the resulting file name for the archive
     */
    String className = System.getProperty("install.archiveName");
    if (className == null)
      {
	className = "ptm";
      }
    setArchiveName(className);

    /*   
     * Set *localized* "about..." text.  Note that creating a Msg
     * using the these arguments will create a new Msg.  This Msg
     * will, at runtime, look up the specified resource bundle (in
     * this case com.sun.install.InstallResources] bundle.  If it
     * finds it, it will look for the corresponding text indexed at
     * "AboutText".  This is what the user actually sees.  The user
     * never sees the words "AboutText" unless a translation error
     * occursas.  If you want to provide plain, untranslated text,
     * use the Msg(String) constructor.  For example,
     * 
     * setAboutMsg(new Msg("This is my untranslated About text")); 
     */
    setAboutMsg(new Msg("PTM: Particle Tracking Model"));
      
    /*
     * Set 'Cancel' Text in the same manner.  
     */
    setCancelMsg(new Msg("com.sun.install.Install", InstallResources.MSG_CANCEL_ARE_YOU_SURE));
      
    /*
     * Set exit dialog text.
     */
    setExitMsg(new Msg("com.sun.install.Install", InstallResources.MSG_EXIT_ARE_YOU_SURE));
  }
  /**
    *
    */
  public void createWizardTree(){
    super.createWizardTree();
    Sequence install_seq = wizardState.getSequence(InstallConstants.installSequence);
    install_seq.addTask(new PlatformTask(new Platform(Platform.SOLARIS),new ScriptFileConfigTask()));
    install_seq.addTask(new PlatformTask(new Platform(Platform.WINDOWS),new BatchFileConfigTask()));
  }
  
  /** 
    * First method when instantiating this class.  This instantiates
    * one of these classes and runs the builder.
    *
    * @param args A string array representing the arguments to this class 
    */
  public static void main(String[] args)
    {
      PTMBuilder sampleBuilder = new PTMBuilder();
      sampleBuilder.writeArchive();
      System.exit(0);
    }
}

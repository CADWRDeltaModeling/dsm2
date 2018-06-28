package DWR.DMS.PTM.gui;
import DWR.DMS.PTM.*;
import java.awt.event.*;
import java.io.*;

public class RunPTM implements Runnable {
  static String GUIHOME = "/home/valleyoak/miller/models/dsm2/ptm";
  //static String runPTM = "ptm.bat";
  static String runPTM = "runptm.ksh";
  boolean modelRunning;
  boolean ready;
  //static String runPTM = "jre -classpath "+GUIHOME+"/gui:/site/lib/java/classes.zip:"+
  //GUIHOME+"/lib/xml.jar:"+GUIHOME+"/lib/ptm.jar:"+GUIHOME+"/lib/COM.jar:"+GUIHOME+"/lib/edu.jar:"+
  //GUIHOME+"/lib/swingall.jar:"+GUIHOME+"/classes DWR.DMS.PTM.mainPTM";

  Process runProcess;
  Display outputDisplay;

  public RunPTM (Display output){
    outputDisplay = output;
  }

  public void run(){
    String line = null;
    try {
      //      runProcess = Runtime.getRuntime().exec(runPTM);
      //      modelRunning = true;
      PTMRuntime ptmrt = new PTMRuntime();
      Thread rt = new Thread(ptmrt);
      rt.start();

      while(!ready){
	// keep trying until ready
	if(modelRunning){
	  ready = true;
	  WatchExit watch = new WatchExit();
	  Thread we = new Thread(watch);
	  we.start();

	  BufferedReader read = new BufferedReader(new InputStreamReader(runProcess.getInputStream()));
	  while(modelRunning || line != null){
	    rt.yield();
	    line = read.readLine();
	    if (line != null) outputDisplay.PrintOutput(line+"\n");
	  }
	}
      }
    } catch (Exception e){System.out.println(e);}
  }

  // watch for and grab exit status
   class WatchExit implements Runnable {
    public void run() {
      try {
	int value = runProcess.waitFor();
	System.out.println("System Exit Value "+value);
      } catch (Exception e) {System.out.println(e);}
      System.out.println("Done Executing Ptm");
      modelRunning = false;
    }
  }

  class PTMRuntime implements Runnable {
    public void run() {
      try {
	runProcess = Runtime.getRuntime().exec(runPTM);
	modelRunning = true;
      } catch (Exception e) {System.out.println(e);}
    }
  }

} 

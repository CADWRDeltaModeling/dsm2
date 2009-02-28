package DWR.DMS.PTM;
/**
 * Encapsulates input/output information for the particle tracking
 * model
 *
 * @author Nicky Sandhu
 * @version $Id: IOFileFixedData.java,v 1.1.1.1 1999/09/28 23:43:13 miller Exp $
 */
public class IOFileFixedData {
  /**
   * 
   */
public IOFileFixedData(String animationFileName, int animationOutputInterval,
		       String traceFileName, int traceOutputInterval,
		       String restartOutputFileName, int restartOutputInterval,
		       String restartInputFileName){
  
  this.animationFileName=animationFileName;
  this.animationOutputInterval=animationOutputInterval;
  this.traceFileName=traceFileName;
  this.traceOutputInterval=traceOutputInterval;
  this.restartOutputFileName=restartOutputFileName;
  this.restartOutputInterval=restartOutputInterval;
  this.restartInputFileName=restartInputFileName;
}
  /**
   *
   */
public String toString(){
  String rep = "IO File Fixed Data" + "\n";
  rep += "Animation File Name: " + animationFileName + "\n";
  rep += "Trace File Name: " + traceFileName + "\n";
  rep += "RestartOutput File Name: " + restartOutputFileName + "\n";
  rep += "RestartInput File Name: " + restartInputFileName + "\n";
  return rep;
}
  /**
   *
   */
public String animationFileName;
  /**
   *
   */
public int animationOutputInterval;
  /**
   *
   */
public String traceFileName;
  /**
   *
   */
public int traceOutputInterval;
  /**
   *
   */
public String restartOutputFileName;
  /**
   *
   */
public int restartOutputInterval;
  /**
   *
   */
public String restartInputFileName;
}

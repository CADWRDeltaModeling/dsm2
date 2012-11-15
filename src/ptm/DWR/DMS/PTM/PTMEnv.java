/*<license>
C!    Copyright (C) 1996, 1997, 1998, 2001, 2007, 2009 State of California,
C!    Department of Water Resources.
C!    This file is part of DSM2.

C!    DSM2 is free software: you can redistribute it and/or modify
C!    it under the terms of the GNU General Public !<license as published by
C!    the Free Software Foundation, either version 3 of the !<license, or
C!    (at your option) any later version.

C!    DSM2 is distributed in the hope that it will be useful,
C!    but WITHOUT ANY WARRANTY; without even the implied warranty of
C!    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
C!    GNU General Public !<license for more details.

C!    You should have received a copy of the GNU General Public !<license
C!    along with DSM2.  If not, see <http://www.gnu.org/!<licenses/>.
</license>*/

package DWR.DMS.PTM;
import java.io.*;
import java.util.HashMap;
/**
 *  PTM is an acronym for "Particle Tracking Model". This is version 2 of PTM
 *  which utilizes information from DSM2 to track particles moving according
 *  to hydrodynamics and quality information.<p>
 * 
 * This class defines the parameters and information about the environment
 * a Particle moves in. This information is either fixed or dynamic. Fixed information
 * is information that is relatively fixed during a run of the model such
 * as the output filenames, the flow network etcetra. Dynamic information
 * is information that most likely changes every time step such as the
 * flow, velocity, volume, etcetra.<p>
 *
 * The network consists of nodes and waterbodies. Waterbody is an entity
 * containing water such as a Channel, reservoir, etcetra. Node is a connection
 * between waterbodies. Each Waterbody is given a unique id and also contains
 * information about which nodes it is connected to. Each Node also has a unique
 * id and the information about which waterbodies it is connected to.
 *
 * @see Waterbody
 * @see Node
 * @author Nicky Sandhu
 * @version $Id: PTMEnv.java,v 1.5.6.4 2006/04/04 18:16:23 eli2 Exp $
 */
public class PTMEnv{
private static boolean DEBUG = false;
  /**
   * Constructs the network of nodes and waterbodies
   * @param fixedInputFilename File containing fixed input information
   */
public PTMEnv(String fixedInputFilename){
  //
  fixedInput = new PTMFixedInput(fixedInputFilename);
  hydroInput = new PTMHydroInput();
  //
  numberOfWaterbodies = fixedInput.getNumberOfWaterbodies();
  maxNumberOfWaterbodies = fixedInput.getMaximumNumberOfWaterbodies();
  if(DEBUG) System.out.println("number Of waterbodies " + numberOfWaterbodies);
  //
  numberOfNodes = fixedInput.getNumberOfNodes();
  maxNumberOfNodes = fixedInput.getMaximumNumberOfNodes();
  if(DEBUG) System.out.println("# of nodes: " + numberOfNodes +" max # nodes: "+maxNumberOfNodes);
  //
  numberOfXSections = fixedInput.getNumberOfXSections();
  maxNumberOfXSections = fixedInput.getMaximumNumberOfXSections();
  if(DEBUG) System.out.println("# of cross-sections: " + numberOfXSections);  
  //
  numberOfGroups = fixedInput.getNumberOfChannelGroups();
  
  // creating....
  // fill in waterbodies, nodes, xSections with information
  if(DEBUG) System.out.println("Creating waterbodies ");
  wbArray = fixedInput.createWaterbodyFixedInfo();
  
  //todo: eli is this working?
  // wbArray[0]=NullWaterbody.getInstance();
  
  //
  if(DEBUG) System.out.println("Creating nodes ");
  nodeArray = fixedInput.createNodeFixedInfo();
  //
  if(DEBUG) System.out.println("Creating xSections ");
  xSectionArray = fixedInput.createXSectionFixedInfo();
  //
  if(DEBUG) System.out.println("Creating filters ");
  filterArr = new HashMap<String,Filter>();
  //
  if(DEBUG) System.out.println("Waterbodies, nodes, xsections, filters created");
  // provide waterbodies with XSection* array 
  // and nodes with Waterbody* array
  if(DEBUG) System.out.println("Setting waterbody info");
  setWaterbodyInfo();
  if(DEBUG) System.out.println("Setting node info");
  setNodeInfo();
  if(DEBUG) System.out.println("Setting filter info");
  setNodeFilters();
  if(DEBUG) System.out.println("Network initialized");
  if (DEBUG){
    for(int i=0; i< wbArray.length; i++){
      System.out.println(wbArray[i]);
    }
  }
  // get fixed input for PTM model
  pInfo = new ParticleFixedInfo();
  fixedInput.getPTMFixedInfo(pInfo);
  if(DEBUG) System.out.println(pInfo);
  numberOfAnimatedParticles = pInfo.getAnimatedParticles();
}

  /**
   *  Fills in the Waterbody information and Node/XSection array pointers
   */
private final void setWaterbodyInfo(){

  // updates waterbodies to give them a pointer array of xSections
  for(int i=1; i<= fixedInput.getNumberOfChannels(); i++) {
    if (wbArray[i] != null) {
      // if (DEBUG) System.out.println("Doing xsects for Waterbody # " + i);
      XSection[] xSPtrArray;
      xSPtrArray = new XSection[((Channel) wbArray[i]).getNumberOfXSections()];
      
      for(int j=0; j< ((Channel)wbArray[i]).getNumberOfXSections(); j++) {
    	  xSPtrArray[j] = xSectionArray[((Channel) wbArray[i]).getXSectionEnvIndex(j)];
      }
      
      ((Channel)wbArray[i]).setXSectionArray(xSPtrArray);
    }
  }
  if (DEBUG) System.out.println("Done with initialzing xSections");
  // updates waterbodies to give them an array of pointers to connected nodes
  for (int i=1; i<=fixedInput.getMaximumNumberOfWaterbodies(); i++){
    if(wbArray[i] != null) {
      // if (DEBUG) System.out.println("Doing nodes for Waterbody # " + i);
      Node[] nodePtrArray;
      int nNodes = wbArray[i].getNumberOfNodes();
      nodePtrArray = new Node[nNodes];
      for(int j=0; j< nNodes; j++){
	    nodePtrArray[j] = nodeArray[wbArray[i].getNodeEnvIndex(j)];
      }
      wbArray[i].setNodeArray(nodePtrArray);
    }
  } 
  if (DEBUG) System.out.println("Done with initialzing nodes");
}

  /**
   * returns the current PTMFixedInput object
   */
  public PTMFixedInput getPTMFixedInput(){
    return (fixedInput);
  }

  /**
   * returns the current PTMHydroInput object
   */
  public PTMHydroInput getPTMHydroInput(){
    return (hydroInput);
  }

  /**
   * sets the current Particle behavior object
   */
  public final boolean setParticleBehavior() throws IOException {
    // initialize behavior file
    boolean fileExists = false;
    String behaviorFileName = getBehaviorFileName();
    if (behaviorFileName.length() != 0 && behaviorFileName != null){
      fileExists = true;
      if (checkFile(behaviorFileName)) {
	behaviorIn = new ParticleBehavior(behaviorFileName);
	if (behaviorIn != null) {
	  getParticleFixedInfo().setBehavior(behaviorIn);
	  System.out.println("Opened Behavior File "+behaviorFileName);
	}
      }
      else {
	System.out.println("Behavior File \""+behaviorFileName+"\" Does Not Exist \nExiting");
	System.exit(0);
      }
    }
    return fileExists;
  }

  private boolean checkFile(String filenm){
    File tmpfile = new File(filenm);
    return tmpfile.isFile();
  }

  /**
   * gets the Waterbody object for given unique id
   * @param   wbId a unique id
   * @return  Waterbody object
   */
  public Waterbody getWaterbody(int wbId){
    return wbArray[wbId];
  }

  /** 
   * gets the Node object for given unique Node id
   * @param nodeId a unique Node id
   * @return a Node object
   */
  public Node getNode( int nodeId){
    return nodeArray[nodeId];
  }
  /** 
   * returns a XSection* to the particular Node
   */
  XSection getXSection( int xSectionId){
    return xSectionArray[xSectionId];
  }
  /** 
   * @return The number of waterbodies
   */
  int getNumberOfWaterbodies(){
    return(numberOfWaterbodies);
  }

  /** 
   * @return The number of nodes
   */
  int getNumberOfNodes(){
    return(numberOfNodes);
  }

  /** 
   * The number of animated particles
   */
  int getNumberOfAnimatedParticles(){
    return(numberOfAnimatedParticles);
  }
  
  /** 
   * @return the number of cross Sections
   */
  int getNumberOfXSections(){
    return(numberOfXSections);
  }

  /** 
   * @return the number of Channel groups
   */
  int getNumberOfGroups(){
    return(numberOfGroups);
  }

  /**
   * @return The maximum number of waterbodies
   */
  public int getMaxNumberOfWaterbodies(){
    return(maxNumberOfWaterbodies);
  }

  /**
   * @return The maximum number of nodes
   */
  public int getMaxNumberOfNodes(){
    return(maxNumberOfNodes);
  }

  /**
   * @return the maximum number of cross Sections
   */
  int getMaxNumberOfXSections(){
    return(maxNumberOfXSections);
  }
  /**
   * @return pointer to structure containing Particle fixed info.
   */
  ParticleFixedInfo getParticleFixedInfo(){
    return(pInfo);
  }
  /**
   * updates FluxInfo structure
   */
  FluxInfo getFluxFixedInfo(){
    FluxInfo fI = fixedInput.getfluxInfo();
    //checkNodeCompatibility(fI);   unclear benefits
    return fI;
  }

  GroupInfo getGroupFixedInfo(){
    GroupInfo fI = fixedInput.getgroupInfo();
    return fI;
  }
  
  Group[] getOutputGroups(){
  	return fixedInput.getOutputGroups();
  }


  /**
   * calculates an array of common nodes amongst a set of 
   * waterbodies
   */
public int [] getCommonNodes(Waterbody [] Wbs){
  if(Wbs.length < 2) return null;
  int [] nodes = getCommonNodes(Wbs[0], Wbs[1]);
  for(int i=2; i < Wbs.length; i++){
    int [] newNodes = getCommonNodes(Wbs[0], Wbs[i]);
    nodes = getIntersection(nodes, newNodes);
  }
  return nodes;
}
  /**
   * returns the intersection of two sets of integers
   */
private int [] getIntersection(int[] x, int[] y){
  if (x == null || y == null) return null;
  int [] intersection = new int[x.length];
  int nIntersect = 0;
  for(int i=0 ; i< x.length ; i++){
    for(int j=0; j < y.length; j++){
      if(x[i] == y[j]){
	intersection[nIntersect] = x[i];
	nIntersect++;
      }
    }
  }
  if (nIntersect == 0) intersection = null;
  else{
    int [] newArray = new int[nIntersect];
    System.arraycopy(intersection,0,newArray,0,nIntersect);
    intersection = newArray;
  }
  return intersection;
}
  /**
   * Calculates an array of common nodes to the waterbodies x and y.
   * It creates the array of nodes twice. Once with an upperbound of the
   * number of common nodes and second time with the actual number of 
   * common nodes. If no common nodes are found it returns a null pointer.
   * @return an array of global Node ids or null if no common Node found
   */
public int [] getCommonNodes(Waterbody x, Waterbody y){
  int [] nodes = new int[x.getNumberOfNodes()];
  int nCommon = 0;
  for(int i=0 ; i < x.getNumberOfNodes(); i++){
    if ( y.getNodeLocalIndex( x.getNodeEnvIndex(i) ) != -1){
      nodes[nCommon] = x.getNodeEnvIndex(i);
      nCommon ++;
    }
  }
  if (nCommon == 0) nodes = null;
  else {
    int [] newNodes = new int[nCommon];
    System.arraycopy(nodes, 0, newNodes, 0, nCommon);
    nodes = newNodes;
  }
  return nodes;
}
  /**
   * return start time
   */
public int getStartTime(){
  return fixedInput.getStartTime();
}
  /** 
   *return model run length
   */
public int getRunLength(){
  return fixedInput.getRunLength();
}
  /**
   * @return time step 
   */
public int getPTMTimeStep(){
  return fixedInput.getPTMTimeStep();
}
  /**
   * @return display interval in minutes
   */
int getDisplayInterval(){
  return fixedInput.getDisplayInterval();
}
  /**
   * @return total number of particles injected.
   */
int getNumberOfParticlesInjected(){
  return pInfo.getTotalNumberOfInjectedParticles();
}
  /**
   * sets insertion time and location into Particle
   */
void setParticleInsertionInfo(Particle [] particlePtrArray,
				      int numberOfRestartParticles){
  int pNum=numberOfRestartParticles;
  int injNum=1;
  if (DEBUG) System.out.println("Injection info in PTMEnv");
  while(pNum < getNumberOfParticlesInjected() + numberOfRestartParticles){
    int injectionNode = pInfo.getLocationOfParticlesInjected(injNum);
    long injectionLength = pInfo.getInjectionLengthJulmin(injNum);
    long numberOfInjectedParticles = pInfo.getNumberOfParticlesInjected(injNum);
    for(long injection=0; 
        injection < numberOfInjectedParticles; 
        injection++){
      int insertionTime = pInfo.getInjectionStartJulmin(injNum) + (int)
                          ((injectionLength*injection)/numberOfInjectedParticles);
      Node nd = getNode(injectionNode);
      particlePtrArray[pNum].setInsertionInfo(insertionTime, nd);
      pNum++;
      if (DEBUG) System.out.println("Particle injection for particle " + pNum);
    } // end of for( injection = 0;...
    injNum++;
  }
  if (DEBUG) System.out.println("Finished injection");
}
  /**
   *
   */
void updateBoundaryWaterbodiesHydroInfo(){
  if (DEBUG) System.out.println("in updateBoundaryWaterbodiesHydroInfo");
  int flowNumber = fixedInput.getMaximumNumberOfChannels()
    + fixedInput.getMaximumNumberOfReservoirs()
    + fixedInput.getMaximumNumberOfDiversions() 
    + fixedInput.getMaximumNumberOfPumps();
  if (DEBUG) System.out.println("Waterbody array size is " + wbArray.length);
  if (DEBUG) System.out.println(flowNumber);
  for(int nBoundary = flowNumber + 1; 
      nBoundary <= flowNumber + fixedInput.getMaximumNumberOfBoundaryWaterbodies();
      nBoundary++){
    if (wbArray[nBoundary] != null){
      if (DEBUG) System.out.println("Boundary # " + nBoundary);
      Node nd = wbArray[nBoundary].getNode(0);
      float sumOfFlows=0.0f;
      for(int connection=0; connection < nd.getNumberOfWaterbodies(); connection++){
	if(nd.getWaterbody(connection).getType() != Waterbody.BOUNDARY)
	  sumOfFlows+=nd.getSignedOutflow(connection);
      }
      float[] outflow = new float[1];
      outflow[0] = sumOfFlows;
      wbArray[nBoundary].setFlow(outflow);
    }
  }
}
  /**
   *  Fills in Node info. It also collects info from waterbodies defined
   *  non uniformly such as reservoirs which do not have Node info. That is
   *  reservoirs have Node info however the Node as defined by fortran routines
   *  are unaware that they are connected to one.
   */
private void setNodeInfo(){
  if (DEBUG) System.out.println("Initializing nodes with wb arrays");
  if (DEBUG) System.out.println(nodeArray[361]);
  //
  for(int i=0; i < fixedInput.getMaximumNumberOfNodes()+1; i++){
    if ( nodeArray[i] == null ) continue;
    Waterbody [] wbs = new Waterbody[nodeArray[i].getNumberOfWaterbodies()];
    for(int j=0; j < wbs.length; j++){
      wbs[j] = wbArray[nodeArray[i].getWaterbodyEnvIndex(j)];
    }
    nodeArray[i].setWbArray(wbs);
  }
  // Now initialize each Node object with an array of waterbodies it connects to
  if (DEBUG) System.out.println("Done with setNodeInfo");
}

/**
 *  Fills in Node filter info. 
 */
private void setNodeFilters(){
  if (DEBUG) System.out.println("Initializing nodes with filter arrays");
  //
  for(int i=0; i < fixedInput.getFiltersFixedData().getNumberOfFilters(); i++){
    int filterIndex = fixedInput.getFiltersFixedData().getFilterIndex(i);
    String filterName = fixedInput.getFiltersFixedData().getFilterName(i);
    int nodeIndex = fixedInput.getFiltersFixedData().getFilterNode(i);
    int wbIndex = fixedInput.getFiltersFixedData().getFilterWb(i);
    int wbType = fixedInput.getFiltersFixedData().getFilterWbType(i);
		  
    Filter filter = new Filter(filterIndex, filterName, nodeIndex, wbIndex, wbType);
    String key = ""+nodeIndex+","+""+wbIndex;
    nodeArray[nodeIndex].setFilterArr(key,filter);
    filterArr.put(key, filter);
  }
  if (DEBUG) System.out.println("Done with setNodeFilter");
}

/**
 *  gets filter operations for current timestamp
 */
public float[] getFilterOps(){
  return filterOps;
}

  /**
   *  gets the next chunk of data for time step given
   *  and updates the Waterbody array with that information.
   */
public final void getHydroInfo(int currentTime){

  hydroInput.getNextChunk(currentTime);

  hydroInput.updateWaterbodiesHydroInfo(wbArray, fixedInput.getLimitsFixedData());
  
  hydroInput.updateFilterOps();
  filterOps = hydroInput.getFiltersOps();

//  System.out.println("test");
  
  //  updateBoundaryWaterbodiesHydroInfo();
}
  /**
   *  animation file name
   */
public final String getAnimationFileName(){
  return fixedInput.getAnimationFileName();
}
  /**
   *  animation output interval in minutes
   */
public final int getAnimationOutputInterval(){
  return fixedInput.getAnimationOutputInterval();
}
  /**
   *  behavior file name
   */
public final String  getBehaviorFileName(){
  return fixedInput.getBehaviorFileName();
}
  /**
   *  trace file name
   */
public final String  getTraceFileName(){
  return fixedInput.getTraceFileName();
}
  /**
   *  output restart file name
   */
public final String  getOutputRestartFileName(){
  return fixedInput.getOutputRestartFileName();
}
  /**
   *  restart output interval
   */
public final int getRestartOutputInterval(){
  return fixedInput.getRestartOutputInterval();
}
  /**
   *  input restart file name
   */
public final String  getInputRestartFileName(){
  return fixedInput.getInputRestartFileName();
}
  /**
   *  checks to see if this run is restart
   */
public final boolean isRestartRun(){
  return fixedInput.isRestartRun();
}
  /**
   *  returns type of file ASCII/BINARY
   */
public final int getFileType(String  fileName){
  if (fileName.endsWith(".bin"))
    return Globals.BINARY;
  else
    return Globals.ASCII;
}

  public ParticleBehavior getBehavior(){
    return behaviorIn;
  }

  /**
   *  Particle behavior input
   */
private ParticleBehavior behaviorIn;
  /**
   *  fixed input to initialize and fill in Waterbody, Node, XSection info
   */
private PTMFixedInput fixedInput;
  /**
   *  hydrodynamic input from tide file
   */
private PTMHydroInput hydroInput;
  /**
   *  array of waterbodies
   */
private Waterbody [] wbArray;
  /**
   *  # of waterbodies
   */
private int numberOfWaterbodies;
  /**
   *  array of nodes
   */
private Node [] nodeArray;
  /**
   *  # of nodes
   */
private int numberOfNodes;
  /**
   *  array of cross Sections
   */
private XSection [] xSectionArray;
  /**
   *  # of xSections
   */
private int numberOfXSections;
  /**
   *  # of animated particles
   */
private int numberOfAnimatedParticles;
  /**
   *  fixed info for particles
   */
ParticleFixedInfo pInfo;

/**
 *  collection of filters
 */
private HashMap<String,Filter> filterArr;
/**
 *  filters' operation for the current timestamp
 */
private float [] filterOps;   //1-pass;0-block.

private int  maxNumberOfWaterbodies;
private int  maxNumberOfNodes;
private int  maxNumberOfXSections;
private int  numberOfGroups;
}


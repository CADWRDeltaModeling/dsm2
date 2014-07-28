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
import java.util.*;
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
 * containing water such as a Channel, Reservoir, Conveyor, Boundary, etcetra. 
 * Node is a connection between waterbodies. Each Waterbody is given a unique id 
 * and contains information about which nodes it is connected to. Each Node also
 * has a unique id and the information about which waterbodies it is connected to.
 *
 * @see Waterbody
 * @see Node
 * @author Nicky Sandhu
 * @version $Id: PTMEnv.java,v 1.5.6.4 2006/04/04 18:16:23 eli2 Exp $
 */
// this is the rally point for PTMFixedInput/PTMFixedData, PTMHydroInput, and PTMBehaviorInputs
public class PTMEnv{
  private static boolean DEBUG = false;
  /**
   * Construct the network of nodes and waterbodies
   * @param fixedInputFilename File containing fixed input information
   */
  public PTMEnv(String fixedInputFilename){
    //Input files
    fixedInput = new PTMFixedInput(fixedInputFilename);
    hydroInput = new PTMHydroInput();
    
    //no and max no of waterbodies, nodes, xsections
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
    
    // fill in waterbodies, nodes, xSections with information
    if(DEBUG) System.out.println("Creating waterbodies ");
    wbArray = fixedInput.createWaterbodyFixedInfo();
    //TODO: eli is this working?
    //  wbArray[0]=NullWaterbody.getInstance();
    if(DEBUG) System.out.println("Creating nodes ");
    nodeArray = fixedInput.createNodeFixedInfo();
    //
    if(DEBUG) System.out.println("Creating xSections ");
    xSectionArray = fixedInput.createXSectionFixedInfo();
    //
    if(DEBUG) System.out.println("Waterbodies, nodes, xsections created");
    
    // provide waterbodies with XSection* array and 
    // nodes with Waterbody* array
    if(DEBUG) System.out.println("Setting waterbody info");
    setWaterbodyInfo();
    if(DEBUG) System.out.println("Setting node info");
    setNodeInfo();
    if(DEBUG) System.out.println("Network initialized");
    if(DEBUG){
      for(int i=0; i< wbArray.length; i++){
        System.out.println(wbArray[i]);
      }
    }
    
    // get fixed input for PTM model
    pInfo = new ParticleFixedInfo();
    fixedInput.getPTMFixedInfo(pInfo);
    if(DEBUG) System.out.println(pInfo);
    numberOfAnimatedParticles = pInfo.getAnimatedParticles();
    
    /*
     * add behaviors
     */
    // behaviors (basic and special) are added when instantiating PTMBehaviorInputs
    _behaviorInputs = new PTMBehaviorInputs(fixedInput.getBehaviorInfileName());
    _behaviorInputs.setWaterbodyInfo(wbArray);
    _behaviorInputs.setNodeInfo(nodeArray);
    _particleType = _behaviorInputs.getFishType();
  }

  /**
   *  Fill in the Waterbody information and Node/XSection array pointers
   */
  private final void setWaterbodyInfo(){
	  // when wbArray is setup, the channels are filled in first.
	  //wbArray starts from 1. see PTMFixedInput.java line 180
	  _reservoirObj2objNameID = new HashMap<String, Integer>();
	  for(int i=1; i<= fixedInput.getNumberOfChannels(); i++) {
		  if (wbArray[i] != null) {
			  if (DEBUG) System.out.println("Doing xsects for Waterbody # " + i);
			  XSection[] xSPtrArray;
			  xSPtrArray = new XSection[((Channel) wbArray[i]).getNumberOfXSections()];
			  Channel aChan = (Channel) wbArray[i];
			  for(int j=0; j< aChan.getNumberOfXSections(); j++) 
				  xSPtrArray[j] = xSectionArray[aChan.getXSectionEnvIndex(j)];
			  aChan.setXSectionArray(xSPtrArray);
		  }  
	  }
	  if (DEBUG) System.out.println("Done with initialzing xSections");
	  //set nodes for wb (not only channels)
	  for (int i=1; i<=fixedInput.getMaximumNumberOfWaterbodies(); i++){
		  if(wbArray[i] != null) {
			  if (DEBUG) System.out.println("Doing nodes for Waterbody # " + i);
			  Node[] nodePtrArray;
			  int nNodes = wbArray[i].getNumberOfNodes();
			  nodePtrArray = new Node[nNodes];
			  for(int j=0; j< nNodes; j++){
				  // the node id array has already been created in PTMFixedInput line 208, here to create a node array
				  nodePtrArray[j] = nodeArray[wbArray[i].getNodeEnvIndex(j)];
			  }//end for xsect
			  wbArray[i].setNodeArray(nodePtrArray);
			  int wbType = wbArray[i].getType();
			  if (wbType == Waterbody.RESERVOIR)
				  _reservoirObj2objNameID.put(((Reservoir)wbArray[i]).getName(), i);
			  else if (wbType == Waterbody.CONVEYOR)
				  _reservoirObj2objNameID.put(((Conveyor)wbArray[i]).getName(), i);
		  }//end if
	  }//end for wb
	  if (DEBUG) System.out.println("Done with initialzing nodes");
  	}
		  
  /**
   * Return the current PTMFixedInput object
   */
  public PTMFixedInput getPTMFixedInput(){
    return (fixedInput);
  }
  
  /**
   * Return the current PTMHydroInput object
   */
  public PTMHydroInput getPTMHydroInput(){
    return (hydroInput);
  }
  
  /**
   * Set the current Particle behavior object
   */
  //TODO this is Aaron's original code and needed to be changed
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
   * Get the Waterbody object for a given unique id
   * @param   wbId a unique id
   * @return  Waterbody object
   */
  public Waterbody getWaterbody(int wbId){
    return wbArray[wbId];
  }

  /** 
   * Get the Node object for given unique Node id
   * @param nodeId a unique Node id
   * @return a Node object
   */
  public Node getNode(int nodeId){
    return nodeArray[nodeId];
  }
  
  /** 
   * Return a XSection* to the particular Node
   */
  XSection getXSection(int xSectionId){
    return xSectionArray[xSectionId];
  }
  
  /** 
   * @Return The number of waterbodies
   */
  int getNumberOfWaterbodies(){
    return(numberOfWaterbodies);
  }

  /** 
   * @Return The number of nodes
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
   * Calculate an array of common nodes amongst a set of 
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
   * Return the intersection of two sets of integers
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
    }//end for
    if (nIntersect == 0) intersection = null;
    else{
      int [] newArray = new int[nIntersect];
      System.arraycopy(intersection,0,newArray,0,nIntersect);
      intersection = newArray;
    }
    return intersection;
  }
  
  /**
   * Calculate an array of common nodes to the waterbodies x and y.
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
    }//end for
    if (nCommon == 0) nodes = null;
    else {
      int [] newNodes = new int[nCommon];
      System.arraycopy(nodes, 0, newNodes, 0, nCommon);
      nodes = newNodes;
    }
    return nodes;
  }
  
  /**
   * Return start time
   */
  public int getStartTime(){
    return fixedInput.getStartTime();
  }
  /** 
   * Return model run length
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
	  int totalParticles = _behaviorInputs.getTotalParticlesReleased();
	  if ( totalParticles > 0)
		  return totalParticles;
	  return pInfo.getTotalNumberOfInjectedParticles();
  }
  /**
   * Set insertion time and location into Particle
   */
  void setParticleInsertionInfo(Particle [] particlePtrArray,
  				                int numberOfRestartParticles){
    int pNum=numberOfRestartParticles;
    int injNum=1;
    if (DEBUG) System.out.println("Injection info in PTMEnv");
    
    while(pNum < getNumberOfParticlesInjected() + numberOfRestartParticles){
    	if (_behaviorInputs.getTotalParticlesReleased()>0){
    		Map<Integer, FishReleaseGroup> fgs = _behaviorInputs.getFishReleaseGroups();
    		for (int releaseNode:fgs.keySet()){
    			for (FishRelease fr: fgs.get(releaseNode).getFishReleases()){
    				for (int i = 0; i < fr.getFishNumber(); i++ ){
    					particlePtrArray[pNum].setInsertionInfo(((int)PTMUtil.convertCalendar(fr.getRelaseTime())), getNode(releaseNode));
    					pNum++;
    				}	
    			}
    		}
    	}
    	else{
		      int injectionNode = pInfo.getLocationOfParticlesInjected(injNum);
		      long injectionLength = pInfo.getInjectionLengthJulmin(injNum);
		      long numberOfInjectedParticles = pInfo.getNumberOfParticlesInjected(injNum);
		      int start = pInfo.getInjectionStartJulmin(injNum);
		      // each injection row in PTM input
		      for(long injection=0;
		  	      injection < numberOfInjectedParticles;
		  	      injection++){
		    	// force to its integer time
		        int insertionTime = start + (int)
		  	                        ((injectionLength*injection)/numberOfInjectedParticles);
		        Node nd = getNode(injectionNode);
		        particlePtrArray[pNum].setInsertionInfo(insertionTime, nd);
		        pNum++;
		        if (DEBUG) System.out.println("Particle injection for particle " + pNum);
		      } // end for
		      
		      injNum++;
    	}
    } // end while
    if (DEBUG) System.out.println("Finished injection");
  }
  
  public String getParticleType(){
	  return _particleType;
  }
  
  /**
   *
   */
  //TODO this is never called, please investigate. commented out for now
  /*
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
      }//end if
    }//end for
  }
  */
  
  /**
   *  Fills in Node info. It also collects info from waterbodies defined
   *  non uniformly such as reservoirs which do not have Node info. That is
   *  reservoirs have Node info however the Node as defined by fortran routines
   *  are unaware that they are connected to one.
   */
  private void setNodeInfo(){
    if (DEBUG) System.out.println("Initializing nodes with wb arrays");
    if (DEBUG) System.out.println(nodeArray[361]);
    //nodeArray starts from 1. see PTMFixedInput.java line 287
    for(int i=1; i < fixedInput.getMaximumNumberOfNodes()+1; i++){
      if ( nodeArray[i] == null ) continue;
      // waterbodies connecting to the node
      Waterbody [] wbs = new Waterbody[nodeArray[i].getNumberOfWaterbodies()];
      for(int j=0; j < wbs.length; j++){
    	  int wbId = nodeArray[i].getWaterbodyEnvIndex(j);
    	  wbs[j] = wbArray[wbId];
      }
      nodeArray[i].setWbArray(wbs);
    }
    // Now initialize each Node object with an array of waterbodies it connects to
    if (DEBUG) System.out.println("Done with setNodeInfo");
  }

  /**
   *  Get the next chunk of data for time step given
   *  and updates the Waterbody array with that information.
   */
  //currentTime here is in minute
  public final void getHydroInfo(int currentTime){
    hydroInput.getNextChunk(currentTime);
    hydroInput.updateWaterbodiesHydroInfo(wbArray, fixedInput.getLimitsFixedData());
    hydroInput.updateNodesHydroInfo(nodeArray);
    //  updateBoundaryWaterbodiesHydroInfo();
    _behaviorInputs.updateCurrentInfo(nodeArray, wbArray, currentTime);
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
  public final String getBehaviorFileName(){
	  System.out.println(fixedInput.getBehaviorFileName());
    return fixedInput.getBehaviorFileName();
  }
  /**
   *  trace file name
   */
  public final String getTraceFileName(){
    return fixedInput.getTraceFileName();
  }
  /**
   *  output restart file name
   */
  public final String getOutputRestartFileName(){
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
  public final String getInputRestartFileName(){
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
  public final int getFileType(String fileName){
    if (fileName.endsWith(".bin"))
      return Globals.BINARY;
    else
      return Globals.ASCII;
  }

  public ParticleBehavior getBehavior(){
    return behaviorIn;
  }
  
  public PTMBehaviorInputs getBehaviorInputs(){ return _behaviorInputs;}
  public RouteHelper getRouteHelper(){ return _behaviorInputs.getRouteInputs().getRouteHelper();}
  public SurvivalHelper getSurvivalHelper(){return _behaviorInputs.getSurvivalInputs().getSurvivalHelper();}
  public SwimHelper getSwimHelper(){ return _behaviorInputs.getSwimInputs().getSwimHelper();}
  public static Integer getReservoirObj2ObjEnvId(String name){
	  if (_reservoirObj2objNameID == null || _reservoirObj2objNameID.isEmpty())
			  PTMUtil.systemExit("the map for reservoir/Object to Object name vs waterbody ID is empty!");
	  return _reservoirObj2objNameID.get(name);
  }

  /**
   *  Particle behavior input
   */
  private ParticleBehavior behaviorIn;
  /**
   *  Fixed input to initialize and fill in Waterbody, Node, XSection info
   */
  private PTMFixedInput fixedInput;
  /**
   *  Hydrodynamic input from tide file
   */
  private PTMHydroInput hydroInput;
  /**
   *  Array of waterbodies
   */
  private Waterbody [] wbArray;
  /**
   *  # of waterbodies
   */
  private int numberOfWaterbodies;
  /**
   *  Array of nodes
   */
  private Node [] nodeArray;
  /**
   *  # of nodes
   */
  private int numberOfNodes;
  /**
   *  Array of cross Sections
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
   *  Fixed info for particles
   */
  ParticleFixedInfo pInfo;
  private int maxNumberOfWaterbodies;
  private int maxNumberOfNodes;
  private int maxNumberOfXSections;
  private int numberOfGroups;
  private String _particleType;
  private PTMBehaviorInputs _behaviorInputs;
  private static Map<String, Integer> _reservoirObj2objNameID = null; 
}


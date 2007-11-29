//    Copyright (C) 1996 State of California, Department of Water
//    Resources.
//
//    Delta Simulation Model 2 (DSM2): A River, Estuary, and Land
//    numerical model.  No protection claimed in original FOURPT and
//    Branched Lagrangian Transport Model (BLTM) code written by the
//    United States Geological Survey.  Protection claimed in the
//    routines and files listed in the accompanying file "Protect.txt".
//    If you did not receive a copy of this file contact Dr. Paul
//    Hutton, below.
//
//    This program is licensed to you under the terms of the GNU General
//    Public License, version 2, as published by the Free Software
//    Foundation.
//
//    You should have received a copy of the GNU General Public License
//    along with this program; if not, contact Dr. Paul Hutton, below,
//    or the Free Software Foundation, 675 Mass Ave, Cambridge, MA
//    02139, USA.
//
//    THIS SOFTWARE AND DOCUMENTATION ARE PROVIDED BY THE CALIFORNIA
//    DEPARTMENT OF WATER RESOURCES AND CONTRIBUTORS "AS IS" AND ANY
//    EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
//    IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
//    PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE CALIFORNIA
//    DEPARTMENT OF WATER RESOURCES OR ITS CONTRIBUTORS BE LIABLE FOR
//    ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
//    CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT
//    OR SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA OR PROFITS; OR
//    BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
//    LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
//    (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE
//    USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH
//    DAMAGE.
//
//    For more information about DSM2, contact:
//
//    Dr. Paul Hutton
//    California Dept. of Water Resources
//    Division of Planning, Delta Modeling Section
//    1416 Ninth Street
//    Sacramento, CA  95814
//    916-653-5601
//    hutton@water.ca.gov
//
//    or see our home page: http://wwwdelmod.water.ca.gov/
package DWR.DMS.PTM;
import java.io.*;
/**
 *  PTM is an acronym for "Particle Tracking Model". This is version 2 of PTM
 *  which utilizes information from DSM2 to track particles moving according
 *  to hydrodynamics and quality information.<p>
 * 
 * 
 *  GENERAL OUTLINE<br>
 * 
 *  For those of us who are in touch with the deeper things in life
 *  the Particle Tracking Model concepts are explained in detail
 *  <a href="http://wwwdelmod/docs/dsm2/ptm/ptm_descript.html">here</a><p>
 * 
 *  For the programmers in use this is a description of the implementation
 *  using such an approach is to allow various hypothesis to be incorporated
 *  in the code much more easily.<p>
 * 
 *  The figure below gives the general outline of the conceptual implementation
 *  model.<p>
 * 
 *  <img align=MIDDLE src="./ptmsystem.gif">
 * 
 *  Furthermore to look at how messages are passed between the various classes
 *  the diagram below explains the main message passing.<p>
 * 
 *  <img align=MIDDLE src="./interactions.gif">
 * 
 * 
 *  FUTURE DIRECTIONS<p>
 * 
 *  Some future directions are compiled in this
 *  <a href = "./PTM_tasklist.txt"> file </a>
 *  However this file has been static for quite some time so some of the changes may
 *  already be implemented<p>
 * 
 *  For any suggestions you may mail the persons involved in this project.<p>
 * 
 * 
 *  CONTRIBUTING PEOPLE !!<p>
 * 
 *  The pioneering lady: Tara<br>
 * 
 *  The director: Ralph<br>
 * 
 *  The drone: Nicky<br>
 * 
 * This defines the parameters and information about the environment
 * of a particle.<p>
 * Most of this information is either fixed or dynamic. Fixed information
 * is information that is relatively fixed during a run of the model such
 * as the output filenames, the flow network etcetra. Dynamic information
 * is information that most likely changes every time step such as the
 * flow, velocity, volume, etcetra.<p>
 *
 * The network consists of nodes and waterbodies. Waterbody is an entity
 * containing water such as a channel, reservoir, etcetra. Node is a connection
 * between waterbodies. Each waterbody is given a unique id and also contains
 * information about which nodes it is connected to. Each node also has a unique
 * id and the information about which waterbodies it is connected to.
 *
 * @see waterbody
 * @see node
 * @author Nicky Sandhu
 * @version $Id: PTMEnv.java,v 1.5.8.1 2003/04/08 21:21:15 miller Exp $
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
  if(DEBUG) System.out.println("# of nodes: " + numberOfNodes);
  //
  numberOfXSections = fixedInput.getNumberOfXSections();
  maxNumberOfXSections = fixedInput.getMaximumNumberOfXSections();
  //
  numberOfGroups = fixedInput.getNumberOfChannelGroups();
  // fill in waterbodies, nodes, xSections with information
  if(DEBUG) System.out.println("Creating waterbodies ");
  wbArray = fixedInput.createWaterbodyFixedInfo();
  //
  if(DEBUG) System.out.println("Creating nodes ");
  nodeArray = fixedInput.createNodeFixedInfo();
  //
  if(DEBUG) System.out.println("Creating xSections ");
  xSectionArray = fixedInput.createXSectionFixedInfo();
  //
  if(DEBUG) System.out.println("Waterbodies, nodes, xsections created");
  // provide waterbodies with xSection* array and nodes
  // with waterbody* array
  if(DEBUG) System.out.println("Setting waterbody info");
  setWaterbodyInfo();
  if(DEBUG) System.out.println("Setting node info");
  setNodeInfo();
  if(DEBUG) System.out.println("Network initialized");
  if (DEBUG){
    for(int i=0; i< wbArray.length; i++){
      System.out.println(wbArray[i]);
    }
  }
  // get fixed input for PTM model
  pInfo = new particleFixedInfo();
  fixedInput.getPTMFixedInfo(pInfo);
  if(DEBUG) System.out.println(pInfo);
  numberOfAnimatedParticles = pInfo.getAnimatedParticles();
}

  /**
   *  Fills in the waterbody information and node/xSection array pointers
   */
private final void setWaterbodyInfo(){
  int i, j;
  boolean DEBUG = false;
  // updates waterbodies to give them a pointer array of xSections
  for(i=1; i<= fixedInput.getMaximumNumberOfChannels(); i++) {
    if (wbArray[i] != null) {
      if (DEBUG) System.out.println("Doing waterbody # " + i);
      xSection[] xSPtrArray;
      xSPtrArray = new xSection[((channel) wbArray[i]).getNumberOfXSections()];
      
      for(j=0; j< ((channel)wbArray[i]).getNumberOfXSections(); j++) {
	xSPtrArray[j] = xSectionArray[((channel) wbArray[i]).getXSectionEnvIndex(j)];
     }
      
      ((channel)wbArray[i]).setXSectionArray(xSPtrArray);
      
    }
  }
  if (DEBUG) System.out.println("Done with initialzing xSecitons");
  //updates waterbodies to give them an array of pointers to connected nodes
  for (i=1; i<=fixedInput.getMaximumNumberOfWaterbodies(); i++){
    if(wbArray[i] != null) {
      node[] nodePtrArray;
      int nNodes = wbArray[i].getNumberOfNodes();
      nodePtrArray = new node[nNodes];
      for(j=0; j< nNodes; j++)
	nodePtrArray[j] = nodeArray[wbArray[i].getNodeEnvIndex(j)];
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
   * sets the current particle behavior object
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
   * gets the waterbody object for given unique id
   * @param   a unique id
   * @return  waterbody object 
   */
  public waterbody getWaterbody(int wbId){
    return wbArray[wbId];
  }

  /** 
   * gets the node object for given unique node id
   * @param a unique node id
   * @return a node object
   */
  public node getNode( int nodeId){
    return nodeArray[nodeId];
  }
  /** 
   * returns a xSection* to the particular node
   */
  xSection getXSection( int xSectionId){
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
   * @return the number of channel groups
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
   * @return pointer to structure containing particle fixed info.
   */
  particleFixedInfo getParticleFixedInfo(){
    return(pInfo);
  }
  /**
   * updates fluxInfo structure
   */
  fluxInfo getFluxFixedInfo(){
    fluxInfo fI = fixedInput.getfluxInfo();
    checkNodeCompatibility(fI);
    return fI;
  }

  groupInfo getGroupFixedInfo(){
    groupInfo fI = fixedInput.getgroupInfo();
    return fI;
  }

  /**
   * checks to see if given flux waterbodies have a common node
   */
  private void checkNodeCompatibility(fluxInfo fi){
    for(int i=0; i<fi.getNumberOfFluxes(); i++){
      boolean typeFlux = false;
      waterbody iWb=null;
      //      System.out.println("fi "+fi.info[i].getNumberIncoming()+" | "+fi.info[i].getNumberOutgoing());
	if (fi.info[i].getNumberIncoming() != 0){
	  //  System.out.println("type in"+fi.info[i].inAccountTypeArray[0]);
	  if (fi.info[i].inArray[0] <= 0 ){
	    // System.out.println("inarray"+fi.info[i].inArray[0]);
	    typeFlux = true;
	  }
	  else{
	    // System.out.println(i+" in "+fi.info[i].inArray[0]);
	    iWb = wbArray[fi.info[i].inArray[0]];
	    if(iWb == null) System.out.print("Empty waterbody # " + fi.info[i].inArray[0]);
	  }
	}
	else if (fi.info[i].getNumberOutgoing() != 0){
	  //  System.out.println("type out"+fi.info[i].outAccountTypeArray[0]);
	  if (fi.info[i].outArray[0] <= 0){
	    //  System.out.println("outarray"+fi.info[i].outArray[0]);
	    typeFlux = true;
	  }
	  else{
	    //  System.out.println(i+" out "+fi.info[i].outArray[0]);
	    iWb = wbArray[fi.info[i].outArray[0]];
	    if(iWb == null) System.out.print("Empty waterbody # "+fi.info[i].outArray[0]);
	  }
	}
	else{
	  System.out.print( "Empty flux information");
	}
      //
      if( typeFlux == false){
	int nIncoming = fi.info[i].getNumberIncoming();
	int nOutgoing = fi.info[i].getNumberOutgoing();
	
	waterbody [] iWbs = new waterbody[nIncoming + nOutgoing];
	
	int k = 0;
	
	for (k = 0; k < nIncoming; k++){
	  iWbs[k] = wbArray[fi.info[i].inArray[k]];
	  if (DEBUG) 
	    System.out.println(iWbs[k]);
	}
	
	for(k = 0; k < nOutgoing; k++){
	  iWbs[k + nIncoming] = wbArray[fi.info[i].outArray[k]];
	  if (DEBUG) 
	    System.out.println(iWbs[k]);
	}
	
	int [] commonNodes = getCommonNodes(iWbs);
	if ( commonNodes == null ) {
	  System.out.print("No common nodes for waterbodies ");
	  for( int kk=0; kk < iWbs.length; kk++){
	    System.out.println(" #" + iWbs[kk] );
	  }
	  System.out.println();
	}
	else if (commonNodes.length == 1) {
	  if (DEBUG) System.out.println("common node: " + commonNodes[0]);
	  fi.info[i].nodeId = commonNodes[0];
	}
	else{
	  System.out.println("No common node Id for waterbodies given for flux");
	}
      }
      else{
	fi.info[i].nodeId = -1;
      }
    }
  }

  /**
   * calculates an array of common nodes amongst a set of 
   * waterbodies
   */
public int [] getCommonNodes(waterbody [] Wbs){
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
   * @return an array of global node ids or null if no common node found
   */
public int [] getCommonNodes(waterbody x, waterbody y){
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
   * sets insertion time and location into particle
   */
void setParticleInsertionInfo(particle [] particlePtrArray, 
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
		int insertionTime = pInfo.getInjectionStartJulmin(injNum) + 
		  (int)((injectionLength*injection)/numberOfInjectedParticles);
      node nd = getNode(injectionNode);
      particlePtrArray[pNum].setInsertionInfo(insertionTime, nd);
      pNum++;
      if (DEBUG) System.out.println("Particle injection for particle "+pNum+" at "+insertionTime);
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
      node nd = wbArray[nBoundary].getNode(0);
      float sumOfFlows=0.0f;
      for(int connection=0; connection < nd.getNumberOfWaterbodies(); connection++){
	if(nd.getWaterbody(connection).getType() != waterbody.BOUNDARY)
	  sumOfFlows+=nd.getSignedOutflow(connection);
      }
      float[] outflow = new float[1];
      outflow[0] = sumOfFlows;
      wbArray[nBoundary].setFlow(outflow);
    }
  }
}
  /**
   *  Fills in node info. It also collects info from waterbodies defined
   *  non uniformly such as reservoirs which do not have node info. That is
   *  reservoirs have node info however the node as defined by fortran routines
   *  are unaware that they are connected to one.
   */
private void setNodeInfo(){
  if (DEBUG) System.out.println("Initializing nodes with wb arrays");
  if (DEBUG) System.out.println(nodeArray[361]);
  //
  for(int i=0; i < fixedInput.getMaximumNumberOfNodes()+1; i++){
    if ( nodeArray[i] == null ) continue;
    waterbody [] wbs = new waterbody[nodeArray[i].getNumberOfWaterbodies()];
    for(int j=0; j < wbs.length; j++){
      wbs[j] = wbArray[nodeArray[i].getWaterbodyEnvIndex(j)];
    }
    nodeArray[i].setWbArray(wbs);
  }
  // Now initialize each node object with an array of waterbodies it connects to
  if (DEBUG) System.out.println("Done with setNodeInfo");
}



  /**
   *  gets the next chunk of data for time step given
   *  and updates the waterbody array with that information.
   */
public final void getHydroInfo(int currentTime){

  hydroInput.getNextChunk(currentTime);

  hydroInput.updateWaterbodiesHydroInfo(wbArray, fixedInput.getLimitsFixedData());
  
  //  updateBoundaryWaterbodiesHydroInfo();
}
  /**
   *  animation file name
   */
public final String  getAnimationFileName(){
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
   *  particle behavior input
   */
private ParticleBehavior behaviorIn;
  /**
   *  fixed input to initialize and fill in waterbody, node, xSection info
   */
private PTMFixedInput fixedInput;
  /**
   *  hydrodynamic input from tide file
   */
private PTMHydroInput hydroInput;
  /**
   *  array of waterbodies
   */
private waterbody [] wbArray;
  /**
   *  # of waterbodies
   */
private int numberOfWaterbodies;
  /**
   *  array of nodes
   */
private node [] nodeArray;
  /**
   *  # of nodes
   */
private int numberOfNodes;
  /**
   *  array of cross Sections
   */
private xSection [] xSectionArray;
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
  particleFixedInfo pInfo;
private int  maxNumberOfWaterbodies;
private int  maxNumberOfNodes;
private int  maxNumberOfXSections;
private int  numberOfGroups;
}


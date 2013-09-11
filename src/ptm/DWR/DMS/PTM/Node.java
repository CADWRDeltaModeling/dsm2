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
import edu.cornell.RngPack.*;
import java.util.ArrayList;
/**
 *  Node is defined as the connection between two or more waterbodies.
 *  Node handles connection information as well as constructing outflow
 *  from or to kind of information
 *  <p>
 * 
 *  FUTURE DIRECTIONS
 *  <p>
 */
public class Node{
  private RandomElement randomNumberGenerator;
  private static int INITIAL_SEED = 10000;
  
  /**
   *  Node constructor
   */
  public Node(int nId, int[] wbIdArray, String bdType){
    EnvIndex = nId;
    //set the number of waterbodies & number upstream / downstream channels
    //? This is number of channels only.. add ag. drains, pumps, reservoirs
    //? later as that would information would have to be extracted from 
    //? reservoirs, pumps, and ag. drains.
    numberOfWaterbodies = wbIdArray.length;
    // create arrays to store index of array of waterbodies in PTMEnv
    //? This will later be converted to pointer information.. ie. an
    //? array of pointers to waterbodies..
    wbIndexArray = new int[numberOfWaterbodies];
    wbArray = new Waterbody[numberOfWaterbodies];
    for (int i=0 ; i<numberOfWaterbodies; i++){
      wbIndexArray[i] = wbIdArray[i];
    }
    boundaryType = bdType;
    randomNumberGenerator = new Ranecu(INITIAL_SEED);
  }
  
  /**
   *  Simpler Node initializer
   */
  public Node(int nId){
    EnvIndex = nId;
    numberOfWaterbodies=0;
    LABoundaryType = -1;
  }
  
  /**
   *  Clean up only if initialized
   */
  
  public boolean equals(Node n){
	  if (n.getEnvIndex() == this.getEnvIndex())
		  return true;
	  return false;		  
  }
  /**
   *  Return a uniformly random number between 0-1 using drand48
   */
  public final float getRandomNumber(){
    return (float) randomNumberGenerator.uniform(0,1);
  }
  
  /**
   *  Return number of waterbodies connecting to the node
   */
  public final int getNumberOfWaterbodies(){
    return (numberOfWaterbodies);
  }
  
  //xiao
  public final int getNumberOfChannels(){
	  int chan = 0;
	  for (int i = 0; i<wbArray.length; i++){
		  if (wbArray[i].getType()==Waterbody.CHANNEL)
			  chan++;
	  }
	  return chan;	  
  }
  public final ArrayList<Channel> getChannels(){
	  ArrayList<Channel> chans = new ArrayList<Channel>();
	  for (int i = 0; i<wbArray.length; i++){
		  if (wbArray[i].getType()==Waterbody.CHANNEL)
			  chans.add((Channel)wbArray[i]);
	  }
	  return chans;	  
  }
  public final ArrayList<Float> getChannelOutflows(){
	  ArrayList<Float> chanflows = new ArrayList<Float>();
	  for (int i = 0; i<wbArray.length; i++){
		  if (wbArray[i].getType()==Waterbody.CHANNEL)
			  chanflows.add(getOutflow(i));
	  }
	  return chanflows;	  
  }
  public final Waterbody getChannel(int envIndex){
	  for (int i = 0; i<wbArray.length; i++){
		  Waterbody awb = wbArray[i];
		  if (awb.getType()==Waterbody.CHANNEL && awb.getEnvIndex() == envIndex)
			  return awb;
	  }
	  return null;
  }
  /**
   *  Return an integer pointing to the number Id of the Waterbody
   */
  public final int getWaterbodyId(int id){
    return(wbArray[id].getEnvIndex());
  }
  
  /**
   *  Return a pointer to desired Waterbody
   */
  public final Waterbody getWaterbody(int id){
    return(wbArray[id]);
  }
  
  /**
   *  Check to see if junction is a dead end.
   */
  public final boolean isJunctionDeadEnd(){
    return(false);
  }
  
  /**
   *  Get total positive outflow from Node<br>
   *  Add up all flows leaving the Node
   *  for particle decision making at junction
   */
  public final float getTotalOutflow(boolean addSeepDicu){
    float outflow=0.0f;
    // for each Waterbody connected to junction add the outflows
    for (int id=0; id < numberOfWaterbodies; id++) {
  	  if(!addSeepDicu){  
  	      //TODO there is only one boundary type (no seep or ag dicu type in DSM2. addSeep is passed in false)
  	      // how about this: if this is a boundary type but not included in the open boundary nodes list (only a short list),
  	      // 				 this flow will not be counted because boundary flows are open boundary flow + seep + dicu 
  		  // 				 but how to get rid of seep + dicu in a open boundary	--Xiao
  		  
        outflow += getOutflow(id);
  		  
  	  }
  	  else{
  		outflow += getOutflow(id);
  	  }
    }
    return (outflow);
  }
  
  /**
   *  Get the positive outflow to a particular Waterbody from this Node
   *  It returns a zero for negative outflow or inflow
   */
  public final float getOutflow(int id){
    return Math.max(0.0f,getSignedOutflow(id));
  }
  
  /**
   *  Return signed flow to a particular Waterbody from this Node
   */
  public final float getSignedOutflow(int id){
    int junctionIdInWaterbody= 0;
    //  System.out.println("In signed outflow: index: " + id 
    //		     + " wb[i]= "   + wbArray[id]);
    junctionIdInWaterbody = wbArray[id].getNodeLocalIndex(EnvIndex);
    if (junctionIdInWaterbody == -1)
      System.out.println("Exception thrown in node " + this.toString());
    return wbArray[id].getFlowInto(junctionIdInWaterbody);
  }
  
  /**
   *  Return the node index
   */
  public final int getEnvIndex(){
    return(EnvIndex);
  }
  
  /**
   *  Return the index of Waterbody in PTMEnv array using local index
   */
  public final int getWaterbodyEnvIndex(int localIndex){
    return(wbIndexArray[localIndex]);
  }
  
  /**
   *  Fill the wbArray in one node, and cleans up the index array
   */
  public final void setWbArray(Waterbody[] wbPtrArray){
    for(int i=0; i< numberOfWaterbodies; i++){
      // make a copy of every water body?
      wbArray[i] = wbPtrArray[i];
    }
    cleanUp();
  }
  
  /**
   *  Add Waterbody of given PTMEnv index to Node wbIndexArray.
   *  These operations should be done prior to calling setWbArray
   */
  public final void addWaterbodyId(int envIndex){
    
    if(numberOfWaterbodies > 0) {
      // store Waterbody indices and types in temporary arrays...
      int[] indexArray = new int[numberOfWaterbodies+1];
      for(int i=0; i< numberOfWaterbodies; i++) {
        indexArray[i] = wbIndexArray[i];
      }
      
      // delete the memory for these arrays
      wbIndexArray = null;
      wbArray = null;
      // increment the number of waterbodies
      numberOfWaterbodies++;
      
      // reallocate bigger chunks of memory
      wbIndexArray = new int[numberOfWaterbodies];
      wbArray = new Waterbody[numberOfWaterbodies];
      
      // fill them up again..
      for(int i=0; i< numberOfWaterbodies-1; i++){
        wbIndexArray[i] = indexArray[i];
      }
      wbIndexArray[numberOfWaterbodies-1] = envIndex;
      indexArray = null;
    }
    else{
      numberOfWaterbodies=1;
      wbIndexArray = new int[numberOfWaterbodies];
      wbArray = new Waterbody[numberOfWaterbodies];
      wbIndexArray[numberOfWaterbodies-1] = envIndex;
    }
  }
  
  /**
   * String representation
   */
  public String toString(){
    String rep = null;
    if (this != null) {
      rep =  " Node # " + this.EnvIndex + "\n" 
           + " Number of Waterbodies = " + this.numberOfWaterbodies + "\n";
      for(int i=0 ; i< getNumberOfWaterbodies(); i++)
        rep += " Waterbody # " + i + " EnvIndex is " + wbIndexArray[i];
    }
    return rep; 
  }
  //xiao
  /**
   *  install a non-physical barrier
   */
  public final void installBarrier(){
	  barrierInstalled = true;
  }
  
  /**
   *  get non-physical barrier op info
   */
  public final boolean isBarrierInstalled(){
	  return barrierInstalled;
  }
  
  /**
   *  Node global index
   */
  private int EnvIndex;
  
  /**
   *  Number of waterbodies connecting to the node
   */
  private int numberOfWaterbodies;
  
  /**
   *  Array of waterbodies connecting to this node 
   */
  private Waterbody[] wbArray;
  
  /**
   *  A storage for index of waterbodies in PTMEnv till wbArray
   *  gets filled.
   */
  private int[] wbIndexArray;
  
  /**
   *  Boundary array as defined from fixed input. This is not needed
   *  for boundary Waterbody information as only waterbodies can be
   *  boundaries.
   */
  private String boundaryType;
  
  /**
   *  Length of boundary array.
   */
  private int LABoundaryType;
  
  private boolean barrierInstalled = false;
  
  /**
   *  Delete wbIndexArray and anything else to save space.
   */
  private final void cleanUp(){
    //  wbIndexArray = null;
  }
}

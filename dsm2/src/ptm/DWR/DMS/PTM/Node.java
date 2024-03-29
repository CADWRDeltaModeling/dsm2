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
    //boundaryType = bdType;
    randomNumberGenerator = new Ranecu(INITIAL_SEED);
  }

  /**
   *  Simpler Node initializer
   */
  public Node(int nId){
    EnvIndex = nId;
    numberOfWaterbodies=0;
    //LABoundaryType = -1;
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

  //xiao added
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

  public final Waterbody[] getWaterbodies() {return wbArray; }


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

  //TODO clean up, no longer used
  /*
  public final void setTotalWaterbodyInflowsWMeanSV(){
	  float totalInflows = 0.0f;
	  float totalags = 0.0f;
	  for (Waterbody wb: wbArray){
		  // not count for negative inflow
		  float thisFlow = Math.max(0, wb.getInflow(EnvIndex));
		  if (thisFlow != 0 && !wb.isAgSeep()
	        		&& !(this._fishScreenInstalled && wb.isFishScreenInstalled()))
	        	totalInflows += thisFlow;
		  if (wb.isAgDiv())
			  // because DICU diversion is also treated as an inflow
			  // this flow is also included the swimming flow
			  totalags += thisFlow;
	  }
	  _totalWBInflowsWMeanSV = totalInflows;
	  _totalAgInflows = totalags;
  }

  public float getTotalAgDiversionWMeanSV(){return _totalAgInflows;}
  */
  /**
   *  Get total positive inflow to the water bodies<br>
   *  Add up all flows leaving the Node
   *  for particle decision making at junction
   */
  //public final float getTotalWaterbodyInflowsWMeanSV(){return _totalWBInflowsWMeanSV;}

  public void setTotalWaterbodyInflows(){
	  float totalInflows = 0.0f;
	  for (Waterbody wb: wbArray){
		  // not count for negative inflow
		  float thisFlow = Math.max(0, wb.getInflow(EnvIndex));
		  if (thisFlow != 0 && !wb.isAgSeep()
	        		&& !(this._fishScreenInstalled && wb.isFishScreenInstalled()))
	        	totalInflows += thisFlow;
	  }
	  _totalWaterbodyInflows =  totalInflows;
  }
  public float getTotalWaterbodyInflows(){ return _totalWaterbodyInflows;}
  public float getTotalAgDiversions(){ return _totalAgInflows;}
  public void setTotalAgDiversions(){
	  float totalags = 0.0f;
	  for (Waterbody wb: wbArray){
		  // 1) not count for negative inflow
		  // 2) a boundary waterbody doesn't have an area and swimming velocity.
		  //    it therefore doesn't have a swimming flow
		  float thisFlow = Math.max(0, wb.getInflow(EnvIndex));
		  if (wb.isAgDiv())
			  totalags += thisFlow;
	  }
	  _totalAgInflows = totalags;
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
  public void installBarrier(){
	  _barrierInstalled = true;
  }
  public void installFishScreen(){
	  _fishScreenInstalled = true;
  }

  /**
   *  get non-physical barrier op info
   */
  public boolean isBarrierInstalled(){
	  return _barrierInstalled;
  }
  public boolean isFishScreenInstalled(){
	  return _fishScreenInstalled;
  }
  public void setOutputNode(){_isOutputNode = true;}
  public boolean isOutputNode(){return _isOutputNode;}

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
  //private String boundaryType;

  /**
   *  Length of boundary array.
   */
  //private int LABoundaryType;

  private boolean _barrierInstalled = false;
  private boolean _fishScreenInstalled = false;
  //TODO Clean up, not used anymore
  //private float _totalWBInflowsWMeanSV=0.0f;  // this number will never be negative
  private float _totalAgInflows=0.0f;
  private float _totalWaterbodyInflows=0.0f;
  private boolean _isOutputNode = false;

  /**
   *  Delete wbIndexArray and anything else to save space.
   */
  private final void cleanUp(){
    //  wbIndexArray = null;
  }
}


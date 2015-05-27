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
import java.util.*;

import edu.cornell.RngPack.*;

/**
 * 
 *  This class is the core definition of a Particle and its movement in
 *  a Waterbody. Most of the movement functions are separated to make this
 *  a modular class. The core function is updatePosition() and its related.
 *  <p>
 *
 *  CODING NOTES<br>
 * 
 * 
 *  CODING NOTES: POSITION DETERMINATION<br>
 *  A Particle is modeled as a non interacting object.<br>
 * 
 *  Position in any direction is calculated by adding those components of various
 *  displacements over the time step.<p>
 * 
 *  For example
 * <p>
 *  DX = DX_External_Deterministic + DX_External_Random +
 *       DX_Internal_Deterministic + DX_Internal_Random
 * <p>
 *  This leads to a more modular approach and helps one to separate out the
 *  effect of different components of movement.<br>
 *  updatePosition() is the core function for the particle obj. 
 * <p>
 * 
 * 
 *  CODING NOTES: JUNCTION DECISION<br>
 *  Particle's junction decision is based on flow ratios.<br>
 * <p>
 * 
 *  CODING NOTES: BOUNDARY REFLECTIONS<br>
 *  A Particle during its movement may step out of the boundaries of a Waterbody. This
 *  effect is minimized by choosing a sub time step small enough to limit this jump to.
 * <p>
 *  However when the Particle does step out of bounds it is bounced back the extraneous
 *  distance is traveled in a manner similar to a ball bouncing off the floor.
 * <p>
 * 
 * 
 *  CODING NOTES: CROSSING RESERVOIRS<br>
 *  The total volume of the Reservoir = Vtotal<br>
 *  Flow volume out of Node i = Vi<br>
 *  Then the probability that a Particle will leave the Reservoir through a
 *  certain Node is proportional to the ratio of Vi:Vtotal.
 *  <p>
 *  
 *  CODING NOTES: RESERVOIRS PROBLEMS<br>
 *  A Particle's swimming in the Reservoir is not simulated instead it is assumed
 *  that a Particle entering a Reservoir is fully mixed and therefore it is possible
 *  that a Particle entering a Reservoir could exit the Reservoir from some other
 *  Node in the next time step giving it almost the speed of light !!! One solution
 *  may be to keep track of a Particle's position for the first few time steps in the
 *  Reservoir or some other method. Any other solutions can be mailed to use folks.
 *  <p>
 *  
 * @author Nicky Sandhu
 * @version $Id: Particle.java,v 1.6.6.1 2006/04/04 18:16:25 eli2 Exp $
 * 
 */
public class Particle{
  //TODO what if SwimmingInputs is null?!	
  private static SwimInputs  SwimmingInputs = Globals.Environment.getBehaviorInputs().getSwimInputs();
  private static boolean IsRandomAccess = SwimmingInputs.getRandomAccess();
  private static float AccessProb = SwimmingInputs.getAccessProbability();
	/**
    *  unique Particle identity
    */
  public int Id;
  /**
    *  x position is the distance along the length of the Channel.<br>
    *  direction being from upnode to downnode direction
    */
  public float x;
  /**
    *  y position is the distance from the center line.<br>
    *  direction being to the right side if one is facing the +ve x direction.
    */
  public float y;
  /**
    *  z position is the distance from the bottom of the Channel.<br>
    *  direction being the direction from the bottom to the top.
    */
  public float z;
  /**
    *  age of Particle in minutes since insertion
    */
  // age is in seconds actually
  public float age;
  /**
    *  keeps track of if Particle alive or dead
    */
  public boolean isDead;
  /**
    *  inserted or not inserted
    */
  public boolean inserted;
  /**
    *
    */
  public static boolean DEBUG = false;
  
  /**
    *  Creates a default Particle<br>
    *  The random number generator is initialized to random_seed<br>
    *  The vertical/transverse profiles are set to true
    */
  public Particle(ParticleFixedInfo pFI){
    totalNumberOfParticles++; 
    Id = totalNumberOfParticles;
    if (DEBUG) System.out.println("Initializing particle " + Id);
    if (Id == 1) setFixedInfo(pFI);
    if (DEBUG) System.out.println("Initializing static info for particle ");
    // first means the particle never be in any water body
    first = true;
    inserted = false;//particle not in the system yet
    Particle.dfac = 0.1f;
    age = 0;
	wb = null;
    //todo: eli did this work?
	//wb = NullWaterbody.getInstance();
    nd = null;
    isDead = false;
    //if (DEBUG) System.out.println("Fall velocity");
    //    fallvel = pFI.getFallVelocity();
    //    behaviorData = pFI.getBehavior();
    
  }
  /**
    *  Sets the location of Particle by Node Id # and random positioning
    *  of Particle within the Waterbody it enters thereupon.
    */
  public final void setLocation(Node n){
    nd = n;
  }
  
  /**
    *  Sets fixed info for Particle
    */
  public final static void setFixedInfo(ParticleFixedInfo pFI){
	  
    if (DEBUG) System.out.println("in setfixedinfo for particle");
    Particle.verticalConstant = pFI.getVerticalConstant();
    if (DEBUG) System.out.println("vertical constant = "+verticalConstant);
    Particle.transverseConstant = pFI.getTransverseConstant();
    if (DEBUG) System.out.println("trans constant = "+transverseConstant);
    Particle.CtCv = (float) (Math.sqrt(transverseConstant/verticalConstant));
    if (DEBUG) System.out.println("CtCv = " + CtCv);
    
    Particle.vertMove = pFI.moveVertically();
    if (DEBUG) System.out.println("vert move");
    Particle.transMove = pFI.moveLaterally();
    if (DEBUG) System.out.println("trans move");
    //    Particle.behavior = pFI.getBehaviorExists();
    Channel.useVertProfile = pFI.doVerticalProfile();
    Channel.useTransProfile = pFI.doTransverseProfile();
    Channel.constructProfile();
    Channel.constructProfile();
    // use PTMUtil random methods instead
    //  if (DEBUG) System.out.println("set random seed");
    if(randomNumberGenerator == null)
      randomNumberGenerator = new Ranecu(pFI.getRandomSeed());
  }
  
  /**
    *  Installs observer.<br>
    *  This observer observes events such as change from
    *  one Waterbody to another and sends a message to other
    *  objects from there.
    */
  public final void installObserver(ParticleObserver ob){
    observer = ob;
  }
  
  /**
    *  uninstalls observer, ie. sets it to null. This may save some
    *  runtime, however no flux information can be gleaned from the run.
    */
  public final void uninstallObserver(){
    observer = null;
  }
  
  public final void installRouteHelper(RouteHelper routeHelper){
	    _routeHelper = routeHelper;
	  }
	  
  /**
    *  uninstalls RouteHelper
    */
  public final void uninstallRouteHelper(){
    _routeHelper = null;
  }
  
  public final void installSwimHelper(SwimHelper swimHelper){
	    _swimHelper = swimHelper;
	  }
	  
/**
  *  uninstall SwimHelper
  */
public final void uninstallSwimHelper(){
  _swimHelper = null;
}

public final void installSurvivalHelper(SurvivalHelper survivalHelper){
    _survivalHelper = survivalHelper;
  }
  
/**
*  uninstall SurvivalHelper
*/
public final void uninstallSurvivalHelper(){
_survivalHelper = null;
}
  
  /**
    * gets the location of Particle by returning a pointer to the Waterbody
    * recursionCounter=0;<br>
    * currently only used for animation
    */
  public final Waterbody getLocation(float [] px, float [] py, float [] pz){
    Waterbody w;
    w = wb;
    if (inserted) {
      if (wb.getPTMType() == Waterbody.CHANNEL) {
        px[0] = x;
        py[0] = y;
        pz[0] = z;
      }
      else {
        px[0] = -1;//-1 for animation output use
        py[0] = -1;
        pz[0] = -1;
      }
      return (w);
    }
    else {
      px[0] = -1;
      py[0] = -1;
      pz[0] = -1;
      return (null);
    }
  }
  
  /**
    *  returns the unique id of Particle
    */
  public final int getId(){
    return Id;
  }
  
  /**
    *  returns the current model time
    */
  public final int getCurrentParticleTime(){
    return (Globals.currentModelTime);
  }
  public final int getInsertionTime(){
	  return insertionTime;
  }
 
  public final boolean checkSurvival(float timeToAdvance){
    // check survival if not survived isDead is set to true	
    _survivalHelper.helpSurvival(this, timeToAdvance);
    if(isDead == true) {
    	observer.observeDeath(this);
    	return false;
    }  
    return true;
  }
  /**
    *  updates the position and parameters of Particle.
    */
  //delT is time step in seconds, usually 900 seconds (15 minutes) 
  public final void updatePosition(float delT, float flowVelThreshold, boolean isDaytime, float daytimeNotSwimPercent){ //delT in seconds
	if (isDead) return;
	if (observer == null)
		PTMUtil.systemExit("expect an observer be installed, but not, system exit");
  	particleWait = false;  //set or reset particle wait variable
    if (DEBUG) System.out.println("In updating position for particle " + this);
    
    // insertion time is checked in PTMEnv, if a insertion time is earlier than the model start time, simulation exits.  
    if(!inserted && Globals.currentModelTime >= insertionTime){ //when current time reach insertion time
    	insert();
    	if (DEBUG) System.out.println("Inserted particle No. " + Id); 
    }
    
    if (inserted){
    	if(!isDaytime || PTMUtil.getRandomNumber() >= daytimeNotSwimPercent){
    		updateXYZPosition(delT, flowVelThreshold); 
			updateOtherParameters(delT); //TODO Why need this?
    	}
    	else
    		// diel holding, wait a time step
    		age += delT;
    }
  }
  
  /**
    *  Insertion time and insertion Node
    */
  public final void setInsertionInfo(int particleInsertionTime, Node inNode){
    this.insertionTime = particleInsertionTime;
    nd = inNode;
  }
  
  public final void setInsertionInfo(int particleInsertionTime, Node inNode, Waterbody inWb, int distance, String name){
	    this.insertionTime = particleInsertionTime;
	    wb = inWb;
	    nd = inNode;
	    x = distance;
	    _insertStationName = name;
  }
  
  // return particle age in seconds
  public float getParticleAge(){
	  return age;
  }
  /**
    *  Get the recent Node which particle just passed or was inserted in 
    */
  public final Node getRecentNode(){ return nd; }
  
  /**
    *  Get current Waterbody
    */
  public final Waterbody getCurrentWaterbody(){ return wb;}
  
  /**
    *  Channel parameters
    */
  protected float channelLength, channelWidth, channelDepth, channelVave, channelArea;
  
  /**
    *  a flag to see if vertical movement is to be allowed, in other words
    *  if vertical mixing is to be allowed
    */
  protected static boolean vertMove;
  
  /**
    *  a flag to check if transverse movement is to be allowed.
    */
  protected static boolean transMove;
  
  /**
    *  The transverse constant for mixing
    */
  protected static float transverseConstant;
  
  /**
    *  The vertical constant for mixing
    */
  protected static float verticalConstant;

  /**
    *  A transverse diffusivity factor based on the Darcy-Wiesbach friction factor
    */
  protected static float CtCv;
  
  /**
    *  Limiting factor for the movement during 1 time step due to mixing.<br>
    *  Used for sub-time step calculation
    *  to prevent excessive bouncing of particles at boundaries<br>
    *  usually keep particle movement <10% channel (width, depth) in 1 sub-time step
    */
  protected static float dfac;
  
  /**
    *  Total number of particles in the system.
    */
  protected static int totalNumberOfParticles;
  
  /**
    *  true if it is the first update of position after insertion
    */
  protected boolean first;
  
  /**
    *  the Waterbody which the particle currently stays in
    */
  protected Waterbody wb;
  
  /**
    *  the Node which the particle was inserted or just passed
    */
  protected Node nd;
  
  /**
    *  Time left for completing the current PTM input time step
    */
  
  /**
    *  Mixing co-efficients
    */
  protected float Ev,Evdt,Etdt;
  
  /**
    *  Falling velocity of Particle through water
    */
  protected float fallvel;

  /**
    *  A Particle may be asked to wait instead of move with the velocity
    *  An example is when seep flows control at a Node and Particle to seep
    *  has been turned off. The Particle is asked to remain at its current
    *  position.
    */
  protected boolean particleWait;

  /**
    *  Gaussian random number generator for y and z dispersive movements
    */
  // use method in PTMUtil instead
  protected static RandomElement randomNumberGenerator;
  
  /**
    *  Particle observer
    */
  protected ParticleObserver observer;
  
  /**
   *  Particle Helpers
   */
 private RouteHelper _routeHelper;
 // SwimHelper will be used later
 private SwimHelper _swimHelper;
 private SurvivalHelper _survivalHelper;
 private boolean _travelTimeRecorded = false;
 private float _swimmingVelocity = 0.0f;
 private float _meanSwimmingVelocity = 0.0f;
 private int _confusionFactor = 1;
  
  /**
    *  updates the Particle position for the given time step;
    *  input time step is usually divided into small sub-time step to complete the calculation; 
    *  The Particle is moved for the time step given; 
    *  The new position of the Particle is available as
    *  Particle.x, Particle.y and Particle.z
    */
 //delT in seconds, is ptm input time step 
 protected final void updateXYZPosition(float delT, float velThreshold){
	 float tmLeft = delT;
	 while (tmLeft>0 && !isDead){
		 // Channel
		 if (wb.getPTMType() ==  Waterbody.CHANNEL) {
			 if (DEBUG) System.out.println("Particle " + this + " in channel " + wb.getEnvIndex());
			 if (IsRandomAccess){
			    	if (PTMUtil.getRandomNumber() < AccessProb){
			    		if (PTMUtil.getRandomNumber() < ((Channel)wb).getProbConfusion())
			    			_confusionFactor = -1;
			    		else
			    			_confusionFactor = 1;
			    				
			    	}
		     }
			 _swimmingVelocity = ((Channel)wb).getSwimmingVelocity(_meanSwimmingVelocity);
			 //TODO clean up
			 if(Id == 1)
				 System.err.println(PTMHydroInput.getExtFromIntChan(wb.getEnvIndex()) + "  " +_meanSwimmingVelocity + "  " + _swimmingVelocity
						 + "  " + _confusionFactor + "  " + ((Channel)wb).getChanDir());
			 _swimmingVelocity = _confusionFactor*((Channel)wb).getChanDir()*_swimmingVelocity;
			 // update sub-time step due to y & z mixing
			 int numOfSubTimeSteps = getSubTimeSteps(tmLeft);
			 float tmstep = tmLeft/numOfSubTimeSteps;
			 // PTM internal calculation time step
			 float tmToAdv = 0.0f;
			 //y, z set up for particles which are just out of reservoir or conveyor or inserted
			 //it is not necessary to set x because makeNodeDecision or setInsertInfo will be called and x will be set then
			 if (PTMUtil.floatNearlyEqual(y, MISSING) || PTMUtil.floatNearlyEqual(z,MISSING)) {
				 setYZLocationInChannel((Channel) wb);
			 }
			 
			 // update particle's x,y,z position every sub-time step
			 while (tmLeft > 0 && !isDead){
				 if (tmLeft >= tmstep) // for all sub-time steps except the last
					 tmToAdv = tmstep;
				 else // for the last sub-time step; deal with division precision & truncation
					 tmToAdv = tmLeft;
				 // the statistical formula of survival is only for channels
				 if (!checkSurvival(tmToAdv)) {
					 age += tmToAdv;
					 return;
				 }
				 updateAllParameters(tmToAdv);
				 // if an average cross section velocity (channelVave) is less than a user specified threshold, make the particle hold for one time step (channelVave will not change in that time step)
				 if (channelVave < velThreshold){
					 // wait time is the time left for the time step.
					 age += tmLeft;
					 return;
				 }
				 boolean needToBeRecorded = false;
				 int n = -999999, w= -999999, d = 0;
				 if (!particleWait){
					 //Calculate X direction movement
					 float xPos = x + calcXDisplacementExtDeterministic(tmToAdv)
						 		+ calcXDisplacementExtRandom(tmToAdv)
						 		+ calcXDisplacementIntDeterministic(tmToAdv)
						 		+ calcXDisplacementIntRandom(tmToAdv);
					 
					 /**
					  * wb and nd will be updated after isNodeReached and makeNodeDecision calls, 
					  * so test if the travel time needs to be recorded before those calls
					  * only record the travel time for particles from upstream because the output node is an upstream node
					  * xPos - x is always positive because the particle is from the upstream  
					  */					 
					 
					 // n and w will be used to check if the particle stay in the same node
					 n = nd.getEnvIndex();
					 w = wb.getEnvIndex();
					 if (!_travelTimeRecorded && wb.isOutputWb() && nd.isOutputNode() && 
							 ((xPos > wb.getOutputDistance()) || PTMUtil.floatNearlyEqual(xPos, wb.getOutputDistance()))){
						 d = wb.getOutputDistance();
						 needToBeRecorded = true;
						 //TODO !!! use age as travel time, because when calibrate for travel time, particles will be inserted 
						 //!!! in the upstream end of the river reach.  May consider modify later.
					 }
					 if (isNodeReached(xPos) == true){
						 float totalVelocity = calcXVelocityExtDeterministic() + calcXVelocityExtRandom()
				  			 	 			 + calcXVelocityIntDeterministic() + calcXVelocityIntRandom();
						 if(Math.abs(totalVelocity)<Float.MIN_VALUE) //xPos == x
							 PTMUtil.systemExit("when calculate x position, encountered a 0 velocity which is impossible");
						 else{ 
							 //if isNodeReached(xPos) == true, xPos could only > length or < 0
							 if (xPos < 0)
								 // now the entire tmToAdv is not totally used up.  Reset tmToAdv to the time only used 
								 tmToAdv = Math.abs(x/totalVelocity); // tmToAdv is less than tmstep
							 else if (xPos > channelLength)
								 tmToAdv = (channelLength-x)/totalVelocity; 
							 else
								 PTMUtil.systemExit("when a node is reached xPos can be only < 0 or > length, but xPos ="+xPos
										 + " in channel:"+PTMHydroInput.getExtFromIntChan(wb.getEnvIndex()));	
						 }
						 // makeNodeDecision updates wb and sets x for new wb, x = 0 or channel length
						 makeNodeDecision();
						 // water body is updated in makeNodeDecision.  Now check if the new water body is a channel
						 // if not, exit the while loop and find a block that deal with the waterbody type
						 if (wb.getPTMType() != Waterbody.CHANNEL){
							 tmLeft -= tmToAdv;
							 age += tmToAdv;
							 // before exit the loop, check if time should be recorded
							 if (needToBeRecorded){
								 _travelTimeRecorded = true;
								 needToBeRecorded = false;
								 _tto.setTravelTime(n,w,d, _insertStationName, PTMUtil.modelTimeToCalendar(insertionTime), Id, age/60);
							 } 
							 break;
						 }
						 // if channel, check to see if stay in the same node, 
						 // if yes, wait until next time step
						 // if not, continue on the code that calc y, z
						 else{
							 // if particle stays in the same node, wait for a time step (exit both while loops).  
							 if (tmToAdv < Float.MIN_VALUE && wb.getEnvIndex() == w && nd.getEnvIndex() == n 
									 && totalVelocity*wb.getInflowWSV(nd.getEnvIndex(), _swimmingVelocity)<0){
								 age += tmLeft;
								 return;
							 }
						 }
					 } //end if (isNodeReached(xPos) == true) 
					 else
						 x = xPos;
				 
					 /* 	
					  * Careful!!! wb, nd are updated in makeNodeDecision!!!
					  * but channel parameters (channel width, etc.) are not (!!!) 
					  * because updateAllParameters is not called 
					  * y, z calculation are based on previous wb, nd,
					  * and will be updated for the new wb and nd at the beginning of the function
					  */
					 if (!isDead) {// save time if particle's dead
						 y = calcYPosition(tmToAdv);
						 z = calcZPosition(tmToAdv);
					 }
				 }//end if(!particleWait)
				 tmLeft -= tmToAdv;
				// age in seconds
				 age += tmToAdv;
				// check if need record before going to next sub-timestep	 					 
				 if (needToBeRecorded){
					 _travelTimeRecorded = true;
					 needToBeRecorded = false;
					 _tto.setTravelTime(n,w,d,_insertStationName, PTMUtil.modelTimeToCalendar(insertionTime), Id, age/60);
				 } 
			 }// end the while in Channel
      
		 }// end if(CHANNEL)
    
		 else if (wb.getPTMType() ==  Waterbody.RESERVOIR){
			 if (DEBUG) System.out.println("Particle " + this + " in reservoir " + wb.getEnvIndex() );
			 //TODO clean up later unnecessary function call, commented out
			 //tryCrossReservoir(tmLeft); 
			 
		    nd = makeReservoirDecision(tmLeft);
		  
		    if ( nd != null){
		    // zero time delay
		      //makes decision of which Waterbody to go into
		      makeNodeDecision();
		      // set previous depth and width to current depth and width
		      first = true;
		      //? what should be new x,y,z for the pParticle in the Waterbody?
		      setXYZLocationInChannel(true);
		    }
		    else{
		    	// if no node found the particle will still in the reservior until next time step
		    	age += tmLeft;
		    	tmLeft = 0.0f;
		    }
		    if (!_travelTimeRecorded && wb.isOutputWb() && nd.isOutputNode()){
				 _travelTimeRecorded = true;
				 _tto.setTravelTime(nd.getEnvIndex(), wb.getEnvIndex(), 0,_insertStationName, PTMUtil.modelTimeToCalendar(insertionTime), Id, age/60); 
		    }
		 }
    
		 else if (wb.getPTMType() == Waterbody.CONVEYOR){
			 if (DEBUG) System.out.println("Particle " + this + " in conveyor " + wb.getEnvIndex() );
			 // zero time delay
			 moveInConveyor(tmLeft);
			 if (!_travelTimeRecorded && wb.isOutputWb() && nd.isOutputNode()){
				 _travelTimeRecorded = true;
				 _tto.setTravelTime(nd.getEnvIndex(), wb.getEnvIndex(), 0,_insertStationName, PTMUtil.modelTimeToCalendar(insertionTime), Id, age/60); 
		    }
		 }

		 else if (wb.getPTMType() ==  Waterbody.BOUNDARY) {
			 if (DEBUG) System.out.println("Particle " + this + " in boundary " + wb.getEnvIndex() );
			 if (!_travelTimeRecorded && wb.isOutputWb() && nd.isOutputNode()){
				 _travelTimeRecorded = true;
				 _tto.setTravelTime(nd.getEnvIndex(), wb.getEnvIndex(), 0,_insertStationName, PTMUtil.modelTimeToCalendar(insertionTime), Id, age/60); 
		    }
			 setParticleDead();
			 break;
		 }
	 } // end first while
    
  }
  
  /**
    *  x,y,z positioning
    *  called after Particle insertion or returned from Reservoir/Conveyor
    */
  protected final void setXYZLocationInChannel(boolean calcX){
    if (wb.getPTMType() == Waterbody.CHANNEL) {
      if (calcX)
    	  x = getXLocationInChannel((Channel)wb);
      if (first) updateChannelParameters();
      setYZLocationInChannel((Channel)wb);
    }else{ 
      x=MISSING; y=MISSING; z=MISSING;
    }
  }
  
  /**
    *  y, z positioning for particle just out of reservoir/conveyor
    *  w random numbers generation 
    */
  protected final void setYZLocationInChannel(Channel c){
      //y = ((Channel)wb).getWidth(x)*(wb.getRandomNumber()-0.5f);
      //z = ((Channel)wb).getDepth(x)*wb.getRandomNumber();
      y = c.getWidth(x)*((float)PTMUtil.getRandomNumber()-0.5f);
      z = c.getDepth(x)*(float)PTMUtil.getRandomNumber();
  }
  
  /**
    *  X Position calculation for the time step given
    */
  //TODO, clean up later
  //timeStep in seconds
  /* unnecessary function call comment out
  protected final float calcXPosition(float timeStep){
    // get current position
	float xPos = this.x;
    // calculate position after timeStep
    xPos = xPos + calcXDisplacementExtDeterministic(timeStep)
                + calcXDisplacementExtRandom(timeStep)
                + calcXDisplacementIntDeterministic(timeStep)
                + calcXDisplacementIntRandom(timeStep);
    float totalVelocity = calcTotalXVelocity();
    */
    // TODO consider rewrite this: 
    /*
     * isNodeReached (...) reset the node to the node just encountered, 
     * but the waterbody will be reset to the waterbody currently entered until makeNodeDecision, very confusing.
     */
    /* 
    if (isNodeReached(xPos) == true) {
    */
      //TODO clean Up
      /*  	
      float timeToReachNode = calcTimeToNode(xPos);
      tmLeft -= timeToReachNode;
      age = age - timeStep + timeToReachNode;
      */
      
      /* after this method call
       * 1) x set to the start of the node passed
       * 2) waterbody to set the waterbody just entered
       */
  	 /*
      makeNodeDecision();
      
      // for dead-end, or not flow-conflict node (not effective outflows)
      // if (recursionCounter++ > 5) error("Too many recursions in calcXPosition(float)");
      if (recursionCounter++ > 5) {
        if (repositionFactor < MAX_R_F){
   	      repositionFactor += RFIncrement;//increase timestep
   	      
   	      System.out.println("Reposition Factor set to " + repositionFactor
                           + " for particle " + getId() + " at node "
                           + PTMHydroInput.getExtFromIntNode(nd.getEnvIndex())
                           //+ PTMHydroInput.getExtFromIntNode(((Channel)wb).getUpNodeId())
                           + " in channel " + PTMHydroInput.getExtFromIntChan(((Channel)wb).getEnvIndex()));
                           
        }
        recursionCounter = 0;
      }
      */
      //TODO clean Up
      /* dangerous recursive calls as many class variables are involved! 	
      // update XYZ for the rest of 1 PTM input time step in new waterbody (water body is updated after make NodeDecision
      if ( tmLeft > 1.0e-3f )
    	  updateXYZPosition(tmLeft);
      return x;
      */
      /*
      if (xPos < 0)
    	  _leftoverTime = Math.abs(xPos/totalVelocity);
      else
    	  _leftoverTime = Math.abs((xPos-channelLength)/totalVelocity);
      return x;
    }// end if (nodeReached)
    return xPos;
  }
   */
  
  /**
    *  Y Position calculation for the time step given
    */
  protected final float calcYPosition(float timeStep){
    // get current position
    float yPos = this.y; 
    // calculate position after timeStep
    yPos = yPos + calcYDisplacementExtDeterministic(timeStep)
                + calcYDisplacementExtRandom(timeStep)
                + calcYDisplacementIntDeterministic(timeStep)
                + calcYDisplacementIntRandom(timeStep);
  
    // reflection from banks of Channel
    int k = 0;
    int MAX_BOUNCING = 100; // max num of iterations to do reflection
    float halfWidth = channelWidth/2.0f;
    while ((yPos < -halfWidth || yPos > halfWidth) && (k <= MAX_BOUNCING)){
      if (yPos < -halfWidth) yPos = -halfWidth + (-halfWidth - yPos);
      if (yPos > halfWidth ) yPos =  halfWidth - (yPos - halfWidth);
      k++;
    }
    
    if (k > MAX_BOUNCING)     
      PTMUtil.systemExit("Too many iterations in calcYPosition()");

    return (yPos);
  }
  
  /**
    *  Z Position calculation for time step given
    */
  protected final float calcZPosition(float timeStep){
    // get current position
    float zPos = this.z;
    // calculate position after timeStep
    zPos = zPos + calcZDisplacementExtDeterministic(timeStep)
                + calcZDisplacementExtRandom(timeStep)
                + calcZDisplacementIntDeterministic(timeStep)
                + calcZDisplacementIntRandom(timeStep);

    // reflections from bottom of Channel and water surface
    int k = 0;
    int MAX_BOUNCING = 100;
    while ((zPos < 0.0 || zPos > channelDepth) && (k <= MAX_BOUNCING)){
      if (zPos < 0.0) zPos = -zPos;
      else if (zPos > channelDepth) zPos = channelDepth - (zPos - channelDepth);
      k++;
    }
    if (k > MAX_BOUNCING) {
    	PTMUtil.systemExit("Too many iterations in calcZPosition()");
    }
    return (zPos);
  }
  
  /**
    *  Externally induced Deterministic x
    */
  protected float calcXDisplacementExtDeterministic(float timeStep){
    float xVel = calcXVelocityExtDeterministic();
    return (xVel*timeStep);
  }
  
  /**
    *  Externally induced Random x
    */
  protected float calcXDisplacementExtRandom(float timeStep){return 0.0f;}
  
  /**
    *  Internally induced Deterministic x
    */
  protected float calcXDisplacementIntDeterministic(float timeStep){return 0.0f;}
  
  /**
    *  Internally induced Random x
    */
  protected float calcXDisplacementIntRandom(float timeStep){return 0.0f;}
  
   /**
    *  Externally induced Deterministic y
    */
  protected float calcYDisplacementExtDeterministic(float timeStep){return 0.0f;}
  
  /**
    *  Externally induced Random y
    */
  protected float calcYDisplacementExtRandom(float timeStep){
    // get y random mixing component
    //float dy = (float) (randomNumberGenerator.gaussian()*Etdt);
	float dy = (float) (PTMUtil.getNextGaussian()*Etdt);
    // return the random y movement if transverse mixing allowed
    if (transMove) return (dy);
    else return 0.0f;
  }
  
  /**
    *  Internally induced Deterministic y
    */
  protected float calcYDisplacementIntDeterministic(float timeStep){return 0.0f;}
  
  /**
    *  Internally induced Random y
    */
  protected float calcYDisplacementIntRandom(float timeStep){return 0.0f;}
  
  /**
    *  Externally induced Deterministic z
    */
  protected float calcZDisplacementExtDeterministic(float timeStep){
    //    return(-getFallVel()*timeStep);
    return 0.0f;
  }
  
  /**
    *  Externally induced Random z
    */
  protected float calcZDisplacementExtRandom(float timeStep){
    // get z random mixing component
    //float dz = (float) (randomNumberGenerator.gaussian()*Evdt);
	float dz = (float) (PTMUtil.getNextGaussian()*Evdt);
    // return the random z movement if vertical mixing allowed
    if (vertMove) return (dz);
    else return 0.0f;
  }
  
  /**
    *  Internally induced Deterministic z
    */
  protected float calcZDisplacementIntDeterministic(float timeStep){return 0.0f;}
  
  /**
    *  Internally induced Random z
    */
  protected float calcZDisplacementIntRandom(float timeStep){return 0.0f;}
  
  /**
    *  Externally induced Deterministic x
    */
  //TODO can be separated from particle using particle.move()
  protected float calcXVelocityExtDeterministic(){
      return ((Channel)wb).getVelocity(x,y,z, channelVave, channelWidth, channelDepth) + _swimmingVelocity;
  }
  
  /**
    *  Externally induced Random x
    */
  protected float calcXVelocityExtRandom() {return 0.0f;}
  
  /**
    *  Internally induced Deterministic x
    */
  protected float calcXVelocityIntDeterministic() {return 0.0f;}
  
  /**
    *  Internally induced Random x
    */
  protected float calcXVelocityIntRandom() {return 0.0f;}
  protected void setMeanSwimmingVelocity(float msv){_meanSwimmingVelocity = msv;}
  protected void setSwimmingVelocity(float sv){_swimmingVelocity = sv; }
  protected float getSwimmingVelocity() {
	  return _swimmingVelocity;
  }
  /**
    *  Makes Node decision on which Waterbody to enter into next;
    *  update nd, wb, x
    */
  protected void makeNodeDecision(){
	/*
	 * isNodeReached() replaces current nd with the node just reached
	 * now node is the current Node in which Particle entered
	 * send message to observer about change 
	*/
	if (observer != null) 
	  observer.observeChange(ParticleObserver.NODE_CHANGE,this); 
	
	// decide which water body to go and set the paticle with the new water body and new x
	if(_routeHelper ==  null)
		PTMUtil.systemExit("routeHelper not initialized, exit from Particle.java line 727.");
	_routeHelper.helpSelectRoute(this);
	
	//TODO clean up, the block below is done in RouteHelper
	/*  
	if (observer != null) 
		observer.observeChange(ParticleObserver.WATERBODY_CHANGE,this);
	// set x at beginning/end of the new channel,if type is not a channel x = 0
	x = getXLocationInChannel();
	*/
  }
  /**
    *  updates pParticle position after calling makeReservoirDecision
    */
  //TODO clean up later  
  /* unnecessary and confusing function call
  protected final void tryCrossReservoir(float timeStep){
	// a bug? particle may go through the reservoir without taking any time!
	// adjust time and age
    //age += timeStep;
    //tmLeft = tmLeft - timeStep;
  
    //get a pointer to the Node into which pParticleenters from Reservoir
    nd = makeReservoirDecision(timeStep);
  
    if ( nd != null){
      //makes decision of which Waterbody to go into
      makeNodeDecision();
      // set previous depth and width to current depth and width
      first = true;
      //? what should be new x,y,z for the pParticle in the Waterbody?
      setXYZLocationInChannel();
    }
    else{
    	// if no node found the particle will still in the reservior until next time step
    	tmLeft -= timeStep;
    	age += timeStep;
    }
  }
  */
  /**
    * moves to the Node with inflow and decides where to go from there...
    */
  protected void moveInConveyor(float delT){
    Conveyor c = (Conveyor)wb;
    if (DEBUG) System.out.println("Particle in conveyor: " + c );
    float flow = c.getInflow(0);
    if (flow > 0) 
      setLocation( c.getNode(1) );
    else
      setLocation( c.getNode(0) );
    if (DEBUG) System.out.println("Current node: " + nd);
    makeNodeDecision();
    if (DEBUG) System.out.println("Current wb: " + wb);
    if (DEBUG) System.out.println(" wb type: " + wb.getPTMType() 
                                + ", waterbody.CHANNEL=" 
                                + Waterbody.CHANNEL);
    if (wb.getPTMType() == Waterbody.CHANNEL) {
      first=true;
      setXYZLocationInChannel(true);
    }
  }  
  
  /**
    *  returns Node to which pParticle transitions or null
    */
  protected Node makeReservoirDecision(float timeStep){
    //Get total volume of Reservoir and multiply by random number
    float totvol = ((Reservoir)wb).getTotalVolume(timeStep);
    //float rand = wb.getRandomNumber();
    totvol = totvol*((float)PTMUtil.getRandomNumber());
  
    //Get flow volume out first Node
    int nodeId = -1;
    float flowvol = 0.0f;
    //while outflow volume over last i nodes is less than random value
    do {
      nodeId++;
      flowvol += Math.max(0.0f,((Reservoir)wb).getVolumeOutflow(nodeId, timeStep));
    }
    while( flowvol < totvol && nodeId < wb.getNumberOfNodes()-1);
  
    if(flowvol > totvol) return wb.getNode(nodeId);
    else return null;
  }
  
  /**
    *  generates error
    */

  
  /**
    *  generates warning
    */
  protected final void warning(String msg){
    System.out.println( "WARNING: " + msg + " !" );
  }
  
  /**
    *  inputs state of pParticle
    */
  /**
    *  outputs state to ostream
    */

  private static final float Emin=0.0001f;
  private static final int MAX_NUM_OF_SUB_TIME_STEPS=10000;
  private static final int MISSING = -99999;

  /**
   *  insert particle in the system
   */
  private final void insert(){
    observer.observeChange(ParticleObserver.INSERT,this);
    inserted = true;
    if (wb == null){
    	makeNodeDecision();
    	setXYZLocationInChannel(true);
    }
    else{
    	 if (wb.getPTMType() == Waterbody.CHANNEL){
    		 _meanSwimmingVelocity = SwimmingInputs.getParticleMeanSwimmingVelocity(Id, (Channel) wb);   		 
 	    	 _swimmingVelocity = ((Channel)wb).getSwimmingVelocity(_meanSwimmingVelocity);
    	 }
    	 // if a particle is inserted in a channel instead of a node
    	 // channel number and distance x are known.  No need to calc x so pass a false 
    	 setXYZLocationInChannel(false);
    }
  }

  /**
    *  
    */ 
  private final void updateAllParameters(float tmstep){
    updateChannelParameters();
    updateParticleParameters(tmstep); 
  }

  /**
    *  updates channel length, width, depth, average velocity, area
    *  and previous depth, width
    */ 
  //TODO can be separated from particle
  private final void updateChannelParameters(){
    ((Channel)wb).updateChannelParameters(x,cL,cW,cD,cV,cA);
    channelLength = cL[0];
    channelWidth  = cW[0];
    channelDepth  = cD[0];
    channelVave   = cV[0];
    channelArea   = cA[0];
    if (first) {
      //previous=current, if transfer from reservoir/conveyor to channel
      previousChannelDepth = channelDepth;
      previousChannelWidth = channelWidth;
      first=false;
    }
  }

  /**
    *  
    */ 
  protected void updateOtherParameters(float delT){
  }    

  /**
    *  updates particle y, z position, Ev, Evdt, Etdt
    */ 
  //TODO can be separated from particle
  private final void updateParticleParameters(float timeStep){
	//map y & z in new xsection over the node
    z = z*channelDepth/previousChannelDepth;
    y = y*channelWidth/previousChannelWidth;
    
    //set previouses to the news..
    previousChannelDepth = channelDepth;
    previousChannelWidth = channelWidth;
  
    //recalculate mixing co-efficients..
    //new calculation of diffusion (test code)
    Ev = Math.abs(verticalConstant*channelDepth*channelVave);
    Ev = Math.max(Ev,Emin);
    //System.out.println("vert_const="+verticalConstant+"channelDepth="+channelDepth+" Vave="+channelVave+" dt="+timeStep+" Ev="+Ev);    
    
    Evdt = (float) Math.sqrt(2.0f*Ev*timeStep);
    Etdt = Evdt*CtCv;
  }
  
  /**
    *  true is the particle reach the next node within 1 timestep
    */ 
  private final boolean isNodeReached(float xpos){
    if (xpos < 0.0f) {// crossed starting Node of Channel
      nd = wb.getNode(Channel.UPNODE);
    }
    else if (xpos > channelLength) {// crossed ending Node of Channel
      nd = wb.getNode(Channel.DOWNNODE);
    }
    else return false;
	
	if (particleWait) return false; // false if the particle is asked to wait
	
    return true;
  }

  /**
    *  calculate the rest time for particle reaching its target node
    */
  //TODO this is not used anymore clean up
  /*
  private final float calcTimeToNode(float xpos){
  
    float dT = 0.0f;
    // get deterministic velocity sum in X direction
    float xVel = calcXVelocityExtDeterministic() + calcXVelocityIntDeterministic();
    float xStart = x;
  
    if (xpos < 0.0f) {// if starting node
      dT = -xStart/xVel;
    }
    else if (xpos > channelLength) {// if ending node
      dT = (channelLength-xStart)/xVel;
    }
  
    return dT;
  }
  */
  
  /**
    *  get the sub-time step used for PTM calculation 
    *  it is always <= input time step
    */ 
  private final int getSubTimeSteps(float timeStep){
  
    float minTimeStep = timeStep;
  
    if ((vertMove!=true) && (transMove!=true))
      minTimeStep = timeStep;
    else
      minTimeStep = getMinTimeStep();
  
    int numOfSubTimeSteps = 1;
    if (minTimeStep < timeStep) numOfSubTimeSteps=(int) (timeStep/minTimeStep+1);
    //else numOfSubTimeSteps=1;
    
    //System.out.println("timeStep="+timeStep+" minTimeStep="+minTimeStep);
    if (numOfSubTimeSteps > MAX_NUM_OF_SUB_TIME_STEPS){
      warning("Number Of Sub Time Steps exceeds a maximum of "+MAX_NUM_OF_SUB_TIME_STEPS);
      //TODO why need this MAX_NUM_OF_SUB_TIME_STEPS?
      return MAX_NUM_OF_SUB_TIME_STEPS;
    }
    else 
      return numOfSubTimeSteps;
  }

  /**
    *  Minimum time step with vertical or transverse mixing turned on
    *  with consider of terminal fall velocity &
    *  particle's travel distance <10% width/depth in 1 sub-time step
    */ 
  private final float getMinTimeStep(){
    //fallvel -. input behavior
    //  float terminalVelocity=Math.max(getFallVel(),1.0e-10f); 
    float terminalVelocity = getTerminalVelocity();
    
    //get a factor of the Channel
    //the maximum distance a pParticlecan travel in a time step
    updateAllParameters(0.0f); // time step not really a factor here.
    float dzmax = dfac*channelDepth;
    float dymax = dfac*channelWidth;
    float dtz = Math.min(dzmax/terminalVelocity,dzmax*dzmax/Ev);
    float dty = (dymax*dymax)/(CtCv*CtCv*Ev);
  
    //  System.out.println("dzmax="+dzmax+"; dymax="+dymax+"; Ev="+Ev+"; CtCv="+CtCv+
    //                     "; dtz="+dtz+"; dty="+dty);

    float minTimeStep = 0.0f;
    if ((vertMove==true) && (transMove==true)) minTimeStep = Math.min(dty,dtz);
    else if ((vertMove==true) && (transMove!=true)) minTimeStep = dtz;
    else if ((vertMove!=true) && (transMove==true)) minTimeStep = dty;
    return minTimeStep;
  }

  /**
    *  factor used for calculating minimum time step
    *  set outside of method to allow overwriting to include fall velocity
    */  
  private float getTerminalVelocity(){
    return 1.0e-10f;
  }
  
  /**
    *  Counts number of recursions done to calculate X position
    *  avoid too much small time-step recursion at dead-end channel
    *  or it may crash the computer
    */
  //private static int recursionCounter;
  
  /**
    *  Factor used in repositioning when a no outflow condition is encountered
    */
  protected float repositionFactor = 0.00001f;

  /**
    *  Reposition Factor increment
    */
  //private float RFIncrement = 0.0001f;

  /**
    *  Maximum Repositioning Factor
    */
  //private float MAX_R_F = 0.01f;

  /**
    *  Location state variables
    *  to map y,z from previous xsec to new
    */
  private float previousChannelDepth, previousChannelWidth;
  
  /**
    *  Insertion time for pParticle
    */
  private int insertionTime;
  
  /**
   *  Insertion Node Id for pParticle
   */
 public String _insertStationName;
  
  /**
    *  gets x location in Channel corresponding to upnode and
    *  downnode.
    */
  private final float getXLocationInChannel(Channel c){
      if (c.getUpNodeId() == nd.getEnvIndex())
    	  return 0.0f;
      if (c.getDownNodeId() == nd.getEnvIndex())
    	  return c.getLength();
      PTMUtil.systemExit("the node: " + PTMHydroInput.getExtFromIntNode(nd.getEnvIndex()) 
    		  				+ "doesn't match with Channel: "+PTMHydroInput.getExtFromIntChan(c.getEnvIndex())
    		  				+ ", system exit.");
      return MISSING;
  }

  /**
    *  moves x location in Channel by a repositioning factor
    *  from the closest Node.
    *  this should be in BasicJunctionDispatcher Xiao
    */
  /* 
   * move to SalmonBasicBahavior.java
  private float getPerturbedXLocation(){
    float newXPosition = 0.0f;
    if (wb.getPTMType() ==  Waterbody.CHANNEL) {
      if (((Channel)wb).getUpNodeId() == nd.getEnvIndex())
      newXPosition = channelLength * repositionFactor;

      if (((Channel)wb).getDownNodeId() == nd.getEnvIndex())
      newXPosition= ((Channel)wb).getLength() - (channelLength * repositionFactor);
    }
    return newXPosition;
  }
  */
  //TODO need to clean up
  // this function is overridden in the BehavedParticle class  
  protected void checkHealth(){
    // used for behavior
  }
  
  
  /**
    *  String representation
    */
  public String toString(){
    String rep = Id + " ";
    if (this.wb != null) rep += this.wb.getEnvIndex() + " ";
    else rep += -1 + " ";
    if (this.inserted){
      rep += this.x + " " 
           + this.y + " "
           + this.z + " ";
    }
    else{
      rep += -1.0f + " " 
           + -1.0f + " "
           + -1.0f + " ";
    }
    rep += this.insertionTime + " ";
    if (this.nd != null)
      rep += this.nd.getEnvIndex() + " ";
    else
      rep += -1 + " ";
    return rep;
  }

  public void fromString(String rep){
    StringTokenizer sToken = new StringTokenizer(rep);
    try {
      String token = sToken.nextToken();
      Id = (new Integer(token)).intValue();
      token = sToken.nextToken();
      int wbNum = (new Integer(token)).intValue();
      
      if (wbNum !=  -1){
        this.wb = Globals.Environment.getWaterbody(wbNum);
        this.inserted = true;
      }
      else{
        this.wb = null;
        this.inserted = false;
      }
      
      token = sToken.nextToken();
      x = (new Float(token)).floatValue();
      token = sToken.nextToken();
      y = (new Float(token)).floatValue();
      token = sToken.nextToken();
      z = (new Float(token)).floatValue();
      token = sToken.nextToken();
      insertionTime = (new Integer(token)).intValue();
      token = sToken.nextToken();
      int nodeNum = (new Integer(token)).intValue();
      if(nodeNum != -1) this.nd = Globals.Environment.getNode(nodeNum);
    }catch( NoSuchElementException e){
      System.out.println("Exception while parsing particle string representation");
    }
  }  
  
  private void setParticleDead(){
	  if (observer == null)
		  PTMUtil.systemExit("try to take away particle from the system, but oberser is not set.  system exit");
	  isDead = true;
	  observer.observeDeath(this);
  }

  // array applied for the convenience of vars transfer 
  private float [] cL = new float[1];
  private float [] cW = new float[1];
  private float [] cD = new float[1];
  private float [] cV = new float[1];
  private float [] cA = new float[1];
  private static TravelTimeOutput _tto = Globals.Environment.getBehaviorInputs().getTravelTimeOutput();
    
}


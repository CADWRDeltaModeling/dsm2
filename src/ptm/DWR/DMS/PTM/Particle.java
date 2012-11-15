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
  *  a Waterbody. Most of the movement functions are seprated to make this
  *  a modular class. The only public function needed to use this class is
  *  the updateXYZposition function.
  *  <p>
*Particle's junction decision is based on flow ratios.<br>
 * 
 *  A Particle's swimming in the Reservoir is not simulated instead it is assumed
 *  that a Particle entering a Reservoir is fully mixed and therefore it is possible
 *  that a Particle entering a Reservoir could exit the Reservoir from some other
 *  Node in the next time step giving it almost the speed of light !!! One solution
 *  may be to keep track of a Particle's postion for the first few time steps in the
 *  Reservoir or some other method. Any other solutions can be mailed to use folks.<p>
 * 
 * 
 *  CODING NOTES<p>
 * 
 *  CODING NOTES: POSITION DETERMINATION<br>
 *  A Particle is modelled as a non interacting object.<br>
 * 
 *  Position in any direction is calculated by adding those components of various
 *  displacements over the time step.<p>
 * 
 *  For example
 * <p>
 *  DX = DX_External_Deterministic + DX_External_Random +
 * 
 *  DX_Internal_Deterministic + DX_Internal_Random
 * 
 *  This leads to a more modular approach and helps one to seperate out the
 *  effect of different components of movement
 * <p>
 *  CODING NOTES: BOUNDARY REFLECTIONS
 *  A Particle during its movement may step out of the boundaries of a Waterbody. This
 *  effect is minimized by choosing a sub time step small enough to limit this jump to
 * <p>
 *  However when the Particle does step out of bounds it is bounced back the extraeous
 *  distance is travelled in a manner similar to a ball bouncing off the floor.
 * <p>
 * 
 * 
 *  CODING NOTES: CROSSING RESERVOIRS<br>
 *  The total volume of the Reservoir = Vtotal
 *  Flow volume out of Node i = Vi
 *  Then the probability that a Particle will leave the Reservoir through a
 *  certain Node is proportional to the ratio of Vi:Vtotal.
 * @author Nicky Sandhu
 * @version $Id: Particle.java,v 1.6.6.1 2006/04/04 18:16:25 eli2 Exp $
 * 
 */
public class Particle{
  /**
    *  unique Particle identity
    */
  public int Id;
  
  /**
    *  x position is the distance along the length of the Channel.
    *  direction being from upnode to downnode direction
    */
  public float x;
  
  /**
    *  y position is the distance from the center line.
    *  direction being to the right side if one is facing the +ve x direction.
    */
  public float y;
  
  /**
    *  z position is the distance from the bottom of the Channel.
    *  direction being the direction from the bottom to the top.
    */
  public float z;
  
  /**
    *  age of Particle in minutes since insertion
    */
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
    *  creates a default Particle
    *  The random number generator is initialized to random_seed
    *  The vertical/transverse profiles are set to true
    */
  public Particle(ParticleFixedInfo pFI){
    totalNumberOfParticles++; 
    Id = totalNumberOfParticles;
    if (DEBUG) System.out.println("Initializing particle " + Id);
    if(Id == 1) Particle.setFixedInfo(pFI);
    if (DEBUG) System.out.println("Initializing static info for particle ");
    first=true;
    inserted=false;
    Particle.dfac=0.1f;
    age=0;
	wb = null;
    //todo: eli did this work?
	//wb = NullWaterbody.getInstance();
    nd = null;
    isDead = false;
    if (DEBUG) System.out.println("Fall velocity");
    //    fallvel = pFI.getFallVelocity();
    //    behaviorData = pFI.getBehavior();
    
  }
  
  
  /**
    *  sets the location of Particle by identifying the Waterbody
    *  it is in and the co-ordinate position w.r.t the Waterbody
    */
  public final void setLocation(Waterbody w, float xPos, float yPos, float zPos){
    wb = w;
    x = xPos;
    y = yPos;
    z = zPos;
  
  }
  
  /**
    *  sets the location of Particle by Node Id # and random positioning
    *  of Particle within the Waterbody it enters thereupon.
    */
  public final void setLocation(Node n){
    nd = n;
  }
  
  
  /**
    *  sets fixed info for Particle
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
    // Particle.behavior = pFI.getBehaviorExists();
    Channel.useVertProfile=pFI.doVerticalProfile();
    Channel.useTransProfile=pFI.doTransverseProfile();
    Channel.constructProfile();
    Channel.constructProfile();
    // if (DEBUG) System.out.println("set random seed");
    if( randomNumberGenerator == null)
      randomNumberGenerator = new Ranecu(pFI.getRandomSeed());
  }
  
  
  /**
    *  installs observer.
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
  
  
  /**
    * gets the location of Particle by returning a pointer to the Waterbody the
    * recursionCounter=0;
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
        px[0] = -1;
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
  
  
  /**
    *  updates position of Particle.
    */
  public final void updatePosition(float delT){
  	particleWait = false;  //set or reset particle wait variable
    if (DEBUG) System.out.println("In update position for particle " + this);
    if(inserted){
      recursionCounter=0;
      updateXYZPosition(delT);
      updateOtherParameters(delT);
      //      System.out.println("update "+Id);
      if(! isDead) checkHealth();
      //      if (Id == 1) System.out.println(Id+" "+age+" "+getFallVel());
      
    }    
    else if (!inserted && Globals.currentModelTime >= insertionTime) {
      if ( (Globals.currentModelTime - insertionTime)/60.0 > delT )
        warning("Particle insertion time specification may be incorrect");
      insert();
      recursionCounter=0;
      //if (DEBUG) 
      updateXYZPosition(delT);
	  updateOtherParameters(delT);
    }
  }
  
  
  /**
    *  insertion time and insertion Node
    */
  public final void setInsertionInfo(int particleInsertionTime, Node injectionNode){
    this.insertionTime = particleInsertionTime;
    setLocation(injectionNode);
  }
  
  
  /**
    *  get recent Node
    */
  public final Node getRecentNode(){ return nd; }
  
  
  /**
    *  get current Waterbody
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
    *  Limiting factor for the movement during one time step due to mixing.
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
    *  A pointer to the Waterbody where the particle resides
    */
  protected Waterbody wb;
  
  /**
    *  A pointer to the Node the particle enters.
    */
  protected Node nd;
  
  /**
    *  time left for completing the current time step
    */
  protected float tmLeft;
  
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
	 *  has been turned off.  The Particle is asked to remain at its current
     *  position.
  */
	private boolean particleWait;

  /**
    *  Gaussian random number generator for y and z dispersive movements
    */
  protected static RandomElement randomNumberGenerator;
  
  /**
    *  Particle observer
    */
  protected ParticleObserver observer;
  
  /**
    *  updates the Particle postion for the given time step
    *  The Particle is moved for the time step given
    *  The new position of the Particle is available as
    *  Particle.x, Particle.y and Particle.z
    */
  protected final void updateXYZPosition(float delT){
    if( wb.getPTMType() ==  Waterbody.CHANNEL) {
      if (DEBUG) System.out.println("Particle " + this + " in channel " + wb.getEnvIndex() );
      //get minimum time step
      int numOfSubTimeSteps = getSubTimeSteps(delT);
      float tmstep = delT/numOfSubTimeSteps;
    
      tmLeft=delT;

      if(Macro.APPROX_EQ( y, MISSING) || Macro.APPROX_EQ(z,MISSING)) {
    	  setYZLocationInChannel();
      }
    
      while( tmLeft >= tmstep && isDead == false){
    	  age+=tmstep;
    	  
    	  updateAllParameters(tmstep);
    	  if (particleWait == false){
    		  // gets the x,y, and z position of the Particle after time step
    		  x=calcXPosition(tmstep);
    		  if ( wb.getPTMType() != Waterbody.CHANNEL ) return;
    		  // after recursion this may be true.
    		  if ( tmLeft >= tmstep && isDead == false ) {
    			  y=calcYPosition(tmstep);
    			  z=calcZPosition(tmstep);
    		  }
    	   }
	       // update number of time steps taken
	       tmLeft -= tmstep;
      }// end while
    } // end if(wb.getPTMType() ==  CHANNEL) 
    else if (wb.getPTMType() ==  Waterbody.RESERVOIR){
      if (DEBUG) System.out.println("Particle " + this + " in reservoir " + wb.getEnvIndex() );
      tryCrossReservoir(delT); 
    }
    else if ( wb.getPTMType() == Waterbody.CONVEYOR){
      if (DEBUG) System.out.println("Particle " + this + " in conveyor " + wb.getEnvIndex() );
      // zero time delay
      moveInConveyor(delT);
    }
    else if(wb.getPTMType() ==  Waterbody.BOUNDARY) {
      if (DEBUG) System.out.println("Particle " + this + " in boundary " + wb.getEnvIndex() );
      isDead=true;
    }
  }
  
  
  /**
    *  This is called after Particle returns from a Reservoir and
    *  needs a random y and z positioning and x corresponding to
    *  upnode or downnode.
    */
  protected final void setXYZLocationInChannel(){
    if (wb.getPTMType() == Waterbody.CHANNEL) {
      x=getXLocationInChannel();
      //y = MISSING; z = MISSING;
      if (first) updateChannelParameters();
      setYZLocationInChannel();
    }else {
      x=MISSING; y=MISSING; z=MISSING;
    }
  
  }
  
  
  /**
    *  Generates random numbers for y and z positioning
    */
  protected final void setYZLocationInChannel(){
      y = ((Channel)wb).getWidth(x)*(wb.getRandomNumber()-0.5f);
      z = ((Channel)wb).getDepth(x)*wb.getRandomNumber();
  }
  
  
  /**
    *  X Position calculation for time step given
    */
  protected final float calcXPosition(float timeStep){
      // get latest position
      // add all the individual displacements to get total displacement
      float xPos = this.x + calcXDisplacementExtDeterministic(timeStep)
                          + calcXDisplacementExtRandom(timeStep)
                          + calcXDisplacementIntDeterministic(timeStep)
                          + calcXDisplacementIntRandom(timeStep);
      
      // nodeReached updates the Node to new Node
      if ( isNodeReached(xPos) == true ) {
         float timeToReachNode = calcTimeToNode(xPos);
         // calculate time to reach Node
         tmLeft-=timeToReachNode;
         age = age - timeStep + timeToReachNode;
         
         // block particle before it enters a node, with filter operation 0
         if (nd.inFilter(wb)) {
        	 tmLeft=0;
        	 age = age - timeToReachNode + timeStep;// Kijin: should this be 2*timeStep
        	 if (xPos <= 0.0f){// upstream
        		 xPos = 0;
        	 } else if (xPos >= channelLength){// downstream
      		 xPos = channelLength;
      		 }
         }
         else {
      	 // make decision on what wb to be entered
      	 makeNodeDecision();
      	 }
         
         // if (recursionCounter++ > 5) error("Too many recursions in calcXPosition(float)");
         if (recursionCounter++ > 5) {
           if (repositionFactor < MAX_R_F){
         	 repositionFactor += RFIncrement;
      	   System.out.println("Reposition Factor set to "+repositionFactor+" for particle "+getId()+" at node "+((Channel)wb).getUpNodeId());
           }
           recursionCounter = 0;
         }
         if ( tmLeft > 1.0e-3f )  updateXYZPosition(tmLeft);
         return x;
      }// if ( nodeReached() == true )
      return xPos;
  }
  
 
  /**
    *  Y Position calculation for time step given
    */
  protected final float calcYPosition(float timeStep){
    // get current position
    float yPos=this.y; 
  
    // calculate new position
    yPos += calcYDisplacementExtDeterministic(timeStep)
          + calcYDisplacementExtRandom(timeStep)
          + calcYDisplacementIntDeterministic(timeStep)
          + calcYDisplacementIntRandom(timeStep);
  
    // reflection from ends of Channel
    int k=0, MAX_BOUNCING=100; // max num of iterations to do reflection
    float halfWidth = channelWidth/2.0f;
    while ((yPos < -halfWidth || yPos > halfWidth) && (k<=MAX_BOUNCING)){
      if (yPos < -halfWidth) yPos = -halfWidth + (-halfWidth-yPos);
      if (yPos > halfWidth ) yPos =  halfWidth - (  yPos-  halfWidth);
      k++;
    }
    
    if (k > MAX_BOUNCING)     
      error("Too many iterations in calcYPosition()");

    return (yPos);
  }  
  
  /**
    *  Z Position calculation for time step given
    */
  protected final float calcZPosition(float timeStep){
    // get current position
    float zPos=this.z;
  
    // calculate position after timeStep
    zPos = zPos + calcZDisplacementExtDeterministic(timeStep)
                + calcZDisplacementExtRandom(timeStep)
                + calcZDisplacementIntDeterministic(timeStep)
                + calcZDisplacementIntRandom(timeStep);
  
    // reflections from bottom of Channel and water surface
    int k=0;
    int MAX_BOUNCING = 100;
    while ((zPos < 0.0 || zPos > channelDepth) && (k<=MAX_BOUNCING)){
      if (zPos < 0.0) zPos=-zPos;
      else if (zPos > channelDepth) zPos = channelDepth - (zPos - channelDepth);
      k++;
    }
     
    if (k > MAX_BOUNCING) {
      error("Too many iterations in calcZPosition()");
    }
    return (zPos);
  }
  
  
  /**
    *  Externally induced Deterministic
    */
  protected float calcXDisplacementExtDeterministic(float timeStep){
    float xVel = calcXVelocityExtDeterministic( );
    return (xVel*timeStep);
  }
  
  
  /**
    *  Externally induced Random
    */
  protected  float calcXDisplacementExtRandom(float timeStep){return 0.0f;}
  
  
  /**
    *  Internally induced Deterministic
    */
  protected  float calcXDisplacementIntDeterministic(float timeStep){return 0.0f;}
  
  
  /**
    *  Internally induced Random
    */
  protected  float calcXDisplacementIntRandom(float timeStep){return 0.0f;}
  
  
  /**
    *  Externally induced Deterministic
    */
  protected float calcYDisplacementExtDeterministic(float timeStep){return 0.0f;}
  
  
  /**
    *  Externally induced Random
    */
  protected float calcYDisplacementExtRandom(float timeStep){

    // get a gaussian distributed random number for y mixing
    float dypos = (float) (randomNumberGenerator.gaussian()*Etdt);

    // if transverse mixing allowed return the random y movement
    if (transMove) return(dypos);
    else return 0.0f;
  }
  
  
  /**
    *  Internally induced Deterministic
    */
  protected  float calcYDisplacementIntDeterministic(float timeStep){return 0.0f;}
  
  
  /**
    *  Internally induced Random
    */
  protected float calcYDisplacementIntRandom(float timeStep){return 0.0f;}
  
  
  /**
    *  Externally induced Deterministic
    */
  protected  float calcZDisplacementExtDeterministic(float timeStep){
    //    return(-getFallVel()*timeStep);
    return 0.0f;
  }
  
  
  /**
    *  Externally induced Random
    */
  protected float calcZDisplacementExtRandom(float timeStep){
  
    // get the random mixing component
    float dz = (float) (randomNumberGenerator.gaussian()*Evdt);
    
    if (vertMove) return(dz);
    else return 0.0f;
  }
  
  
  /**
    *  Internally induced Deterministic
    */
  protected  float calcZDisplacementIntDeterministic(float timeStep){return 0.0f;}
  
  
  /**
    *  Internally induced Random
    */
  protected  float calcZDisplacementIntRandom(float timeStep){return 0.0f;}
  
  
  /**
    *  Externally induced Deterministic
    */
  protected  float calcXVelocityExtDeterministic(){
    return( ( (Channel)wb).getVelocity(x,y,z, channelVave, channelWidth, channelDepth));
  }
  
  
  /**
    *  Externally induced Random
    */
  protected  float calcXVelocityExtRandom() { return 0.0f;}
  
  
  /**
    *  Internally induced Deterministic
    */
  protected  float calcXVelocityIntDeterministic(){ return 0.0f; }
  
  
  /**
    *  Internally induced Random
    */
  protected  float calcXVelocityIntRandom() { return 0.0f;}
  
  
  /**
    *  Makes Node decision on which Waterbody to enter into next...
    */
  protected void makeNodeDecision(){
  
    // Node is the current Node in which Particle enters
    // get total outflow from Node and multiply it by random number
    // send message to observer about change 
    if (observer != null) 
      observer.observeChange(ParticleObserver.NODE_CHANGE,this);
    float outflow = nd.getTotalEffectiveOutflow(false);

    // if the Node is at a Node with zero flow, for example at the
    // end of a slough, then move the pParticleinto the Channel a
    // small amount.
    if (outflow == 0.0f && nd.getNumberOfWaterbodies() == 1) {
      x = getPerturbedXLocation();
      return;
    }
    //float out2 = outflow;
    
    float rand = nd.getRandomNumber();
    outflow = rand*outflow;
  
    float flow=0.0f;
  
    if(outflow == 0.0){
    	particleWait = true;
    	return;
    }
    
    // add flow from each Node till the flow is greater than the outflow
    int waterbodyId = -1;
	do {
	  waterbodyId ++;
	  // this conditional statement added to exclude seepage
	  // this should be read in as an argument
	  //@todo: disabled this feature
      //if(nd.getWaterbody(waterbodyId).getAccountingType() != flowTypes.evap){
      flow += nd.getFilterOp(waterbodyId) * nd.getOutflow(waterbodyId);
      //}
    }while ( flow < outflow && 
	    waterbodyId < nd.getNumberOfWaterbodies());
  
    // get a pointer to the water body in which Particle enters
    wb = nd.getWaterbody(waterbodyId);
    // send message to observer about change 
    if (observer != null) 
      observer.observeChange(ParticleObserver.WATERBODY_CHANGE,this);
    // set x as beginning of Channel...
    x=getXLocationInChannel();
  }
  
  
  /**
    *  updates pParticleposition after calling makeReservoirDecision
    */
  protected final void tryCrossReservoir(float timeStep){
    // adjust time and age
    age+=timeStep;
    tmLeft = tmLeft-timeStep;
  
    //get a pointer to the Node into which pParticleenters from Reservoir
    nd =  makeReservoirDecision(timeStep);
  
    if ( nd != null){
      //makes decision of which Waterbody to go into
      makeNodeDecision();
      // set previous depth and width to current depth and width
      first=true;
      //? what should be new x,y,z for the pParticlein the Waterbody?
      setXYZLocationInChannel();
    }
  }
  /**
    * moves to the Node with inflow and decides where to go from there...
    */
  protected void moveInConveyor(float delT){
    Conveyor c = (Conveyor)wb;
    if (DEBUG) System.out.println("Particle in conveyor: " + c );
    float flow = c.getFlowInto(0);
    //TODO
    if ((flow > 0) && !(c.getNode(1).inFilter(wb)))// if no filter
      setLocation( c.getNode(1) );
    else
      setLocation( c.getNode(0) );
    if (DEBUG) System.out.println("Current node: " + nd);
    makeNodeDecision();
    if (DEBUG) System.out.println("Current wb: " + wb);
    if (DEBUG) System.out.println(" wb type: " + wb.getPTMType() 
				  + ", waterbody.CHANNEL=" + Waterbody.CHANNEL);
    if ( wb.getPTMType() == Waterbody.CHANNEL) {
      first=true;
      setXYZLocationInChannel();
    }
  }  
  
  /**
    *  returns Node to which pParticletransitions or null
    */
  protected Node makeReservoirDecision(float timeStep){
  
    //Get total volume of Reservoir and multiply by random number
    float totvol = ((Reservoir)wb).getTotalVolume(timeStep);
    float rand = wb.getRandomNumber();
    totvol = totvol*rand;
  
    //Get flow volume out first Node
    int nodeId=-1;
    float flowvol=0.0f;
    //while outflow volume over last i nodes is less than random value
    do {
      nodeId++;
      nd = wb.getNode(nodeId);
      if(nd.inFilter(wb)){continue;} //skip the node if it has a res->nd filter
      flowvol += Math.max(0.0f,((Reservoir)wb).getVolumeOutflow(nodeId, timeStep));
    }
    while( flowvol < totvol && nodeId < wb.getNumberOfNodes()-1);
  
    if(flowvol > totvol) return wb.getNode(nodeId);
    else return null;
  }
  
  
  /**
    *  generates error
    */
  protected final void error(String msg){
    System.out.println( "An error occurred in particle.cc: " );
    System.out.println( "ERROR: " + msg );
    System.exit(-1);
  }
  
  
  /**
    *  generates warning
    */
  protected final void warning(String msg){
    System.out.println( "WARNING: " + msg + " !" );
  }
  
  
  /**
    *  inputs state of pParticle    */
  /**
    *  outputs state to ostream
    */

  private static final float Emin=0.0001f;
  
  private static final int MAX_NUM_OF_SUB_TIME_STEPS=10000;
  private static final int MISSING = -99999;
  
  private final void insert(){
  if (observer != null) 
    observer.observeChange(ParticleObserver.INSERT,this);
  inserted=true;
  makeNodeDecision();
  setXYZLocationInChannel();
  }
  
  private final void updateAllParameters(float tmstep){
    //updates length,width,depth, previousdepth, previouswidth
    updateChannelParameters();
    //updates particles: depth, width, Ev, Evdt, Etdt
    updateParticleParameters(tmstep); 
  }
  
  private final void updateChannelParameters(){
    ((Channel)wb).updateChannelParameters(x,cL,cW,cD,cV,cA);

    channelLength = cL[0];
    channelWidth  = cW[0];
    channelDepth  = cD[0];
    channelVave   = cV[0];
    channelArea   = cA[0];
    if (first) {
      previousChannelDepth=channelDepth;
      previousChannelWidth=channelWidth;
      first=false;
    }
  
  }
  
  protected void updateOtherParameters(float delT){
  }    
  
  private final void updateParticleParameters(float timeStep){
    z = z*channelDepth/previousChannelDepth; // adjust for changing depth
    y = y*channelWidth/previousChannelWidth; // adjust for changing width
    // set previouses to the news..

    previousChannelDepth=channelDepth;
    previousChannelWidth=channelWidth;
  
    // recalculate mixing co-efficients..
    // new calculation of diffusion (test code)
    Ev = Math.abs(verticalConstant*channelDepth*channelVave);
    Ev = Math.max(Ev,Emin);
    //System.out.println("vert_const="+verticalConstant+"channelDepth="+channelDepth+" Vave="+channelVave+" dt="+timeStep+" Ev="+Ev);    
    
    Evdt=(float) Math.sqrt(2.0f*Ev*timeStep);
    Etdt=Evdt*CtCv;
  }
  
  
  private final boolean isNodeReached(float xpos){
    if (xpos < 0.0f) {// crossed starting Node of Channel
      // get a pointer to the Node in question
      nd = wb.getNode(Channel.UPNODE);
    }
    else if (xpos > channelLength) { // crossed end Node of Channel
      // get a pointer to the Node in question
      nd = wb.getNode(Channel.DOWNNODE);
    }
    else return false;
	// if the pParticlehas been asked to wait then return false
	if(particleWait)
		return false;
    
	return true;
  }
  
  private final float calcTimeToNode(float xpos){
  
    float dT = 0.0f;
  
    // get deterministic velocity sum in X direction
    float xVel = (calcXVelocityExtDeterministic()+calcXVelocityIntDeterministic());
    float xStart = x;
  
    // calculate time taken to reach Node
    if (xpos < 0.0f) {
      dT=-xStart/xVel;
    }
    else if (xpos > channelLength) {
      dT=(channelLength-xStart)/xVel;
    }
  
    return dT;
  }
  
  
  private final int getSubTimeSteps(float timeStep){
  
    float minTimeStep = timeStep;
  
    if ((vertMove!=true) && (transMove!=true))
      minTimeStep =timeStep;
    else
      minTimeStep = getMinTimeStep();
  
    int numOfSubTimeSteps=1;
    if(minTimeStep < timeStep) numOfSubTimeSteps=(int) (timeStep/minTimeStep+1);
    else numOfSubTimeSteps=1;
    //System.out.println("timeStep="+timeStep+" minTimeStep="+minTimeStep);
    if (numOfSubTimeSteps > MAX_NUM_OF_SUB_TIME_STEPS){
      warning("Number Of Sub Time Steps exceeds a maximum of "+MAX_NUM_OF_SUB_TIME_STEPS);
      return MAX_NUM_OF_SUB_TIME_STEPS;
    }
    else 
      return numOfSubTimeSteps;
  }
  
  private final float getMinTimeStep(){
  
    // float terminalVelocity=Math.max(getFallVel(),1.0e-10f); //fallvel -. input behaviour
    float terminalVelocity = getTerminalVelocity();
    // get a factor of the Channel length the maximum distance
    // a pParticlecan travel in a time step
    updateAllParameters(0.0f); // time step not really a factor here.
    float dzmax=dfac*channelDepth;
    float dymax=dfac*channelWidth;
    float dtz=Math.min(dzmax/terminalVelocity,dzmax*dzmax/Ev);
    float dty = (dymax*dymax)/(CtCv*CtCv*Ev);
  
    // System.out.println("dzmax="+dzmax+"; dymax="+dymax+"; Ev="+Ev+"; CtCv="+CtCv+
    //  					   "; dtz="+dtz+"; dty="+dty);

    float minTimeStep = 0.0f;
  
    if ((vertMove==true) && (transMove==true)) minTimeStep=Math.min(dty,dtz);
    else if ((vertMove==true) && (transMove!=true)) minTimeStep=dtz;
    else if ((vertMove!=true) && (transMove==true)) minTimeStep=dty;
    
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
    */
  private static int recursionCounter;
  
  /**
    *  Factor used in repositioning when a no outflow condtion is encountered
    */
  private float repositionFactor = 0.00001f;

  /**
    *  Reposition Factor increment
    */
  private float RFIncrement = 0.0001f;

  /**
    *  Maximum Repositioning Factor
    */
  private float MAX_R_F = 0.01f;

  /**
    *  Location state variables
    */
  private float previousChannelDepth, previousChannelWidth;
  
  /**
    *  Insertion time for pParticle    */
  private int insertionTime;
  
  /**
    *  gets x location in Channel corresponding to upnode and
    *  downnode.
    */
  private final float getXLocationInChannel(){
    float newXPosition=0.0f;
    if (wb.getPTMType() ==  Waterbody.CHANNEL) {
      if (((Channel)wb).getUpNodeId() == nd.getEnvIndex())
	    newXPosition=0;
      if (((Channel)wb).getDownNodeId() == nd.getEnvIndex())
	    newXPosition= ((Channel)wb).getLength();
    }
    return newXPosition;
  }

  /**
    *  moves x location in Channel by a repositioning factor
    *  from the closest Node.
    */
  private float getPerturbedXLocation(){
    float newXPosition=0.0f;    
    if (wb.getPTMType() ==  Waterbody.CHANNEL) {
      if (((Channel)wb).getUpNodeId() == nd.getEnvIndex())
	    newXPosition = channelLength * repositionFactor;

      if (((Channel)wb).getDownNodeId() == nd.getEnvIndex())
	    newXPosition= ((Channel)wb).getLength() - (channelLength * repositionFactor);
    }
    return newXPosition;
  }
  
  protected void checkHealth(){
    // used for behavior
  }

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

  private float [] cL = new float[1];
  private float [] cW = new float[1];
  private float [] cD = new float[1];
  private float [] cV = new float[1];
  private float [] cA = new float[1];


}


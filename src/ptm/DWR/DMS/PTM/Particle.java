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
	    *  age of Particle in seconds since insertion
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
	  public static boolean DEBUG = false;
  
	  /**
	   *  Total number of particles in the system.
	   */
	  static int totalNumberOfParticles;
	 
	  /**
	   	*  true if it is the first update of position after insertion
	   	*/
	  boolean first;
	 
	  /**
	   	*  the Waterbody which the particle currently stays in
	   	*/
	  Waterbody wb;
	 
	  /**
	   	*  the Node which the particle was inserted or just passed
	   	*/
	  Node nd;
	 
	  /**
	   	*  A Particle may be asked to wait instead of move with the velocity
	   	*  An example is when seep flows control at a Node and Particle to seep
	   	*  has been turned off. The Particle is asked to remain at its current
	   	*  position.
	   	*/
	  boolean particleWait;
	  /**
	   	*  Particle observer
	   	*/
	  ParticleObserver observer;
	 
	  /**
	   *  Particle Helpers
	   */
	  private RouteHelper _routeHelper;
	  private SwimHelper _swimHelper;
	  private SurvivalHelper _survivalHelper;
	  private int _confusionFactor = 1;
	  private float _swimmingVelocity = 0.0f;
	  static final int MISSING = -99999;
		
	  /**
	   *  Factor used in repositioning when a no outflow condition is encountered
	   */
	  protected float repositionFactor = 0.00001f;
	  
	  /**
	   *  Insertion time for pParticle
	   */
	  private long insertionTime;
	  
	  /**
	   *  Insertion Node Id for pParticle
	   */
	  private String _insertStationName;
	  private long _swimmingTime; 
	  // time in second used by a particle from the beginning of a time step
	  private float _timeUsedInSecond;
	  /**
	   *  Creates a default Particle<br>
	   *  The random number generator is initialized to random_seed<br>
	   *  The vertical/transverse profiles are set to true
	   */
	  public Particle(ParticleFixedInfo pFI){
		  totalNumberOfParticles++; 
		  Id = totalNumberOfParticles;
		  if (DEBUG) System.out.println("Initializing particle " + Id);
		  first = true;
		  inserted = false;//particle not in the system yet
		  age = 0;
		  wb = null;
		  nd = null;
		  isDead = false;
		  _swimmingTime = 0;
		  _timeUsedInSecond = 0.0f;
	  }
	  /**
	   *  Sets the location of Particle by Node Id # and random positioning
	   *  of Particle within the Waterbody it enters there upon.
	   */
	  public final void setLocation(Node n){
	  		nd = n;
	  }
	  /**
	   *  Installs observer.<br>
	   *  This observer observes events such as change from
	   *  one Waterbody to another and sends a message to other
	   *  objects from there.
	   */
	  public final void installObserver(ParticleObserver ob){observer = ob;}
	  /**
	   *  un-install observer, ie., sets it to null. This may save some
	   *  runtime, however no flux information can be gleaned from the run.
	   */
	  public final void uninstallObserver(){observer = null;}
	  public final void installRouteHelper(RouteHelper routeHelper){_routeHelper = routeHelper;}
	  public final void uninstallRouteHelper(){_routeHelper = null;} 
	  public final void installSwimHelper(SwimHelper swimHelper){_swimHelper = swimHelper;}
	  public final void uninstallSwimHelper(){_swimHelper = null;}	
	  public final void installSurvivalHelper(SurvivalHelper survivalHelper){_survivalHelper = survivalHelper;}
	  public final void uninstallSurvivalHelper(){_survivalHelper = null;}
	  
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
	   *  returns the current particle time
	   */
	  public final long getCurrentParticleTime(){return Globals.currentModelTime + Math.round(_timeUsedInSecond/60.0f);}
	  public final long getInsertionTime(){return insertionTime;}
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
	  public final void updatePosition(float delT){ //delT in seconds
		  if (isDead) return;
		  if (observer == null)
			  PTMUtil.systemExit("expect an observer be installed, but not, system exit");
		  particleWait = false;  //set or reset particle wait variable
		  if (DEBUG) System.out.println("In updating position for particle " + this);
	
		  // insertion time is checked in PTMEnv.
		  // if a insertion time is earlier than the model start time, simulation exits because a particle needs hydro info when inserted.
		  _timeUsedInSecond = 0;
		  long currPTime = getCurrentParticleTime();
		  if(!inserted && currPTime >= insertionTime){ //when current time reach insertion time
			  _swimHelper.insert(this);
			  if (DEBUG) System.out.println("Inserted particle No. " + Id); 
		  }
	
		  if (inserted){
			  if(currPTime >= _swimmingTime)
				  _swimHelper.helpSwim(this, delT);
			  else{
				  //rearing holding, wait a time step
				  age += delT;
				  // particle _timeUsedInsecond will be reset @ the beginning of the loop
				  //TODO clean up later	
				  
				  if (wb.getType() == Channel.CHANNEL && wb.getEnvIndex() < 801)
					  System.err.println(Id + " " +(getCurrentParticleTime()-56300000)+" " 
							  + PTMHydroInput.getExtFromIntNode(nd.getEnvIndex())+" "
							 +PTMHydroInput.getExtFromIntChan(wb.getEnvIndex())+" "
							 +(Globals.currentModelTime-56300000) + " "
							 //+PTMUtil.modelTimeToCalendar(_particleCurrentTime).getTime() + " "
							 //+ PTMUtil.modelTimeToCalendar(_swimmingTime).getTime() +" "
							 +"End rearing");
				  else
					  System.err.println(Id + " " +(getCurrentParticleTime()-56300000)+" " 
							  + PTMHydroInput.getExtFromIntNode(nd.getEnvIndex())+" "
							 +wb.getEnvIndex()+" "
							 +(Globals.currentModelTime-56300000) + " "
							 //+PTMUtil.modelTimeToCalendar(_particleCurrentTime).getTime() + " "
							 //+ PTMUtil.modelTimeToCalendar(_swimmingTime).getTime() +" "
							 +"End rearing no channel");
							 
				 
			  }		  
		  }
	  }
	 
	  /**
	   *  Insertion time and insertion Node
	   */
	  public final void setInsertionInfo(int particleInsertionTime, Node inNode){
	    this.insertionTime = particleInsertionTime;
	    nd = inNode;
	  }
	  
	  public final void setInsertionInfo(long particleInsertionTime, Node inNode, Waterbody inWb, int distance, String name){
		    this.insertionTime = particleInsertionTime;
		    wb = inWb;
		    nd = inNode;
		    x = distance;
		    _insertStationName = name;
	  }
	  /**
	   *  Get the recent Node which particle just passed or was inserted in 
	   */
	  public final Node getRecentNode(){ return nd; }
	  
	  /**
	   *  Get current Waterbody
	   */
	  public final Waterbody getCurrentWaterbody(){ return wb;}
	  public SwimHelper getSwimHelper(){return _swimHelper;}
	  public RouteHelper getRouteHelper() {return _routeHelper;}
	  public SurvivalHelper getSurvivalHelper() {return _survivalHelper;}
	  public int getConfusionFactor() {return _confusionFactor;}
	  //swimming velocity here includes the confusion factor
	  public float getSwimmingVelocity() {return _swimmingVelocity;}
	  void setSwimmingVelocity(float sv){_swimmingVelocity = sv; }
	  void setConfusionFactor(int cf) {_confusionFactor = cf;}
	  public String getInsertionStation() {return _insertStationName;}
	  public void clear(){ _timeUsedInSecond = 0.0f;}
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
	
	  	}
	
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
	      _swimHelper.setXYZLocationInChannel(this);
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
	  protected final void warning(String msg){
	    System.out.println( "WARNING: " + msg + " !" );
	  }
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
	  
	  void setParticleDead(){
		  if (observer == null)
			  PTMUtil.systemExit("try to take away particle from the system, but oberser is not set.  system exit");
		  isDead = true;
		  observer.observeDeath(this);
	  }
	  // rearing holding time in minutes
	  public void setSwimmingTime(long time){
		  if (time < 0)
			  PTMUtil.systemExit("encountered negative time when tried to set Rearing Holding Time, system exit.");
		  _swimmingTime = time;
	  }
	  public void addTimeUsed(float deltaT){ _timeUsedInSecond += deltaT; }
	  //will be overridden in Arron's behaved particle
	  protected void updateOtherParameters(float delT){}
	  protected float calcZDisplacementExtRandom(float timeStep){return 0.0f;}
	  
}

/**
 *  Sets fixed info for Particle
 */
/*
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
*/

/**
  *  gets x location in Channel corresponding to upnode and
  *  downnode.
  */
/*
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
*/
/**
 *  Channel parameters
 */
//protected float channelLength, channelWidth, channelDepth, channelVave, channelArea;

/**
 *  a flag to see if vertical movement is to be allowed, in other words
 *  if vertical mixing is to be allowed
 */
//protected static boolean vertMove;

/**
 *  a flag to check if transverse movement is to be allowed.
 */
//protected static boolean transMove;

/**
 *  The transverse constant for mixing
 */
//protected static float transverseConstant;

/**
 *  The vertical constant for mixing
 */
//protected static float verticalConstant;

/**
 *  A transverse diffusivity factor based on the Darcy-Wiesbach friction factor
 */
//protected static float CtCv;

/**
 *  Limiting factor for the movement during 1 time step due to mixing.<br>
 *  Used for sub-time step calculation
 *  to prevent excessive bouncing of particles at boundaries<br>
 *  usually keep particle movement <10% channel (width, depth) in 1 sub-time step
 */
//protected static float dfac;
/**
 *  Time left for completing the current PTM input time step
 */

/**
 *  Mixing co-efficients
 */
//protected float Ev,Evdt,Etdt;

/**
 *  Falling velocity of Particle through water
 */
//protected float fallvel;
//TODO what if SwimmingInputs is null?!	clean up move to swimming behavior
//private static SwimInputs  SwimmingInputs = Globals.Environment.getBehaviorInputs().getSwimInputs();
//private static boolean IsRandomAccess = SwimmingInputs.getRandomAccess();
//private static float AccessProb = SwimmingInputs.getAccessProbability();
//if (Id == 1) setFixedInfo(pFI);
//if (DEBUG) System.out.println("Initializing static info for particle ");
// first means the particle never be in any water body
//if (DEBUG) System.out.println("Fall velocity");
//    fallvel = pFI.getFallVelocity();
//    behaviorData = pFI.getBehavior();
// age is in seconds actually
//Particle.dfac = 0.1f;
// commented out because a particle's rearing holding does not only happen at the insertion time
// but also at the beginning when a particle hits a new channel group
//if((curTime >= insertionTime +_rearingHoldingTime)
/**
 *  inputs state of pParticle
 */
/**
 *  outputs state to ostream
 */

//private static final float Emin=0.0001f;
//private static final int MAX_NUM_OF_SUB_TIME_STEPS=10000;
/**
 *  Gaussian random number generator for y and z dispersive movements
 */
// use method in PTMUtil instead
//protected static RandomElement randomNumberGenerator;
/*
// return particle age in seconds
public float getParticleAge(){
	  return age;
}
*/

/**
  *  Internally induced Random x
  */
//protected float calcXVelocityIntRandom() {return 0.0f;}
//TODO clean up not used anymore
//protected void setMeanSwimmingVelocity(float msv){_meanSwimmingVelocity = msv;}
//updateOtherParameters(delT); //TODO Why need this?
/**
 *  insert particle in the system
 */
/*
private final void insert(){
  observer.observeChange(ParticleObserver.INSERT,this);
  inserted = true;
  if (wb == null){
  	makeNodeDecision();
  	_swimHelper.setXYZLocationInChannel(this);
  }
  else{
  	 if (wb.getPTMType() == Waterbody.CHANNEL){
  		 int chId = wb.getEnvIndex();
  		 _swimHelper.setMeanSwimmingVelocity(Id, wb.getEnvIndex());   
	    	 _swimmingVelocity = _swimHelper.getSwimmingVelocity(Id, chId)
	    			 			*_swimHelper.getConfusionFactor(chId);
	    	 
	    	 _swimmingTime = _swimHelper.getSwimmingTime(Id, chId);
  	 }
  	 // if a particle is inserted in a channel instead of a node
  	 // channel number and distance x are known.  No need to calc x so pass a false 
  	 _swimHelper.setXYZLocationInChannel(this);
  	 
  	 if (p.first) _hydroCalc.updateChannelParameters(p);
			_hydroCalc.setYZLocationInChannel(p);
		}else{ 
			p.x=MISSING; p.y=MISSING; p.z=MISSING;
		}
  }
}
*/
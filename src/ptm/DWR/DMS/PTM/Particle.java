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
//import edu.cornell.RngPack.*;

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
	  //TODO change to double???
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
	  private boolean _swimVelSetInJunction = false;
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
		  _particleTrace = new BasicParticleTrace();
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
	  public final long getCurrentParticleTime(){return Globals.currentModelTime;} //+ Math.round(_timeUsedInSecond/60.0f);}
	  //TODO when return a exact current particle time it breaks the particle flux calculation.  So keep getCurrentParticleTime as it is
	  // and create a new method below for set swimming time.  Need to change the flux calculation part to make it consistant
	  public final long getCurrentParticleTimeExact(){return Globals.currentModelTime + Math.round(_timeUsedInSecond/60.0f);}
	  public final long getInsertionTime(){return insertionTime;}
	  public final boolean checkSurvival(){
	    // check survival if not survived isDead is set to true	
	    _survivalHelper.helpSurvival(this);
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
		  long currPTime = getCurrentParticleTimeExact();
		  if(!inserted && currPTime >= insertionTime){ //when current time reach insertion time
			  _swimHelper.insert(this);
			  if (DEBUG) System.out.println("Inserted particle No. " + Id); 
		  }
	
		  if (inserted){
			  // move when 1) it is a neutrally buoyant particle (swimming time never be set and no holding)
			  // or 2) fish passes holding time
			  if(_swimmingTime == 0 || currPTime >= _swimmingTime)
				  _swimHelper.helpSwim(this, delT);
			  else
				  //rearing holding, wait a time step
				  age += delT;
				  // particle _timeUsedInsecond will be reset @ the beginning of the loop
	  
		  }
		  /*
		  if (wb.getType()==Waterbody.CHANNEL && Id == 1)
			  System.err.println(PTMUtil.modelTimeToCalendar(getCurrentParticleTimeExact()).getTime()+" " + PTMHydroInput.getExtFromIntNode(nd.getEnvIndex())+" "
					 +PTMHydroInput.getExtFromIntChan(wb.getEnvIndex())+" "+x+" "+y+" "+z);
					 */
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
	
		  // decide which water body to go and set the particle with the new water body and new x
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
	  //TODO need to clean up left over from Aaron's code
	  // this function is overridden in the BehavedParticle class  
	  //protected void checkHealth(){
	    // used for behavior
	  //}
	  
	  
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
	  boolean isSwimVelSetInJunction(){return _swimVelSetInJunction;}
	  void swimVelSetInJunction(boolean sVSet){_swimVelSetInJunction = sVSet;}
	  
	  //to be used in survival model
	  private boolean _fromUpstream;
	  private BasicParticleTrace _particleTrace;
	  void addParticleTrace(long time, int wbId, int nodeId){_particleTrace.addTrack(time, wbId, nodeId);}
	  int particlePassed(ArrayList<Integer> fromWbIds, ArrayList<Integer> toWbIds){
		  return _particleTrace.particlePassed(fromWbIds, toWbIds);
	  }
	  void setFromUpstream(boolean fromUp){_fromUpstream = fromUp;}
	  boolean getFromUpstream(){return _fromUpstream;}
	  
	  
}

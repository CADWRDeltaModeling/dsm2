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

//$Id: particle.java,v 1.5.2.2 2003/10/09 19:20:55 miller Exp $
package DWR.DMS.PTM;
import java.lang.*;
import java.util.*;
import edu.cornell.RngPack.*;
/**
  * 
  *  This class is the core definition of a particle and its movement in
  *  a waterbody. Most of the movement functions are seprated to make this
  *  a modular class. The only public function needed to use this class is
  *  the updateXYZposition function.
  *  <p>
  * 
  *  FUTURE DIRECTIONS
  * 
  *  Some further classes that may be dervied later are the egg class and the
  *  fish class.
 * 
 *  Groups or schools of such objects can be defined. Interaction between
 *  particles could be handled by friend functions although some more elegant
 *  way may be devised later.
 *  <p>
 *  At some later date the node decision function should be change to
 *  probablities due to different environmental parameters and this function
 
 *  Ralph or Tara about this)
 *  Perhaps the most effective way to achieve this is by defining the node
 *  decision function in terms of probablities and have hookup functions to
 *  define the actual calculation of such probabilities.
 *  <p>
 *  particle should have a moveIn function which should be overloaded with the
 *  different types of waterbodies. This would allow some kind of double dispatching
 *  mechanism to call the appropriate function for the particle
 *  <p>
 *  dfac is a factor that limits the movement due to dispersion in one calculation time step.
 *  It is really important when using particles that have fall velocities,
 *  i.e. non-neutrally bouyant particles. There could be significant differences between similar
 *  runs with different values for this parameter.
 *  <p>
 *  CONCEPT NOTES<p>
 * 
 *  A particle's position is defined by x,y and z values and the waterbody in
 *  which it is located.<br>
 * 
 *  A particle in a pump or agricultural diversion is assumed to have died.
 * 
 *  A particle's junction decision is based on flow ratios.<br>
 * 
 *  A particle's swimming in the reservoir is not simulated instead it is assumed
 *  that a particle entering a reservoir is fully mixed and therefore it is possible
 *  that a particle entering a reservoir could exit the reservoir from some other
 *  node in the next time step giving it almost the speed of light !!! One solution
 *  may be to keep track of a particle's postion for the first few time steps in the
 *  reservoir or some other method. Any other solutions can be mailed to use folks.<p>
 * 
 * 
 *  CODING NOTES<p>
 * 
 *  CODING NOTES: POSITION DETERMINATION<br>
 *  A particle is modelled as a non interacting object.<br>
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
 *  A particle during its movement may step out of the boundaries of a waterbody. This
 *  effect is minimized by choosing a sub time step small enough to limit this jump to
 * <p>
 *  However when the particle does step out of bounds it is bounced back the extraeous
 *  distance is travelled in a manner similar to a ball bouncing off the floor.
 * <p>
 * 
 * 
 *  CODING NOTES: CROSSING RESERVOIRS<br>
 *  The total volume of the Reservoir = Vtotal
 *  Flow volume out of node i = Vi
 *  Then the probability that a particle will leave the reservoir through a
 *  certain node is proportional to the ratio of Vi:Vtotal.
 * @author Nicky Sandhu
 * @version $Id: particle.java,v 1.5.2.2 2003/10/09 19:20:55 miller Exp $
 * 
 */
public class particle{
  /**
    *  unique particle identity
    */
  public int Id;
  
  /**
    *  x position is the distance along the length of the channel.
    *  direction being from upnode to downnode direction
    */
  public float x;
  
  /**
    *  y position is the distance from the center line.
    *  direction being to the right side if one is facing the +ve x direction.
    */
  public float y;
  
  /**
    *  z position is the distance from the bottom of the channel.
    *  direction being the direction from the bottom to the top.
    */
  public float z;
  
  /**
    *  age of particle in minutes since insertion
    */
  public float age;
  
  /**
    *  keeps track of if particle alive or dead
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
    *  creates a default particle
    *  The random number generator is initialized to random_seed
    *  The vertical/transverse profiles are set to true
    */
  public particle(particleFixedInfo pFI){
    totalNumberOfParticles++; 
    Id = totalNumberOfParticles;
    if (DEBUG) System.out.println("Initializing particle " + Id);
    if(Id == 1) particle.setFixedInfo(pFI);
    if (DEBUG) System.out.println("Initializing static info for particle ");
    first=true;
    inserted=false;
    particle.dfac=0.1f;
    age=0; 
    wb = null;
    nd = null;
    isDead = false;
    if (DEBUG) System.out.println("Fall velocity");
    //    fallvel = pFI.getFallVelocity();
    //    behaviorData = pFI.getBehavior();
  }
  
  
  /**
    *  sets the location of particle by identifying the waterbody
    *  it is in and the co-ordinate position w.r.t the waterbody
    */
  public final void setLocation(waterbody w, float xPos, float yPos, float zPos){
    wb = w;
    x = xPos;
    y = yPos;
    z = zPos;
  
  }
  
  /**
    *  sets the location of particle by node Id # and random positioning
    *  of particle within the waterbody it enters thereupon.
    */
  public final void setLocation(node n){
    nd = n;
  }
  
  
  /**
    *  sets fixed info for particle
    */
  public final static void setFixedInfo(particleFixedInfo pFI){
    if (DEBUG) System.out.println("in setfixedinfo for particle");
    particle.verticalConstant = pFI.getVerticalConstant();
    if (DEBUG) System.out.println("vertical constant");
    particle.transverseConstant = pFI.getTransverseConstant();
    if (DEBUG) System.out.println("trans constant");
    particle.CtCv = (float) (Math.sqrt(transverseConstant/verticalConstant));
    if (DEBUG) System.out.println("CtCv");
    particle.vertMove = pFI.moveVertically();
    if (DEBUG) System.out.println("vert move");
    particle.transMove = pFI.moveLaterally();
    if (DEBUG) System.out.println("trans move");
    //    particle.behavior = pFI.getBehaviorExists();
    channel.useVertProfile=pFI.doVerticalProfile();
    channel.useTransProfile=pFI.doTransverseProfile();
    channel.constructProfile();
    //  if (DEBUG) System.out.println("set random seed");
    if( randomNumberGenerator == null)
      randomNumberGenerator = new Ranecu(pFI.getRandomSeed());
  
  }
  
  
  /**
    *  installs observer.
    *  This observer observes events such as change from
    *  one waterbody to another and sends a message to other
    *  objects from there.
    */
  public final void installObserver(particleObserver ob){
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
    * gets the location of particle by returning a pointer to the waterbody the
    * recursionCounter=0;
    */
  public final waterbody getLocation(float [] px, float [] py, float [] pz){
    waterbody w;
    w = wb;
    if (inserted) {
      if (wb.getPTMType() == waterbody.CHANNEL) {
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
    *  returns the unique id of particle
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
    *  updates position of particle.
    */
	public final void updatePosition(float delT){
		particleWait = false;  //set or reset pariticle wait variable
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
			if (DEBUG) System.out.println("updating xyz positions");
			updateXYZPosition(delT);
			updateOtherParameters(delT);
		}
	}
  
  
  /**
    *  insertion time and insertion node
    */
  public final void setInsertionInfo(int particleInsertionTime, node injectionNode){
    this.insertionTime = particleInsertionTime;
    setLocation(injectionNode);
  }
  
  
  /**
    *  get recent node
    */
  public final node getRecentNode(){ return nd; }
  
  
  /**
    *  get current waterbody
    */
  public final waterbody getCurrentWaterbody(){ return wb;}
  
  /**
    *  channel parameters
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
    *  A factor calculated from transverse and vertical constant's for mixing
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
    *  A pointer to the waterbody
    */
  protected waterbody wb;
  
  /**
    *  A pointer to the node.
    */
  protected node nd;
  
  /**
    *  time left for completing the current time step
    */
  protected float tmLeft;
  
  /**
    *  Mixing co-efficients
    */
  protected float Ev,Evdt,Etdt;
  
  /**
    *  Falling velocity of particle through water
    */
  //  protected float fallvel;
	/**
	 *  A particle may be asked to wait instead of move with the velocity
	 *  An example is when seep flows control at a node and particle to seep
	 *  has been turned off.  The particle is asked to remain at its current 
	 *  position.
	 */
	private boolean particleWait;

  /**
    *  Gaussian random number generator for y and z dispersive movements
    */
  protected static RandomElement randomNumberGenerator;
  
  /**
    *  particle observer
    */
  protected particleObserver observer;
  
  /**
    *  updates the particle postion for the given time step
    *  The particle is moved for the time step given
    *  The new position of the particle is available as
    *  particle.x, particle.y and particle.z
    */
  protected final void updateXYZPosition(float delT){
    if( wb.getPTMType() ==  waterbody.CHANNEL) { 
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
		// gets the x,y, and z position of the particle after time step			
		x=calcXPosition(tmstep);												
		if ( wb.getPTMType() != waterbody.CHANNEL ) return;						
		// after recursion this may be true.									
		if ( tmLeft >= tmstep && isDead == false) {	
			y=calcYPosition(tmstep);												
			z=calcZPosition(tmstep);												
		}																		
	}
	// update number of time steps taken
	tmLeft -= tmstep;
      }// end while
    } //  if(wb.getPTMType() ==  CHANNEL) 
    else if (wb.getPTMType() ==  waterbody.RESERVOIR){   
      if (DEBUG) System.out.println("Particle " + this + " in reservoir " + wb.getEnvIndex() );
      tryCrossReservoir(delT); 
    }//  else if (wb.getPTMType() ==  waterbody.RESERVOIR)
    else if ( wb.getPTMType() == waterbody.CONVEYOR){
      if (DEBUG) System.out.println("Particle " + this + " in conveyor " + wb.getEnvIndex() );
      // zero time delay
      moveInConveyor(delT);
    }
    else if(wb.getPTMType() ==  waterbody.BOUNDARY) { 
      if (DEBUG) System.out.println("Particle " + this + " in boundary " + wb.getEnvIndex() );
      isDead=true;
    }
  }
  
  
  /**
    *  This is called after particle returns from a reservoir and
    *  needs a random y and z positioning and x corresponding to
    *  upnode or downnode.
    */
  protected final void setXYZLocationInChannel(){
    if (wb.getPTMType() == waterbody.CHANNEL) {
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
    y = ((channel)wb).getWidth(x)*(wb.getRandomNumber()-0.5f);
    z = ((channel)wb).getDepth(x)*wb.getRandomNumber();
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
  
//nodeReached updates the node to new node 
 if ( isNodeReached(xPos) == true ) { 
   float timeToReachNode = calcTimeToNode(xPos);
   //calculate time to reach node
   tmLeft-=timeToReachNode;
   age = age - timeStep + timeToReachNode;
   
   //updates particle location in wb
   makeNodeDecision(); 
    
   //   if (recursionCounter++ > 5) error("Too many recursions in calcXPosition(float)");
   if (recursionCounter++ > 5) {
	   if (repositionFactor < MAX_R_F){
		   repositionFactor += RFIncrement;
		   System.out.println("Reposition Factor set to "+repositionFactor+" for particle "+getId()+" at node "+((channel)wb).getUpNodeId());
	   }
     recursionCounter = 0;
	 
   }
   updateXYZPosition(tmLeft);
   return x;
 }//     if ( nodeReached() == true )
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
  
// reflection from ends of channel
 int k=0, MAX_BOUNCING=100; // max num of iterations to do reflection
 float halfWidth = channelWidth/2.0f;
 while ((yPos < -halfWidth || yPos > halfWidth) && (k<MAX_BOUNCING)){
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
  
// reflections from bottom of channel and water surface
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
  
    return( ( (channel)wb).getVelocity(x,y,z, channelVave, channelWidth, channelDepth));
  
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
    *  Makes node decision on which waterbody to enter into next...
    */
  protected  void makeNodeDecision(){
  
    // node is the current node in which particle entered
    // get total outflow from node and multiply it by random number
  
    // send message to observer about change 
    if (observer != null) 
      observer.observeChange(particleObserver.NODE_CHANGE,this);

	// changed getTotalOut

    float outflow = nd.getTotalOutflow(false);

    // if the node is at a node with zero flow, for example at the
    // end of a slough, then move the particle into the channel a 
    // small amount.
    if (outflow == 0.0f && nd.getNumberOfWaterbodies() == 1) {
      x = getPerturbedXLocation();
      return;
    }

	float out2 = outflow;

    float rand = nd.getRandomNumber();
    outflow = rand*outflow;

    int waterbodyId=-1;
    float flow=0.0f;

	if(outflow == 0.0){
		particleWait = true;
		return;
	}

    // add flow from each node till the flow is greater than the outflow
    do {
      waterbodyId++;
	  
	  // this conditional statement added to exclude seepage
	  // this should be read in as an argument
	  if(nd.getWaterbody(waterbodyId).getAccountingType() != flowTypes.evap){
		  flow += nd.getOutflow(waterbodyId);
	  }
    }
    while ( flow < outflow && 
	    waterbodyId < nd.getNumberOfWaterbodies());
  
    // get a pointer to the water body in which particle entered.
    wb = nd.getWaterbody(waterbodyId);
	
    // send message to observer about change 
    if (observer != null) 
      observer.observeChange(particleObserver.WATERBODY_CHANGE,this);
    // set x as beginning of channel...
  
    x=getXLocationInChannel();
  }
  
  
  /**
    *  updates particle position after calling makeReservoirDecision
    */
  protected final void tryCrossReservoir(float timeStep){
  
    // adjust time and age
    age+=timeStep;
    tmLeft = tmLeft-timeStep;
  
    //get a pointer to the node into which particle enters from Reservoir
    nd =  makeReservoirDecision(timeStep);
  
    if ( nd != null){
      //makes decision of which waterbody to go into
      makeNodeDecision();
      // set previous depth and width to current depth and width
      first=true;
      //? what should be new x,y,z for the particle in the waterbody?
      setXYZLocationInChannel();
    }
  }
  /**
    * moves to the node with inflow and decides where to go from there...
    */
  protected void moveInConveyor(float delT){
    conveyor c = (conveyor) wb;
    if (DEBUG) System.out.println("Particle in conveyor: " + c );
    float flow = c.getFlowInto(0);
    if (flow > 0) 
      setLocation( c.getNode(1) );
    else
      setLocation( c.getNode(0) );
    if (DEBUG) System.out.println("Current node: " + nd);
    makeNodeDecision();
    if (DEBUG) System.out.println("Current wb: " + wb);
    if (DEBUG) System.out.println(" wb type: " + wb.getPTMType() 
				  + ", waterbody.CHANNEL=" + waterbody.CHANNEL);
    if ( wb.getPTMType() == waterbody.CHANNEL) {
      first=true;
      setXYZLocationInChannel();
    }
  }  
  
  /**
    *  returns node to which particle transitions or null
    */
  protected node makeReservoirDecision(float timeStep){
  
    //Get total volume of Reservoir and multiply by random number
    float totvol = ((reservoir)wb).getTotalVolume(timeStep);
    float rand = wb.getRandomNumber();
    totvol = totvol*rand;
  
    //Get flow volume out first node
    int nodeId=-1;
    float flowvol=0.0f;
    //while outflow volume over last i nodes is less than random value
    do {
      nodeId++;
      flowvol += Math.max(0.0f,((reservoir)wb).getVolumeOutflow(nodeId, timeStep)); 
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
    *  inputs state of particle
    */
  /**
    *  outputs state to ostream
    */

  private static final float Emin=0.0001f;
  
  private static final int MAX_NUM_OF_SUB_TIME_STEPS=10000;
  private static final int MISSING = -99999;
  
  private final void insert(){
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
    ((channel)wb).updateChannelParameters(x,cL,cW,cD,cV,cA);

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
    //set previouses to the news..
    previousChannelDepth=channelDepth;
    previousChannelWidth=channelWidth;
  
    //recalculate mixing co-efficients..
    Ev = Math.abs(verticalConstant*channelDepth*channelVave);
    Ev = Math.max(Ev,Emin);
    Evdt=(float) Math.sqrt(2.0f*Ev*timeStep);
    Etdt=Evdt*CtCv;
  
  }
  
  
  private final boolean isNodeReached(float xpos){
    if (xpos < 0.0f) {// crossed starting node of channel
      // get a pointer to the node in question
      nd = wb.getNode(channel.UPNODE);
    }
    else if (xpos > channelLength) { // crossed end node of channel
      // get a pointer to the node in question
      nd = wb.getNode(channel.DOWNNODE);
    }
    else return false;

	// if the particle has been asked to wait then return false
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
  
    if (numOfSubTimeSteps > MAX_NUM_OF_SUB_TIME_STEPS){
      warning("Number Of Sub Time Steps exceeds a maximum of MAX_NUM_OF_SUB_TIME_STEPS");
      return MAX_NUM_OF_SUB_TIME_STEPS;
    }
    else 
      return numOfSubTimeSteps;
  
  }
  
  private final float getMinTimeStep(){
  
    //    float terminalVelocity=Math.max(getFallVel(),1.0e-10f); //fallvel -. input behaviour
    float terminalVelocity = getTerminalVelocity();
    //get a factor of the channel length the maximum distance
    //a particle can travel in a time step
    updateAllParameters(0.0f); // time step not really a factor here.
    float dzmax=dfac*channelDepth;
    float dymax=dfac*channelWidth;
    float dtz=Math.min(dzmax/terminalVelocity,dzmax*dzmax/Ev);
    float dty = (dymax*dymax)/(CtCv*CtCv*Ev);
  
//  	System.out.println("dzmax="+dzmax+"; dymax="+dymax+"; Ev="+Ev+"; CtCv="+CtCv+
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
    *  Insertion time for particle
    */
  private int insertionTime;
  
  /**
    *  gets x location in channel corresponding to upnode and
    *  downnode.
    */
  private final float getXLocationInChannel(){
    float newXPosition=0.0f;
    if (wb.getPTMType() ==  waterbody.CHANNEL) {
      if (((channel)wb).getUpNodeId() == nd.getEnvIndex()) 
	newXPosition=0;
      if (((channel)wb).getDownNodeId() == nd.getEnvIndex()) 
	newXPosition= ((channel)wb).getLength();
    }
    return newXPosition;
  }

  /**
    *  moves x location in channel by a repositioning factor
    *  from the closest node.
    */
  private float getPerturbedXLocation(){
    float newXPosition=0.0f;    
    if (wb.getPTMType() ==  waterbody.CHANNEL) {

      if (((channel)wb).getUpNodeId() == nd.getEnvIndex()) 
	newXPosition = channelLength * repositionFactor;

      if (((channel)wb).getDownNodeId() == nd.getEnvIndex()) 
	newXPosition= ((channel)wb).getLength() - (channelLength * repositionFactor);
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


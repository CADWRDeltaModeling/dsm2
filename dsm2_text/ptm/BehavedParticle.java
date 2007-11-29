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
/**
 * @author Aaron Miller
 * @version $Id: BehavedParticle.java,v 1.5 2001/01/06 00:49:00 miller Exp $
 */
public class BehavedParticle extends particle{

  /**
   * 
   *  value of ratio of internal to external velocity
   */
public BehavedParticle(particleFixedInfo pFI){
  super(pFI);
  behaviorData = pFI.getBehavior();
}

  /**
    *  Returns the normalized upper bound of the allowable in channel depth range
    */
  private float getZUpperBound(){
    float upper;
    upper = behaviorData.getZUpperLimit(age,getModelTime());
    if(upper != 0) upper = upper/100f;
    return upper;
  }

  /**
    *  Returns the normalized lower bound of the allowable in channel depth range
    */
  private float getZLowerBound(){
    float lower;
    lower = behaviorData.getZLowerLimit(age,getModelTime());
    if(lower != 0) lower = lower/100f;
    return lower;
  }

  /**
    *  Returns the normalized allowable in channel depth range
    */
  private float getZRangeMagnitude(){
    return getZUpperBound()-getZLowerBound();
  }

  /**
    *  Externally induced Random
    */
  protected float calcZDisplacementExtRandom(float timeStep){
    // get the random mixing component
    Evdt = (float) Math.sqrt(2.0f * Ev * timeStep);
    float dz = (float) (randomNumberGenerator.gaussian()*Evdt);
    dz = dz + verDifGra*timeStep;

    // if there is positioning information available
    if (getZRangeMagnitude() != 1f){
      // if the particle is outside the allowable range
      if (z < getZLowerBound()*channelDepth || z > getZUpperBound()*channelDepth){
	// if the particle is below
	if (z < getZLowerBound()*channelDepth)
	  dz = (float) (randomNumberGenerator.gaussian()*Evdt*getZRangeMagnitude() 
			+ (getZLowerBound() * channelDepth - z));
	// if the particle is above
	else if (z > getZUpperBound()*channelDepth)
	  dz = (float) (randomNumberGenerator.gaussian()*Evdt*getZRangeMagnitude() 
			- (z - getZUpperBound() * channelDepth));
      }
      // if the particle is with in the allowable range
      else dz = (float) (randomNumberGenerator.gaussian()*Evdt*getZRangeMagnitude());
    }
    // if the particle is not restricted to a range
    else dz = (float) (randomNumberGenerator.gaussian()*Evdt);
    if (DEBUG) System.out.println(" Id = "+Id+" dz = "+dz+" channelDepth = "+channelDepth
				  +" z = "+z+" mag = "+getZRangeMagnitude());
    if (vertMove) return(dz);
    else return 0.0f;
  
  }

  /**
    *  Externally induced Deterministic
    */
  protected  float calcZDisplacementExtDeterministic(float timeStep){
    return(-getFallVel()*timeStep);
  }

  /**
    *  Check to see if this particle dies
    */
  protected void checkHealth(){
    float mortRate;
    mortRate = behaviorData.getMortality(age);
    if (mortRate != _prevMortRate) { 
      // assign a random number to this particle (Health of particle)
      _rand = (float) randomNumberGenerator.uniform(0.0, 1.0);
      _prevMortRate = mortRate;
      _baseAge = behaviorData.getPhaseAge();
      if (DEBUG) System.out.println("_baseAge"+_baseAge+" "+mortRate+" "+_prevMortRate);
    }
    // exponential probability distribution
    float deathDeterminer = 1.0f - (float) Math.exp(-mortRate*(age-_baseAge));
    // if this particles number is < the resulting probability then particle dies
    if (_rand <= deathDeterminer) {
      isDead = true;
      observer.observeChange(particleObserver.DEATH,this);
      if (DEBUG) System.out.println(Id+" died at age "+age+" and "+_baseAge+" with a mortality rate of "+
				    mortRate+" and random "+_rand+" and deathDet = "+deathDeterminer);
    }
  }

  /**
    *  returns a particles fall velocity
    */
  private float getFallVel(){
      return behaviorData.getFallVel(age);
  }
  /**
    *  returns a particles terminal velocity
    */
  private float getTerminalVelocity(){
    return Math.max(getFallVel(),1.0e-10f);
  }

  /**
    *  returns the 24 hour model time
    */
  private int getModelTime(){
    return (Globals.currentMilitaryTime);
  }

  boolean DEBUG = false;

  /**
    *  a pointer to ParticleBehavior containing behavior data
    */
  protected ParticleBehavior behaviorData;

  /**
    *  a particles assigned random number.
    *  a large number increases chance of survival
    *  a small number reduces chance of survival
    */
  protected float _rand;

  /**
    *  holds the previous mortality rate
    *  particles are re-assigned a random number after a new mortality rate
    *  is assigned
    */
  protected float _prevMortRate = -1;

  /**
    *  the age at which a particular phase begins
    *  used to calculate the age in a particular phase
    */
  protected float _baseAge;
}


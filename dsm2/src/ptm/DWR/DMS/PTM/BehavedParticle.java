//    Copyright (C) 1996, 2009 State of California, Department of Water
//    Resources.
//
//    Delta Simulation Model 2 (DSM2): A River, Estuary, and Land
//    numerical model.  No protection claimed in original FOURPT and
//    Branched Lagrangian Transport Model (BLTM) code written by the
//    United States Geological Survey.  Protection claimed in the
//    routines and files listed in the accompanying file "Protect.txt".
//    If you did not receive a copy of this file contact
//    Tara Smith, below.
//
//    This program is licensed to you under the terms of the GNU General
//    Public License, version 2, as published by the Free Software
//    Foundation.
//
//    You should have received a copy of the GNU General Public License
//    along with this program; if not, contact Tara Smith, below,
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
//    Tara Smith
//    California Dept. of Water Resources
//    Division of Planning, Delta Modeling Section
//    1416 Ninth Street
//    Sacramento, CA  95814
//    916-653-9885
//    tara@water.ca.gov
//
//    or see our home page: http://baydeltaoffice.water.ca.gov/modeling/deltamodeling/

package DWR.DMS.PTM;
/**
 * @author Aaron Miller
 * @version $Id: BehavedParticle.java,v 1.5.6.1 2006/04/04 18:16:23 eli2 Exp $
 */
public class BehavedParticle extends Particle{

  /**
   *
   *  value of ratio of internal to external velocity
   */
	public BehavedParticle(ParticleFixedInfo pFI){
		super(pFI);
	}
	/*
	 * Aaron's original code, no longer used
	 */
	/*
		behaviorData = pFI.getBehavior();
	}

    @Override
	protected void updateOtherParameters(float delT){
    	System.err.println("here in behavior");
		updateStageInfo(delT);
	}

	private void updateStageInfo(float delT){
		float stageValue, slope;
		if (wb.getPTMType() == Waterbody.CHANNEL) {
			stageValue = ((Channel)wb).getStage(x);
			if(_stageSet){
				slope = (stageValue - _lastStageValue)/delT;
				if(slope >= MIN_SLOPE){
					_stagePhase = STAGE_RISING;
				}
				else if(slope <= -MIN_SLOPE){
					_stagePhase = STAGE_FALLING;
				}
				else{
					_stagePhase = STAGE_TRANSITIONAL;
				}
			}
			_lastStageValue = stageValue;
			_stageSet = true;
		}
	}
	*/

  /**
    *  Returns the normalized upper bound of the allowable in Channel depth range
    */
	/*
	private float getZUpperBound(){
		float upper = 100;
		upper = behaviorData.getTimeZUpperLimit(age,getModelTime());
		if(upper != 0 && upper != 100){
			upper = upper/100f;
			return upper;
		}
		if(_stagePhase == STAGE_FALLING){
			upper = behaviorData.getStageZUpperLimit(age,STAGE_FALLING);
		}
		else if(_stagePhase == STAGE_RISING){
			upper = behaviorData.getStageZUpperLimit(age,STAGE_RISING);
		}
		if(upper != 0) upper = upper/100f;
		return upper;
  }
*/
  /**
    *  Returns the normalized lower bound of the allowable in Channel depth range
    */
	/*
	private float getZLowerBound(){
		float lower = 0;
		//  	if(_ageId != behaviorData.getCurrentAgeId()){
		lower = behaviorData.getTimeZLowerLimit(age,getModelTime());
		if(lower != 0){
			lower = lower/100f;
//  			_lastLowerValue = lower;
			return lower;
		}
		if(_stagePhase == STAGE_FALLING){
			lower = behaviorData.getStageZLowerLimit(age,STAGE_FALLING);
		}
		else if(_stagePhase == STAGE_RISING){
			lower = behaviorData.getStageZLowerLimit(age,STAGE_RISING);
		}
		if(lower != 0) lower = lower/100f;
//  		_lastLowerValue = lower;
//  	}
//  	else
//  		lower = _lastLowerValue;
		return lower;
	}
	*/

  /**
    *  Returns the normalized allowable in Channel depth range
    */
	/*
  private float getZRangeMagnitude(){
    return getZUpperBound()-getZLowerBound();
  }
*/
  /**
    *  Externally induced Random
    */
    //@Override
  /*
	protected float calcZDisplacementExtRandom(float timeStep){
		// get the random mixing component
//  		behaviorData.setCurrentAgeId(age);

		float dz = 0.0f;
		if (getZRangeMagnitude() != 1f){  // if there is positioning information available
			if (z < getZLowerBound()*channelDepth || z > getZUpperBound()*channelDepth){  // if the Particle is outside the allowable range
				if (z < getZLowerBound()*channelDepth) // if the Particle is below
					dz = (float) (getGaussian()*Evdt*getZRangeMagnitude() + (getZLowerBound() * channelDepth - z));
				else if (z > getZUpperBound()*channelDepth) // if the Particle is above
					dz = (float) (getGaussian()*Evdt*getZRangeMagnitude() - (z - getZUpperBound() * channelDepth));
			}
			else dz = (float) (randomNumberGenerator.gaussian()*Evdt*getZRangeMagnitude()); // if the Particle is with in the allowable range
		}
		else dz = (float) (getGaussian()*Evdt); // if the Particle is not restricted to a range
		if (DEBUG) System.out.println(" Id = "+Id+" dz = "+dz+" channelDepth = "+channelDepth+" z = "+z+" mag = "+getZRangeMagnitude());
		if (vertMove) return(dz);
		else return 0.0f;

		return 0;
	}
*/
  /**
    *  Externally induced Deterministic
    */
    //@Override
   /*
  protected  float calcZDisplacementExtDeterministic(float timeStep){
    return(-getFallVel()*timeStep);
  }

*/
  /**
    *  Check to see if this Particle dies
    */
    //@Override
  /*
  protected void checkHealth(){
    float mortRate;
    mortRate = behaviorData.getMortality(age);
    if (mortRate != _prevMortRate) {
      _rand = getRandomNumber(); // assign a random number to this Particle (Health of Particle)
      _prevMortRate = mortRate;
      _baseAge = behaviorData.getPhaseAge();
      if (DEBUG) System.out.println("_baseAge"+_baseAge+" "+mortRate+" "+_prevMortRate);
    }
    float deathDeterminer = 1.0f - (float) Math.exp(-mortRate*(age-_baseAge)); // exponential probability distribution
    if (_rand <= deathDeterminer) { // if this particles number is < the resulting probability then Particle dies
      isDead = true;
      observer.observeChange(ParticleObserver.DEATH,this);
      if (DEBUG) System.out.println(Id+" died at age "+age+" and "+_baseAge+" with a mortality rate of "+
				    mortRate+" and random "+_rand+" and deathDet = "+deathDeterminer);
    }
  }
*/
  /**
    *  returns a particles fall velocity
    */
 /*
  private float getFallVel(){
      return behaviorData.getFallVel(age);
  }
 */
  /**
    *  returns a particles terminal velocity
    */
   /*
  private float getTerminalVelocity(){
    return Math.max(getFallVel(),1.0e-10f);
  }
*/
  /**
    *  returns the 24 hour model time
    */
 /*
  private int getModelTime(){
    return (Globals.currentMilitaryTime);
  }
*/
// knam: The parent class has DEBUG already
//  boolean DEBUG = false;

  /**
    *  a pointer to ParticleBehavior containing behavior data
    */
  //protected ParticleBehavior behaviorData;

  /**
    *  a particles assigned random number.
    *  a large number increases chance of survival
    *  a small number reduces chance of survival
    */
  //protected float _rand;

  /**
    *  holds the previous mortality rate
    *  particles are re-assigned a random number after a new mortality rate
    *  is assigned
    */
  //protected float _prevMortRate = -1;

  /**
    *  the age at which a particular phase begins
    *  used to calculate the age in a particular phase
    */
	/*
  protected float _baseAge;

	protected float _lastStageValue;

	protected int _stagePhase;

	protected int _ageId = -901; // set to non ageId number

	protected float _lastLowerValue = 0; // set to initial value

	protected float _lastUpperValue = 1; // set to initial value

	public static int STAGE_RISING = 0;

	public static int STAGE_FALLING = 1;

	public static int STAGE_TRANSITIONAL = 2;

	public static float MIN_SLOPE = 0.0001f; // about 0.1 ft in 15 min or 900 sec
*/
	/**
	 * True after one cycle when _lastStageValue has been set.
	 */
	//protected boolean _stageSet = false;
}


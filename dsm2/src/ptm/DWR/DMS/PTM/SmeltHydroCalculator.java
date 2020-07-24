/**
 * 
 */
package DWR.DMS.PTM;

import java.util.concurrent.ConcurrentHashMap;

/**
 * @author xwang
 *
 */
public class SmeltHydroCalculator extends BasicHydroCalculator {

	/**
	 * 
	 */
	public SmeltHydroCalculator() {
		super();
		_pStages = new ConcurrentHashMap<Integer, Float>();
		_pLastStageValue = new ConcurrentHashMap<Integer, Float>();
		_pStageSet = new ConcurrentHashMap<Integer, Boolean>();
		_pStagePhase = new ConcurrentHashMap<Integer, Integer>();
	}
    public void setSmeltBehaviorData(ParticleBehavior behaviorData){_behaviorData = behaviorData;}
	/*
	 * update channel length, width, depth, velocity, area, previous width, depth info 
	 */
    /*
	public void updateChannelParameters(Particle p){
		super.updateChannelParameters(p);
		//TODO move to updateStageInfo
		_pStages.put(p.Id, ((Channel)p.wb).getStage(p.x));
		//System.err.println("stageValue: "+_pStages.get(p.Id)+"  age: "+p.age+"  x: "+p.x);
	}
	*/
	void updateStageInfo(Particle p, float delT){
		float stageValue, slope;
		stageValue = ((Channel)p.wb).getStage(p.x);
		if(_pStageSet.get(p.Id)!=null){			
			slope = (stageValue - _pLastStageValue.get(p.Id))/delT;
			if(slope >= MIN_SLOPE){
				_pStagePhase.put(p.Id, STAGE_RISING);
			}
			else if(slope <= -MIN_SLOPE){
				_pStagePhase.put(p.Id, STAGE_FALLING);
			}
			else{
				_pStagePhase.put(p.Id, STAGE_TRANSITIONAL);
			}			
			if (DEBUG)
				System.err.println(PTMHydroInput.getExtFromIntChan(p.wb.getEnvIndex())+"  "+stageValue+"  "+_pLastStageValue.get(p.Id)+"  "+delT+"  "+_pStagePhase.get(p.Id));
		}		
		_pLastStageValue.put(p.Id, stageValue);
		if(_pStageSet.get(p.Id)==null)
			_pStageSet.put(p.Id, true);		
	}

  /**
    *  Returns the normalized upper bound of the allowable in Channel depth range
    */
	private float getZUpperBound(Particle p){
		float upper = 100;
		upper = _behaviorData.getTimeZUpperLimit(p.age,getModelTime());
		if(upper != 0 && upper != 100){
			upper = upper/100f;
			return upper;
		}
		if(_pStagePhase!=null && _pStagePhase.get(p.Id) != null){
			if(_pStagePhase.get(p.Id) == STAGE_FALLING){
				upper = _behaviorData.getStageZUpperLimit(p.age,STAGE_FALLING);
			}
			else if(_pStagePhase.get(p.Id) == STAGE_RISING){
				upper = _behaviorData.getStageZUpperLimit(p.age,STAGE_RISING);
			}
		}
		if(upper != 0) upper = upper/100f;
		return upper;
  }

  /**
    *  Returns the normalized lower bound of the allowable in Channel depth range
    */
	private float getZLowerBound(Particle p){
		float lower = 0;
		//  	if(_ageId != behaviorData.getCurrentAgeId()){
		lower = _behaviorData.getTimeZLowerLimit(p.age,getModelTime());
		if(lower != 0){
			lower = lower/100f;
//  			_lastLowerValue = lower;
			return lower;
		}
		if(_pStagePhase!=null && _pStagePhase.get(p.Id) != null){
			if(_pStagePhase.get(p.Id) == STAGE_FALLING){
				lower = _behaviorData.getStageZLowerLimit(p.age,STAGE_FALLING);
			}
			else if(_pStagePhase.get(p.Id) == STAGE_RISING){
				lower = _behaviorData.getStageZLowerLimit(p.age,STAGE_RISING);
			}
		}
		if(lower != 0) lower = lower/100f;
//  		_lastLowerValue = lower;
//  	}
//  	else
//  		lower = _lastLowerValue;
		return lower;
	}

  /**
    *  Returns the normalized allowable in Channel depth range
    */
  private float getZRangeMagnitude(Particle p){
	if (DEBUG)  System.err.println(_pStagePhase.get(p.Id)+"  "+getZUpperBound(p)+"  "+getZLowerBound(p));
    return getZUpperBound(p)-getZLowerBound(p);
  }
  

  /**
    *  Externally induced Random
    */
    //@Override
  
	protected float calcZDisplacementExtRandom(Particle p, float z, float timeStep, double gaussian){
		// get the random mixing component
//  		behaviorData.setCurrentAgeId(age);
		
		float dz = 0.0f;
		//0: length, 1: width, 2: depth, 3: velocity, 4: area
		float depth = getChannelInfo(p.Id)[2];
		double Evdt = Math.sqrt(2.0f*getVerticalDiffCoef().get(p.Id)*timeStep);
		if (getZRangeMagnitude(p) != 1f){  // if there is positioning information available
			if (z < getZLowerBound(p)*depth || z > getZUpperBound(p)*depth){  // if the Particle is outside the allowable range
				if (z < getZLowerBound(p)*depth) // if the Particle is below
					dz = (float) (gaussian*Evdt*getZRangeMagnitude(p) + (getZLowerBound(p) * depth - z));
				else if (z > getZUpperBound(p)*depth){ // if the Particle is above
					dz = (float) (-gaussian*Evdt*getZRangeMagnitude(p) - (z - getZUpperBound(p) * depth));
					if(DEBUG) System.err.println(gaussian+"  "+timeStep+"  "+getVerticalDiffCoef().get(p.Id)+"  "+dz+"  "+z+"  "+depth+"  "+getZUpperBound(p)+"  "+getZRangeMagnitude(p));
				}
			}
			else dz = (float) (gaussian*Evdt*getZRangeMagnitude(p)); // if the Particle is with in the allowable range
		}
		else dz = (float) (gaussian*Evdt); // if the Particle is not restricted to a range
		if (DEBUG) System.out.println(" Id = "+p.Id+" dz = "+dz+" channelDepth = "+depth+" z = "+z+" mag = "+getZRangeMagnitude(p));
		if (getVertMove()) return(dz);
		else return 0.0f;
 
	}

	 /**
	   *  Externally induced Deterministic
	   */
	 //@Override
	   
	 protected  float calcZDisplacementExtDeterministic(Particle p, float timeStep){
	    return(-getFallVel(p)*timeStep);
	  }
	      
	  
	/**
	  *  Z Position calculation for time step given
	  */
	public float getZPosition(Particle p, float z, float timeStep, double gaussian){
		//TODO move to SmeltBasicSwimBehavior
		//updateStageInfo(p, timeStep);
		// get current position
		float zPos = z;
		float depth = getChannelInfo(p.Id)[2];
		// calculate position after timeStep
		if(getVertMove())
			zPos += (calcZDisplacementExtRandom(p, z, timeStep, gaussian) + calcZDisplacementExtDeterministic(p,timeStep));
			//zPos += (float) (gaussian*((float) Math.sqrt(2.0f*_pVertD.get(id)*timeStep)));
		
		// reflections from bottom of Channel and water surface
		int k = 0;
		int MAX_BOUNCING = 100;
		while ((zPos < 0.0 || zPos > depth) && (k <= MAX_BOUNCING)){
		  if (zPos < 0.0) zPos = -zPos;
		  else if (zPos > depth) zPos = depth - (zPos - depth);
		  k++;
		}
		if (k > MAX_BOUNCING) 
			PTMUtil.systemExit("Too many iterations in calcZPosition()");
		return (zPos);
	}
	
	 
	/**
	  *  returns a particles fall velocity
	  */
	 
	private float getFallVel(Particle p){
	      return _behaviorData.getFallVel(p.age);
	}

	/**
	  *  returns the 24 hour model time
	  */
	    
	private int getModelTime(){
	    return (Globals.currentMilitaryTime);
	}
	
	 // knam: The parent class has DEBUG already
	 //  boolean DEBUG = false;
	
	/**
	*  a pointer to ParticleBehavior containing behavior data
	*/
	protected ParticleBehavior _behaviorData;
	
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
	//protected float _baseAge;
			
	//protected float _lastStageValue;
	
	//protected int _stagePhase;
	
	//protected int _ageId = -901; // set to non ageId number
	
	//protected float _lastLowerValue = 0; // set to initial value
	
	//protected float _lastUpperValue = 1; // set to initial value
	
	public static int STAGE_RISING = 0;
	
	public static int STAGE_FALLING = 1;
	
	public static int STAGE_TRANSITIONAL = 2;
	
	public static float MIN_SLOPE = 0.0001f; // about 0.1 ft in 15 min or 900 sec
	
	/**
	 * True after one cycle when _lastStageValue has been set.
	 */
	//protected boolean _stageSet = false;
	private ConcurrentHashMap<Integer, Float> _pStages;
	private ConcurrentHashMap<Integer, Float> _pLastStageValue;
	private ConcurrentHashMap<Integer, Boolean> _pStageSet;
	private ConcurrentHashMap<Integer, Integer> _pStagePhase;
	private boolean DEBUG = false;
}

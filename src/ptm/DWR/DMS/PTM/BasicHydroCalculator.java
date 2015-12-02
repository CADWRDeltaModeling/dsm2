/**
 * 
 */
package DWR.DMS.PTM;

import java.util.concurrent.ConcurrentHashMap;

/**
 * @author xwang
 *
 */
public class BasicHydroCalculator {

	/**
	 * 
	 */
	public BasicHydroCalculator() {
		_pChanInfo = new ConcurrentHashMap<Integer, float []>();
		_prePWidthDepth = new ConcurrentHashMap<Integer, float[]>();
		_pVertD = new ConcurrentHashMap<Integer, Float>();
		ParticleFixedInfo pFI = Globals.Environment.getParticleFixedInfo();
	    EvConst = pFI.getVerticalConstant();
	    if (PTMUtil.floatNearlyEqual(EvConst, 0.0f))
	    	PTMUtil.systemExit("vertical diffusion coefficient cannot be 0, system exit");
	    EtToEvConst = Math.abs(pFI.getTransverseConstant()/pFI.getVerticalConstant());
	    if (PTMUtil.floatNearlyEqual(EtToEvConst, 0.0f))
	    	PTMUtil.systemExit("lateral diffusion coefficient cannot be 0, system exit");
	    _transMove = pFI.moveLaterally();
	    _vertMove = pFI.moveVertically();
	}
	void updateDiffusion(Particle p){
		float [] chanInfo = getChannelInfo(p.Id);
		//diffusion coefficients are from Fisher Mixing in Inland and Coastal Waters p106 Eqs. (5.3) (5.6)
		//shear velocity is assumed 1/10 of the channel average velocity (see Fisher p138 Example 5.5 assumptions)
		//TODO may consider change
		// trans diffusion coefficient can be calculated using (EtConst/EvConst)*Ev so only need to store Ev
		_pVertD.put(p.Id, Math.max(Math.abs(EvConst*chanInfo[2]*chanInfo[3]*0.1f),EMIN));
		
	}
	void updateChannelParameters(Particle p){
		float [] cL = new float[1], cW = new float[1], cD = new float[1], cV = new float[1], cA = new float[1];
	    ((Channel)p.wb).updateChannelParameters(p.x,cL,cW,cD,cV,cA);
	    //_pChanInfo and _prePChanInfo will be initialized in the constructor no need to check null
	    float [] chanInfo = _pChanInfo.get(p.Id);
	    float [] preWD = _prePWidthDepth.get(p.Id);
	    if (preWD == null) {
	    	//previous=current, if transfer from reservoir/conveyer to channel
	    	preWD = new float[2];
	    	_prePWidthDepth.put(p.Id, preWD);
	    }
	    if (chanInfo == null){
	    	chanInfo = new float[5];
	    	_pChanInfo.put(p.Id, chanInfo);
	    }
	    else{
	    	preWD[0] = chanInfo[1];
	    	preWD[1] = chanInfo[2];
	    }	    	
	    chanInfo[0] = cL[0];
	    chanInfo[1]  = cW[0];
	    chanInfo[2]  = cD[0];
	    chanInfo[3]   = cV[0];
	    chanInfo[4]   = cA[0];
	    if (p.first){
	    	p.first=false;
	    	preWD[0] = chanInfo[1];
	    	preWD[1] = chanInfo[2]; 
	    }
	}
	float[] getChannelInfo(int pId){
		float [] chanInfo = _pChanInfo.get(pId);
		// this method should be call after call update Channel parameters
		// therefore current info should not be null
		if (chanInfo == null){
			PTMUtil.systemExit("Particle hydrodynamic info in a channel was not properly updated, system exit.");
			return null;
		}
		return chanInfo;
	}
	/**
	 *  updates particle y, z position, Ev, Evdt, Etdt
	 */ 
	void updateYZPosition(Particle p){
		// 
		float [] chanInfo = getChannelInfo(p.Id);
		float [] preWD = _prePWidthDepth.get(p.Id);
		// previous channel info should not be null and previous width and depth should not be 0
		if (preWD == null || PTMUtil.floatNearlyEqual(preWD[0], 0.0f)
									|| PTMUtil.floatNearlyEqual(preWD[1], 0.0f))
			PTMUtil.systemExit("Particle previous width and depth were not properly updated, system exit.");
		//map y & z in new xsection over the node
		p.y = p.y*chanInfo[1]/preWD[0];
		p.z = p.z*chanInfo[2]/preWD[1];
  
		preWD[0] = chanInfo[1];
		preWD[1] = chanInfo[2];
	}
	int getSubTimeSteps(float timeStep, Particle p){
	    // the function will be called at the beginning of the time step so that parameters needs to be updated as hydrodynamic condition changes then.
		//TODO should move updates at the beginning of a time step? if not doing vertMove or transMove, channel parameters should still be updated?
		updateChannelParameters(p); 
	    updateDiffusion(p);	    
	    float minTimeStep = timeStep;
	  
	    if ((_vertMove) || (_transMove))
	    	minTimeStep = getMinTimeStep(timeStep, p);
	  
	    int numOfSubTimeSteps = 1;
	    if (minTimeStep < timeStep) numOfSubTimeSteps=(int) (timeStep/minTimeStep+1);

	    if (numOfSubTimeSteps > MAX_NUM_OF_SUB_TIME_STEPS){
	    	System.err.println("Warning: Number Of Sub Time Steps exceeds a maximum of "+MAX_NUM_OF_SUB_TIME_STEPS);
	    	return MAX_NUM_OF_SUB_TIME_STEPS;
	    }
	    else 
	      return numOfSubTimeSteps;
	}
	
	float getMinTimeStep(float timeStep, Particle p){
	    float terminalVelocity = getTerminalVelocity();
	    float [] chanInfo = getChannelInfo(p.Id);
	    Float Ev = _pVertD.get(p.Id);
	    if (Ev == null)
	    	PTMUtil.systemExit("Particle vertical diffusion coefficient were not properly updated, system exit.");
	    float dymax = DFAC*chanInfo[1];
	    float dzmax = DFAC*chanInfo[2];
	    float dtz = Math.min(dzmax/terminalVelocity,dzmax*dzmax/Ev);
	    float dty = (dymax*dymax)/(EtToEvConst*Ev);

	    if (_vertMove && _transMove) return Math.min(dty,dtz);
	    else if (_vertMove && !_transMove) return dtz;
	    else if (!_vertMove && _transMove) return dty;
	    PTMUtil.systemExit("lateral or vertical move was not set properly. One of them has to be true, but not, system exit.");
	    return timeStep;
	}
	float calcXAdvectionVelocity(Particle p){
		if (p.wb.getType() != Waterbody.CHANNEL)
			PTMUtil.systemExit("calcXAdvectionVelocity(Particle p) should be only called when Particle is in channel, exit");
		float [] cInfo = getChannelInfo(p.Id);
	    return ((Channel)p.wb).getVelocity(p.x,p.y,p.z, cInfo[3], cInfo[1], cInfo[2]);
	}
	float calcXAdvection(float XAdvectionVelocity, float deltaT){
		return XAdvectionVelocity*deltaT;
	}
	private float calcDiffusionMove(Particle p, float timeStep, float ratio, boolean usingProfile){
		float dzy = (float) (PTMUtil.getNextGaussian()*ratio*(float) Math.sqrt(2.0f*_pVertD.get(p.Id)*timeStep));
		// return the random z movement if vertical mixing allowed
		if (usingProfile) return (dzy);
		else return 0.0f;
	}
	/**
	  *  Z Position calculation for time step given
	  */
	float getZPosition(Particle p, float timeStep){
		// get current position
		float zPos = p.z;
		float depth = getChannelInfo(p.Id)[2];
		// calculate position after timeStep
		zPos += calcDiffusionMove(p, timeStep, 1, _vertMove);
		
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
	 *  Y Position calculation for the time step given
	 */
	float getYPosition(Particle p, float timeStep){
	    // get current position
		float yPos = p.y; 
		float width = getChannelInfo(p.Id)[1];
		// calculate position after timeStep
	    yPos += calcDiffusionMove(p, timeStep, EtToEvConst, _transMove);
	    // reflection from banks of Channel
	    int k = 0;
	    int MAX_BOUNCING = 100; // max num of iterations to do reflection
	    float halfWidth = width/2.0f;
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
	  *  y, z positioning for particle just out of reservoir/conveyor
	  *  w random numbers generation 
	  */
	void setYZLocationInChannel(Particle p){
		// this method should be called only when a particle is in a channel
		Channel c = (Channel) p.wb;
		p.y = c.getWidth(p.x)*((float)PTMUtil.getRandomNumber()-0.5f);
	    p.z = c.getDepth(p.x)*(float)PTMUtil.getRandomNumber();
	}
	/**
	    *  gets x location in Channel corresponding to upnode and
	    *  downnode.
	    */
	float getXLocationInChannel(Particle p){
	  Channel c = (Channel) p.wb;
      if (c.getUpNodeId() == p.nd.getEnvIndex())
    	  return 0.0f;
      if (c.getDownNodeId() == p.nd.getEnvIndex())
    	  return c.getLength();
      // the node doesn't match the channel
      PTMUtil.systemExit("the node: " + PTMHydroInput.getExtFromIntNode(p.nd.getEnvIndex()) 
			+ "doesn't match with Channel: "+PTMHydroInput.getExtFromIntChan(c.getEnvIndex())
			+ ", system exit.");
      return MISSING;
	}

	  
	float calcTimeToNode(Particle p, float xAdvectionVelocity, float xPos){
		if (p.wb.getType() != Waterbody.CHANNEL)
			PTMUtil.systemExit("calcTimeToNode(Particle p, float xPos) should be only called when Particle is in channel, exit");
		Channel ch = (Channel) p.wb;
		float l = ch.getLength();
		// xAdvectionVelocity should not be exact 0
		if (xPos <= 0)
			 return Math.abs(p.x/xAdvectionVelocity); 
		else if (xPos >= l)
			 return (l-p.x)/xAdvectionVelocity; 
		else
			 PTMUtil.systemExit("calcTimeToNode(Particle p, float xPos) shold be only call when a node is reached, i.e., xPos < 0 or > length, but xPos ="+xPos
					 + " in channel:"+PTMHydroInput.getExtFromIntChan(p.wb.getEnvIndex()));	
		return 0.0f;
			
	}
	/**
	  *  factor used for calculating minimum time step
	  *  set outside of method to allow overwriting to include fall velocity
	  */  	
	float getTerminalVelocity(){
	    return 1.0e-10f;
	}
	// pid vs. channel info (0: length, 1: width, 2: depth, 3: velocity, 4: area)
	private ConcurrentHashMap<Integer, float[]> _pChanInfo;
	// pid vs. previous Width Depth (0: width, 1:depth)
	private ConcurrentHashMap<Integer, float[]> _prePWidthDepth;
	// pid vs. vertical diffusion coefficient
	private ConcurrentHashMap<Integer, Float> _pVertD;
	private float EtToEvConst;
	private float EvConst;
	private boolean _transMove, _vertMove;
	private static final float EMIN=0.0001f;
	private static final int MAX_NUM_OF_SUB_TIME_STEPS=10000;
	/**
	 *  Limiting factor for the movement during 1 time step due to mixing.<br>
	 *  Used for sub-time step calculation
	 *  to prevent excessive bouncing of particles at boundaries<br>
	 *  usually keep particle movement <10% channel (width, depth) in 1 sub-time step
	 */
	private static final float DFAC = 0.1f;
	private float MISSING = -9999999999.0f;
}

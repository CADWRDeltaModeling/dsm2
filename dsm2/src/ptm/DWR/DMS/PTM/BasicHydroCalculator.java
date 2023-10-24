/**
 *
 */
package DWR.DMS.PTM;

import java.util.concurrent.ConcurrentHashMap;


/**
 * @author xwang
 *
 */
public class BasicHydroCalculator implements HydroCalculator {

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
	    EtToEvSqrt = (float) Math.sqrt(pFI.getTransverseConstant()/pFI.getVerticalConstant());
	    if (PTMUtil.floatNearlyEqual(EtToEvSqrt, 0.0f))
	    	PTMUtil.systemExit("lateral diffusion coefficient cannot be 0, system exit");
	    _transMove = pFI.moveLaterally();
	    _vertMove = pFI.moveVertically();

	    Channel.useVertProfile = pFI.doVerticalProfile();
	    Channel.useTransProfile = pFI.doTransverseProfile();
	    Channel.constructProfile();
	    //Channel.constructProfile();

	}
	public void updateDiffusion(Particle p){
		float [] chanInfo = getChannelInfo(p.Id);
		//diffusion coefficients are from Fisher Mixing in Inland and Coastal Waters p106 Eqs. (5.3) (5.6)
		//shear velocity is assumed 1/10 of the channel average velocity (see Fisher p138 Example 5.5 assumptions)
		// trans diffusion coefficient can be calculated using (EtConst/EvConst)*Ev= EtToEvSqrt*EtToEvSqrt*Ev
		// Ev = EvConst*depth*0.1*average_velocity (see Fisher p138 eqs.5.3, EvConst=0.067 not 0.0067)
		_pVertD.put(p.Id, Math.max(Math.abs(EvConst*chanInfo[2]*chanInfo[3]*0.1f),EMIN));
		//TODO original PTM line, calculate the shear velocity wrong -
		//_pVertD.put(p.Id, Math.max(Math.abs(EvConst*chanInfo[2]*chanInfo[3]),EMIN));

	}
	float getVerticalDiffusionCoeff(int pid){ return _pVertD.get(pid);}
	/*
	 * update channel length, width, depth, velocity, area, previous width, depth info
	 */
	public void updateChannelParameters(Particle p){
		float [] cL = new float[1], cW = new float[1], cD = new float[1], cV = new float[1], cA = new float[1];
	    ((Channel)p.wb).updateChannelParameters(p.x,cL,cW,cD,cV,cA);
	    //_pChanInfo and _prePChanInfo will be initialized in the constructor no need to check null
	    //obtain previous channel info
	    float [] chanInfo = _pChanInfo.get(p.Id);
	    float [] preWD = _prePWidthDepth.get(p.Id);

	    //if no previous info available, put an empty one, otherwise ignore.
	    if (preWD == null) {
	    	//previous=current, if transfer from reservoir/conveyer to channel
	    	preWD = new float[2];
	    	_prePWidthDepth.put(p.Id, preWD);
	    }

	    //if no previous info available, put an empty one, otherwise put value to the preWD basket.
	    if (chanInfo == null){
	    	chanInfo = new float[5];
	    	_pChanInfo.put(p.Id, chanInfo);
	    }
	    else{
	    	preWD[0] = chanInfo[1];
	    	preWD[1] = chanInfo[2];
	    }
	    //update current channel info
	    chanInfo[0] = cL[0];
	    chanInfo[1]  = cW[0];
	    chanInfo[2]  = cD[0];
	    chanInfo[3]   = cV[0];
	    chanInfo[4]   = cA[0];
	    //if this is the first update of channel info set previous = current.
	    if (p.first){
	    	p.first=false;
	    	preWD[0] = chanInfo[1];
	    	preWD[1] = chanInfo[2];
	    }
	}
	/*
	 * get channel length, width, depth, velocity, area info
	 */
	public float[] getChannelInfo(int pId){
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
	 *  map particle y, z position in previous cross section to current cross section
	 */
	public void mapYZ(Particle p){
		//
		float [] chanInfo = getChannelInfo(p.Id);
		float [] preWD = _prePWidthDepth.get(p.Id);
		// previous channel info should not be null and previous width and depth should not be 0
		if (preWD == null || PTMUtil.floatNearlyEqual(preWD[0], 0.0f)
									|| PTMUtil.floatNearlyEqual(preWD[1], 0.0f))
			PTMUtil.systemExit("Particle previous width and depth were not properly updated, system exit.");
		//to avoid rounding error
		float ry = p.y/preWD[0];
		float rz = p.z/preWD[1];
		if (ry>0.5f)
			ry = 0.5f;
		if (ry < -0.5f)
			ry = -0.5f;
		p.y = ry*chanInfo[1];
		if (rz > 1.0f)
			rz = 1.0f;
		if(rz<0.0f)
			rz = 0.0f;
		p.z = rz*chanInfo[2];

	}
	public int getSubTimeSteps(float timeStep, Particle p){
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

	public float getMinTimeStep(float timeStep, Particle p){
		//TODO calculated for each sub time step and everything will be updated before the function call (see line 87, 97 BasicSwimBehavior)
		//don't need follow lines because getMinTimeStep is put back inside of the loop
		//updateChannelParameters(p);
		//updateDiffusion(p);
		//mapYZ(p);
	    float terminalVelocity = getTerminalVelocity();
	    float [] chanInfo = getChannelInfo(p.Id);
	    Float Ev = _pVertD.get(p.Id);
	    if (Ev == null)
	    	PTMUtil.systemExit("Particle vertical diffusion coefficient were not properly updated, system exit.");
	    float dymax = DFAC*chanInfo[1];
	    float dzmax = DFAC*chanInfo[2];
	    float dtz = Math.min(dzmax/terminalVelocity,dzmax*dzmax/Ev);
	    float dty =  (dymax*dymax)/(EtToEvSqrt*EtToEvSqrt*Ev);
//System.err.println(p.age/60+"  "+dtz+"  "+dty+"  "+p.wb.getEnvIndex()+"  "+chanInfo[1]+"  "+chanInfo[2]+"  "+chanInfo[3]);
	    if (_vertMove && _transMove) return Math.min(dty,dtz);
	    else if (_vertMove && !_transMove) return dtz;
	    else if (!_vertMove && _transMove) return dty;
	    PTMUtil.systemExit("lateral or vertical move was not set properly. One of them has to be true, but not, system exit.");
	    return timeStep;
	}
	public float calcXAdvectionVelocity(int id, float x, float y, float z, Channel c){
		float [] cInfo = getChannelInfo(id);
		if(z>cInfo[2])
			 System.err.println(c.getEnvIndex()+"  "+id+"  "+x + "  "+y+"  "+z+"  "+cInfo[3] + "  "+cInfo[1]+"  "+cInfo[2]+"  "+cInfo[0]+"  "+"  "+c.getDepth(0)+"  "+c.getWidth(0)+"  "+c.getFlow(0)+"  "+c.getVelocity(x, y, z, cInfo[3], cInfo[1], cInfo[2]));
	    //if(id == 1 && (c.getEnvIndex()== 113))
			 //System.err.println(c.getEnvIndex()+"  "+x + "  "+y+"  "+z+"  "+cInfo[3] + "  "+cInfo[1]+"  "+cInfo[2]+"  "+cInfo[0]+"  "+"  "+c.getDepth(0)+"  "+c.getWidth(0)+"  "+c.getFlow(0)+"  "+c.getVelocity(x, y, z, cInfo[3], cInfo[1], cInfo[2]));
	    return c.getVelocity(x, y, z, cInfo[3], cInfo[1], cInfo[2]);
	}
	public float calcXAdvection(float XAdvectionVelocity, float deltaT){
		return XAdvectionVelocity*deltaT;
	}
	/**
	  *  Z Position calculation for time step given
	  */
	//TODO changed the way random numbers are called
	//float getZPosition(int id, float z, float timeStep){
	public float getZPosition(Particle p, float z, float timeStep, double gaussian){
		int id = p.Id;
		// get current position
		float zPos = z;
		float depth = getChannelInfo(id)[2];
		// calculate position after timeStep
		if(_vertMove)
			//vertical diffusion coefficient _pvertD.get(id) = EvConst*depth*average_velocity*0.1f
			//dz = gaussian*sqrt(2*vertical_diffusion_coefficient*dt)
			zPos += (float) (gaussian*((float) Math.sqrt(2.0f*_pVertD.get(id)*timeStep)));

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
	//TODO changed the way random numbers are called
	//float getYPosition(int id, float y, float timeStep){
	public float getYPosition(Particle p, float y, float timeStep, double gaussian){
		int id = p.Id;
	    // get current position
		float yPos = y;
		float width = getChannelInfo(id)[1];
		// calculate position after timeStep
	    //yPos += calcDiffusionMove(id, timeStep, EtToEvConst, _transMove);
		if(_transMove)
			yPos += (float) (gaussian*EtToEvSqrt*((float) Math.sqrt(2.0f*_pVertD.get(id)*timeStep)));;
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
	public void setYZLocationInChannel(Particle p){
		// this method should be called only when a particle is in a channel
		Channel c = (Channel) p.wb;
		//TODO changed the way random numbers are called
		//p.y = c.getWidth(p.x)*((float)PTMUtil.getRandomNumber()-0.5f);
	    //p.z = c.getDepth(p.x)*(float)PTMUtil.getRandomNumber();
		p.y = c.getWidth(p.x)*((float)c.getRandomNumber()-0.5f);
	    p.z = c.getDepth(p.x)*(float)c.getRandomNumber();
	}
	/**
	    *  gets x location in Channel corresponding to upnode and
	    *  downnode.
	    */
	public float getXLocationInChannel(Particle p){
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


	public float calcTimeToNode(Channel c, float advVel, float swimVel, float x, float xPos){
		float xAdvectionVelocity = advVel + swimVel;
		if (Math.abs(xAdvectionVelocity) < 0.000001)
			System.err.println("Warning: very small advection + swimming velocities:" + xAdvectionVelocity + ".  It'll take a long time to reach a node.");
		float l = c.getLength();
		// xAdvectionVelocity should not be exact 0
		if (xPos < 0)
			 return Math.abs(x/xAdvectionVelocity);
		else if (xPos > l)
			 return (l-x)/xAdvectionVelocity;
		else
			PTMUtil.systemExit("passed in wrong calculated x value while calculating time to node, system exit.");
		return 0.0f;

	}

	public float calcDistanceToNode(Channel c, float x, float xPos){
		float l = c.getLength();
		if (xPos < 0)
			 return x;
		else if (xPos > l)
			 return (l-x);
		else
			PTMUtil.systemExit("passed in wrong calculated xPos value while calculating distance to node, system exit.");
		return 0.0f;

	}

	/**
	  *  factor used for calculating minimum time step
	  *  set outside of method to allow overwriting to include fall velocity
	  */
	public float getTerminalVelocity(){
	    return 1.0e-10f;
	}
	ConcurrentHashMap<Integer, Float> getVerticalDiffCoef(){return _pVertD;}
	ConcurrentHashMap<Integer, float[]> getPreWidthDepth(){return _prePWidthDepth;}
	float getEtToEvSqrt() {return EtToEvSqrt;}
	float getEvConst() {return EvConst;}
	boolean getTransMove(){return _transMove;}
	boolean getVertMove(){ return _vertMove;}
	// pid vs. channel info (0: length, 1: width, 2: depth, 3: velocity, 4: area)
	private ConcurrentHashMap<Integer, float[]> _pChanInfo;
	// pid vs. previous Width Depth (0: width, 1:depth)
	private ConcurrentHashMap<Integer, float[]> _prePWidthDepth;
	// pid vs. vertical diffusion coefficient
	private ConcurrentHashMap<Integer, Float> _pVertD;
	private float EtToEvSqrt;
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

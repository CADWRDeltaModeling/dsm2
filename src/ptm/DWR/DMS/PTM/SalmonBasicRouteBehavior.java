/**
 * 
 */
package DWR.DMS.PTM;

/**
 * @author xwang
 *
 */

//TODO Should make this class universal? but how to make sure type safe?
public class SalmonBasicRouteBehavior implements SalmonRouteBehavior {
	// is that good to have so many private variables?
	private Waterbody _wb;
	private Node _nd;
	private float _channelLength;
	private float _repositionFactor;
	private float _waterbodyInflows;
	private float _rand;
	private Particle _p = null;
	static double _dicuEfficiency = 0.0;

	/**
	 * 
	 */
	public SalmonBasicRouteBehavior() {
		
	}
	
	public static void setDicuFilterEfficiency(double eff) {
		_dicuEfficiency = eff;
	}
	private final float getPerturbedXLocation(){
	    float newXPosition = 0.0f;
	    if (_wb.getPTMType() ==  Waterbody.CHANNEL) {
	      if (((Channel)_wb).getUpNodeId() == _nd.getEnvIndex())
	      newXPosition = _channelLength * _repositionFactor;

	      if (((Channel)_wb).getDownNodeId() == _nd.getEnvIndex())
	      newXPosition= ((Channel)_wb).getLength() - (_channelLength * _repositionFactor);
	    }
	    return newXPosition;
	}
	private final float getXLocationInChannel(Waterbody wb){
	    float newXPosition = 0.0f;
	    if (wb.getPTMType() ==  Waterbody.CHANNEL) {
	      if (((Channel) wb).getUpNodeId() == _nd.getEnvIndex())
	      newXPosition = 0;
	      if (((Channel) wb).getDownNodeId() == _nd.getEnvIndex())
	      newXPosition = ((Channel) wb).getLength();
	    }
	    return newXPosition;
	}
	private final boolean prescreen(Particle p) {
	    // if the Node is at a Node with zero flow, for example at the
	    // end of a slough, then move the Particle into the Channel a
	    // small amount.

	    if (_waterbodyInflows == 0.0f && _nd.getNumberOfWaterbodies() == 1) {
	      p.x = getPerturbedXLocation();
	      return false;
	    }
	    //float out2 = outflow;
	    

	    _waterbodyInflows = _rand*_waterbodyInflows;
	  
	    if (_waterbodyInflows == 0.0){
	      p.particleWait = true;
	      return false;
	    }
	    return true;
	}
	private void assignVariables(){
		_nd = _p.nd;
		_wb = _p.wb;
		_channelLength = _p.channelLength;
		_repositionFactor = _p.repositionFactor;
		if (_nd == null){
			System.out.println("Particle doesn't know the node! exit.");
			System.exit(-1);
		}
		// false means that seepage flows are excluded.  the total out flow does not include ag seepage flow
	    _waterbodyInflows = _nd.getTotalWaterbodyInflows();
	    _rand = _nd.getRandomNumber();
	}

	/* 
	 * be careful! when isNodeReached() method is called in Particle,
	 * the current node is replaced by the node just reached and the total outflows is related to
	 * that node.  
	 */
	public void makeRouteDecision(Particle p) {
		if (p == null)
			PTMUtil.systemExit("the particle passed in SalmonBasicRouteBehavior is null");
	    _p = p;
		assignVariables();
		// after prescreen() call, _waterbodyInflows = _rand*_waterbodyInflows
	    if (!prescreen(p))
	    	return;
	    
	    int waterbodyId = -1;
	    Waterbody thisWb = null;
	    float flow = 0.0f;
	    float totalAgDivFlows = _nd.getTotalAgDiversion();
	    float totalInflowWOAgDiv = _nd.getTotalWaterbodyInflows() - totalAgDivFlows;
	    float totalAgDivLeftOver = ((float) (totalAgDivFlows*(1-_dicuEfficiency)));
	    boolean dicuFilter = (totalAgDivFlows > 0 && _dicuEfficiency > 0);
	    
	    //TODO need to be changed here bugs about ag dicu efficiency
	    //float agDivFlowLeft = 0.0f;
	    //float totalFlowWOAg = 0.0f;
	    //int numWb = _nd.getNumberOfWaterbodies();
	    //float [] wbs = new float[numWb];

	    
	    //System.err.println("nd id="+_nd.getEnvIndex()+" # of wb =");
	    do {
	    	waterbodyId ++;
	    	thisWb = _nd.getWaterbody(waterbodyId);
	    	//System.out.println("wb id ="+thisWb.getEnvIndex());
	    	float thisFlow = Math.max(0, thisWb.getInflow(_nd.getEnvIndex()));
		    float modFlow = 0.0f;
		    //float agDivFlowLeft = 0.0f;
		    //float totalFlowWOAg = 0.0f;
		    //TODO check for conveyor and reservoir types
	    	if (dicuFilter){
	    		if (thisWb.isAgDiv())
	    			modFlow = ((float) (thisFlow*_dicuEfficiency)); 
    			//agDivFlowLeft = thisFlow - modFlow;
    			//totalFlowWOAg = _waterbodyInflows - thisFlow;
	    		else if (totalInflowWOAgDiv > 0.0f)
	    			modFlow = thisFlow + (thisFlow/totalInflowWOAgDiv)*totalAgDivLeftOver;
	    	}
	    	//else if (agDivFlowLeft > 0.0f && totalFlowWOAg > 0.0f)
	    		//modFlow = thisFlow + agDivFlowLeft*thisFlow/totalFlowWOAg;
	    	else if (thisWb.isAgSeep() || (_nd.isFishScreenInstalled() && thisWb.isFishScreenInstalled()))
	    		modFlow = 0;	
	    	else
	    		modFlow = thisFlow;
	    	flow += modFlow;	    	
	    	// _outflow here is total _outflow * _rand
	    }while (flow < _waterbodyInflows && waterbodyId < _nd.getNumberOfWaterbodies());
	    // get a pointer to the waterbody in which pParticle entered.
	    p.wb = thisWb;
	    // send message to observer about change 
	    
	    if (p.observer != null) 
	      p.observer.observeChange(ParticleObserver.WATERBODY_CHANGE,p);
	    // set x as beginning of Channel...
	    p.x = getXLocationInChannel(p.wb);
	}
}

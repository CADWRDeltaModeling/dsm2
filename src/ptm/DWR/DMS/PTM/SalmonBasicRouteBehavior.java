/**
 * 
 */
package DWR.DMS.PTM;

/**
 * @author xwang
 *
 */

public class SalmonBasicRouteBehavior extends BasicRouteBehavior implements SalmonRouteBehavior {
//public class SalmonBasicRouteBehavior implements SalmonRouteBehavior {
	
	/**
	 * 
	 */
	public SalmonBasicRouteBehavior() {
		super();
	}
	
	//TODO there is no specific basic route behavior for Salmon for now.....
	
	//TODO clean up
	// is that good to have so many private variables?
	/*
	private Waterbody _wb;
	private Node _nd;
	private float _channelLength;
	private float _repositionFactor;
	private float _waterbodyInflows;
	private float _rand;
	private Particle _p = null;
	static double _dicuEfficiency = 0.0;


	
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
		// false means that seepage flows are excluded.  the total waterbody inflows do not include ag seepage flow
	    _waterbodyInflows = _nd.getTotalWaterbodyInflows();
	    _rand = _nd.getRandomNumber();
	}
	*/
	 
	/* 
	 * be careful! when isNodeReached() method is called in Particle, the current node is replaced 
	 * by the node just reached and the total waterbodyInflows is related to
	 * that node.  
	 */
	/*
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

	    do {
	    	waterbodyId ++;
	    	thisWb = _nd.getWaterbody(waterbodyId);
	    	float thisFlow = Math.max(0, thisWb.getInflow(_nd.getEnvIndex()));
		    float modFlow = 0.0f;
	    	if (thisWb.isAgSeep() || (_nd.isFishScreenInstalled() && thisWb.isFishScreenInstalled()))
	    		modFlow = 0;
	    	else if (dicuFilter){
	    		if (thisWb.isAgDiv())
	    			modFlow = ((float) (thisFlow*_dicuEfficiency)); 
	    		else if (totalInflowWOAgDiv > 0.0f)
	    			modFlow = thisFlow + (thisFlow/totalInflowWOAgDiv)*totalAgDivLeftOver;
	    		else
		    		modFlow = thisFlow;
	    	}
	    	else
	    		modFlow = thisFlow;
	    	flow += modFlow;	    	
	    	// _waterbodyInflows here is total _waterbodyInflows * _rand
	    }while (flow < _waterbodyInflows && waterbodyId < _nd.getNumberOfWaterbodies());
	    // get a pointer to the waterbody in which pParticle entered.
	    p.wb = thisWb;
	    // send message to observer about change 
	    
	    if (p.observer != null) 
	      p.observer.observeChange(ParticleObserver.WATERBODY_CHANGE,p);
	    // set x as beginning of Channel...
	    p.x = getXLocationInChannel(p.wb);
	}
	*/
}

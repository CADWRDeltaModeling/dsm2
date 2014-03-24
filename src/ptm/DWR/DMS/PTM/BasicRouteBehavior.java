/**
 * 
 */
package DWR.DMS.PTM;

/**
 * @author xwang
 *
 */
public class BasicRouteBehavior {
	private Particle _p = null;
	private Waterbody _wb = null;
	private Node _nd = null;
	private float _waterbodyInflows;
	private float _rand;
	static double _dicuEfficiency = 0.0;

	/**
	 * 
	 */
	public BasicRouteBehavior() {
		// TODO Auto-generated constructor stub
	}

	
	public static void setDicuFilterEfficiency(double eff) {
		_dicuEfficiency = eff;
	}
	
	private float getPerturbedXLocation(Channel chan, Node, nd, float repostionFactor){
	    float newXPosition = 0.0f;
	    if (chan != null && nd != null) {
		    float cLength = chan.getLength();
		    if (chan.getUpNodeId() == nd.getEnvIndex())
		    	newXPosition = cLength *repositionFactor;
		    else if (chan.getDownNodeId() == nd.getEnvIndex())
		    	newXPosition= cLength - cLength * repositionFactor;
	    }
	    return newXPosition;
	}
	
	private float getXLocationInChannel(Channel chan, Node nd){
	    float newXPosition = 0.0f;
	    if (chan != null && nd != null) {
		    float cLength = chan.getLength();
		    if (chan.getUpNodeId() == nd.getEnvIndex())
		    	newXPosition = 0;
		    if (chan.getDownNodeId() == nd.getEnvIndex())
		    	newXPosition = chan.getLength();
	    }
	    return newXPosition;
	}
	
	private boolean prescreen(Particle p) {
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

	/* 
	 * be careful! when isNodeReached() method is called in Particle, the current node is replaced 
	 * by the node just reached and the total waterbodyInflows is related to
	 * that node.  
	 */
	public void makeRouteDecision(Particle p) {
		if (p == null)
			PTMUtil.systemExit("the particle passed in SalmonBasicRouteBehavior is null");
		Waterbody wb = p.wb;
		Node nd = p.nd;
		if (_wb == null)
			PTMUtil.systemExit("Particle doesn't have a water body to stay! exit.");
		if (_nd == null)
			PTMUtil.systemExit("Particle doesn't know the node! exit.");
		float waterbodyInflows = nd.getTotalWaterbodyInflows();
		float rand = nd.getRandomNumber();
		
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
}

/**
 * 
 */
package DWR.DMS.PTM;

/**
 * @author xwang
 *
 */
public class BasicRouteBehavior {
	static float _dicuEfficiency = 0.0f;

	/**
	 * 
	 */
	public BasicRouteBehavior() {
		// TODO Auto-generated constructor stub
	}

	
	public static void setDicuFilterEfficiency(float eff) {
		if (eff < 0.0)
			PTMUtil.systemExit("negative Dicu efficiency coefficient, system exit.");
		_dicuEfficiency = eff;
	}
	/*
	private void checkNull(Waterbody wb, Node nd){
		if (wb == null || nd == null)
			PTMUtil.systemExit("water body or node not exist while making node decision, system exit.");
	}
	*/
	private float getPerturbedXLocation(Channel chan, Node nd, float repositionFactor){
	    float cLength = chan.getLength();
	    if (chan.getUpNodeId() == nd.getEnvIndex())
	    	return (cLength *repositionFactor);
	    if (chan.getDownNodeId() == nd.getEnvIndex())
	    	return (cLength - cLength * repositionFactor);
	    PTMUtil.systemExit("Node: " + nd.getEnvIndex() + " and channel: " + chan.getEnvIndex() + "doesn't match! exit.");
	    return -999999.0f;
	}
	
	protected float getXLocationInChannel(Channel chan, Node nd){
	    if (chan.getUpNodeId() == nd.getEnvIndex())
	    	return 0;
	    if (chan.getDownNodeId() == nd.getEnvIndex())
	    	return chan.getLength();
	    PTMUtil.systemExit("Node: " + nd.getEnvIndex() + " and channel: " + chan.getEnvIndex() + "doesn't match! exit.");
	    return -999999.0f;
	}
	
	private boolean prescreen(Particle p, float wbInflows) {
	    // if the Node is at a Node with zero flow, for example at the
	    // end of a slough, then move the Particle into the Channel a
	    // small amount.

	    if (Math.abs(wbInflows) < Float.MIN_VALUE && p.nd.getNumberOfWaterbodies() == 1) {
	    	if (p.wb == null || p.wb.getPTMType() != Waterbody.CHANNEL)
	    		p.x = 0;
	    	else
	    		p.x = getPerturbedXLocation(((Channel) p.wb), p.nd, p.repositionFactor);
	    	return false;
	    }
	    //float out2 = outflow;
	    return true;
	}

	/* 
	 * be careful! when isNodeReached() method is called in Particle, the current node is replaced 
	 * by the node just reached and the total waterbodyInflows is related to
	 * that node.  
	 */
	public void makeRouteDecision(Particle p) {
		if (p == null)
			PTMUtil.systemExit("the particle passed in BasicRouteBehavior is null");
		//when particle just inserted wb = null, but node is assigned.
		if (p.nd == null)
			PTMUtil.systemExit("Particle is not assigned a node! system exit.");
		float waterbodyInflows = p.nd.getTotalWaterbodyInflows();
		float totalAgDiversions = p.nd.getTotalAgDiversion();
		if (PTMUtil.floatNearlyEqual(waterbodyInflows, totalAgDiversions))
			waterbodyInflows = totalAgDiversions*_dicuEfficiency;
		
		// after prescreen() call, _waterbodyInflows = _rand*_waterbodyInflows
	    if (!prescreen(p, waterbodyInflows))
	    	return;
	    
	    waterbodyInflows = p.nd.getRandomNumber()*waterbodyInflows;
		  
	    if (Math.abs(waterbodyInflows) < Float.MIN_VALUE){
	      p.particleWait = true;
	      return;
	    }
	    
	    int waterbodyId = -1;
	    Waterbody thisWb = null;
	    float flow = 0.0f;
	    float totalAgDivFlows = p.nd.getTotalAgDiversion();
	    float totalInflowWOAgDiv = p.nd.getTotalWaterbodyInflows() - totalAgDivFlows; 
	    float totalAgDivLeftOver = ((float) (totalAgDivFlows*(1-_dicuEfficiency)));
	    //boolean dicuFilter = (totalAgDivFlows > 0 && _dicuEfficiency > 0);
	    do {
	    	waterbodyId ++;
	    	thisWb = p.nd.getWaterbody(waterbodyId);
	    	float thisFlow = Math.max(0, thisWb.getInflow(p.nd.getEnvIndex()));
		    float modFlow = 0.0f;
	    	if (thisWb.isAgSeep() || (p.nd.isFishScreenInstalled() && thisWb.isFishScreenInstalled()))
	    		modFlow = 0;
	    	else if (thisWb.isAgDiv())
	    		// only allow _dicuEfficiency*ag_flow to go through 
	    		modFlow = ((float) (thisFlow*_dicuEfficiency)); 
	    	else if (totalInflowWOAgDiv > 0.0f)
	    		modFlow = thisFlow + (thisFlow/totalInflowWOAgDiv)*totalAgDivLeftOver;
	    	else
		    	modFlow = thisFlow;
	    	flow += modFlow;	    	
	    	// _waterbodyInflows here is total _waterbodyInflows * _rand
	    	// waterbodyId start from 0 
	    }while (flow < waterbodyInflows && waterbodyId < (p.nd.getNumberOfWaterbodies()-1));
	    // get a pointer to the waterbody in which pParticle entered.
	    	    
	    p.wb = thisWb;
	    // send message to observer about change 
	    if (p.observer != null) 
	      p.observer.observeChange(ParticleObserver.WATERBODY_CHANGE,p);
	    //set x to only channels.  other water body types don't need to be set. 
	    if (p.wb != null && p.wb.getPTMType() == Waterbody.CHANNEL)
	    	p.x = getXLocationInChannel((Channel)p.wb, p.nd);
	}
}

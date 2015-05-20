/**
 * 
 */
package DWR.DMS.PTM;

import java.util.Map;

/**
 * @author xwang
 *
 */
public class BasicRouteBehavior {
	static float _dicuEfficiency = 0.0f;
	private static SwimInputs _si = null;

	/**
	 * 
	 */
	public BasicRouteBehavior() {

	}

	public static void setMeanSwimmingVelocityMap(SwimInputs si){
		if (si == null)
			PTMUtil.systemExit("Swimming inputs are not found, system exit.");
		_si = si;
	}
	public static void setDicuFilterEfficiency(float eff) {
		if (eff < 0.0)
			PTMUtil.systemExit("negative Dicu efficiency coefficient, system exit.");
		_dicuEfficiency = eff;
	}
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
		if (_si == null)
			PTMUtil.systemExit("Swimming input is not defined! system exit.");
		// calculate total waterbody Inflows:
		// because swimming velocity changes from channel to channel and from particle to particle
		// first need to get the swimming velocity for each waterbody of this particle
		Waterbody [] wbs = p.nd.getWaterbodies();
		if (wbs == null)
			PTMUtil.systemExit("node: "+PTMHydroInput.getExtFromIntNode(p.nd.getEnvIndex())
					              +" doesn't have any waterbody connect to it, please check, exit.");
		float[] pMeanSwimmingVels = new float[wbs.length];
		float[] swimmingVels = new float[wbs.length];
		float[] wbFlows = new float[wbs.length];
	    
	    float totalWbInflows = 0.0f;
	    int nodeId = p.nd.getEnvIndex();
	    int wbId = 0;
		for (Waterbody wb: wbs){
			if(wb != null && wb.getPTMType() == Waterbody.CHANNEL){
				Channel c = (Channel) wb;
				pMeanSwimmingVels[wbId] = _si.getParticleMeanSwimmingVelocity(p.Id, c);
				swimmingVels[wbId] = c.getSwimmingVelocity(pMeanSwimmingVels[wbId]);
	    		// not count for negative inflow
				wbFlows[wbId] = Math.max(0.0f, c.getInflowWSV(nodeId, swimmingVels[wbId]));
				// comment out this line is for make swimming velocity not change at node
				//wbFlows[wbId] = Math.max(0.0f, c.getInflowWSV(nodeId, p.getSwimmingVelocity()));
	    	}
			else{
				// no swimming velocity other than the water body type channel
				pMeanSwimmingVels[wbId] = 0.0f;
	    		swimmingVels[wbId] = 0.0f;
	    		if(wb.isAgSeep()
		        		|| (p.nd.isFishScreenInstalled() && wb.isFishScreenInstalled()))
	    			wbFlows[wbId] = 0.0f;
	    		else
	    			wbFlows[wbId] = Math.max(0.0f,wb.getInflow(nodeId));
			}
			totalWbInflows += wbFlows[wbId];
			wbId++;
		}
		
		// if in a dead end, move some distance and recalculate
		if (!prescreen(p, totalWbInflows))
		    return;
		
		float totalAgDiversions = p.nd.getTotalAgDiversions();
		if (PTMUtil.floatNearlyEqual(totalWbInflows, totalAgDiversions))
			totalWbInflows = totalAgDiversions*_dicuEfficiency;
		
		float randTotalWbInflows = ((float)PTMUtil.getRandomNumber())*totalWbInflows;
		
		// if total flow is 0 wait for a time step
		if (Math.abs(randTotalWbInflows) < Float.MIN_VALUE){
		      p.particleWait = true;
		      if (p.wb != null && p.wb.getPTMType() == Waterbody.CHANNEL)
			    	p.x = getXLocationInChannel((Channel)p.wb, p.nd);	
		      return;
		}
		
		float diversionsLeftover = totalAgDiversions*(1-_dicuEfficiency);
		float totalFlowsWOAg = totalWbInflows - totalAgDiversions;
		
		wbId = -1;
		float flow = 0.0f;
		do {
		    float modFlow = 0.0f;
		    wbId ++;
	    	if (wbs[wbId].isAgDiv())
	    		// the probability of this route reduce (1-_dicuEfficiency)
	    		modFlow = ((float) (wbFlows[wbId]*_dicuEfficiency)); 
	    	else if (totalFlowsWOAg > 0.0f)
	    		modFlow = wbFlows[wbId] + (wbFlows[wbId]/totalFlowsWOAg)*diversionsLeftover;
	    	else
		    	modFlow = wbFlows[wbId];
	    	flow += modFlow;
	    }while (flow < randTotalWbInflows && wbId < (p.nd.getNumberOfWaterbodies()-1));
		p.wb = wbs[wbId];
	    // send message to observer about change 
	    if (p.observer != null) 
	      p.observer.observeChange(ParticleObserver.WATERBODY_CHANGE,p);
	    //set x to only channels.  other water body types don't need to be set. 
	    if (p.wb != null && p.wb.getPTMType() == Waterbody.CHANNEL){
	    	p.x = getXLocationInChannel((Channel)p.wb, p.nd);
	    	p.setMeanSwimmingVelocity(pMeanSwimmingVels[wbId]);
	    	p.setSwimmingVelocity(swimmingVels[wbId]);
	    }
	}
	
}

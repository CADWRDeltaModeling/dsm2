/**
 * 
 */
package DWR.DMS.PTM;

/**
 * @author xwang
 *
 */

public class SalmonBasicRouteBehavior extends BasicRouteBehavior implements SalmonRouteBehavior {
	private static SwimInputs _si = null;
	public static void setSwimmingInputs(SwimInputs si){
		if (si == null)
			PTMUtil.systemExit("Swimming inputs are not found, system exit.");
		_si = si;
	}
	/**
	 * 
	 */
	public SalmonBasicRouteBehavior() {
		super();
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
		int[] confusionFactors = new int[wbs.length];
		
	    float totalWbInflows = 0.0f;
	    int nodeId = p.nd.getEnvIndex();
	    int wbId = 0;
		for (Waterbody wb: wbs){
			if(wb != null && wb.getPTMType() == Waterbody.CHANNEL){
				Channel c = (Channel) wb;
				// mean swimming velocity is per particle per channel group
				pMeanSwimmingVels[wbId] = _si.getParticleMeanSwimmingVelocity(p.Id, c); 
				swimmingVels[wbId] = c.getSwimmingVelocity(pMeanSwimmingVels[wbId]);
				
				if (_si.getRandomAccess()&&(PTMUtil.getRandomNumber()<_si.getAccessProbability())
						 &&(PTMUtil.getRandomNumber()<c.getProbConfusion())){
					swimmingVels[wbId] *= -1;
					confusionFactors[wbId] = -1;
				}
				else
					confusionFactors[wbId] = 1;
				swimmingVels[wbId] *= c.getChanDir();
				
	    		// not count for negative inflow
				wbFlows[wbId] = Math.max(0.0f, c.getInflowWSV(nodeId, swimmingVels[wbId]));
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
		// at this point, totalFlows is the sum of all inflows and the dicuEfficiency doesn't matter for the totalFlows 
		// because leftover flows are added to other inflows and the total remains the same.
		// except for the case above with that only inflows are ag diversions, in which the dicuEfficiency has to be counted for.
		// 
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
			// get a rearing holding time for the particle for this channel group
	    	p.setSwimmingTime(_si.getParticleRearingHoldingTime(p.Id, (Channel)p.wb));
	    	p.setConfusionFactor(confusionFactors[wbId]);  
	    }
	}
}

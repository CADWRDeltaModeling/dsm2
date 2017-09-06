/**
 * 
 */
package DWR.DMS.PTM;

/**
 * @author xwang
 *
 */

public class SalmonBasicRouteBehavior extends BasicRouteBehavior implements SalmonRouteBehavior {
	private boolean DEBUG = false;
	/**
	 * 
	 */
	public SalmonBasicRouteBehavior(RouteInputs in) {
		super(in);
	}
	
	/* 
	 * be careful! when isNodeReached() method is called in Particle, the current node is replaced 
	 * by the node just reached and the total waterbodyInflows is related to
	 * that node.  
	 */
	public void makeRouteDecision(Particle p) {
		if (p == null)
			PTMUtil.systemExit("the particle passed in BasicRouteBehavior is null");
		//wb can be null (when just inserted), but node is assigned.
		if (p.nd == null)
			PTMUtil.systemExit("Particle is not assigned a node! system exit.");
		// calculate total waterbody Inflows:
		// because swimming velocity changes from channel to channel and from particle to particle
		// get the swimming velocity for EACH waterbody of this particle
		Waterbody [] wbs = p.nd.getWaterbodies();
		if (wbs == null)
			PTMUtil.systemExit("node: "+PTMHydroInput.getExtFromIntNode(p.nd.getEnvIndex())
					              +" doesn't have any waterbody connect to it, please check, exit.");
		
		//float[] pMeanSwimmingVels = new float[wbs.length];
		float[] swimmingVels = new float[wbs.length];
		float[] wbFlows = new float[wbs.length];
		int[] confusionFactors = new int[wbs.length];
		
	    float totalWbInflows = 0.0f;
	    int nodeId = p.nd.getEnvIndex();
	    int wbId = 0;
		for (Waterbody wb: wbs){
			if (wb == null)
					PTMUtil.systemExit("when trying to route the particle, one of the water bodies is null, system exit.");
			swimmingVels[wbId] = 0.0f;
			if (wb.isAgSeep()
	        		|| (p.nd.isFishScreenInstalled() && wb.isFishScreenInstalled()))
				wbFlows[wbId] = 0.0f;	
			else{
				if(wb.getPTMType() == Waterbody.CHANNEL){ 
					Channel c = (Channel) wb;
					int cId = c.getEnvIndex();
					//mean swimming velocity set once per particle per channel group.
					// therefore here is the only place to set a mean swimming velocity
					p.getSwimHelper().setMeanSwimmingVelocity(p.Id, cId);
					//Swimming velocity here doesn't include confusion factor
					swimmingVels[wbId] = ((SalmonSwimHelper) p.getSwimHelper()).getSwimmingVelocity(p.Id, cId);
					confusionFactors[wbId] = ((SalmonSwimHelper) p.getSwimHelper()).getConfusionFactor(cId);
					wbFlows[wbId] = Math.max(0.0f, c.getInflowWSV(nodeId, swimmingVels[wbId]*confusionFactors[wbId]));
					//TODO clean up
					//System.err.println(PTMHydroInput.getExtFromIntChan(c.getEnvIndex())+" " +swimmingVels[wbId]*confusionFactors[wbId]+ " "
					//+wbFlows[wbId]);
		    	}
				else
					// swimming velocity = 0 in the other types of water bodies
		    		wbFlows[wbId] = Math.max(0.0f,wb.getInflow(nodeId));
			}
			totalWbInflows += wbFlows[wbId];
			wbId++;
		}
				
		// if in a dead end, move some distance and recalculate (p.x will be reset)
		if (!prescreen(p, totalWbInflows)){
			if(p.Id == 1 && DEBUG)
				System.err.println("in a dead end, wait until next time step");
			p.particleWait = true;
		    return;
		}
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
			if(p.Id == 1 && DEBUG)
				System.err.println("total flow = 0, wait until next time step");
		      p.particleWait = true;
		      if (p.wb != null && p.wb.getPTMType() == Waterbody.CHANNEL)
			    	p.x = getXLocationInChannel((Channel)p.wb, p.nd);	
		      return;
		}
		
		float diversionsLeftover = totalAgDiversions*(1-_dicuEfficiency);
		//if totalWbInflows = totalAgDiversions*_dicuEfficiency totalFlowsWOAg = totalAgDiversions*(_dicuEfficiency-1) <= 0
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
	    		//wbFlows[wbId] always >= 0, if totalFlowsWOAg > 0.0f wbFlows[wbId] > = 0
	    		modFlow = wbFlows[wbId] + (wbFlows[wbId]/totalFlowsWOAg)*diversionsLeftover;
	    	else
	    		// if (totalFlowsWOAg <= 0.0f) && (!wbs[wbId].isAgDiv()) always false. 
	    		// this block will never be executed
		    	modFlow = wbFlows[wbId];
	    	flow += modFlow;
	    }while (flow < randTotalWbInflows && wbId < (p.nd.getNumberOfWaterbodies()-1));
		p.wb = wbs[wbId];
	    // send message to observer about change 
	    if (p.observer != null) 
	      p.observer.observeChange(ParticleObserver.WATERBODY_CHANGE,p);
	    // need to set x only channels.   
	    if (p.wb != null && p.wb.getPTMType() == Waterbody.CHANNEL){
	    	Channel chan = (Channel) p.wb;
	    	int chanId = chan.getEnvIndex();
	    	p.x = getXLocationInChannel(chan, p.nd);
	    	p.setSwimmingVelocity(swimmingVels[wbId]*confusionFactors[wbId]);
	    	p.setConfusionFactor(confusionFactors[wbId]);
	    	p.swimVelSetInJunction(true);
	    	// swimming time is set one per particle per channel group, here is the only place set a swimming time
	    	p.getSwimHelper().setSwimmingTime(p, chanId); // to set swimming time in SalmonHoldingTimeCalculator
	    	// set Swimming time in particle
	    	p.setSwimmingTime(((SalmonSwimHelper) p.getSwimHelper()).getSwimmingTime(p.Id, ((Channel)p.wb).getEnvIndex()));
			if(p.nd.getEnvIndex() == chan.getUpNodeId())
				p.setFromUpstream(true);
			else
				p.setFromUpstream(false);
	    	
			//TODO clean up	
			/* 
	    	System.err.println(p.Id + " " +(p.getCurrentParticleTimeExact()-56300000)+" " + PTMHydroInput.getExtFromIntNode(p.nd.getEnvIndex())+" "
					 +PTMHydroInput.getExtFromIntChan(p.wb.getEnvIndex())
					 + " " +"in route"+ " "+ wbFlows[wbId]//p.wb.getInflowWSV(p.nd.getEnvIndex(), p.getSwimmingVelocity())
					 +" "+p.getSwimmingVelocity()+" "+ totalWbInflows);
	    	*/
	    	
	    }
	}
	public void updateCurrentInfo(Waterbody[] allWbs, int currT){
		RouteInputs rIn = getRouteInputs();
		if (rIn != null)
			rIn.updateCurrentBarrierInfo(allWbs, currT);
	}
}

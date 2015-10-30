/**
 * 
 */
package DWR.DMS.PTM;

//import java.util.Map;

/**
 * @author xwang
 *
 */
public class BasicRouteBehavior {
	static float _dicuEfficiency = 0.0f;
	private RouteInputs _rIn;

	/**
	 * 
	 */
	public BasicRouteBehavior(RouteInputs in) {
		_rIn = in;
	}
	public RouteInputs getRouteInputs() {return _rIn;}
	public static void setDicuFilterEfficiency(float eff) {
		if (eff < 0.0)
			PTMUtil.systemExit("negative Dicu efficiency coefficient, system exit.");
		_dicuEfficiency = eff;
	}
	float getPerturbedXLocation(Channel chan, Node nd, float repositionFactor){
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
	
	boolean prescreen(Particle p, float wbInflows) {
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

   /**
    *  Makes Node decision on which Waterbody to enter into next;
    *  update nd, wb, x
    */
	public void makeRouteDecision(Particle p) {
		if (p == null)
			PTMUtil.systemExit("the particle passed in BasicRouteBehavior is null! system exit");
			//when particle just inserted wb = null, but node is assigned.
		if (p.nd == null)
			PTMUtil.systemExit("Particle is not assigned a node! system exit.");
			/*
			float outflow = p.nd.getTotalEffectiveOutflow(false);
	
		// if the Node is at a Node with zero flow, for example at the
		// end of a slough, then move the pParticle into the Channel a
		// small amount.
		if (outflow == 0.0f && nd.getNumberOfWaterbodies() == 1) {
		  x = getPerturbedXLocation();
		  return;
		}
		//float out2 = outflow;
		    
		    float rand = nd.getRandomNumber();
		    outflow = rand*outflow;
		  
		    float flow = 0.0f;
		  
		    if (outflow == 0.0){
		      particleWait = true;
		      return;
		    }
		    
		    // loop determines which wb is for particle to enter
		int waterbodyId = -1;
		do {
		  waterbodyId ++;
		  // this conditional statement added to exclude seepage
		  // this should be read in as an argument
		  //@todo: disabled this feature
		  //if(nd.getWaterbody(waterbodyId).getAccountingType() != flowTypes.evap){
		  flow += nd.getFilterOp(waterbodyId) * nd.getOutflow(waterbodyId);
		  //}
		    }while (flow < outflow && 
			        waterbodyId < nd.getNumberOfWaterbodies());
		  
		    // get a pointer to the waterbody in which pParticle entered.
		wb = nd.getWaterbody(waterbodyId);
		// send message to observer about change 
		if (observer != null) 
		  observer.observeChange(ParticleObserver.WATERBODY_CHANGE,this);
		// set x as beginning of Channel...
		x = getXLocationInChannel();
		// @todo: redesign the coding structure of this feature
		 */
	}
			  	
	
}

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
	private float _outflow;
	private float _rand;
	private Particle _p = null;

	/**
	 * 
	 */
	public SalmonBasicRouteBehavior() {
		// TODO do nothing for now
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

	    if (_outflow == 0.0f && _nd.getNumberOfWaterbodies() == 1) {
	      p.x = getPerturbedXLocation();
	      return false;
	    }
	    //float out2 = outflow;
	    

	    _outflow = _rand*_outflow;
	  
	    if (_outflow == 0.0){
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
	    _outflow = _nd.getTotalOutflow(false);
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
		// after prescreen() call, _outflow = _rand*_outflow
	    if (!prescreen(p))
	    	return;
	    
	    int waterbodyId = -1;
	    Waterbody thisWb = null;
	    float flow = 0.0f;
	    do {
	    	waterbodyId ++;
	    	thisWb = _nd.getWaterbody(waterbodyId);
	    	float thisFlow = _nd.getOutflow(waterbodyId);
		    float modFlow = 0.0f;
		    float agDivFlowLeft = 0.0f;
		    float totalFlowWOAg = 0.0f;
	    	if (thisWb.getType() == Waterbody.BOUNDARY && ((Boundary) thisWb).getBoundaryType().equals("AG_DIV")){
    			modFlow = ((float) (thisFlow*Globals.Environment.getBehaviorInputs().getRouteInputs().getDicuFilterEfficiency())); 
    			agDivFlowLeft = thisFlow - modFlow;
    			totalFlowWOAg = _outflow - thisFlow;
	    	}
	    	else if (agDivFlowLeft > 0.0f && totalFlowWOAg > 0.0f)
	    		modFlow = thisFlow + agDivFlowLeft*thisFlow/totalFlowWOAg;
	    	else if (Globals.Environment.getBehaviorInputs().getRouteInputs().getFishScreenMap().containsKey(PTMUtil.concatNodeWbIds(_nd.getEnvIndex(), thisWb.getEnvIndex())))
	    		modFlow = 0;	
	    	else
	    		modFlow = thisFlow;
	    	flow += modFlow;	    	
	    	// _outflow here is total _outflow * _rand
	    }while (flow < _outflow && waterbodyId < _nd.getNumberOfWaterbodies());
	    // get a pointer to the waterbody in which pParticle entered.
	    p.wb = thisWb;
	    // send message to observer about change 
	    
	    if (p.observer != null) 
	      p.observer.observeChange(ParticleObserver.WATERBODY_CHANGE,p);
	    // set x as beginning of Channel...
	    p.x = getXLocationInChannel(p.wb);
	}
}

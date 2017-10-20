/**
 * 
 */
package DWR.DMS.PTM;
import java.util.ArrayList;

import org.apache.commons.math3.special.Beta;

//import java.util.ArrayList;

/**
 * @author xwang
 *
 */
public class SalmonGSJRouteBehavior extends SalmonBasicRouteBehavior {
	private int _nodeId;
	private RouteInputs _rIn = null;
	private boolean DEBUG = false;
	/**
	 * 
	 */
	public SalmonGSJRouteBehavior(RouteInputs rIn, Integer nodeId) {
		super(rIn);
		_rIn = rIn;
		_nodeId = nodeId;
		System.out.println("Created SalmonGSJRouteBehavior...");
	}
	private Channel[] getChannels(Particle p){
		Node curNode = p.nd;
		if (curNode == null)
			PTMUtil.systemExit("p.nd is null, exit");
		if (curNode.getEnvIndex()!=_nodeId)
			PTMUtil.systemExit("in Georgiana Slough Junction, node id in input file is different from node id the particle encountered, exit.");
		ArrayList<Channel> chans = curNode.getChannels();
		if (chans == null || chans.size() != 3)
			PTMUtil.systemExit("in Georgiana Slough Junction, channel number is not 3, exit.");
		
		Channel sacUp = (Channel) curNode.getChannel(_rIn.getChannelId("SACUP")); 
		Channel sacDown = (Channel) curNode.getChannel(_rIn.getChannelId("SACDOWN"));
		Channel gs = (Channel) curNode.getChannel(_rIn.getChannelId("GS"));
		if (gs == null || sacUp == null || sacDown == null)
			PTMUtil.systemExit("Behavior input file entered wrong GS channel names, should be SACUP, SACDOWN and GS, exit.");
		return new Channel[] {sacUp, sacDown, gs};
	}

	/* (non-Javadoc)
	 * @see DWR.DMS.PTM.JunctionHandler#execute(DWR.DMS.PTM.Junction, DWR.DMS.PTM.Particle)
	 */
	// this one will be called as long as a SalmonGSJRouteBehavior is passed
	public void makeRouteDecision(Particle p) {
		try{
			if (p.nd.getNumberOfChannels() != 3)
				throw new ClassCastException("particle is in wrong a junction! channels != 3");
			else if (!Globals.Environment.getParticleType().equalsIgnoreCase("SALMON"))
				throw new ClassCastException("particle is not SALMON!");
		}
		catch(ClassCastException cce){
			cce.printStackTrace();
			PTMUtil.systemExit("Error: " + cce.getMessage());
		} 
				
		Channel[] channels = getChannels(p);
		Channel sacUp = channels[0]; // first sac up second sac down third gs
		Channel sacDown = channels[1];
		Channel gs = channels[2];
		//the flow unit in DSM2 is cfs. scale the flow to the unit used by route model, i.e., cms/500 
		float scaled = 0.0283f/500.0f;
		float qUpSac = scaled*sacUp.getFlow(0.0f);
		float qDownSac = scaled*sacDown.getFlow(0.0f);
		float qGs = scaled*gs.getFlow(0.0f);
		if (DEBUG == true)
			System.err.println("nodeId:"+PTMHydroInput.getExtFromIntNode(p.nd.getEnvIndex())
				+" wb:"+PTMHydroInput.getExtFromIntChan(p.wb.getEnvIndex())+"  pId="+p.Id+" sacUpFlow:"
				+sacUp.getFlow(0.0f)+" sacDownFlow:"+sacDown.getFlow(0.0f)+" gsFlow:"+gs.getFlow(0.0f)
				+" swimming velocity:"+p.getSwimmingVelocity());
		
		// if particle from upstream and flow in the range of the regression model do the regression model
		// assume unidirectional flow greater than 14000cfs
		// else do the regular super.makeRouteDecision(...)
		
		if (qUpSac > 0.7924 && qGs > 0 && qDownSac > 0 && (p.wb.getEnvIndex() == sacUp.getEnvIndex())){ 
			float sr = qGs/(qGs + qDownSac);
			double si = 1/(1+Math.exp(0.209-0.618*Math.log(sr/(1-sr))));
			// day d=0 night d=1
		    int d = 0;
		    if (!SwimInputs.isDayTime())
		    	d =1;
		    //barrier operation 1: on, 0: off
		    int bOp = gs.getCurrentBarrierOp(p.nd.getEnvIndex());
		    //pr1 pr0 conditional probability of entrainment
		    double mu, epow, epowPR1, epowPR0, pr1, pr0, a, b, p1, p0, gsProbability;
		    //TODO, should use qDownSac+qGs instead of qUpSac?  Because of div flow, qDownSac+qGs != qUpSac
		    double fi = Math.exp(2.496-0.267*qUpSac);
		    if(d == 0){
		    	if(bOp == 1){
		    		epow = Math.exp(-0.149+0.096*qUpSac);
		    		epowPR1 = Math.exp(-2.941+1.027*qUpSac);
		    		epowPR0 = Math.exp(-2.564-0.468*qUpSac);
		    	}
		    	else{
		    		epow = Math.exp(-0.212+0.096*qUpSac);
		    		epowPR1 = Math.exp(-1.8+1.027*qUpSac);
		    		epowPR0 = Math.exp(-1.423-0.468*qUpSac);
		    	}
		    }
		    else{
		    	if(bOp == 1){
		    		epow = Math.exp(-0.068+0.096*qUpSac);
		    		epowPR1 = Math.exp(-1.948+1.027*qUpSac);
		    		epowPR0 = Math.exp(-2.636-0.468*qUpSac);
		    	}
		    	else{		    		
		    		epow = Math.exp(-0.131+0.096*qUpSac);
		    		epowPR1 = Math.exp(-0.807+1.027*qUpSac);
		    		epowPR0 = Math.exp(-1.495-0.468*qUpSac);
		    	}
		    }
		    mu = epow/(1+epow);
		    pr1 = epowPR1/(1+epowPR1);
		    pr0 = epowPR0/(1+epowPR0);
		    a = mu*fi;
		    b = (1-mu)*fi;
		    p1 = Beta.regularizedBeta(si, a, b);
		    p0 = 1-p1;
		    gsProbability = pr1*p1 + pr0*p0;		    		    		    		  
		    if (DEBUG == true)
		    	System.err.println("qUpSac:"+qUpSac+" wb:"+PTMHydroInput.getExtFromIntChan(p.wb.getEnvIndex())+"  p:"+gsProbability
		    			+"  pId="+p.Id+" day:"+d+" op:"+bOp+" si:"+si+" epow:"+epow+" epowPR1:"+epowPR1+" epowPR0:"+epowPR0+" p1:" + p1);
		    if (gsProbability < p.nd.getRandomNumber()){
		    	p.wb = sacDown;
		    }
		    else
		    	p.wb = gs;			
		    if (p.observer != null) 
		    	p.observer.observeChange(ParticleObserver.WATERBODY_CHANGE,p);
		    _rIn.putEntrainmentRate(_nodeId, qUpSac/scaled, d, bOp, sr, gsProbability);
		    setChannelStartingCondition(p);
		    //TODO use a method setChannelStartingCondition(Particle p)instead, code below to be cleaned up.
		    /*
			// need to set x only channels.   
	    	Channel chan = (Channel) p.wb;
	    	int chanId = chan.getEnvIndex();
	    	p.x = super.getXLocationInChannel(chan, p.nd);
	    	float swimVel = ((SalmonSwimHelper) p.getSwimHelper()).getSwimmingVelocity(p.Id, chanId);
	    	int confFactor = ((SalmonSwimHelper) p.getSwimHelper()).getConfusionFactor(chanId);
	    	p.setSwimmingVelocity(swimVel*confFactor);
	    	p.setConfusionFactor(confFactor);
	    	p.swimVelSetInJunction(true);
	    	// swimming time is set one per particle per channel group, here is the only place set a swimming time
	    	p.getSwimHelper().setSwimmingTime(p, chanId); // to set swimming time in SalmonHoldingTimeCalculator
	    	// set Swimming time in particle
	    	p.setSwimmingTime(((SalmonSwimHelper) p.getSwimHelper()).getSwimmingTime(p.Id, chanId));
			if(p.nd.getEnvIndex() == chan.getUpNodeId())
				p.setFromUpstream(true);
			else
				p.setFromUpstream(false);
			*/
		} //if (qUpSac > 0.7924 && qGs > 0 && qDownSac > 0)
		else		
			super.makeRouteDecision(p);
	}

}

//TODO original code to be cleaned up

/*
 public SalmonGSJRouteBehavior(RouteInputs rIn, Integer nodeId, Integer gsWbId) {
		super(rIn);
		_nodeId = nodeId;
		_gsWbId = gsWbId;
		System.out.println("Created SalmonGSJRouteBehavior...");
	}
	
	private Channel[] getChannels(Particle p){
		Node curNode = p.nd;
		if (curNode == null)
			PTMUtil.systemExit("p.nd is null, exit");
		if (curNode.getEnvIndex()!=_nodeId)
			PTMUtil.systemExit("in Georgiana Slough Junction, node id in input file is different from node id the particle encountered, exit.");
		ArrayList<Channel> chans = curNode.getChannels();
		if (chans == null || chans.size() != 3)
			PTMUtil.systemExit("in Georgiana Slough Junction, channel number is not 3, exit.");
		
		Channel sacUp = null, sacDown = null;
		Channel gs = (Channel) curNode.getChannel(_gsWbId);
		if (gs == null)
			PTMUtil.systemExit("could not found Georgiana Slough channel at Georgiana Slough Junction, exit.");
		for (int i = 0; i< 3; i++){
			Channel ch = chans.get(i);
			if (ch == null)
				PTMUtil.systemExit("could not found Sac River channels at Georgiana Slough Junction, exit.");
			if (ch.getDownNode().equals(curNode))
				sacUp = ch;
			else if (ch.getUpNode().equals(curNode)){
				if (!ch.equals(gs))
					sacDown = ch;
			}
		}
		return new Channel[] {sacUp, sacDown, gs};
	}

	// this one will be called as long as a SalmonGSJRouteBehavior is passed
	public void makeRouteDecision(Particle p) {
		try{
			if (p.nd.getNumberOfChannels() != 3)
				throw new ClassCastException("particle is in wrong a junction! channels != 3");
			else if (!Globals.Environment.getParticleType().equalsIgnoreCase("SALMON"))
				throw new ClassCastException("particle is not SALMON!");
		}
		catch(ClassCastException cce){
			cce.printStackTrace();
			PTMUtil.systemExit("Error: " + cce.getMessage());
		} 
				
		Channel[] channels = getChannels(p);
		Channel sacUp = channels[0]; // first sac up second sac down third gs
		Channel sacDown = channels[1];
		Channel gs = channels[2];
		// flow units are cfs*1000
		float qUpSac = sacUp.getFlow(0.0f)/1000;
		float qDownSac = sacDown.getFlow(0.0f)/1000;
		float qGs = gs.getFlow(0.0f)/1000;

		// if flow in the range of the regression model do the regression model
		//assume unidirectional flow and less than 50000cfs
		// else do the regular super.makeRouteDecision(...)
		if (qUpSac > 0 && qGs > 0 && qDownSac > 0 && qUpSac < 50 ){
			// day d=1 night d=0
		    int d = 1;
		    // unit of channel width is meter and all special units are meter		
		    float w = sacUp.getWidth(sacUp.getLength())*0.3048f;
		    float s = (qGs/(qGs+qDownSac) - 37.5f/144.8f)*w;
		    float pos = (p.y*0.3048f+0.241022f*w);  // pos y starts from center
		    float multi_v = -2.104f-0.531f*d-1.7f*(gs.getCurrentBarrierOp(p.nd.getEnvIndex()))
		    			+0.082f*s+0.068f*qUpSac+0.045f*pos-0.006f*qUpSac*pos;
		    double possibility = Math.exp(multi_v)/(1+Math.exp(multi_v));
		    if (possibility < p.nd.getRandomNumber()){
		    	p.wb = sacDown;
		    }
		    else
		    	p.wb = gs;			
		    if (p.observer != null) 
		    	p.observer.observeChange(ParticleObserver.WATERBODY_CHANGE,p);
			//set x to only channels.  other water body types don't need to be set. 
			 p.x = super.getXLocationInChannel((Channel)p.wb, p.nd);
		}
		else
			super.makeRouteDecision(p);
	}
 */

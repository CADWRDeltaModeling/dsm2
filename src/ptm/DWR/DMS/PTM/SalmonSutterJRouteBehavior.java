/**
 * 
 */
package DWR.DMS.PTM;

import java.util.Arrays;
import java.util.List;
import java.util.ArrayList;

/**
 * @author xwang
 *
 */
public class SalmonSutterJRouteBehavior extends SalmonUpSacRouteBehavior {

	/**
	 * @param in
	 * @param nodeId
	 */
	public SalmonSutterJRouteBehavior(RouteInputs in, Integer nodeId) {
		super(in, nodeId);
	}

	/* (non-Javadoc)
	 * @see DWR.DMS.PTM.SalmonUpSacRouteBehavior#calcA(float[])
	 */
	@Override
	Double calcA(float[] flows) {
		if (flows.length != 3)
			PTMUtil.systemExit("Error when calculate sutter junction flows for parameter a, system exit");
		//flow[0]: qStm; flow[1]: qSacDD; flow[2]: deltaQSacDD
		return (-1.624+1.077*flows[0]-0.728*flows[2]+0.159*flows[1]-4.108*(flows[0]*flows[2])) ;
	}

	/* (non-Javadoc)
	 * @see DWR.DMS.PTM.SalmonUpSacRouteBehavior#calcB(float[])
	 */
	@Override
	Double calcB(float[] flows) {
		if (flows.length != 2)
			PTMUtil.systemExit("Error when calculate sutter junction flows for parameter a, system exit");
		//flow[0]: qSut; flow[1]: qSacDD
		return (-0.964+0.814*flows[0]-3.650*flows[1]-3.028*(flows[0]*flows[1]));
	}

	/* (non-Javadoc)
	 * @see DWR.DMS.PTM.SalmonUpSacRouteBehavior#makeRouteDecision(DWR.DMS.PTM.Particle)
	 */
	@Override
	public void makeRouteDecision(Particle p) {
		check(p);
		RouteInputs rIn = getRouteInputs();
		int nodeId = getNodeId();
		Channel[] channels = getChannels(p, (new String[] {"SACUPSUT", "SACDOWNSUT", "SUT", "STM", "SACDOWNSTM"}),"UP");
		if (channels.length != 5)
			PTMUtil.systemExit("Error when read sutter junction channel info, system exit");
		Channel sacDown = channels[1];
		Channel sut = channels[2];
		Channel stm = channels[3];
		Channel sacDownDown = channels[4];
		//channel.getFlow(0.0f) is the same as channel.flowAt[0]
		float qSacDDCFS = sacDownDown.getFlow(0.0f);
		float qSutCFS = sut.getFlow(0.0f);
		float qSutPreCFS = sut.previousFlowAt[0];
		float qStmCFS = stm.getFlow(0.0f);
		float[] ratios = calcFlowProportions(new float[]{qSacDDCFS, qSutCFS, qStmCFS});
		//the flow unit in DSM2 is cfs. scale the flow to the unit used by route model, i.e., cms
		// and perform standardization
		float qSut = (scaled*qSutCFS-qSutMean)/qSutSD;
		float qSutPre = (scaled*qSutPreCFS-qSutMean)/qSutSD;
		float deltaQSut = qSut - qSutPre;
		float qStm = (scaled*qStmCFS-qStmMean)/qStmSD;
		float pSut = (ratios[1]-pSutMean)/pSutSD;
		float pStm = (ratios[2]-pStmMean)/pStmSD;
		double piSut = pi(calcB(new float[]{qSut, pSut}), calcA(new float[]{qStm, deltaQSut, pStm}), 1, 1);
		rIn.putEntrainmentRate(nodeId, 
				new ArrayList<Object>(Arrays.asList(ratios[1], ratios[2],qSutCFS, qStmCFS, deltaQSut, piSut)));
		selectChannel(p, new Channel[]{channels[0], channels[1], channels[2]}, nodeId, piSut);
}

/* (non-Javadoc)
 * @see DWR.DMS.PTM.JunctionHandler#execute(DWR.DMS.PTM.Junction, DWR.DMS.PTM.Particle)
 */
// this one will be called as long as a SalmonGSJRouteBehavior is passed
/*
//rIn.putEntrainmentRate(PTMHydroInput.getExtFromIntNode(nodeId), ratios[1], Math.round(qSutCFS), 
				//Math.round(qStmCFS), deltaQSut*qSutSD/scaled, piSut);
		//rIn.putEntrainmentRate(PTMHydroInput.getExtFromIntNode(nodeId), qSacDD/scaled, 1, 0, 0.0f, piSut);
		p.getSwimHelper().setMeanSwimmingVelocity(p.Id, p.wb.getEnvIndex());
		setChannelStartingCondition(p);
if (piSut < p.nd.getRandomNumber())
	    	p.wb = sacDown;
	    else
	    	p.wb = sut;
		if (p.observer != null) 
	    	p.observer.observeChange(ParticleObserver.WATERBODY_CHANGE,p);


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
	//else		
		//super.makeRouteDecision(p);
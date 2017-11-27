/**
 * 
 */
package DWR.DMS.PTM;

import java.util.ArrayList;
import java.util.Arrays;

/**
 * @author xwang
 *
 */
public class SalmonDCCRouteBehavior extends SalmonUpSacRouteBehavior {

	/**
	 * @param in
	 * @param nodeId
	 */
	public SalmonDCCRouteBehavior(RouteInputs in, Integer nodeId) {
		super(in, nodeId);
		// TODO Auto-generated constructor stub
	}

	/* (non-Javadoc)
	 * @see DWR.DMS.PTM.SalmonUpSacRouteBehavior#calcA(float[])
	 */
	@Override
	Double calcA(float[] flows) {
		if (flows.length != 3)
			PTMUtil.systemExit("Error when calculate GS junction flows for parameter a, system exit");
		//flow[0]: qSac; flow[1]: QGs; flow[2]: direction of qSac: U=0 qSac > 0; U=1 qSac < 0 
		return (-0.9-1.163*flows[0]+0.852*flows[1]+1.595*flows[2]) ;
	}

	/* (non-Javadoc)
	 * @see DWR.DMS.PTM.SalmonUpSacRouteBehavior#calcB(float[])
	 */
	@Override
	Double calcB(float[] flows) {
		if (flows.length != 2)
			PTMUtil.systemExit("Error when calculate DCC junction flows for parameter b, system exit");
		//flow[0]: qSac; flow[1]: deltaQSac; 
		return (-2.337-2.694*flows[0]-0.474*flows[1]) ;
	}

	/* (non-Javadoc)
	 * @see DWR.DMS.PTM.SalmonUpSacRouteBehavior#makeRouteDecision(DWR.DMS.PTM.Particle)
	 */
	@Override
	public void makeRouteDecision(Particle p){
		check(p);
		RouteInputs rIn = getRouteInputs();
		int nodeId = getNodeId();
		Channel[] channels = getChannels(p, (new String[] {"SACUPDCC", "SACUPGS", "DCC", "GS", "SACDOWNGS"}),"UP");
		if (channels.length != 5)
			PTMUtil.systemExit("Error when read sutter junction channel info, system exit"); 
		//sacUpDcc, channels[0]; sacUpgs, channels[1]; dcc, channels[2]; gs, channels[3]; sacDownDown, channels[4];
		//channel.getFlow(0.0f) is the same as channel.flowAt[0]
		float qDCCFS = channels[2].getFlow(0.0f);
		float qSacDDCFS = channels[4].getFlow(0.0f);
		float qGsCFS = channels[3].getFlow(0.0f);
		float dtSacDDCFS = channels[4].deltaFlowAt[0];
		//the flow unit in DSM2 is cfs. scale the flow to the unit used by route model, i.e., cms 
		//and standardized it
		float qSacDD = (scaled*qSacDDCFS-qSacDownGsMean)/qSacDownGsSD;
		float qGs = (scaled*qGsCFS-qGsMean)/qGsSD;
		float dtQSac = (scaled*dtSacDDCFS-dtQSacDownGsMean)/dtQSacDownGsSD;
		float dir = 0;
		if(qSacDDCFS < 0)
			dir = 1;
		int dccGate = 1;
		if ((Math.abs(qDCCFS) < GATECLOSEDFLOW))
			dccGate = 0;		
		double piDcc = pi(calcB(new float[]{qSacDD, dtQSac}), calcA(new float[]{qSacDD, qGs, dir}), dccGate, 1);
		selectChannel(p, new Channel[] {channels[0], channels[1], channels[2]}, nodeId, piDcc);
		rIn.putEntrainmentRate(nodeId, 
				new ArrayList<Object>(Arrays.asList(qSacDDCFS,dtSacDDCFS, qGsCFS, qDCCFS, piDcc)));
	
	}
}

/*
		//TODO put this block in a function, only need to pass three channels and piDcc
		//void selectChannel(Channels[] chans, double prob){}
		int sacUpDccId = sacUpDcc.getEnvIndex(), sacDownDccId = sacUpgs.getEnvIndex(), dccId = dcc.getEnvIndex();
		//mean swimming velocity set once per particle per channel group.
		//Here is the only place to set a mean swimming velocity.
		p.getSwimHelper().setMeanSwimmingVelocity(p.Id, sacUpDccId);
		p.getSwimHelper().setMeanSwimmingVelocity(p.Id, sacDownDccId);
		p.getSwimHelper().setMeanSwimmingVelocity(p.Id, dccId);
		//Swimming velocity here doesn't include confusion factor
		float swVelSacUpDcc = ((SalmonSwimHelper) p.getSwimHelper()).getSwimmingVelocity(p.Id, sacUpDccId);
		float swVelSacDownDcc = ((SalmonSwimHelper) p.getSwimHelper()).getSwimmingVelocity(p.Id, sacDownDccId);
		float swVelDcc = ((SalmonSwimHelper) p.getSwimHelper()).getSwimmingVelocity(p.Id, dccId);
		int confFacSacUpDcc = ((SalmonSwimHelper) p.getSwimHelper()).getConfusionFactor(sacUpDccId);
		int confFacSacDownDcc = ((SalmonSwimHelper) p.getSwimHelper()).getConfusionFactor(sacDownDccId);
		int confFacDcc = ((SalmonSwimHelper) p.getSwimHelper()).getConfusionFactor(dccId);
		float wbFlowSacUpDcc = Math.max(0.0f, sacUpgs.getInflowWSV(nodeId, swVelSacUpDcc*confFacSacUpDcc));
		float wbFlowSacDownDcc = Math.max(0.0f, sacDownDown.getInflowWSV(nodeId, swVelSacDownDcc*confFacSacDownDcc));
	
		if(piDcc < p.nd.getRandomNumber()){
			float total = wbFlowSacUpDcc + wbFlowSacDownDcc;
			if(wbFlowSacUpDcc/total < p.nd.getRandomNumber()){
				p.wb = sacUpgs;
				setChannelStartingCondition(p, swVelSacDownDcc, confFacSacDownDcc);
			}
			else{
				p.wb = sacUpDcc;
				setChannelStartingCondition(p, swVelSacUpDcc, confFacSacUpDcc);
			}	
		}
	    else{
	    	p.wb = dcc;
	    	setChannelStartingCondition(p, swVelDcc, confFacDcc);
	    }		
		if (p.observer != null) 
	    	p.observer.observeChange(ParticleObserver.WATERBODY_CHANGE,p);
	    	
*/

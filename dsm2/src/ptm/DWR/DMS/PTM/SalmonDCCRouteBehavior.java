/**
 * 
 */
package DWR.DMS.PTM;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;

/**
 * @author xwang
 * using junction models developed by Russ Perry's group in USGS to calculate entrainment/routing probability
 * the equations used are in the paper
 * "Effect of Tides, River Flow, and Gate Operations on Entrainment of Juvenile Salmon into the interior Sacramento-san Joaquin river Delta"
 * by Perry et al. 
 */
public class SalmonDCCRouteBehavior extends SalmonUpSacRouteBehavior {

	/**
	 * @param in
	 * @param nodeId
	 */
	public SalmonDCCRouteBehavior(RouteInputs in, Integer nodeId) {
		super(in, nodeId);
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
		selectChannel(p, new Channel[] {channels[0], channels[1], channels[2]}, nodeId, piDcc,2);
		rIn.putEntrainmentRate(nodeId, 
				new ArrayList<Object>(Arrays.asList(p.Id, qSacDDCFS,dtSacDDCFS, qGsCFS, qDCCFS, piDcc)));
	
	}
}

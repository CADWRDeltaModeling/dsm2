/**
 * 
 */
package DWR.DMS.PTM;

import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.ArrayList;

/**
 * @author xwang
 * junction model used to calculate entrainment/routing probability for Sutter Slough
 * the equations used are in paper:
 * "Effects of tidally varying river flow on entrainment of juvenile salmon into Sutter and Steamboat Sloughs"
 * by Romine et al.
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
		float dtQSutCFS = qSutCFS - qSutPreCFS;
		float qStmCFS = stm.getFlow(0.0f);
		float[] ratios = calcFlowProportions(new float[]{qSacDDCFS, qSutCFS, qStmCFS});
		//the flow unit in DSM2 is cfs. scale the flow to the unit used by the junction model, i.e., cms
		// and standardize it
		float qSut = (scaled*qSutCFS-qSutMean)/qSutSD;
		float qSutPre = (scaled*qSutPreCFS-qSutMean)/qSutSD;
		float deltaQSut = (scaled*dtQSutCFS-dtQSutMean)/dtQSutSD;
		//float deltaQSut = qSut - qSutPre;
		float qStm = (scaled*qStmCFS-qStmMean)/qStmSD;
		//from Jason email proportions should not be standardized
		//float pSut = (ratios[1]-pSutMean)/pSutSD;
		//float pStm = (ratios[2]-pStmMean)/pStmSD;
		float pSut = ratios[1];
		float pStm = ratios[2];
		int sutGate = 1;
		int stmGate = 1;
		if ((Math.abs(qSutCFS) < GATECLOSEDFLOW) || (p.nd.isFishScreenInstalled() && sut.isFishScreenInstalled()))
			sutGate = 0;
		if ((Math.abs(qStmCFS) < GATECLOSEDFLOW) || (p.nd.isFishScreenInstalled() && stm.isFishScreenInstalled()))
			stmGate = 0;
		double piSut = pi(calcB(new float[]{qSut, pSut}), calcA(new float[]{qStm, deltaQSut, pStm}), sutGate, stmGate);
		rIn.putEntrainmentRate(nodeId, 
				new ArrayList<Object>(Arrays.asList(p.Id, ratios[1], ratios[2],qSutCFS, qStmCFS, deltaQSut, piSut)));
		selectChannel(p, new Channel[]{channels[0], channels[1], channels[2]}, nodeId, piSut,  0);
	}
} 
/**
 *
 */
package DWR.DMS.PTM;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;

/**
 * @author xwangs
 * junction model used to calculate entrainment/routing probability for Steamboat Slough
 * the equations used are in paper:
 * "Effects of tidally varying river flow on entrainment of juvenile salmon into Sutter and Steamboat Sloughs"
 * by Romine et al.
 */
public class SalmonSTMJRouteBehavior extends SalmonSutterJRouteBehavior {

	/**
	 * @param in
	 * @param nodeId
	 */
	public SalmonSTMJRouteBehavior(RouteInputs in, Integer nodeId) {
		super(in, nodeId);
	}

	/* (non-Javadoc)
	 * @see DWR.DMS.PTM.SalmonUpSacRouteBehavior#makeRouteDecision(DWR.DMS.PTM.Particle)
	 */
	@Override
	public void makeRouteDecision(Particle p) {
		check(p);
		RouteInputs rIn = getRouteInputs();
		int nodeId = getNodeId();
		Channel[] channels = getChannels(p, (new String[] {"SACUPSUT", "SACDOWNSUT", "SUT", "STM", "SACDOWNSTM"}),"DOWN");
		if (channels.length != 5)
			PTMUtil.systemExit("Error when read sutter junction channel info, system exit");
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
		//the flow unit in DSM2 is cfs. scale the flow to the unit used by route model, i.e., cms
		float qSut = (scaled*qSutCFS-qSutMean)/qSutSD;
		float qSutPre = (scaled*qSutPreCFS-qSutMean)/qSutSD;
		//float deltaQSut = qSut - qSutPre;
		float deltaQSut = (scaled*dtQSutCFS-dtQSutMean)/dtQSutSD;
		float qStm = (scaled*qStmCFS-qStmMean)/qStmSD;
		//from Jason email proportions should not be standardized
		//float pSut = (ratios[1]-pSutMean)/pSutSD;
		//float pStm = (ratios[2]-pStmMean)/pStmSD;
		float pSut = ratios[1];
		float pStm = ratios[2];
		double a = calcA(new float[]{qStm, deltaQSut, pStm});
		double b = calcB(new float[]{qSut, pSut});
		int sutGate = 1;
		int stmGate = 1;
		if ((Math.abs(qSutCFS) < GATECLOSEDFLOW) || (p.nd.isFishScreenInstalled() && sut.isFishScreenInstalled()))
			sutGate = 0;
		if ((Math.abs(qStmCFS) < GATECLOSEDFLOW) || (p.nd.isFishScreenInstalled() && stm.isFishScreenInstalled()))
			stmGate = 0;
		double piSut = pi(b, a, sutGate, stmGate);
		double piStm = pi(a, b, stmGate, sutGate);
		double stmProb = piStm/(1.0d-piSut);
		rIn.putEntrainmentRate(nodeId,
				new ArrayList<Object>(Arrays.asList(p.Id, ratios[1],ratios[2],qSutCFS,qStmCFS,deltaQSut,stmProb)));
		selectChannel(p, new Channel[]{channels[1],channels[4],channels[3]}, nodeId, stmProb, 1);
	}
}

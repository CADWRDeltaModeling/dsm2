/**
 * 
 */
package DWR.DMS.PTM;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

/**
 * @author xwangs
 *
 */
public class SalmonSTMJRouteBehavior extends SalmonSutterJRouteBehavior {

	/**
	 * @param in
	 * @param nodeId
	 */
	public SalmonSTMJRouteBehavior(RouteInputs in, Integer nodeId) {
		super(in, nodeId);
		// TODO Auto-generated constructor stub
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
		float qStmCFS = stm.getFlow(0.0f);
		float[] ratios = calcFlowProportions(new float[]{qSacDDCFS, qSutCFS, qStmCFS});
		//the flow unit in DSM2 is cfs. scale the flow to the unit used by route model, i.e., cms 
		float qSut = (scaled*qSutCFS-qSutMean)/qSutSD;
		float qSutPre = (scaled*qSutPreCFS-qSutMean)/qSutSD;
		float deltaQSut = qSut - qSutPre;
		float qStm = (scaled*qStmCFS-qStmMean)/qStmSD;
		float pSut = (ratios[1]-pSutMean)/pSutSD;
		float pStm = (ratios[2]-pStmMean)/pStmSD;
		double a = calcA(new float[]{qStm, deltaQSut, pStm});
		double b = calcB(new float[]{qSut, pSut});
		double piSut = pi(b, a, 1, 1);
		double piStm = pi(a, b, 1, 1);
		double stmProb = piStm/(1.0d-piSut);
		rIn.putEntrainmentRate(nodeId, 
				new ArrayList<Object>(Arrays.asList(ratios[1],ratios[2],qSutCFS,qStmCFS,deltaQSut,stmProb)));
		selectChannel(p, new Channel[]{channels[1],channels[4],channels[3]}, nodeId, stmProb);
	}
}
/*
if (stmProb < p.nd.getRandomNumber())
p.wb = sacDownDown;
else
p.wb = stm;
if (p.observer != null) 
p.observer.observeChange(ParticleObserver.WATERBODY_CHANGE,p);

//rIn.putEntrainmentRate(PTMHydroInput.getExtFromIntNode(nodeId), ratios[2], Math.round(qSutCFS), 
	//Math.round(qStmCFS), deltaQSut*qSutSD/scaled, stmProb);
p.getSwimHelper().setMeanSwimmingVelocity(p.Id, p.wb.getEnvIndex());
setChannelStartingCondition(p);
*/
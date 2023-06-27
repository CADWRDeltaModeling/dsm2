/**
 * 
 */
package DWR.DMS.PTM;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;

import org.apache.commons.math3.special.Beta;

/**
 * @author xwang
 * using junction models developed by Russ Perry's group in USGS to calculate entrainment/routing probability
 * inherited from SalmonDCCRouteBehavior so some of the methods such as calcA, calcB can be used.  
 */
public class SalmonGSJRouteBehavior extends SalmonDCCRouteBehavior {
	private int _nodeId;
	private RouteInputs _rIn = null;
	private boolean DEBUG = false;
	/**
	 * 
	 */
	public SalmonGSJRouteBehavior(RouteInputs rIn, Integer nodeId) {
		super(rIn, nodeId);
		_rIn = rIn;
		_nodeId = nodeId;
		System.out.println("Created SalmonGSJRouteBehavior...");
	}

	/* (non-Javadoc)
	 * @see 
	 * override the method in SalmonDCCRouteBehavior
	 */
	public void makeRouteDecision(Particle p) {
		check(p);
		RouteInputs rIn = getRouteInputs();
		int nodeId = getNodeId();
		//Down means this is the downstream junction in the DCC-GS junction group
		Channel[] channels = getChannels(p, (new String[] {"SACUPDCC", "SACUPGS", "DCC", "GS", "SACDOWNGS"}),"DOWN");
		if (channels.length != 5)
			PTMUtil.systemExit("Error when read sutter junction channel info, system exit");
		Channel sacUpgs = channels[1];
		Channel dcc = channels[2];
		Channel gs = channels[3];
		Channel sacDownDown = channels[4];
		//channel.getFlow(0.0f) is the same as channel.flowAt[0]
		float qUpSacGSCFS = sacUpgs.getFlow(0.0f);
		float qDCCFS = dcc.getFlow(0.0f);
		float qSacDDCFS = sacDownDown.getFlow(0.0f);
		float qGsCFS = gs.getFlow(0.0f);
		float dtSacDDCFS = sacDownDown.deltaFlowAt[0];
		double gsProbability;		
				
		/* if particle from upstream and unidirectional flow greater than 14000cfs
		 * using the model in:
		 * "Georgiana Slough: Combined model of the critical streakline, cross-stream fish distribution and entrainment probability"
		 * by Dalton et al.
		 * otherwise use the model in
		 * "Effect of Tides, River Flow, and Gate Operations on Entrainment of Juvenile Salmon into the interior Sacramento-san Joaquin river Delta"
		 * by Perry et al. 
		 */
		// else using Russ' regression model 
		
		if (qUpSacGSCFS > 14000 && qGsCFS > 0 && qSacDDCFS > 0 && (p.wb.getEnvIndex() == sacUpgs.getEnvIndex())){
			//the flow unit in DSM2 is cfs. scale the flow to the unit used by route model, i.e., cms/500 
			float gs_scaled = 0.0283f/500.0f;
			float qUpSac = gs_scaled*qUpSacGSCFS;
			float qDownSac = gs_scaled*qSacDDCFS;
			float qGs = gs_scaled*qGsCFS;
			if (DEBUG == true)
				System.err.println("nodeId:"+PTMHydroInput.getExtFromIntNode(p.nd.getEnvIndex())
					+" wb:"+PTMHydroInput.getExtFromIntChan(p.wb.getEnvIndex())+"  pId="+p.Id+" sacUpFlow:"
					+sacUpgs.getFlow(0.0f)+" sacDownFlow:"+sacDownDown.getFlow(0.0f)+" gsFlow:"+gs.getFlow(0.0f)
					+" swimming velocity:"+p.getSwimmingVelocity());
			float sr = qGs/(qGs + qDownSac);
			double si = 1/(1+Math.exp(0.209-0.618*Math.log(sr/(1-sr))));
			// day d=0 night d=1
		    int d = 0;
		    if (!SwimInputs.isDayTime())
		    	d =1;
		    //barrier operation 1: on, 0: off
		    int bOp = gs.getCurrentBarrierOp(p.nd.getEnvIndex());
		    //pr1 pr0 conditional probability of entrainment
		    double mu, epow, epowPR1, epowPR0, pr1, pr0, a, b, p1, p0;
		    //Although Because of div flow, qDownSac+qGs != qUpSac, qUpSac should still be used because the original equation 
		    //is derived from the upstream Sacramento River flow
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
		    _rIn.putEntrainmentRate(_nodeId, 
		    		new ArrayList<Object>(Arrays.asList(p.Id, qUpSac/gs_scaled, d, bOp, sr, "", "",gsProbability)));
		} //if (qUpSacGSCFS > 14000 && qGsCFS > 0 && qSacDDCFS > 0 && (p.wb.getEnvIndex() == sacUpgs.getEnvIndex()))
		else{					
			//the flow unit in DSM2 is cfs. scale the flow to the unit used by the junction model, i.e., cms 
			//and standardized it			
			float qSacDD = (scaled*qSacDDCFS-qSacDownGsMean)/qSacDownGsSD;
			float qGss = (scaled*qGsCFS-qGsMean)/qGsSD;
			float dtQSac = (scaled*dtSacDDCFS-dtQSacDownGsMean)/dtQSacDownGsSD;
			/* using cfs
			float qSacDD = (qSacDDCFS-qSacDownGsCfsMean)/qSacDownGsCfsSD;
			float qGss = (qGsCFS-qGsCfsMean)/qGsCfsSD;
			float dtQSac = (scaled*dtSacDDCFS-dtQSacDownGsMean)/dtQSacDownGsSD;
			*/
			float dir = 0;
			if(qSacDDCFS < 0)
				dir = 1;
			//gate open 1, close 0
			int dccGate = 1;
			int gsGate = 1;
			if ((Math.abs(qGsCFS) < GATECLOSEDFLOW) || (p.nd.isFishScreenInstalled() && gs.isFishScreenInstalled()))
				gsGate = 0;
			if ((Math.abs(qDCCFS) < GATECLOSEDFLOW) || (p.nd.isFishScreenInstalled() && dcc.isFishScreenInstalled()))
				dccGate = 0;
			double a = calcA(new float[]{qSacDD, qGss, dir});
			double b = calcB(new float[]{qSacDD, dtQSac});
			double piDcc = pi(b, a, dccGate,gsGate);
			double piGs = pi(a, b, gsGate, dccGate);	
			gsProbability = piGs/(1.0d-piDcc);
			rIn.putEntrainmentRate(nodeId, 
					new ArrayList<Object>(Arrays.asList(p.Id, qSacDDCFS,dtSacDDCFS, qGsCFS, qDCCFS, piDcc, piGs, gsProbability)));
		}
		selectChannel(p, new Channel[]{sacUpgs, sacDownDown, gs}, nodeId, gsProbability,3);
		
	}
}


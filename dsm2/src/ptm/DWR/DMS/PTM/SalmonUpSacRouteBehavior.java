/**
 * 
 */
package DWR.DMS.PTM;
import java.util.Map;
import java.util.HashMap;
/**
 * @author xwang
 * including common methods used for the upstream Sacramento River junction models
 */
public abstract class SalmonUpSacRouteBehavior extends SalmonBasicRouteBehavior {
	private int _nodeId;
	private RouteInputs _rIn = null;
	float scaled = 0.0283f;
	/* mean flow in cms
	 * parameters (mean, standard deviation) are used to standardize the junction model variables.
	 * the parameters can be found in the papers
	 * "Effects of tidally varying river flow on entrainment of juvenile salmon into Sutter and Steamboat Sloughs"
	 * and "Effect of Tides, River Flow, and Gate Operations on Entrainment of Juvenile Salmon into the interior Sacramento-san Joaquin river Delta"
	 */

	final static float qStmMean = 77.0f, qStmSD = 46.0f, dtQStmMean = -1.661f, dtQStmSD = 5.63f;
	final static float qSutMean = 91.0f, qSutSD = 36.0f, dtQSutMean = -0.5327f, dtQSutSD = 4.25f;
	final static float qSacDownStmMean = 170.0f, qSacDownStmSD = 143.0f;
	final static float pSutMean = 0.213f, pSutSD = 0.072f;
	final static float pStmMean = 0.165f, pStmSD = 0.059f;
	final static float qSacDownGsMean = 177.1f, qSacDownGsSD = 139.3f;
	final static float dtQSacDownGsMean = -2.1f, dtQSacDownGsSD = 13.5f;
	final static float qGsMean = 86.2f, qGsSD = 32.4f;
	final static float qGsCfsMean = 3044f, qGsCfsSD = 1143f;
	final static float qSacDownGsCfsMean = 6254f, qSacDownGsCfsSD = 4918f;
	final static float GATECLOSEDFLOW = Float.MIN_VALUE;
	
	/**
	 * @param in
	 */
	public SalmonUpSacRouteBehavior(RouteInputs in, Integer nodeId) {
		super(in);
		_nodeId = nodeId;
		_rIn = in;
		System.out.println("Created SalmonUpSacRouteBehavior...");
	}
	void check(Particle p){
		try{
			if (p.nd.getNumberOfChannels() != 3)
				throw new ClassCastException("particle is in wrong a junction! channels != 3");
			else if (!Globals.Environment.getParticleType().equalsIgnoreCase("SALMON_PARTICLE"))
				throw new ClassCastException("particle is not SALMON!");
		}
		catch(ClassCastException cce){
			cce.printStackTrace();
			PTMUtil.systemExit("Error: " + cce.getMessage());
		}
	}
	float[] calcFlowProportions(float[] flows){
		int size = flows.length;
		float[] flowsForP = new float[size];
		float[] ratios = new float[size];
		float sum = 0;
		for(int i=0; i<size; i++){
			if(flows[i] > 0)
				flowsForP[i] = flows[i];
			else
				flowsForP[i] = 0;
			sum += flowsForP[i];
		}
		for (int i=0; i<size; i++){
			//sum > = 0
			if(sum < 0.00000000001)
				ratios[i] = 0;
			else
				ratios[i] = flowsForP[i]/sum;
		}
		return ratios;
	}
	Channel[] getChannels(Particle p, String[] channelNames, String upDown){
		Node curNode = p.nd;		
		if ((curNode == null)&& (channelNames.length != 5))
			PTMUtil.systemExit("p.nd is null or number of channels is not 5, exit");
		if (curNode.getEnvIndex()!=_nodeId)
			PTMUtil.systemExit("Node id in input file is different from node id the particle encountered, exit.");
		
		Channel sacUp = null;
		Channel sacDown = null;
		Channel tri1 = null;		 
		Channel tri2 = null;
		Channel sacDownDown = null;
		
		//ex. channelNames[]:{"SACUPDCC", "SACUPGS", "DCC", "GS", "SACDOWNGS"}
		//"up" means current node is at the first junction; "down" means current node is at the second junction
		if(upDown.equalsIgnoreCase("UP")){
			sacUp = (Channel) curNode.getChannel(_rIn.getChannelId(channelNames[0])); 
			sacDown = (Channel) curNode.getChannel(_rIn.getChannelId(channelNames[1]));
			tri1 = (Channel) curNode.getChannel(_rIn.getChannelId(channelNames[2]));
			Node sacDownNd = sacDown.getDownNode();
			tri2 = (Channel)sacDownNd.getChannel(_rIn.getChannelId(channelNames[3]));
			sacDownDown = (Channel)sacDownNd.getChannel(_rIn.getChannelId(channelNames[4]));
		}
		if(upDown.equalsIgnoreCase("DOWN")){
			sacDown = (Channel) curNode.getChannel(_rIn.getChannelId(channelNames[1]));
			Node sacUpNd = sacDown.getUpNode();
			tri1 = (Channel)sacUpNd.getChannel(_rIn.getChannelId(channelNames[2]));
			sacUp = (Channel)sacUpNd.getChannel(_rIn.getChannelId(channelNames[0])); 
			tri2 = (Channel) curNode.getChannel(_rIn.getChannelId(channelNames[3]));
			sacDownDown = (Channel) curNode.getChannel(_rIn.getChannelId(channelNames[4]));
		}
		if (tri1 == null || tri2 == null || sacUp == null || sacDown == null|| sacDownDown == null)
			PTMUtil.systemExit("Behavior input file entered wrong channel names, exit.");
		return new Channel[]{sacUp, sacDown, tri1, tri2, sacDownDown};
	}
	void selectChannel(Particle p, Channel[] chans, int nodeId, double prob, int jId){
		/*
		 * only select twice at these four junctions
		 * jId: Sutter(0), Steamboat(1), DCC(2), Geo(3)
		 */
		double pctPass = 1.0f;
		int vis = _rIn.visited(jId, p.Id);
		//Russ Perry suggested if a particle visited the junction more than twice (first: 0, second: 1), the particle cannot enter the branch anymore.
		//if(vis > 1)
		//Russ Perry suggested if a particle have visited once and not entered the branch, the particle cannot enter the branch anymore when coming back
		if(vis > 0)
			pctPass = 0;
		
		//next code for experiment purpose
		/*
		if(vis > 0){
			if(vis == 1)
				pctPass = 0.5;
			else
				pctPass = 0;
		}
		*/
		
			//decided not to use
			//pctPass = Math.pow(0.6, vis);
		
		//TODO decided not to use anymore
		/*
		int selectedChanId = _rIn.getUpSacJChan(jId, p.Id);
		
		if( selectedChanId > 0){
			//System.err.println(p.Id+"  "+p.wb.getEnvIndex()+"  "+selectedChanId);
			for (Channel chan: chans){
				if(chan.getEnvIndex() == selectedChanId){
					p.wb = chan;
					break;
				}
			}
			setChannelStartingCondition(p, ((SalmonSwimHelper) p.getSwimHelper()).getSwimmingVelocity(p.Id, selectedChanId), 
			((SalmonSwimHelper) p.getSwimHelper()).getConfusionFactor(selectedChanId));
			return;
		}
		*/
		
		int sacUpId = chans[0].getEnvIndex(), sacDownId = chans[1].getEnvIndex(), bId = chans[2].getEnvIndex();
		//mean swimming velocity set once per particle per channel group.
		//Here is the only place to set a mean swimming velocity.
		p.getSwimHelper().setMeanSwimmingVelocity(p.Id, sacUpId);
		p.getSwimHelper().setMeanSwimmingVelocity(p.Id, sacDownId);
		p.getSwimHelper().setMeanSwimmingVelocity(p.Id, bId);
		//Swimming velocity here doesn't include confusion factor
		float swVelSacUp = ((SalmonSwimHelper) p.getSwimHelper()).getSwimmingVelocity(p, sacUpId);
		float swVelSacDown = ((SalmonSwimHelper) p.getSwimHelper()).getSwimmingVelocity(p, sacDownId);
		float swVelBranch = ((SalmonSwimHelper) p.getSwimHelper()).getSwimmingVelocity(p, bId);
		int confFacSacUp = ((SalmonSwimHelper) p.getSwimHelper()).getConfusionFactor(sacUpId);
		int confFacSacDown = ((SalmonSwimHelper) p.getSwimHelper()).getConfusionFactor(sacDownId);
		int confFacBranch = ((SalmonSwimHelper) p.getSwimHelper()).getConfusionFactor(bId);
		float wbFlowSacUp = Math.max(0.0f, chans[0].getInflowWSV(nodeId, swVelSacUp*confFacSacUp));
		float wbFlowSacDown = Math.max(0.0f, chans[1].getInflowWSV(nodeId, swVelSacDown*confFacSacDown));
		float wbFlowBranch = Math.max(0.0f, chans[2].getInflowWSV(nodeId, swVelBranch*confFacBranch));
		double ram = p.nd.getRandomNumber();
		float totalSac = wbFlowSacUp + wbFlowSacDown;
		float total = totalSac + wbFlowBranch;
		
		if (total < Float.MIN_VALUE){
			p.particleWait = true;
		    return;
		}
		
		//original code
		//users can input a modify amount to increase or decrease the chance.
		//double probModified = Math.min(prob*pctPass*((double)getRouteInputs().getPercentToModify(nodeId))/100.0d, 1.0d);
		
		/*according to Russ and Aaron, the entrainment increase to STM should be added (not multiplied)to the calculated
		 *because at a low flow condition, entrainment to STM is very small.  if using multiply, it won't very effective 
		 */
		//TODO should have Math.max(calculated, modAmount) instead?
		
		double probModified = 0.0d;
		double modAmount = ((double)getRouteInputs().getPercentToModify(nodeId))/100.0d;
		//jId: SUT = 0, STM = 1, DCC = 2, GEO = 3
		// at the junction SUT or STM if a user input increase/reduce entrainment > 1, add modAmount - 1, otherwise multiply
		if((modAmount > 1) && (jId == 1 || jId == 0))
			probModified = Math.min((prob+(modAmount-1))*pctPass, 1.0d);
		
		//TODO original algorithm, not used anymore clean up
		//At the junction SUT or STM, if user input increase/reduce entrainment < 1, add, otherwise multiply
		/*
		if((modAmount < 1) && (jId == 1 || jId == 0))
			//add pct (user input) to the calculated route probability
			probModified = Math.min((prob+modAmount)*pctPass, 1.0d);
		*/
		
		else
			//multiply pct to the calculated route probability
			probModified = Math.min(prob*modAmount*pctPass, 1.0d);
		
		// if branch flow = 0 Russ's model won't work because the model doesn't include a swimming velocity & 
		// could ask particles to go down the branch but because of the negative swimming velocity 
		// the particles cannot causing problems 
		//TODO call super not working because branch still is consider if super is called, need to refactoring the method in the super
		/*
		if((wbFlowBranch < Float.MIN_VALUE) || (probModified < ram)){
			//System.err.println("nd:"+p.nd.getEnvIndex()+"  wb:"+p.wb.getEnvIndex()+"  confFacBranch:" + confFacBranch + "  swVelBranch:"+swVelBranch + "  wbFlowBranch:" +wbFlowBranch);		
			super.makeRouteDecision(p);
			//System.err.println("nd:"+p.nd.getEnvIndex()+"  wb:"+p.wb.getEnvIndex());
			return;
		}
		*/
		
		if((wbFlowBranch < Float.MIN_VALUE) || (probModified < ram)){
			/*
			System.err.println(PTMHydroInput.getExtFromIntNode(p.nd.getEnvIndex())
							+"  "+PTMHydroInput.getExtFromIntChan(p.wb.getEnvIndex())
							+"  confFacBranch:" + confFacBranch 
							+ "  swVelBranch:"+swVelBranch 
							+ "  wbFlowBranch:" +wbFlowBranch
							+ "  swVelSacUp:"+swVelSacUp
							+ "  wbFlowSacUp:" +wbFlowSacUp
							+ "  swVelSacDown:"+swVelSacDown
							+ "  wbFlowSacDown:" +wbFlowSacDown);	
			*/
			
			//only allow to go downstream to be consistent with the statistical model
			//p.wb = chans[1];	
			//setChannelStartingCondition(p, swVelSacDown, confFacSacDown);
			
			//if total flow in Sac River is equal to 0, the particle has 50% chance going up or down			
			double chanceUp = 0;
			if (totalSac < Float.MIN_VALUE)
				chanceUp = 0.5;
			else
				chanceUp = wbFlowSacUp/totalSac;
			if(chanceUp < p.nd.getRandomNumber()){
				p.wb = chans[1];
				setChannelStartingCondition(p, swVelSacDown, confFacSacDown);
			}
			else{
				p.wb = chans[0];
				setChannelStartingCondition(p, swVelSacUp, confFacSacUp);
			}
					
			//_rIn.setUpSacJChan(jId, p.Id, -1);
			//System.err.println(PTMHydroInput.getExtFromIntNode(p.nd.getEnvIndex())
					//+"  "+PTMHydroInput.getExtFromIntChan(p.wb.getEnvIndex()));
		}
		
	    else{
	    	p.wb = chans[2];
	    	setChannelStartingCondition(p, swVelBranch, confFacBranch);
	    	//_rIn.setUpSacJChan(jId, p.Id, p.wb.getEnvIndex());
	    }
		if (p.observer != null) 
	    	p.observer.observeChange(ParticleObserver.WATERBODY_CHANGE,p);
		/* for debug
		if(nodeId == 308)
			System.err.println(""+p.Id+"  "+ram+"  "+prob+"  "+p.wb.getEnvIndex());
		*/
	}
	abstract Double calcA(float[] flows);
	abstract Double calcB(float[] flows);
	public abstract void makeRouteDecision(Particle p);
	//switch a, b to find pi for SUT or STEM
	double pi(double a, double b, int gateA, int gateB){
		return (gateA*Math.exp(a)/(1+gateA*Math.exp(a)+gateB*Math.exp(b)));
	}
	int getNodeId(){return _nodeId;}
}

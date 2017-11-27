/**
 * 
 */
package DWR.DMS.PTM;

/**
 * @author xwang
 *
 */
public abstract class SalmonUpSacRouteBehavior extends SalmonBasicRouteBehavior {
	private int _nodeId;
	float scaled = 0.0283f;
	//mean flow in cms
	final static float qStmMean = 77.0f, qStmSD = 46.0f;
	final static float qSutMean = 91.0f, qSutSD = 36.0f;
	final static float qSacDownStmMean = 170.0f, qSacDownStmSD = 143.0f;
	final static float pSutMean = 0.213f, pSutSD = 0.072f;
	final static float pStmMean = 0.165f, pStmSD = 0.059f;
	final static float qSacDownGsMean = 177.1f, qSacDownGsSD = 139.3f;
	final static float dtQSacDownGsMean = -2.1f, dtQSacDownGsSD = 13.5f;
	final static float qGsMean = 86.2f, qGsSD = 32.4f;
	final static float GATECLOSEDFLOW = Float.MIN_VALUE;
	
	/**
	 * @param in
	 */
	public SalmonUpSacRouteBehavior(RouteInputs in, Integer nodeId) {
		super(in);
		_nodeId = nodeId;
		System.out.println("Created SalmonUpSacRouteBehavior...");
	}
	void check(Particle p){
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
		
		RouteInputs rIn = getRouteInputs();
		
		//ex. channelNames[]:{"SACUPSUT", "SACDOWNSUT", "SUT", "STM", "SACDOWNSTM"}
		//"up" means current node is at the first junction; "down" means current node is at the second junction
		if(upDown.equalsIgnoreCase("UP")){
			sacUp = (Channel) curNode.getChannel(rIn.getChannelId(channelNames[0])); 
			sacDown = (Channel) curNode.getChannel(rIn.getChannelId(channelNames[1]));
			tri1 = (Channel) curNode.getChannel(rIn.getChannelId(channelNames[2]));
			Node sacDownNd = sacDown.getDownNode();
			tri2 = (Channel)sacDownNd.getChannel(rIn.getChannelId(channelNames[3]));
			sacDownDown = (Channel)sacDownNd.getChannel(rIn.getChannelId(channelNames[4]));
		}
		if(upDown.equalsIgnoreCase("DOWN")){
			sacDown = (Channel) curNode.getChannel(rIn.getChannelId(channelNames[1]));
			Node sacUpNd = sacDown.getUpNode();
			tri1 = (Channel)sacUpNd.getChannel(rIn.getChannelId(channelNames[2]));
			sacUp = (Channel)sacUpNd.getChannel(rIn.getChannelId(channelNames[0])); 
			tri2 = (Channel) curNode.getChannel(rIn.getChannelId(channelNames[3]));
			sacDownDown = (Channel) curNode.getChannel(rIn.getChannelId(channelNames[4]));
		}
		if (tri1 == null || tri2 == null || sacUp == null || sacDown == null|| sacDownDown == null)
			PTMUtil.systemExit("Behavior input file entered wrong channel names, exit.");
		return new Channel[]{sacUp, sacDown, tri1, tri2, sacDownDown};
	}
	void selectChannel(Particle p, Channel[] chans, int nodeId, double prob){
		int sacUpId = chans[0].getEnvIndex(), sacDownId = chans[1].getEnvIndex(), bId = chans[2].getEnvIndex();
		//mean swimming velocity set once per particle per channel group.
		//Here is the only place to set a mean swimming velocity.
		p.getSwimHelper().setMeanSwimmingVelocity(p.Id, sacUpId);
		p.getSwimHelper().setMeanSwimmingVelocity(p.Id, sacDownId);
		p.getSwimHelper().setMeanSwimmingVelocity(p.Id, bId);
		//Swimming velocity here doesn't include confusion factor
		float swVelSacUp = ((SalmonSwimHelper) p.getSwimHelper()).getSwimmingVelocity(p.Id, sacUpId);
		float swVelSacDown = ((SalmonSwimHelper) p.getSwimHelper()).getSwimmingVelocity(p.Id, sacDownId);
		float swVelBranch = ((SalmonSwimHelper) p.getSwimHelper()).getSwimmingVelocity(p.Id, bId);
		int confFacSacUp = ((SalmonSwimHelper) p.getSwimHelper()).getConfusionFactor(sacUpId);
		int confFacSacDown = ((SalmonSwimHelper) p.getSwimHelper()).getConfusionFactor(sacDownId);
		int confFacBranch = ((SalmonSwimHelper) p.getSwimHelper()).getConfusionFactor(bId);
		float wbFlowSacUp = Math.max(0.0f, chans[0].getInflowWSV(nodeId, swVelSacUp*confFacSacUp));
		float wbFlowSacDown = Math.max(0.0f, chans[1].getInflowWSV(nodeId, swVelSacDown*confFacSacDown));
		double ram = p.nd.getRandomNumber();
		float total = wbFlowSacUp + wbFlowSacDown;  
		if(prob < ram){
			//if total flow in Sac River is equal to 0, the particle has 50% chance going up or down
			double chanceUp = 0;
			if (total < Float.MIN_VALUE)
				chanceUp = 0.5;
			else
				chanceUp = wbFlowSacUp/total;
			if(chanceUp < p.nd.getRandomNumber()){
				p.wb = chans[1];
				setChannelStartingCondition(p, swVelSacDown, confFacSacDown);
			}
			else{
				p.wb = chans[0];
				setChannelStartingCondition(p, swVelSacUp, confFacSacUp);
			}
		}
	    else{
	    	p.wb = chans[2];
	    	setChannelStartingCondition(p, swVelBranch, confFacBranch);
	    }		
		if (p.observer != null) 
	    	p.observer.observeChange(ParticleObserver.WATERBODY_CHANGE,p);
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

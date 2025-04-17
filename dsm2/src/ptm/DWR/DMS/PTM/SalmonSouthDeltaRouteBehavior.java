/**
 *
 */
package DWR.DMS.PTM;

import java.time.ZonedDateTime;
import java.time.format.DateTimeFormatter;

import java.util.ArrayList;
import java.util.List;
/**
 * @author Doug Jackson, QEDA Consulting, LLC
 * Common methods used for the South Delta river junction models
 */
public abstract class SalmonSouthDeltaRouteBehavior extends SalmonBasicRouteBehavior {
	private int _nodeId;
	private RouteInputs _rIn = null;
	protected double transProbToU, transProbToD, transProbToT, distUD_ft, distUT_ft, distDT_ft;
	protected String decisionType;
	final String[] transitions = new String[]{"qUD", "qDU", "qDU", "qDT", "qTU", "qTD"};

	static final float GATECLOSEDFLOW = Float.MIN_VALUE;
	static final int MISSING_VALUE = -999;

	protected VelMethods velMethod;
	public enum VelMethods {
		UPSTREAM_FIRST,
		FLOW_WEIGHTED_AVG;
	}

	protected ChannelGroup fromChannelGroup, toChannelGroup;
	public enum ChannelGroup {
		UPSTREAM,
		DISTRIB,
		DOWNSTREAM;
	}

	public SalmonSouthDeltaRouteBehavior(RouteInputs in, Integer nodeId) {
		super(in);
		_nodeId = nodeId;
		_rIn = in;
		System.out.println("Created SalmonSouthDeltaRouteBehavior...");

		// Specify the velocity calculation method
		velMethod = VelMethods.FLOW_WEIGHTED_AVG;
	}

	/**
	 * Verify that this is a salmon particle
	 * @param p				particle
	 */
	void check(Particle p){
		try{
			if (!Globals.Environment.getParticleType().equalsIgnoreCase("SALMON_PARTICLE"))
				throw new ClassCastException("particle is not SALMON!");
		}
		catch(ClassCastException cce){
			cce.printStackTrace();
			PTMUtil.systemExit("Error: " + cce.getMessage());
		}
	}

	/**
	 * Look up the Channel objects corresponding to the specified channel names
	 * @param p				particle
	 * @param channelNames	String[] of channel names
	 * @return				Array of Channel objects
	 */
	Channel[] getChannels(Particle p, String[] channelNames){

		Node curNode;
		Channel thisChannel;
		List<Channel> channelArrayList;

		curNode = p.nd;

		if ((curNode == null)) {
			PTMUtil.systemExit("p.nd is null. Exit");
		}
		if (curNode.getEnvIndex()!=_nodeId) {
			PTMUtil.systemExit("Node id in input file is different from node id the particle encountered, exit.");
		}

		// Look up the Channel object corresponding to the channel names
		channelArrayList = new ArrayList<Channel>();
		for(int i=0; i<channelNames.length; i++) {
			thisChannel = (Channel) curNode.getChannel(_rIn.getChannelId(channelNames[i]));
			if(thisChannel==null) {
				PTMUtil.systemExit("Behavior input file entered wrong channel names, exit.");
			}
			channelArrayList.add(thisChannel);
		}
		return (Channel[]) channelArrayList.toArray(new Channel[channelArrayList.size()]);
	}

	/**
	 * Select channel based on transition probabilities
	 * @param p							particle
	 * @param nodeId					node
	 * @param upstreamChannel			upstream Channel
	 * @param downstreamChannel			downstream Channel
	 * @param distribChannel			distributary Channel
	 */
	void selectChannel(Particle p, int nodeId,
			Channel upstreamChannel, Channel downstreamChannel, Channel distribChannel) {

		double rand, adjTransProbToU, adjTransProbToD, adjTransProbToT;
		double[] flowSplitTransProbs;
		Channel chosenChannel;
		int confusionFactor, channelId;
		float swimmingVel, outflow, smallLengthDiff, largeSwimmingVel, tmLeft_sec, PTMtimeStep_sec;
		boolean wait;

		// Define a small length difference to compare the current x position to the channel length
		smallLengthDiff = 0.001f;

		// Define a large swimming velocity that will ensure waiting vFish aren't advected
		// away from the junction
		largeSwimmingVel = 20;

		// Initially assume we're not going to wait
		wait = false;

		// If any of the transProbs are missing, revert to flow-split model
		if((int)transProbToU==MISSING_VALUE || (int)transProbToD==MISSING_VALUE || (int)transProbToU==MISSING_VALUE) {
			switch (fromChannelGroup) {
			case UPSTREAM:
				flowSplitTransProbs = calcFlowSplit(p, nodeId, upstreamChannel, downstreamChannel, distribChannel, distUD_ft, distUT_ft);
				transProbToU = flowSplitTransProbs[0];
				transProbToD = flowSplitTransProbs[1];
				transProbToT = flowSplitTransProbs[2];
				break;
			case DOWNSTREAM:
				flowSplitTransProbs = calcFlowSplit(p, nodeId, downstreamChannel, upstreamChannel, distribChannel, distUD_ft, distDT_ft);
				transProbToD = flowSplitTransProbs[0];
				transProbToU = flowSplitTransProbs[1];
				transProbToT = flowSplitTransProbs[2];
				break;
			case DISTRIB:
				flowSplitTransProbs = calcFlowSplit(p, nodeId, distribChannel, upstreamChannel, downstreamChannel, distUT_ft, distDT_ft);
				transProbToT = flowSplitTransProbs[0];
				transProbToU = flowSplitTransProbs[1];
				transProbToD = flowSplitTransProbs[2];
				break;
			default:
				PTMUtil.systemExit("Unrecognized ChannelGroup. Exiting.");
			}
		}
		else {
			decisionType = "msm";
		}

		// Preprocessor provides transition probabilities per PTM time step. These need to be
		// adjusted to match tmLeft
		tmLeft_sec = p.getTmLeftInSecs();
		PTMtimeStep_sec = 60f*Globals.Environment.getPTMTimeStep();

		// Initialize adjusted transition probabilities;
		adjTransProbToU = transProbToU;
		adjTransProbToD = transProbToD;
		adjTransProbToT = transProbToT;

		// Adjust transition probabilities
		switch (fromChannelGroup) {
		case UPSTREAM:
			adjTransProbToU = Math.pow(transProbToU, PTMtimeStep_sec/tmLeft_sec);
			adjTransProbToT = (1-adjTransProbToU)/(transProbToD/transProbToT + 1);
			adjTransProbToD = 1 - (adjTransProbToU + adjTransProbToT);
			break;
		case DOWNSTREAM:
			adjTransProbToD = Math.pow(transProbToD,  PTMtimeStep_sec/tmLeft_sec);
			adjTransProbToU = (1-adjTransProbToD)/(transProbToT/transProbToU + 1);
			adjTransProbToT = 1 - (adjTransProbToD + adjTransProbToU);
			break;
		case DISTRIB:
			adjTransProbToT = Math.pow(transProbToT, PTMtimeStep_sec/tmLeft_sec);
			adjTransProbToU = (1-adjTransProbToT)/(transProbToD/transProbToU + 1);
			adjTransProbToD = 1 - (adjTransProbToT + adjTransProbToU);
			break;
		default:
			PTMUtil.systemExit("Unrecognized ChannelGroup. Exiting.");
		}
		transProbToU = adjTransProbToU;
		transProbToD = adjTransProbToD;
		transProbToT = adjTransProbToT;

		// Choose channel
		rand =  PTMUtil.getRandomNumber();
		if (rand<transProbToU) {
			chosenChannel = upstreamChannel;
			if (fromChannelGroup==ChannelGroup.UPSTREAM) {
				wait=true;
			};
		}
		else if (rand<(transProbToU + transProbToD)) {
			chosenChannel = downstreamChannel;
			if (fromChannelGroup==ChannelGroup.DOWNSTREAM) {
				wait=true;
			};
		}
		else {
			chosenChannel = distribChannel;
			if(fromChannelGroup==ChannelGroup.DISTRIB) {
				wait=true;
			};
		}

		// If we're waiting due to a no-action transition, set swimming velocity strongly towards junction so
		// vFish aren't advected away
		if(wait) {
			if(Math.abs(p.x - ((Channel) p.wb).getLength()) < smallLengthDiff) {
				p.setSwimmingVelocity(largeSwimmingVel);
			}
			else {
				p.setSwimmingVelocity(-largeSwimmingVel);
			}

			// Indicate that we've set the swimming velocity in the junction so it won't be
			// overwritten in the next time step
			p.swimVelSetInJunction(true);
		}
		else {
			// Calculate total effective outflows (including swimming velocity) in the chosen channel
			channelId = chosenChannel.getEnvIndex();
			p.getSwimHelper().setMeanSwimmingVelocity(p.Id, channelId);

			// Swimming velocity here doesn't include confusion factor
			swimmingVel = ((SalmonSwimHelper) p.getSwimHelper()).getSwimmingVelocity(p, channelId);
			confusionFactor = ((SalmonSwimHelper) p.getSwimHelper()).getConfusionFactor(channelId);
			outflow = Math.max(0.0f, chosenChannel.getInflowWSV(nodeId, swimmingVel*confusionFactor));

			// If effective outflow is zero, the vFish can't successfully swim into the channel without being
			// immediately returned to the node, so wait for a time step.
			if (Math.abs(outflow) < Float.MIN_VALUE){
				wait = true;
			}

			// Whether waiting or not, the vFish should be inserted into the chosen channel so it re-enters the routing model
			// from the correct channel.
			p.wb = chosenChannel;
			setChannelStartingCondition(p, swimmingVel, confusionFactor);

			if (p.observer != null)
		    	p.observer.observeChange(ParticleObserver.WATERBODY_CHANGE, p);

			// If we're not waiting, our work is done
			if(!wait) {
				return;
			}
		}

		if (wait){
		      p.particleWait = true;
		      if (p.wb != null && p.wb.getPTMType() == Waterbody.CHANNEL)
			    	p.x = getXLocationInChannel((Channel)p.wb, p.nd);
		      return;
		}
	}

	/**
	 * Calculate flow-split transition probabilities
	 * @param p							particle
	 * @param nodeId					node ID
	 * @param channelFrom				channel vFish is entering from
	 * @param channelTo1				first of channels that vFish could exit into
	 * @param channelTo2				second of channels that vFish could exit into
	 * @param distTo1_ft				distance in feet between entrance and first exit telemetry stations
	 * @param distTo2_ft				distance in feet between entrance and second exit telemetry stations
	 * @return							double[] with [transProbFrom, transProbTo1, transProbTo2]
	 */
	private double[] calcFlowSplit(Particle p, int nodeId, Channel channelFrom, Channel channelTo1, Channel channelTo2,
			double distTo1_ft, double distTo2_ft) {
		double transProbFrom, transProbTo1, transProbTo2, meanWait_sec, distExit_ft;
		float channelAreaFrom, channelAreaTo1, channelAreaTo2, inflowFrom, outflowTo1, outflowTo2,
			posInflowFrom, posOutflowTo1, posOutflowTo2, totalFlow, relTransProbTo1, relTransProbTo2,
			velFrom, velTo1, velTo2, vel, PTMtimeStep_sec;

		// Minimum velocity based on the maximum no-action transition probability from the msm
		float minVel_ftsec = 0.007f;

		// Initialize outputs
		transProbFrom = transProbTo1 = transProbTo2 = MISSING_VALUE;

		PTMtimeStep_sec = 60f*Globals.Environment.getPTMTimeStep();

		channelAreaFrom = getChannelArea(nodeId, channelFrom);
		channelAreaTo1 = getChannelArea(nodeId, channelTo1);
		channelAreaTo2 = getChannelArea(nodeId, channelTo2);

		inflowFrom = -getOutflow(p, nodeId, channelFrom);
		outflowTo1 = getOutflow(p, nodeId, channelTo1);
		outflowTo2 = getOutflow(p, nodeId, channelTo2);

		// Calculate transit velocities
		velFrom = inflowFrom/channelAreaFrom;
		velTo1 = outflowTo1/channelAreaTo1;
		velTo2 = outflowTo2/channelAreaTo2;

		// Use flow-split or area-split to calculate relative transProbs
		if (outflowTo1>0 || outflowTo2>0) {
			posOutflowTo1 = Math.max(0, outflowTo1);
			posOutflowTo2 = Math.max(0,  outflowTo2);
			relTransProbTo1 = posOutflowTo1/(posOutflowTo1 + posOutflowTo2);
			decisionType = "flow-split_";
		}
		else {
			relTransProbTo1 = channelAreaTo1/(channelAreaTo1 + channelAreaTo2);
			decisionType = "area-split_";
		}
		relTransProbTo2 = 1 - relTransProbTo1;

	    // Calculate the relTransProb-weighted distance to the exit stations
	    distExit_ft = relTransProbTo1*distTo1_ft + relTransProbTo2*distTo2_ft;

	    // Determine the appropriate velocity to use for calculating transit time
	    vel = 0.0f;
		switch (velMethod) {
		case UPSTREAM_FIRST:
			if (inflowFrom>0) {
				vel = velFrom;
				decisionType+="fromVel";
			}
			else if (outflowTo1>0 && outflowTo2>0) {
				vel = relTransProbTo1*velTo1 + relTransProbTo2*velTo2;
				decisionType+="weightedAvgVel";
			} else if (outflowTo1>0) {
				vel = velTo1;
				decisionType+="to1vel";
			} else if (outflowTo2>0) {
				vel = velTo2;
				decisionType+="to2vel";
			} else {
				vel = minVel_ftsec;
				decisionType+="minVel";
			}
			break;
		case FLOW_WEIGHTED_AVG:
			posInflowFrom = Math.max(0, inflowFrom);
			posOutflowTo1 = Math.max(0, outflowTo1);
			posOutflowTo2 = Math.max(0, outflowTo2);

			totalFlow = posInflowFrom + posOutflowTo1 + posOutflowTo2;

			vel = (velFrom*posInflowFrom + velTo1*posOutflowTo1 + velTo2*posOutflowTo2)/(totalFlow + Float.MIN_VALUE);

			if (vel<Float.MIN_VALUE) {
				vel = minVel_ftsec;
				decisionType+="minVel";
			}
			else {
				decisionType+="flowWeightedAvgVel";
			}
			break;
		default:
			PTMUtil.systemExit("Unrecognized velocity calculation method. Exiting.");
		}

		// Set mean wait time = mean transit time
		meanWait_sec = distExit_ft/(vel + Float.MIN_VALUE);

		// Calculate no-action transition probability
		transProbFrom = Math.min(1, Math.max(0, 1 - (PTMtimeStep_sec/meanWait_sec)));

		// Adjust the exit transition probabilities
		transProbTo1 = (1 - transProbFrom)*relTransProbTo1;
		transProbTo2 = 1 - (transProbFrom + transProbTo1);

		return new double[] {transProbFrom, transProbTo1, transProbTo2};
	}

	/**
	 * Calculate total effective outflow (including swimming velocity)
	 * @param p							particle
	 * @param nodeId					node ID
	 * @param channel					channel in which to calculate outflow
	 * @return							total effective outflow
	 */
	private float getOutflow(Particle p, int nodeId, Channel channel) {
		int confusionFactor, channelId;
		float swimmingVel, outflow;

		channelId = channel.getEnvIndex();
		p.getSwimHelper().setMeanSwimmingVelocity(p.Id, channelId);

		// Swimming velocity here doesn't include confusion factor
		swimmingVel = ((SalmonSwimHelper) p.getSwimHelper()).getSwimmingVelocity(p, channelId);
		confusionFactor = ((SalmonSwimHelper) p.getSwimHelper()).getConfusionFactor(channelId);
		outflow = channel.getInflowWSV(nodeId, swimmingVel*confusionFactor);

		return outflow;
	}

	/**
	 * Obtain the channel area at the appropriate end of the channel
	 * @param nodeId					node ID
	 * @param channel					channel object
	 * @return							channel area
	 */
	private float getChannelArea(int nodeId, Channel channel) {
		if (channel.getUpNode().getEnvIndex()==nodeId) {
			return channel.getFlowArea(0);
		}
		else {
			return channel.getFlowArea(channel.getLength());
		}
	}

	public abstract void makeRouteDecision(Particle p);

	int getNodeId(){return _nodeId;}

	/**
	 * Obtain a ZonedDateTime with the current ECO-PTM model time
	 * @return				ZonedDateTime with the current ECO-PTM model time
	 */
	public ZonedDateTime getModelDatetime() {
		String modelTime, modelDate;
		ZonedDateTime datetime;

		// Calculate the current datetime
		modelTime = Globals.getModelTime(Globals.currentModelTime);

		modelDate = Globals.getModelDate(Globals.currentModelTime);
		modelDate = modelDate.substring(0, 2) + modelDate.substring(2, 3) + modelDate.substring(3, 5).toLowerCase() +
				modelDate.substring(5);
		datetime = ZonedDateTime.parse(modelDate + modelTime + "(UTC-08:00)", DateTimeFormatter.ofPattern("ddMMMyyyyHHmm(VV)"));

		return datetime;
	}
}

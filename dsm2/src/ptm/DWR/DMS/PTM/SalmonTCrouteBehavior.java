/**
 * 
 */
package DWR.DMS.PTM;

import java.util.Arrays;
import java.util.Map;

import org.threeten.bp.ZonedDateTime;
import org.threeten.bp.format.DateTimeFormatter;

/**
 * @author Doug Jackson, QEDA Consulting, LLC
 * Read pre-processed transition probabilities for Turner Cut based on 
 * continuous time multistate Markov model in [CITATION]
 */
public class SalmonTCrouteBehavior extends SalmonSouthDeltaRouteBehavior {

	public SalmonTCrouteBehavior(RouteInputs in, Integer nodeId) {
		super(in, nodeId);
	}

	@Override
	/**
	 * Make a route decision based on pre-processed transition probabilities
	 * @param p				particle
	 */	
	public void makeRouteDecision(Particle p) {
		int nodeId;
		Channel upstreamChannel, downstreamChannel, distribChannel;
		ZonedDateTime modelDatetime;
		Map<String, Double> transProbs;
		Channel fromChannel;
		String transProbIndex;
		Double transProbToU, transProbToD, transProbToT;

		nodeId = getNodeId();

		// Obtain all of the channels at this junction
		upstreamChannel = getChannels(p, (new String[] {"TC_U"}))[0];
		downstreamChannel = getChannels(p, (new String[] {"TC_D"}))[0];
		distribChannel = getChannels(p, (new String[] {"TC_T"}))[0];

		// Determine if the particle is entering the junction from an upstream, distributary, 
		// or downstream channel
		fromChannel = (Channel) p.wb;
		if (Arrays.asList(upstreamChannel).contains(fromChannel)) {
			fromChannelGroup = ChannelGroup.UPSTREAM;
		}
		else if (Arrays.asList(downstreamChannel).contains(fromChannel)) {
			fromChannelGroup = ChannelGroup.DOWNSTREAM;
		}
		else if (Arrays.asList(distribChannel).contains(fromChannel)) {
			fromChannelGroup = ChannelGroup.DISTRIB;
		}		
		else {
			PTMUtil.systemExit("Current channel not found in lists of upstream, downstream, or distributary channels. Exiting.");
		}		

		// Read the transition probabilities for the current datetime
		modelDatetime = getModelDatetime();
		transProbs = TransProbs.readTransProbs(modelDatetime);

		// Obtain the relevant transition probabilities given fromChannelGroup
		transProbIndex = "TC_" + modelDatetime.format(DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:SS"));

		// Verify that the required transition probabilities are present
		for (String transition : transitions) {
			if (!transProbs.containsKey(transProbIndex + "_" + transition)) {
				PTMUtil.systemExit("Transition probabilities input file does not contain " +
						transProbIndex + "_" + transition + ". Exiting.");
			}
		}

		transProbToU = transProbToD = transProbToT = null;
		switch (fromChannelGroup) {
		case UPSTREAM:
			transProbToD = transProbs.get(transProbIndex + "_qUD");
			transProbToT = transProbs.get(transProbIndex + "_qUT");
			transProbToU = 1 - (transProbToD + transProbToT);
			break;
		case DOWNSTREAM:
			transProbToU = transProbs.get(transProbIndex + "_qDU");
			transProbToT = transProbs.get(transProbIndex + "_qDT");
			transProbToD = 1 - (transProbToU + transProbToT);
			break;
		case DISTRIB:
			transProbToU = transProbs.get(transProbIndex + "_qTU");
			transProbToD = transProbs.get(transProbIndex + "_qTD");
			transProbToT = 1 - (transProbToU + transProbToD);
			break;
		default:
			PTMUtil.systemExit("Unrecognized ChannelGroup. Exiting.");
		}

		selectChannel(p, nodeId, upstreamChannel, downstreamChannel, distribChannel, 
				transProbToU, transProbToD, transProbToT);
	}
}

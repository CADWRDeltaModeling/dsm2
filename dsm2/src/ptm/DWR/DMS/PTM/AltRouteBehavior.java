package DWR.DMS.PTM;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

import org.apache.commons.math3.distribution.BetaDistribution;

import java.time.ZonedDateTime;
import java.time.format.DateTimeFormatter;

/**
 * Abstract class extending SalmonUpSacRouteBehavior and containing the basic methods shared by all alternative routing model junctions.
 *
 * @author Doug Jackson, QEDA Consulting, LLC
 */
public abstract class AltRouteBehavior extends SalmonUpSacRouteBehavior {
	
	protected Map<String, String> channelCrossSectionMap;
	
	protected AltRouteInterface junctionInterface;
	
	protected String className;
	protected String junction;
	protected int port;
	protected String[] channelNames;
	protected String upDown;
	protected int upChannelIndex;
	protected int downChannelIndex;
	protected int branchChannelIndex;
	protected BetaDistribution beta;

	protected static SwimInputs swimInputs;
	protected static SalmonHoldingTimeCalculator holdingTimeCalc;
	protected static RouteInputs routeInputs;
	protected static BasicHydroCalculator hydroCalc;
	
	protected static Config config;
	
	protected enum crossStreamFracMethodEnum {
		DSM2POSITION,
		UNIFORM,
		BETA;
	}
	
	static {

		swimInputs = Globals.Environment.getBehaviorInputs().getSwimInputs();
		holdingTimeCalc = new SalmonHoldingTimeCalculator(swimInputs);
		
		routeInputs = Globals.Environment.getBehaviorInputs().getRouteInputs();
		
		hydroCalc = new BasicHydroCalculator();
		
		config = PTMFixedData.getConfig();
	}
	
	public AltRouteBehavior(RouteInputs rIn, Integer nodeId) {
		super(rIn, nodeId);
	}

	/**
	 * Run the alternative junction model to make the route decision for the particle.
	 */
	public void makeRouteDecision(Particle p) {
		int PTMtimeStep, subTimeStep, upId, downId, branchId, reinsertionChannelID, confusionFactor;
		Channel[] channels;
		Channel upChannel, downChannel, branchChannel, reinsertionChannel;
		float swVel;
		double downstreamSwimSpeed_msec, crossSectionFrac, depthFrac, channelFrac;
		String insertionDatetime, insertionChannelName, reinsertion, reinsertionEvent, reinsertionChannelName, dielActivity, yzMethod;
		String[] fields;
		ZonedDateTime datetime, reinsertionDatetime;
		HashMap<Integer, String> channelMap;
    	boolean daytimeHolding;
		
		PTMtimeStep = Globals.Environment.getPTMTimeStep()*60;
		        
		check(p);
				
		// Set up the channels and calculate the swimming speed and confusion factors for the possible channels
		channels = getChannels(p, (channelNames), upDown);
		if (channels.length != 5) {
			PTMUtil.systemExit("Error when reading " + junction + " junction channel info. System exit.");
		}
		upChannel = channels[upChannelIndex];
		downChannel = channels[downChannelIndex];
		branchChannel = channels[branchChannelIndex];
		
		upId = upChannel.getEnvIndex();
		downId = downChannel.getEnvIndex();
		branchId = branchChannel.getEnvIndex();
		
		channelMap = new HashMap<Integer, String>();
		channelMap.put(upId, channelNames[upChannelIndex]);
		channelMap.put(downId, channelNames[downChannelIndex]);
		channelMap.put(branchId, channelNames[branchChannelIndex]);
		
		// Mean swimming velocity is set once per particle per channel group.
		// Here is the only place to set a mean swimming velocity.
		p.getSwimHelper().setMeanSwimmingVelocity(p.Id, upId);
		p.getSwimHelper().setMeanSwimmingVelocity(p.Id, downId);
		p.getSwimHelper().setMeanSwimmingVelocity(p.Id, branchId);
		
		// Calculate the current datetime
        datetime = junctionInterface.getModelDatetime();
        
        reinsertion = junctionInterface.getReinsertion(p.Id);
        
        // Check if this particle is waiting to be reinserted. If not, schedule an insertion into the alternative junction model
        if(reinsertion!=null) {
        	fields = reinsertion.split(",");
        	reinsertionDatetime = ZonedDateTime.parse(fields[1], DateTimeFormatter.ISO_ZONED_DATE_TIME);
        	reinsertionEvent = fields[2];
        	reinsertionChannelName = fields[3];
        	
        	// Check if the reinsertion time is equal to or before the current PTM time
        	if(reinsertionEvent.startsWith("failed")){
        		System.out.println("Routing model for particle " + p.Id + " failed. Setting particle state to 'dead'");
        		System.out.println("Message from alternative routing model: " + reinsertionEvent);
        		p.setParticleDead();
        		
        		// Add an entry to this particle's _tracks to ensure that it's not empty
        		p.observer.observeChange(ParticleObserver.WATERBODY_CHANGE, p);
        		
        		p.inJunctionModel = false;
        	}
        	else if(reinsertionDatetime.isBefore(datetime) || reinsertionDatetime.isEqual(datetime)) {        		
        		// Remove this reinsertion from the list of pending reinsertions
        		junctionInterface.removeReinsertion(p.Id);
        		
        		// Insert into the specified channel
        		reinsertionChannel = getChannels(p, (channelNames), upDown)[Arrays.asList(channelNames).indexOf(reinsertionChannelName)];
    	    	p.wb = reinsertionChannel;
    	    	reinsertionChannelID = reinsertionChannel.getEnvIndex();
    	    	swVel = ((SalmonSwimHelper) p.getSwimHelper()).getSwimmingVelocity(p, reinsertionChannelID);
    			confusionFactor = ((SalmonSwimHelper) p.getSwimHelper()).getConfusionFactor(reinsertionChannelID);
    			channelFrac = Double.parseDouble(fields[4]);
    			yzMethod = fields[5];
    	    	setChannelStartingCondition(p, swVel, confusionFactor, channelFrac, yzMethod);
    	    	
    	    	if (p.observer != null) {
    	    		p.observer.observeChange(ParticleObserver.WATERBODY_CHANGE, p);
    	    	}
    	    	p.inJunctionModel = false;
        	} 
        	else {
        		p.inJunctionModel = true;
        		p.particleWait = true;
        	}
        }
        else if (p.inJunctionModel) {
        	// This code explicitly ensures that a particle isn't inserted into the alternative junction model multiple times
        	// in a single ECO-PTM-D time step.
        	p.particleWait = true;
        }
        else {
        	
        	// Specify dielActivity based on daytimeHolding
        	dielActivity = "";
        	for(Integer channelID : channelMap.keySet()) {
        		daytimeHolding = holdingTimeCalc.getDaytimeHolding(p, channelID);
            	if(daytimeHolding) {
            		dielActivity+=channelMap.get(channelID) + "_nightOnly_";
            	} 
            	else {
            		dielActivity+=channelMap.get(channelID) + "_nightAndDay_";
            	}
        	}
        	dielActivity = dielActivity.substring(0, dielActivity.length()-1);
        	
    		// Calculate subTimeStep based on the amount of the PTM time step that remains
        	subTimeStep = (int) Math.max(0, Math.min(PTMtimeStep, Math.floor(PTMtimeStep - p.getTmLeft())));
    		
            datetime = datetime.plusSeconds(subTimeStep);
            insertionDatetime = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm").format(datetime);
            
        	// Determine which channel the particle is entering from
        	insertionChannelName = channelMap.get(p.wb.getEnvIndex());

        	downstreamSwimSpeed_msec = AltRouteInterface.fpsToMps(p.getSwimmingVelocity());
        	
        	// Specify crossSectionFrac using selected method
        	if(!PTMFixedData.getConfig().isSet("cross_stream_frac_method")) {
        		// Draw cross-stream position from a uniform distribution
        		crossSectionFrac = PTMUtil.getRandomNumber();
        	}
        	else {
            	switch(crossStreamFracMethodEnum.valueOf(config.cross_stream_frac_method)) {
            	case DSM2POSITION:
                	// In the bioPTM alternative routing model, crossSectionFrac is generally zero at the right-hand bank when facing downstream. In ECO-PTM
                	// it's generally the opposite => need to take 1 - yFrac
                	crossSectionFrac = Math.min(1,  Math.max(0,  1 - (0.5 + p.y/((Channel) p.wb).getWidth(p.x))));
            		break;
            	
            	case UNIFORM:
            		crossSectionFrac = PTMUtil.getRandomNumber();
            		break;
            		
            	case BETA:
            		crossSectionFrac = beta.sample();
            		break;
            		
        		default:
        			throw new IllegalArgumentException("Unrecognized crossStreamFracMethod");
            	}
        	}

        	
        	// In bioPTM, depthFrac is the fractional distance from the surface; in ECO-PTM, z is the distance from the bottom
        	// => need to take 1 - zFrac
        	depthFrac = Math.min(1, Math.max(0, 1 - p.z/((Channel) p.wb).getDepth(p.x)));
        	
        	// Add the insertion specification
        	junctionInterface.addInsertion(p.Id, channelCrossSectionMap.get(insertionChannelName), insertionDatetime, 
        			crossSectionFrac, depthFrac, downstreamSwimSpeed_msec, dielActivity);
    		
        	p.inJunctionModel = true;
        	p.particleWait = true;
        }
	}
	
	/**
	 * Place a particle in a channel and initialize its state.
	 * @param p				Particle
	 * @param swimVel		swimming velocity
	 * @param confFactor	confusion factor
	 * @param channelFrac	channelFrac location to place the particle
	 * @param yzMethod		method to use when setting y and z coordinates
	 */
	void setChannelStartingCondition(Particle p, float swimVel, int confFactor, double channelFrac, String yzMethod) {
		float length;
		
		super.setChannelStartingCondition(p, swimVel, confFactor);
		
		// Read the channel length
		length = ((Channel) p.wb).getLength();
		
		// Set x position based on channelFrac
		p.x = (float) (length*channelFrac);
		
		// Randomize y and z positions
		if(yzMethod.toUpperCase().equals("RANDOMYZ")) {
			hydroCalc.setYZLocationInChannel(p);
		}
	} 
	
	/**
	 * Set junction parameters read from the behavior configuration file.
	 */
	public void setParams() {		
		// Create a beta distribution object
		if(config.isSet("cross_stream_frac_method") && config.cross_stream_frac_method.equalsIgnoreCase("BETA")) {
			if(config.isSet("cross_stream_frac_beta_a") && config.isSet("cross_stream_frac_beta_b")) {
				beta = new BetaDistribution(config.cross_stream_frac_beta_a, config.cross_stream_frac_beta_b);
			}
			else {
				PTMUtil.systemExit("Could not find either cross_stream_frac_betaA or cross_stream_frac_betaB in config file, but cross_stream_frac_method=BETA. Please check config file. System exit.");
			}
		}
	}
	
	/**
	 * Verify that the behavior inputs are appropriate for alternative routing model
	 */
	public void checkInputs() {
		ConcurrentHashMap<Integer, String> specialBehaviorNames;
		int nodeID, extNodeID;
				
		// Verify that Percent_Increase_Decrease for this junction is 100
		// First, find the nodeID associated with this junction
		nodeID = -999;
		extNodeID = -999;
		specialBehaviorNames = routeInputs.getSpecialBehaviorNames();
		for (int key:specialBehaviorNames.keySet()) {
			if (specialBehaviorNames.get(key).equals(className)) {
				nodeID = key;
				extNodeID = PTMHydroInput.getExtFromIntNode(nodeID);
				break;
			}
		}

		if (routeInputs.getPercentToModify(nodeID)!=100) {
			PTMUtil.systemExit("Percent_Increase_Increase for alternative routing model junctions must be 100. Check behavior input file for " + className);
		}
		
		// Abort if a barrier is installed at this node
		if (Globals.Environment.getNode(nodeID).isBarrierInstalled()) {
			PTMUtil.systemExit("Barrier cannot be installed at alternative routing model junctions. Check behavior input file for nodeID " + extNodeID);
		}
	}
	
	Double calcA(float[] flows) {
		return null;
	}
	Double calcB(float[] flows) {
		return null;
	}
}


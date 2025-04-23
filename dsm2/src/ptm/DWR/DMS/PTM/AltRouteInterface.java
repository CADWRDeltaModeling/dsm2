package DWR.DMS.PTM;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.time.ZoneId;
import java.time.ZonedDateTime;
import java.time.format.DateTimeFormatter;

/**
 * Handle communication of insertion (reinsertion) specs to (from) alternative routing model.
 *
 * @author Doug Jackson, QEDA Consulting, LLC
 */
public class AltRouteInterface {
	
	private static List<AltRouteInterface> junctions;
	private String junction;
	private List<Integer> particleIDs;
	private Set<String> crossSections;
	private HashMap<Integer, String> insertionCrossSections;
	private HashMap<Integer, String> datetimes;
	private HashMap<Integer, Double> crossSectionFracs;
	private HashMap<Integer, Double> depthFracs;
	private HashMap<Integer, Double> downstreamSwimSpeeds;
	private HashMap<Integer, Double> crossStreamSwimSpeeds;
	private HashMap<Integer, Double> probSeekLowSVGs;
	private HashMap<Integer, Double> meanLogRads;
	private HashMap<Integer, Double> sdLogRads;
	private HashMap<Integer, String> dielActivities;
	private String sunrise;
	private String sunset;
	private int lastUpdatedModelTime;
	private String insertionSpec;
	private HashMap<Integer, String> reinsertions;
	
	// Conversion factors
	private static final double ftToM = 0.3048;
	
	static {
		junctions = new ArrayList<AltRouteInterface>();
	}
	
	public AltRouteInterface(String junction) {
		SwimInputs swimInputs;
		
		this.junction = junction;
		
        // Create the model client
//        client = new ModelClient();
        
        // Read the sunrise and sunset times
        swimInputs = Globals.Environment.getBehaviorInputs().getSwimInputs();
        sunrise = swimInputs.getSunrise().getFirst() + ":" + swimInputs.getSunrise().getSecond();
        sunset = swimInputs.getSunset().getFirst() + ":" + swimInputs.getSunset().getSecond();
        System.out.println("sunrise:" + sunrise + ", sunset:" + sunset);
        
        lastUpdatedModelTime = -999;

        initializeInsertions();
        
        reinsertions = new HashMap<Integer, String>();
        
        junctions.add(this);
	}
	
	/** 
	 * Initialize variables that hold the insertion specifications
	 */
	public void initializeInsertions() {
		particleIDs = new ArrayList<Integer>();
		crossSections = new HashSet<String>();
		insertionCrossSections = new HashMap<Integer, String>();
		datetimes = new HashMap<Integer, String>();
		crossSectionFracs = new HashMap<Integer, Double>();
		depthFracs = new HashMap<Integer, Double>();
		downstreamSwimSpeeds = new HashMap<Integer, Double>();
		crossStreamSwimSpeeds = new HashMap<Integer, Double>();
		probSeekLowSVGs = new HashMap<Integer, Double>();
		meanLogRads = new HashMap<Integer, Double>();
		sdLogRads = new HashMap<Integer, Double>();
		dielActivities = new HashMap<Integer, String>();
	}
	
	/**
	 * Schedule the specified insertions to run in the alternative junction model
	 */
	public void runInsertions() {
		
		String reinsertionSpec;
		String[] particleReinsertions, fields;
		ZonedDateTime datetime;
		DateTimeFormatter datetimeFormat;
		
		// Return if there are no insertions scheduled
		if(particleIDs.isEmpty()) {return;}
		
		// Calculate time one hour in the future for placeholder reinsertion specs
		datetime = getModelDatetime();
		datetime = datetime.plusMinutes(60);
		datetimeFormat = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss z");
		System.out.println();
		
		if(lastUpdatedModelTime<Globals.currentModelTime) {
						
			assembleInsertionSpec();
			System.out.println("==================================================================================");
			System.out.println("Sending insertion specification to routing model:" + insertionSpec);
//			String response = client.sendMessage(insertionSpec);
//        	System.out.println(response);
//        	reinsertionSpec = client.receiveMessage();
			
			// Placeholder reinsertion specs. This should be replaced with an interface to a routing model that 
			// generates reinsertion specs with the same structure and format.
			reinsertionSpec = "";
			for (int particleID : particleIDs) {
				reinsertionSpec+=Integer.toString(particleID).concat(",");
				reinsertionSpec+=datetime.withZoneSameInstant(ZoneId.of("UTC-08:00")).toString().concat(",");
				reinsertionSpec+="SacGS8,SACDOWNGS,0.5,-999.0,-999.0|";				
			}
        	
        	// Parse and store the reinsertions
        	particleReinsertions = reinsertionSpec.split("\\|");
        	for(String particleReinsertion : particleReinsertions) {
        		fields = particleReinsertion.split(",");
        		System.out.println("Scheduled reinsertion: " + particleReinsertion);
        		reinsertions.put(Integer.parseInt(fields[0]), particleReinsertion);
        	}

			initializeInsertions();
			
			lastUpdatedModelTime = Globals.currentModelTime;
		}
	}
	
	/**
	 * Add this particle's insertion specification.
	 * @param particleID					particle's unique ID
	 * @param crossSection					name of the insertion cross section
	 * @param datetime						insertion datetime ("yyyy-MM-dd HH:mm")
	 * @param crossSectionFrac				fraction of the cross section at which to insert the particle
	 * @param depthFrac						fraction of the depth at which to insert the particle
	 * @param downstreamSwimSpeed_msec		downstream swimming speed (m/sec)
	 * @param crossStreamSwimSpeed_msec		cross-stream swimming speed (m/sec)
	 * @param probSeekLowSVG				probability of turning toward the direction of lowest spatial velocity gradient
	 * @param meanLogRad					mean of radians in log space of the additional random rotation applied in the binary collision algorithm
	 * @param sdLogRad						standard deviation of radians in log space of the additional random rotation applied in the binary collision algorithm
	 * @param dielActivity					diel activity mode (nightOnly, dayOnly, or nightAndDay)
	 */
	public void addInsertion(int particleID, String crossSection, String datetime, double crossSectionFrac, double depthFrac,
			double downstreamSwimSpeed_msec, String dielActivity) {
		
		// Add this insertion spec
		particleIDs.add(particleID);
		crossSections.add(crossSection);
		insertionCrossSections.put(particleID, crossSection);
		datetimes.put(particleID, datetime);
		crossSectionFracs.put(particleID, crossSectionFrac);
		depthFracs.put(particleID, depthFrac);
		downstreamSwimSpeeds.put(particleID, downstreamSwimSpeed_msec);
		dielActivities.put(particleID, dielActivity);
	}
	
	/**
	 * Assemble the insertion spec to send to the alternative junction model.
	 */
	public void assembleInsertionSpec() {
		ZonedDateTime datetime;
		String datetimeString;
		
		datetime = getModelDatetime();
		datetimeString = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm").format(datetime);
		
		insertionSpec = "{\"modelTimeECOPTM\": \"" + datetimeString + "\",";
		insertionSpec+="\"timeStepECOPTM_min\": \"" + Globals.Environment.getPTMTimeStep() + "\",";
		insertionSpec+="\"sunrise\": \"" + sunrise + "\",";
		insertionSpec+="\"sunset\": \"" + sunset + "\",";
	
		insertionSpec+="\"insertions\":[";
		for(String crossSection : crossSections) {
			insertionSpec = insertionSpec + "{\"name\":\"" + crossSection + "\",\"particleID\":[";
			
			// Add the particleIDs
			for(int particleID : particleIDs) {
				if(insertionCrossSections.get(particleID)==crossSection) {
					insertionSpec = insertionSpec + "\"" + particleID + "\", ";
				}
			}
			insertionSpec+="],\"insertionDatetime\":[";
			
			// Add the insertion datetimes
			for(int particleID : particleIDs) {
				if(insertionCrossSections.get(particleID)==crossSection) {
					insertionSpec = insertionSpec + "\"" + datetimes.get(particleID) + "\", ";
				}
			}
			insertionSpec+="],\"crossSectionFrac\":[";
			
			// Add the crossSectionFracs
			for(int particleID : particleIDs) {
				
				if(insertionCrossSections.get(particleID)==crossSection) {
					insertionSpec = insertionSpec +  crossSectionFracs.get(particleID) + ", ";
				}
			}
			insertionSpec+="],\"depthFrac\":[";
			
			// Add the depthFracs
			for(int particleID : particleIDs) {
				
				if(insertionCrossSections.get(particleID)==crossSection) {
					insertionSpec = insertionSpec + depthFracs.get(particleID) + ", ";
				}
			}
			insertionSpec+="],\"downstreamSwimSpeed_msec\":[";

			// Add the downstreamSwimSpeeds
			for(int particleID : particleIDs) {
				
				if(insertionCrossSections.get(particleID)==crossSection) {
					insertionSpec = insertionSpec + downstreamSwimSpeeds.get(particleID) + ", ";
				}
			}
			insertionSpec+="],\"dielActivity\":[";
			
			// Add the dielActivities
			for(int particleID : particleIDs) {
				
				if(insertionCrossSections.get(particleID)==crossSection) {
					insertionSpec = insertionSpec + "\"" + dielActivities.get(particleID) + "\", ";
				}
			}
			
			insertionSpec+="]}, ";		
		}
		insertionSpec+="]}";
		
		// Replace unnecessary commas
		insertionSpec = insertionSpec.replaceAll(", ]", "]");
	}
	
	/**
	 * Obtain the reinsertion spec for the specified particle, if available.
	 * @param particleID					the particle's unique ID
	 * @return
	 */
	public String getReinsertion(int particleID) {		
		// Return reinsertion spec if it exists
		if(reinsertions.containsKey(particleID)) {
			return reinsertions.get(particleID);
		}
		else {
			return null;
		}
	}
	
	/**
	 * Remove the specified particle's reinsertion spec.
	 * @param particleID					the particle's unique ID
	 */
	public void removeReinsertion(int particleID) {
		if(reinsertions.containsKey(particleID)) {
			reinsertions.remove(particleID);
		}
	}
	
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
	
	/**
	 * Trigger all of the scheduled insertions.
	 */
	public static void runAllInsertions() {
		for (AltRouteInterface junction : junctions) {
			junction.runInsertions();
		}
	}
	
	/**
	 * Convert from feet/sec to m/sec
	 * @param fps			speed in feet/sec
	 * @return				speed in m/sec
	 */
	public static double fpsToMps(double fps) {
		return(fps*ftToM);
	}
}

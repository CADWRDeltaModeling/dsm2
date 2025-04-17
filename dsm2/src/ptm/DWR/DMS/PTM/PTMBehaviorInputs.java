/**
 *
 */
package DWR.DMS.PTM;
import java.io.BufferedReader;
import java.nio.IntBuffer;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Calendar;
import java.util.TimeZone;
/**
 * @author xwang
 *
 */
public class PTMBehaviorInputs {
	private String _fishType = null;
	private SurvivalInputs _survivalInputs=null;
	private SwimInputs _swimInputs=null;
	private RouteInputs _routeInputs=null;
	private int _totalParticlesReleased = 0;
	private TravelTimeOutput _travelTimeOutput = null;
	// Map<name of release station, FishReleaseGroup>
	private Map<String, FishReleaseGroup> _fishGroups = null;
	// Map<insert station(node id, wb id, and distance), station name>
	private Node[] _nodeArray=null;
	private Waterbody[] _wbArray=null;
	private TimeZone _timeZone = null;

	private void extractReleaseInputs(){
		Config config;
		List<Object> thisRelease;
		
		config = PTMFixedData.getConfig();
		
		if (config.release_groups==null || config.release_groups.size()==0) {
			PTMUtil.systemExit("Errors in Fish_Release_Inputs, system exit.");
		}
		
		int numOfGroups = config.release_groups.size();
		for (int i=0; i<numOfGroups; i++){
			
			if(config.release_groups.get(i).name==null || 
					config.release_groups.get(i).release_loc_header==null || config.release_groups.get(i).release_loc==null || 
					config.release_groups.get(i).releases_header==null || config.release_groups.get(i).releases==null ||
					config.release_groups.get(i).releases.size()==0) {
				PTMUtil.systemExit("Errors in Fish_Release_Inputs Group_" + (i+1) + " system exit.");
			}
				
			String stationShouldBe[] = {"NODEID", "CHANNELID/RESERVOIRNAME/OBJ2OBJNAME", "DISTANCE", "STATION_NAME"};
			PTMUtil.checkTitle(config.release_groups.get(i).release_loc_header, stationShouldBe);
			Pair<String, IntBuffer> stationName = setIdsDistance(config.release_groups.get(i).release_loc); //PTMHydroInput.getIntFromExtNode(PTMUtil.getInt(groupText.get(0)));  // convert to internal id system
			IntBuffer station = stationName.getSecond();
			String name = stationName.getFirst();
			String[] releaseShouldBe = {"RELEASE_DATE", "RELEASE_TIME", "PARTICLE_NUMBER", "RELEASE_STYLE"};
			if (!PTMUtil.check(config.release_groups.get(i).releases_header, releaseShouldBe))
				PTMUtil.systemExit("SYSTEM EXIT: Title line is wrong while reading particle release info: " + 
									Arrays.toString(config.release_groups.get(i).releases_header));
			else{
				for (int j=0; j<config.release_groups.get(i).releases.size(); j++) {
					thisRelease = config.release_groups.get(i).releases.get(j);

					if (thisRelease.size()<4)
						PTMUtil.systemExit("Errors in Fish_Release_Inputs Group_" + (i+1) + ": " + thisRelease + " system exit.");
					Calendar releaseTime = PTMUtil.getDateTime(thisRelease.get(0).toString(), thisRelease.get(1).toString(), _timeZone);
					int particleNumber = (int) thisRelease.get(2);

					int releaseStyle = FishRelease.RANDOM;
					if(thisRelease.get(3).toString().equalsIgnoreCase("CENTER"))
						releaseStyle = FishRelease.CENTER;
					else if (thisRelease.get(3).toString().equalsIgnoreCase("RANDOM"))
						releaseStyle = FishRelease.RANDOM;
					else
						PTMUtil.systemExit("Errors in Fish_Release_Inputs Group_" + (i+1) + ": " + thisRelease + " system exit.");

					if (_fishGroups == null)
						// map key: node id
						_fishGroups = new HashMap<String, FishReleaseGroup>();

					if (_fishGroups.get(name) == null){
						ArrayList<FishRelease> frList = new ArrayList<FishRelease>();
						frList.add(new FishRelease(releaseTime, particleNumber, releaseStyle));
						_fishGroups.put(name, new FishReleaseGroup(station, name, frList));
					}
					else
						_fishGroups.get(name).addFishRelease(new FishRelease(releaseTime, particleNumber, releaseStyle));
					_totalParticlesReleased += particleNumber;
				}
			}
		}

	}

	//TODO copied from TravelTimeOutput, need write a utility method to read in node, wb ids and distances
	private Pair<String, IntBuffer> setIdsDistance(List<Object> releaseLoc){
		int[] station = new int[3];
		
		if (releaseLoc.size()<4)
			PTMUtil.systemExit("expect at least 4 items in paticle release line in behavior input file, system exit ");
		if (releaseLoc.get(3)==null)
			PTMUtil.systemExit("expect a release station name, but found none, system exit. ");
		try{
			// nodeId
			station[0] = PTMHydroInput.getIntFromExtNode((int) releaseLoc.get(0));
		}catch(NumberFormatException e){
				e.printStackTrace();
				PTMUtil.systemExit("node id:" + releaseLoc.get(0) + " in the travel time output line is wrong, please check");
		}
		try{
			// wbId
			station[1] = PTMHydroInput.getIntFromExtChan((int) releaseLoc.get(1));
		}catch(NumberFormatException e){
			if (PTMEnv.getReservoirObj2ObjEnvId(releaseLoc.get(1).toString()) == null){
				PTMUtil.systemExit("channel/reservior/obj2obj id:" + releaseLoc.get(1) + " in the travel time output line is wrong, please check");
			}
			else
				station[1] = PTMEnv.getReservoirObj2ObjEnvId(releaseLoc.get(1).toString());
		}
		try{
			//distance
			station[2] = (int) releaseLoc.get(2);
		}catch(NumberFormatException e){
			if (releaseLoc.get(2).toString().equalsIgnoreCase("LENGTH"))
				station[2] = -999999;
			else{
				PTMUtil.systemExit("distance input:" + releaseLoc.get(2) +" for channel:" + releaseLoc.get(0) + 
						" and node:" + releaseLoc.get(1) + " in travel time output line is wrong, please check." );
			}
		}
		return new Pair<String, IntBuffer>(releaseLoc.get(3).toString(), IntBuffer.wrap(station));
	}

	/**
	 *
	 */
	public PTMBehaviorInputs() {
		PTMUtil.systemExit("missing nodes, waterbodies and input file info, system exit.");
	}
	public PTMBehaviorInputs(Node[] nodeArray, Waterbody[] wbArray) {
		Config config;
		String scratchString;
		int scratchInt;
		
		config = PTMFixedData.getConfig();
		
		_nodeArray = nodeArray;
		_wbArray = wbArray;

		// read in general info
		_fishType = config.particle_type;
		if (_fishType==null)
			PTMUtil.systemExit("No Particle Type found, exit.");
		if (_fishType.equalsIgnoreCase("Salmon_Particle"))
			Globals.CalculateWritePTMFlux = false;

		String tzString = config.time_zone;
		if (tzString==null){
			_timeZone = TimeZone.getTimeZone("PST");
			System.out.println("Set default time zone to PST");
		}
		else{
			String id = tzString;
			if(id.equalsIgnoreCase("PST")|| id.equalsIgnoreCase("MST")
					|| id.equalsIgnoreCase("CST")|| id.equalsIgnoreCase("EST"))
				_timeZone = TimeZone.getTimeZone(id);
			else
				PTMUtil.systemExit("Wrong time zone input, exit.");
		}
		Globals.TIME_ZONE = _timeZone;
		
		if (config.use_new_random_seed) {PTMUtil.setRandomNumber();}

		_travelTimeOutput = new TravelTimeOutput();
		
		if (config.release_groups==null || config.release_groups.size()==0) {
			if (config.particle_insertion!=null) {
				System.out.println("No fish release timeseries found in release_groups section of the behavior input file. Using specification in particle_insertion instead!");				
			}
			else {
				PTMUtil.systemExit("No fish release specification found in the behavior input file. Please specify either release_groups or particle_insertion. System exit.");
			}
 		}
		else {
			extractReleaseInputs();
		}

		_survivalInputs = new SurvivalInputs(_fishType);
		_swimInputs = new SwimInputs(_fishType);
		_routeInputs = new RouteInputs(_fishType);

		if(config.isSet("display_simulation_timestep_write_all")) {
			Globals.DisplaySimulationTimestep = config.display_simulation_timestep_write_all;
		}
		if(config.isSet("flux_write_all")) {
			Globals.CalculateWritePTMFlux = config.flux_write_all;
		}
		if(config.isSet("entrainment_write_all")) {
			_routeInputs.setWriteEntrainmentAll(config.entrainment_write_all);
		}
		if(config.isSet("survival_write_all")) {
			_survivalInputs.setSurvivalAllWriteout(config.survival_write_all);
		}
		if(config.isSet("route_survival_write_all")) {
			SurvivalCalculation.setWriteRouteSurvival(config.route_survival_write_all);
		}
		if(config.isSet("fates_write_all")) {
			SurvivalCalculation.setWriteFates(config.fates_write_all);
		}
		if(config.isSet("survival_detail_write_all")) {
			SurvivalCalculation.setWriteSurvDetail(config.survival_detail_write_all);
		}
		
		// Verify that the flux output file is specified
		if(Globals.CalculateWritePTMFlux) {
			scratchString = Globals.Environment.getPTMFixedInput().getFileName("flux");
			scratchInt = Globals.Environment.getPTMFixedInput().getIntervalInMin("flux");
		}

		setNodeInfo(_nodeArray);
		setWaterbodyInfo(_wbArray);
		//TODO get rid of it when swiming behavior is completed
		//_routeInputs.setSwimmingInputs(_swimInputs);
	}
	private void setWaterbodyInfo(Waterbody[] allWbs){
		if (_routeInputs != null){
			_routeInputs.setBarrierWbInfo(allWbs);
			_routeInputs.setFishScreenWbInfo(allWbs);
		}
	}
	private void setNodeInfo(Node[] allNodes){
		if (_routeInputs != null){
			_routeInputs.setBarrierNodeInfo(allNodes);
			_routeInputs.setFishScreenNodeInfo(allNodes);
		}
	}
	public String getFishType(){return _fishType;}
	public TimeZone getTimeZone(){return _timeZone;}
	public SwimInputs getSwimInputs(){ return _swimInputs;}
	public SurvivalInputs getSurvivalInputs(){ return _survivalInputs;}
	public RouteInputs getRouteInputs(){ return _routeInputs;}
	// map key: node Id (internal Id system)
	public Map<String, FishReleaseGroup> getFishReleaseGroups() {return _fishGroups;}
	public int getTotalParticlesReleased() {return _totalParticlesReleased;}
	public TravelTimeOutput getTravelTimeOutput(){return _travelTimeOutput;}
	public RouteHelper getRouteHelper(){return _routeInputs.getRouteHelper();}
	public SwimHelper getSwimHelper(){return _swimInputs.getSwimHelper();}
	public SurvivalHelper getSurvivalHelper(){return _survivalInputs.getSurvivalHelper();}
    //helpers are now instantiated in the individual inputs
	/*
	if ( _particleType.equalsIgnoreCase("PARTICLE")){
		//TODO need to create a particle route helper later
		//_routeHelper = new ParticleRouteHelper(new BasicRouteBehavior());
		_routeHelper = new SalmonRouteHelper(new SalmonBasicRouteBehavior(_behaviorInputs.getRouteInputs()));
		System.out.println("Created Particle Route Helper");
		_swimHelper = new SalmonSwimHelper(new SalmonBasicSwimBehavior(_behaviorInputs.getSwimInputs()));
		System.out.println("Created Particle Swim Helper");
		_survivalHelper = new SalmonSurvivalHelper(new SalmonBasicSurvivalBehavior(_behaviorInputs.getSurvivalInputs()));
		System.out.println("Created Particle Survival Helper");
	}
	else if(_particleType.equalsIgnoreCase("SALMON")){
		_routeHelper = new SalmonRouteHelper(new SalmonBasicRouteBehavior(_behaviorInputs.getRouteInputs()));
		System.out.println("Created Salmon Route Helper");
		_swimHelper = new SalmonSwimHelper(new SalmonBasicSwimBehavior(_behaviorInputs.getSwimInputs()));
		System.out.println("Created Salmon Swim Helper");
		_survivalHelper = new SalmonSurvivalHelper(new SalmonBasicSurvivalBehavior(_behaviorInputs.getSurvivalInputs()));
		System.out.println("Created Salmon Survival Helper");
	}
	else if (_particleType.equalsIgnoreCase("SMELT"))
		PTMUtil.systemExit("No smelt helper defined, system exit.");
	else
		PTMUtil.systemExit("No helper defined for this type of particle, system exit.");

	*/
}

//TODO clean up
/*
 		//TODO not needed now, maybe later
		//_survivalInputs.updateCurrentInfo(allNodes, allWbs, currentTime);
		_swimInputs.updateCurrentInfo(allNodes, allWbs, currentTime);
*/
/*
if (_survivalInputs != null){
	_survivalInputs.setChannelInfo(allWbs);
}
if (_swimInputs != null){
	_swimInputs.setChannelInfo(allWbs);
	//TODO need finish this
	//_swimInputs.setWaterbodyInfo(allWbs, reserviorObj2ObjNameID);
}
public void updateCurrentInfo(Node[] allNodes, Waterbody[] allWbs, int currentTime){
		if (_barrierInstalled && _routeInputs != null)
			_barrierInstalled = _routeInputs.updateCurrentBarrierInfo(allWbs, currentTime);
	}
		private boolean _barrierInstalled = true;
*/
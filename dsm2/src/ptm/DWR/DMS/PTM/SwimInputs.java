/**
 *
 */
package DWR.DMS.PTM;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.io.File;
import java.io.IOException;

/**
 * @author xwang
 *
 */
public class SwimInputs {

	/**
	 *
	 */
	public SwimInputs() {
		// TODO Auto-generated constructor stub
	}
	public SwimInputs(String fishType) {
		Config config;
		boolean swimInputsSet;
		
		config = PTMFixedData.getConfig();
		
		// Verify that parameters have been explicitly set in the config file
		// Abort if we're running a salmon or position-oriented particle and the swim inputs parameters aren't set.
		swimInputsSet = true;
		for (String par : new String[] {"sunrise", "sunset", "stst_threshold", "tidal_cycles_to_calculate_channel_direction",
				"confusion_probability_constant", "max_confusion_probability", "confusion_probability_slope", 
				"random_assess", "assess_probability", "stuck_threshold"}) {
			if(!config.isSet(par)) {
				swimInputsSet = false;
				if (fishType.equalsIgnoreCase("SALMON_PARTICLE") || fishType.equalsIgnoreCase("POSITION_ORIENTED_PARTICLE")) {
					PTMUtil.systemExit(par + " missing in Swim inputs section");	
				}
			}
		}
		
		if (swimInputsSet){
			if (fishType.equalsIgnoreCase("POSITION_ORIENTED_PARTICLE")){
				String smeltInputFileName = config.smelt_input_filename;
				if (smeltInputFileName==null) {
					PTMUtil.systemExit("No smelt input file name found, exit.");
				}
				
				try{
					setSmeltParticleBehavior(smeltInputFileName);
				}catch (IOException e){
					e.printStackTrace();
					PTMUtil.systemExit("Error while reading the position oriented particle input file: "+smeltInputFileName);
				}
			}
			else if (fishType.equalsIgnoreCase("SALMON_PARTICLE")){
				try{
					_sunrise = PTMUtil.getPairFromString(config.sunrise);
					_sunset = PTMUtil.getPairFromString(config.sunset);
					_floodHoldVel = config.stst_threshold;
					_numTidalCycles = config.tidal_cycles_to_calculate_channel_direction;
					_constProbConfusion = config.confusion_probability_constant;
					_maxProbConfusion = config.max_confusion_probability;
					_slopeProbConfusion = config.confusion_probability_slope;
					_randomAccess = config.random_assess;
					_accessProb = config.assess_probability;
					THRESHOLD_STUCK = config.stuck_threshold*24*60*60;
				}catch (NumberFormatException e){
					e.printStackTrace();
					PTMUtil.systemExit("number format is wrong in one of the swimming input lines");
				}
				setChannelGroups();
			}
			else
				PTMUtil.systemExit("No swimming input is expected, but found swimming input parameters (sunrise, sunset, etc.). System exit.");
		}
		_fishType = fishType;
	}

	//public float getDaytimeNotSwimPercent(){return _daytimeNotSwimPercent;} // used to be only one value, now one per channel group.  the values is in _swimVelParas
	public float getFloodHoldingThreshold(){return _floodHoldVel;}
	public float getConstProbConfusion(){return _constProbConfusion;}
	public float getMaxProbConfusion(){return _maxProbConfusion;}
	public float getSlopeProbConfusion(){return _slopeProbConfusion;}
	public boolean getRandomAccess() {return _randomAccess;}
	public float getAccessProbability() {return _accessProb;}
	public int getNumberTidalCycles(){return _numTidalCycles;}
	public Pair<Integer, Integer> getSunrise(){ return _sunrise;}
	public Pair<Integer, Integer> getSunset(){ return _sunset;}
	public Map<Integer, String> getChannelGroups(){return _channelGroups;}
	public Map<String, Map<Integer, Float>> getPartcleMeanSwimmingVelocityMap() {return _particleMeanSwimVels;}
	public Map<String, Map<Integer, Long>> getPartcleMeanRearingHoldingMap() {return _particleMeanRearingHoldings;}
	public Map<String, Map<Integer, Boolean>> getPartcleDaytimeHoldingMap() {return _particleDaytimeHoldings;}
	public String getFishType(){return _fishType;}
	public Map<String, float[]> getSwimParameters() {return _swimVelParas;}
	public SwimHelper getSwimHelper(){
		if (_swimHelper == null)
			setSwimHelper();
		return _swimHelper;
	}
	public static boolean isDayTime(){
		Calendar curr = PTMUtil.modelTimeToCalendar(Globals.currentModelTime, Globals.TIME_ZONE);
		int sunrise_hour = _sunrise.getFirst();
        int sunrise_min = _sunrise.getSecond();
        int sunset_hour = _sunset.getFirst();
        int sunset_min = _sunset.getSecond();
    	//TODO will use LocaTime when switch to Java 1.8
    	return ((curr.get(Calendar.HOUR_OF_DAY) > sunrise_hour && curr.get(Calendar.HOUR_OF_DAY) < sunset_hour)
    			|| (curr.get(Calendar.HOUR_OF_DAY) == sunrise_hour && curr.get(Calendar.MINUTE)>sunrise_min)
    			|| (curr.get(Calendar.HOUR_OF_DAY) == sunset_hour && curr.get(Calendar.MINUTE)<sunset_min));
	}

	//TODO do nothing for now, should be included in behavior or helper?
	public void setChannelInfo(Waterbody[] allWbs){}
	public void setNodeInfo(Node[] allNodes, int nodeNum){}
	/*
	 * check if a particle gets stuck in or comes back to the same channel after a threshold time
	 * currently the threshold time is 30 days
	 */
	public boolean checkStuck(int pId, int chanId, double age){
		if(_pStuck.get(pId) == null)
			_pStuck.put(pId, new HashMap<Integer, Double>());
		if(_pStuck.get(pId).get(chanId) == null)
			_pStuck.get(pId).put(chanId, age);
		if ((age - _pStuck.get(pId).get(chanId))> THRESHOLD_STUCK)
			return true;
		return false;
	}

	public ParticleBehavior getSmeltBehavior(){return _smeltBehavior;}


	private void setChannelGroups(){
		Config config;
		List<Object> thisSwimmingVel;
		
		config = PTMFixedData.getConfig();
		
		if (config.channel_groups==null){
			System.err.println("WARNING: No channel groups for Swimming velocities defined in behavior input file!");
			return;
		}
		// get swimming velocities
		if (config.swimming_vel==null)
			PTMUtil.systemExit("No swimming velocities found in the Channel_Groups block, system exit");
		checkTitle(String.join(",", config.swimming_vel_header));
		if (_groupNames != null || _swimVelParas != null)
			PTMUtil.systemExit("Swimming velocities should not have been set before SwimInputs intialization, system exit");
		_groupNames = new ArrayList<String>();
		_swimVelParas = new HashMap<String, float[]>();
		_particleMeanSwimVels = new HashMap<String, Map<Integer, Float>>();
		_particleMeanRearingHoldings = new HashMap<String, Map<Integer, Long>>();
		_particleDaytimeHoldings = new HashMap<String, Map<Integer, Boolean>>();
		boolean includeAll = false;
		for(int i=0; i<config.swimming_vel.size(); i++) {
			thisSwimmingVel = config.swimming_vel.get(i);
			
			try{
				if (thisSwimmingVel.size() < 6)
					throw new NumberFormatException();
				String groupName = thisSwimmingVel.get(0).toString().toUpperCase();
				if(groupName.equals("ALL"))
					includeAll = true;
				// item[1], constant swimming velocity; item[2], std for particles; item[3] std for time steps for each particle; item[4] rearing holding; item[5] day time not swim percent
				_swimVelParas.put(groupName, new float[] {((Number) thisSwimmingVel.get(1)).floatValue(),
						((Number) thisSwimmingVel.get(2)).floatValue(),
						((Number) thisSwimmingVel.get(3)).floatValue(),
						((Number) thisSwimmingVel.get(4)).floatValue()*60.0f,
						((Number) thisSwimmingVel.get(5)).floatValue()}); // converting from hours to minutes
				_groupNames.add(groupName);
				_particleMeanSwimVels.put(groupName, new HashMap<Integer, Float>());
				_particleMeanRearingHoldings.put(groupName, new HashMap<Integer, Long>());
				_particleDaytimeHoldings.put(groupName, new HashMap<Integer, Boolean>());
			}catch(NumberFormatException e){
				PTMUtil.systemExit("expect to read four floats in the swimming velocity line, but read: " + thisSwimmingVel + ", System exit.");
			}
		}
		if(!includeAll)
			PTMUtil.systemExit("Swiming Velocities Input must include an \"ALL\" channel group, system exit");
		//TODO clean up: groupName "ALL" already added in the loop above, no need to do again.
		//_particleMeanSwimVels.put("ALL", new HashMap<Integer, Float>());
		//_particleMeanRearingHoldings.put("ALL", new HashMap<Integer, Long>());
		//_particleDaytimeHoldings.put("ALL", new HashMap<Integer, Boolean>());
		//get Channel list
		_channelGroups = new HashMap<Integer, String>();
		for (String name: _groupNames){
			if (!name.equalsIgnoreCase("ALL")){
				int[] chanList = config.getChannels(name);
				if (chanList == null)
						PTMUtil.systemExit("expect a channel list for a group:"+name+", but got none, please check swimming behavior inputs, system exit.");
				else{
					for (int chanId: chanList){
						Integer envId = PTMHydroInput.getIntFromExtChan(chanId);
						if (envId <= 0)
							PTMUtil.systemExit("No such channel number:"+chanId+", Please check swimming velocity section in behavior input file, system exit.");
						else
							_channelGroups.put(envId, name);		
					}
				}
			}
		}
	}
	
	private void checkTitle(String inTitle){
		String [] title = inTitle.trim().split("[,\\s\\t]+");

		if (title.length < 6
				||!title[0].equalsIgnoreCase("Group_Name")
				|| !title[1].equalsIgnoreCase("Constant_Swimming_velocity")
				|| !title[2].equalsIgnoreCase("Standard_Deviation_Particles")
				|| !title[3].equalsIgnoreCase("Standard_Deviation_Times")
				|| !title[4].equalsIgnoreCase("Rearing_Holding_Mean")
				|| !title[5].equalsIgnoreCase("Day_time_not_swim_percent"))
			PTMUtil.systemExit("SYSTEM EXIT: Expecting Group_Name Constant_Swimming_Velocity ... but get:"+ inTitle);
	}
	private void setSwimHelper(){
		if (_fishType.equalsIgnoreCase("SALMON_PARTICLE")){
			_swimHelper = new SalmonSwimHelper(new SalmonBasicSwimBehavior(this));
			System.out.println("Created Salmon Particle Swim Helper");
		}
		else if (_fishType.equalsIgnoreCase("POSITION_ORIENTED_PARTICLE")){
			_swimHelper = new SmeltSwimHelper(new SmeltBasicSwimBehavior(this));
			System.out.println("Created Position Oriented Particle Swim Helper");
		}
		else if (_fishType.equalsIgnoreCase("NEUTRALLY_BUOYANT_PARTICLE")){
			_swimHelper = new ParticleSwimHelper(new BasicSwimBehavior(this));
			System.out.println("Created Neutrally Buoyant Particle Swim Helper");
		}
		else
			PTMUtil.systemExit("don't know how to deal the fish species: "+_fishType+", system exit.");
	}

	private void setSmeltParticleBehavior(String smeltInputFileName) throws IOException {
		// initialize behavior file
		if (smeltInputFileName.length() != 0 && smeltInputFileName != null){
			if (checkFile(smeltInputFileName)) {
				_smeltBehavior = new ParticleBehavior(smeltInputFileName);
				if (_smeltBehavior == null)
					System.err.println("cannot add smelt behaviors from "+smeltInputFileName);
			}
			else {
				System.err.println("Behavior File \""+smeltInputFileName+"\" Does Not Exist \nExiting");
				System.exit(0);
			}
	    }
	  }

	private boolean checkFile(String filenm){
		File tmpfile = new File(filenm);
	    return tmpfile.isFile();
	}
	private String _fishType = null;
	// group name, swimming velocity parameters[] [0] constSwimmingVelocity; [1]; STD for particles; [2] STD for time steps for an individual particle; [3] rearing holding; [4] day time not swim percent
	private Map<String, float[]> _swimVelParas=null;
	// Map<ChanGroupName, Map<particleId, meanSwimmingVelocity>>
	private Map<String, Map<Integer, Float>> _particleMeanSwimVels = null;
	// Map<ChanGroupName, Map<particleId, particleMeanRearingHoldingTime>>
	private Map<String, Map<Integer, Long>> _particleMeanRearingHoldings = null;
	// Map<ChanGroupName, Map<particleId, particleDaytimeHolding>>
	private Map<String, Map<Integer, Boolean>> _particleDaytimeHoldings = null;
	private ArrayList<String> _groupNames=null;
	// Channel number (internal), chan group name
	private Map<Integer, String> _channelGroups=null;
	//private float _daytimeNotSwimPercent = 0.0f; // used to be only one value, now one per channel group.  the values is in _swimVelParas
	private static Pair<Integer, Integer> _sunrise = null;
	private static Pair<Integer, Integer> _sunset = null;
	private float _floodHoldVel = -999999.0f;
	private float _constProbConfusion = 0.0f, _maxProbConfusion = 2.0f, _slopeProbConfusion = 0.0f;
	private int _numTidalCycles = 0;
	private boolean _randomAccess;
	private float _accessProb;
	private SwimHelper _swimHelper = null;
	//<pid,<channel,initial visit time>> // if previous channel and current channel equal for too long, take the particle out of the system
	private Map<Integer, Map<Integer, Double>> _pStuck = new HashMap<Integer, Map<Integer, Double>>();
	//threshold of a particle being stuck or back to the same channel
	private int THRESHOLD_STUCK;

	private ParticleBehavior _smeltBehavior = null;

}

/**
 *
 */
package DWR.DMS.PTM;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.HashMap;
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
	public SwimInputs(ArrayList<String> inText, String fishType) {
		if (inText != null){
			if (fishType.equalsIgnoreCase("POSITION_ORIENTED_PARTICLE")){
				ArrayList<String> fileNames = PTMUtil.getInputBlock(inText, "Input_Position_Oriented_Particle_File_Name", "END_Input_Position_Oriented_Particle_File_Name");
				if (fileNames==null || fileNames.size()==0)
					PTMUtil.systemExit("No position oriented particle input file name found, exit.");
				String smeltInputFileName = fileNames.get(0).trim();
				try{
					setSmeltParticleBehavior(smeltInputFileName);
				}catch (IOException e){
					e.printStackTrace();
					PTMUtil.systemExit("Error while reading the position oriented particle input file: "+smeltInputFileName);
				}
			}
			else if (fishType.equalsIgnoreCase("SALMON_PARTICLE")){
				if (inText.size()<12)
					PTMUtil.systemExit("information missing in Swim_Inputs section");
				try{
					//_daytimeNotSwimPercent = PTMUtil.getFloatFromLine(inText.get(0), "DAY_TIME_NOT_SWIM_PERCENT"); //no longer used
					_sunrise = PTMUtil.getPairFromLine(inText.get(0), "SUNRISE");
					_sunset = PTMUtil.getPairFromLine(inText.get(1), "SUNSET");
					_floodHoldVel = PTMUtil.getFloatFromLine(inText.get(2), "STST_THRESHOLD");
					_numTidalCycles = PTMUtil.getIntFromLine(inText.get(3), "TIDAL_CYCLES_TO_CALCULATE_CHANNEL_DIRECTION");
					_constProbConfusion = PTMUtil.getFloatFromLine(inText.get(4), "CONSTANT_CONFUSION_PROBABILITY");
					_maxProbConfusion = PTMUtil.getFloatFromLine(inText.get(5), "MAXIMUM_CONFUSION_PROBABILITY");
					_slopeProbConfusion = PTMUtil.getFloatFromLine(inText.get(6), "CONFUSION_PROBABILITY_SLOPE");
					_randomAccess = PTMUtil.getBooleanFromLine(inText.get(7), "RANDOM_ACCESS");
					_accessProb = PTMUtil.getFloatFromLine(inText.get(8), "ACCESS_PROBABILITY");
					THRESHOLD_STUCK = PTMUtil.getIntFromLine(inText.get(9), "Stuck_Threshold")*24*60*60;
				}catch (NumberFormatException e){
					e.printStackTrace();
					PTMUtil.systemExit("number format is wrong in one of first 7 swimming input lines");
				}
				ArrayList<String> sVelInText = PTMUtil.getInputBlock(inText, "CHANNEL_GROUPS", "END_CHANNEL_GROUPS");
				setChannelGroups(sVelInText);
			}
			else
				PTMUtil.systemExit("No swimming input is expected, but get this:"+inText.get(0)+" system exit.");
		}
		else {
			if (fishType.equalsIgnoreCase("SALMON_PARTICLE") || fishType.equalsIgnoreCase("POSITION_ORIENTED_PARTICLE"))
					PTMUtil.systemExit("For SALMON_PARTICLE or POSITION_ORIENTED_PARTICLE, the swimming input section is needed, but not found.");
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


	private void setChannelGroups(ArrayList<String> chanGroups){
		if (chanGroups == null){
			System.err.println("WARNING: No channel groups for Swimming velocities defined in behavior input file!");
			return;
		}
		// get swimming velocities
		ArrayList<String> sVelStrs = PTMUtil.getInputBlock(chanGroups, "SWIMMING_VELOCITIES", "END_SWIMMING_VELOCITIES");
		if (sVelStrs == null)
			PTMUtil.systemExit("No swimming velocities found in the Channel_Groups block, system exit");
		checkTitle(sVelStrs.get(0));
		if (_groupNames != null || _swimVelParas != null)
			PTMUtil.systemExit("Swimming velocities should not have been set before SwimInputs intialization, system exit");
		_groupNames = new ArrayList<String>();
		_swimVelParas = new HashMap<String, float[]>();
		_particleMeanSwimVels = new HashMap<String, Map<Integer, Float>>();
		_particleMeanRearingHoldings = new HashMap<String, Map<Integer, Long>>();
		_particleDaytimeHoldings = new HashMap<String, Map<Integer, Boolean>>();
		boolean includeAll = false;
		for (String line: sVelStrs.subList(1, sVelStrs.size())){
			String [] items = line.trim().split("[,\\s\\t]+");

			try{
				if (items.length < 6)
					throw new NumberFormatException();
				String groupName = items[0].toUpperCase();
				if(groupName.equals("ALL"))
					includeAll = true;
				// item[1], constant swimming velocity; item[2], std for particles; item[3] std for time steps for each particle; item[4] rearing holding; item[5] day time not swim percent
				_swimVelParas.put(groupName, new float[] {Float.parseFloat(items[1]),
														 Float.parseFloat(items[2]),
														 Float.parseFloat(items[3]),
														 Float.parseFloat(items[4])*60.0f,
														 Float.parseFloat(items[5])}); // converting from hours to minutes
				_groupNames.add(groupName);
				_particleMeanSwimVels.put(groupName, new HashMap<Integer, Float>());
				_particleMeanRearingHoldings.put(groupName, new HashMap<Integer, Long>());
				_particleDaytimeHoldings.put(groupName, new HashMap<Integer, Boolean>());
			}catch(NumberFormatException e){
				PTMUtil.systemExit("expect to read four floats in the swimming velocity line, but read: "+line+", System exit.");
			}
		}
		if(!includeAll)
			PTMUtil.systemExit("Swiming Velocities Input must include an \"ALL\" channel group, system exit");
		//TODO clean up: groupName "ALL" already added in the loop above, no need to do again.
		//_particleMeanSwimVels.put("ALL", new HashMap<Integer, Float>());
		//_particleMeanRearingHoldings.put("ALL", new HashMap<Integer, Long>());
		//_particleDaytimeHoldings.put("ALL", new HashMap<Integer, Boolean>());
		//get Channel list
		ArrayList<String> channelListStrs = PTMUtil.getInputBlock(chanGroups, "CHANNEL_LIST", "END_CHANNEL_LIST");
		if (channelListStrs == null)
			PTMUtil.systemExit("No channel list found in behavior input file, system exit");
		_channelGroups = new HashMap<Integer, String>();
		for (String name: _groupNames){
			if (!name.equalsIgnoreCase("ALL")){
				ArrayList<String> chanList = PTMUtil.getInputBlock(channelListStrs, name, "End_".concat(name));
				if (chanList == null)
						PTMUtil.systemExit("expect a channel list for a group:"+name+", but got none, please check swimming behavior inputs, system exit.");
				else{
					for (String line: chanList){
						ArrayList<Integer> chanIds = PTMUtil.getInts(line);
						for (int chanId: chanIds){
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
	}
	private void checkTitle(String inTitle){
		String [] title = inTitle.trim().split("[,\\s\\t]+");

		if (title.length < 5
				||!title[0].equalsIgnoreCase("Group_Name")
				|| !title[1].equalsIgnoreCase("Constant_Swimming_velocity")
				|| !title[2].equalsIgnoreCase("Standard_Deviation_Particles")
				|| !title[3].equalsIgnoreCase("Standard_Deviation_Times")
				|| !title[4].equalsIgnoreCase("Rearing_Holding_Mean"))
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

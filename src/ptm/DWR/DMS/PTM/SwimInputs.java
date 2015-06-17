/**
 * 
 */
package DWR.DMS.PTM;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;

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
			if (inText.size()<11)
				PTMUtil.systemExit("information missing in Swim_Inputs section");
			try{
				_daytimeNotSwimPercent = PTMUtil.getFloatFromLine(inText.get(0), "DAY_TIME_NOT_SWIM_PERCENT");
				_sunrise = PTMUtil.getPairFromLine(inText.get(1), "SUNRISE");
				_sunset = PTMUtil.getPairFromLine(inText.get(2), "SUNSET");
				_floodHoldVel = PTMUtil.getFloatFromLine(inText.get(3), "STST_THRESHOLD");
				_numTidalCycles = PTMUtil.getIntFromLine(inText.get(4), "TIDAL_CYCLES_TO_CALCULATE_CHANNEL_DIRECTION");
				_constProbConfusion = PTMUtil.getFloatFromLine(inText.get(5), "CONSTANT_CONFUSION_PROBABILITY");
				_maxProbConfusion = PTMUtil.getFloatFromLine(inText.get(6), "MAXIMUM_CONFUSION_PROBABILITY");
				_slopeProbConfusion = PTMUtil.getFloatFromLine(inText.get(7), "CONFUSION_PROBABILITY_SLOPE");
				_randomAccess = PTMUtil.getBooleanFromLine(inText.get(8), "RANDOM_ACCESS");
				_accessProb = PTMUtil.getFloatFromLine(inText.get(9), "ACCESS_PROBABILITY"); 
				// rearing holding average time in hours, convert to minutes
				// changes reach by reach now
				// _rearingHolding = PTMUtil.getIntFromLine(inText.get(10), "REARING_HOLDING_AVG")*60;
			}catch (NumberFormatException e){
				e.printStackTrace();
				PTMUtil.systemExit("number format is wrong in one of first 7 swimming input lines");	
			}
			ArrayList<String> sVelInText = PTMUtil.getInputBlock(inText, "CHANNEL_GROUPS", "END_CHANNEL_GROUPS");
			setChannelGroups(sVelInText);
		}
		_fishType = fishType;
		setHelper();
		// TODO Auto-generated constructor stub
		System.out.println("Created SwimHelper...");
	}
	// no longer allow to input from command line
	/*
	public void setSwimmingVelocityForAllFromCommand(String groupName, String sVel){
		String name = groupName.toUpperCase();
		if (_groupNames == null || _swimmingVelocities == null)
			PTMUtil.systemExit("Swimming velocity for Channel groups are not specified in PTM behavior input file, please check.");
		if(_swimmingVelocities.get(name) == null)
			PTMUtil.systemExit("Channel group "+groupName+" is not specified in PTM behavior input file, please check.");
		else
			System.err.println("warning: swiming velocity: "+_swimmingVelocities.get(name) +" for the channel group: "
					+ groupName +" will be replaced by "+sVel+" from the commandline input");
		try{
			Float v = Float.parseFloat(sVel);
			_swimmingVelocities.put(name, v);
			Channel.uSwimmingVelocity = v;
		}catch (NumberFormatException e){
			e.printStackTrace();
			PTMUtil.systemExit("expect a float for a swimming velocity for all channels but get:" + sVel);	
		}	
	}
	*/
	
	public float getDaytimeNotSwimPercent(){return _daytimeNotSwimPercent;}
	public float getFloodHoldingThreshold(){return _floodHoldVel;}
	public float getConstProbConfusion(){return _constProbConfusion;}
	public float getMaxProbConfusion(){return _maxProbConfusion;}
	public float getSlopeProbConfusion(){return _slopeProbConfusion;}
	public boolean getRandomAccess() {return _randomAccess;}
	public float getAccessProbability() {return _accessProb;}
	public int getNumberTidalCycles(){return _numTidalCycles;}
	// now varies from reach to reach
	//public int getRearingHoldingAvg(){return _rearingHolding;}
	public Pair<Integer, Integer> getSunrise(){ return _sunrise;}
	public Pair<Integer, Integer> getSunset(){ return _sunset;}
	//TODO not used for now, may be used later for the general case (no fish behavior)
	public float[] getConstSwimmingVelocityForAll(){
		if (_swimVelParas == null || _swimVelParas.get("ALL") == null)
			return new float[] {0.0f, 0.0f, 0.0f, 0.0f};
		return _swimVelParas.get("ALL");
	}
	public void setChannelInfo(Waterbody[] allWbs){
		if (_swimVelParas != null){
			float[] sv = _swimVelParas.get("ALL");
			if (sv != null)
				Channel.uSwimVelParameters = sv;
			if (_channelGroups != null){
				for (int chan: _channelGroups.keySet()){
					//wbArray starts from 1. see PTMFixedInput.java line 180
					Channel c = (Channel) allWbs[chan];
					if (c == null){
						PTMUtil.systemExit("while seting swiming velocity, find no such channel:" + PTMHydroInput.getExtFromIntChan(chan));
					}
					c.setSwimVelParameters(_swimVelParas.get(_channelGroups.get(new Integer(chan))));
				}
			}
				
		}
		
	}
	public Map<Integer, String> getChannelGroups(){return _channelGroups;}
	public Map<String, Map<Integer, Float>> getPartcleMeanSwimmingVelocityMap() {return _particleMeanSwimVels;}
	public Map<String, Map<Integer, Float>> getPartcleMeanRearingHoldingMap() {return _particleMeanRearingHoldings;}
	public void setNodeInfo(Node[] allNodes, int nodeNum){}
	public void updateCurrentInfo(Node[] allNodes, int nodeNum, Waterbody[] allChans, int chanNum, int currentTime){
		
	}
	public SwimHelper getSwimHelper(){return _swimHelper;}
	public float getParticleMeanSwimmingVelocity(int pId, Channel chan){
		return getParticleMeanValue(pId, chan, "SwimmingVelocity", _particleMeanSwimVels);
	}
	// Rearing holding time is actually current model time + holding time
	// cast float to int e.g., 900.6 = 900 (1 minute is not a big deal in this case)
	public int getParticleRearingHoldingTime(int pId, Channel chan){
		return (int) getParticleMeanValue(pId, chan, "RearingHoldingTime", _particleMeanRearingHoldings);
	}
	private float getParticleMeanValue(int pId, Channel chan, String what, Map<String, Map<Integer, Float>> meanValuesMap){
		// _channelGroups and _particleMeanSwimVels/_particleMeanRearingHoldings have been initialized at the init
		// no need to check null
		String groupName = _channelGroups.get(chan.getEnvIndex());
		// particle id vs. mean swimming velocity map
		Map<Integer, Float> meanMap = null;
		if (groupName == null)
			//the channel is not specified in a channel group.  use "All" group
			meanMap = meanValuesMap.get("ALL");
		else
			meanMap = meanValuesMap.get(groupName);
		if (meanMap == null)
			PTMUtil.systemExit("particle mean swimming velocity map should be defined earlier, but not, system exit.");
		if (meanMap.get(pId) == null){
			//TODO remove next two lines when print line is removed, clean up
			float m = chan.getParticleMeanValue(what);
			meanMap.put(pId, m);
			//TODO put back in when println is removed
			//meanMap.put(pId, chan.getParticleMeanValue(what));
			//TODO clean up
			
			if (what.equalsIgnoreCase("RearingHoldingTime")){
				System.err.println(PTMHydroInput.getExtFromIntChan(chan.getEnvIndex())
								+"  "+groupName+"  "+pId+"  "+m+"  "+Globals.currentModelTime);
			}
			
		}
		return meanMap.get(pId);
	}
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
		_particleMeanRearingHoldings = new HashMap<String, Map<Integer, Float>>();
		for (String line: sVelStrs.subList(1, sVelStrs.size())){
			String [] items = line.trim().split("[,\\s\\t]+");
			// put into the map: group name, survival rate
			try{
				if (items.length < 5)
					throw new NumberFormatException();
				String groupName = items[0].toUpperCase();
				// item[1], constant swimming velocity; item[2], std for particles; item[3] std for time steps for each particle
				_swimVelParas.put(groupName, new float[] {Float.parseFloat(items[1]),
														 Float.parseFloat(items[2]),
														 Float.parseFloat(items[3]),
														 Float.parseFloat(items[4])*60.0f}); // converting from hours to minutes
				_groupNames.add(groupName);
				_particleMeanSwimVels.put(groupName, new HashMap<Integer, Float>());
				_particleMeanRearingHoldings.put(groupName, new HashMap<Integer, Float>());
			}catch(NumberFormatException e){
				PTMUtil.systemExit("expect to read four floats in the swimming velocity line, but read: "+line+", System exit.");
			}
		}
		_particleMeanSwimVels.put("ALL", new HashMap<Integer, Float>());
		_particleMeanRearingHoldings.put("ALL", new HashMap<Integer, Float>());
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
	//private void addSpecialBehaviors(SwimHelper sh, String particleType){}
	private void setHelper(){
		// TODO make particle own helper???
		if(_fishType.equalsIgnoreCase("SALMON") || _fishType.equalsIgnoreCase("PARTICLE"))
			_swimHelper = new SalmonSwimHelper(new SalmonBasicSwimBehavior());
		else if (_fishType.equalsIgnoreCase("SMELT"))
			PTMUtil.systemExit("the special help for smelt has been defined yet");
		else
			PTMUtil.systemExit("the special help for smelt has been defined yet");
	}
	private SwimHelper _swimHelper = null;
	private String _fishType = null;
	// group name, swimming velocity parameters[] [0] constSwimmingVelocity; [1]; STD for particles; [2] STD for time steps for an individual particle
	private Map<String, float[]> _swimVelParas=null;
	// Map<ChanGroupName, Map<particleId, meanSwimmingVelocity>>
	private Map<String, Map<Integer, Float>> _particleMeanSwimVels = null;
	// Map<ChanGroupName, Map<particleId, particleMeanRearingHoldingTime>>
	private Map<String, Map<Integer, Float>> _particleMeanRearingHoldings = null;
	private ArrayList<String> _groupNames=null;
	// Channel number (internal), chan group name
	private Map<Integer, String> _channelGroups=null;
	private float _daytimeNotSwimPercent = 0.0f;
	private Pair<Integer, Integer> _sunrise = null;
	private Pair<Integer, Integer> _sunset = null;
	private float _floodHoldVel = -999999.0f;
	private float _constProbConfusion = -999999.0f, _maxProbConfusion = -999999.0f, _slopeProbConfusion = -999999.0f;
	private int _numTidalCycles = -999999;
	private boolean _randomAccess;
	private float _accessProb;
	// now varies from reach to reach
	//private int _rearingHolding;
}

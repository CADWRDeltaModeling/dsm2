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
			}catch (NumberFormatException e){
				e.printStackTrace();
				PTMUtil.systemExit("number format is wrong in one of first 7 swimming input lines");	
			}
			ArrayList<String> sVelInText = PTMUtil.getInputBlock(inText, "CHANNEL_GROUPS", "END_CHANNEL_GROUPS");
			setChannelGroups(sVelInText);
		}
		_fishType = fishType;
	}

	public float getDaytimeNotSwimPercent(){return _daytimeNotSwimPercent;}
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
	public String getFishType(){return _fishType;}
	public Map<String, float[]> getSwimParameters() {return _swimVelParas;}

	//TODO do nothing for now, should be included in bahavior or helper?
	public void setChannelInfo(Waterbody[] allWbs){}
	public void setNodeInfo(Node[] allNodes, int nodeNum){}
	
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
				_particleMeanRearingHoldings.put(groupName, new HashMap<Integer, Long>());
			}catch(NumberFormatException e){
				PTMUtil.systemExit("expect to read four floats in the swimming velocity line, but read: "+line+", System exit.");
			}
		}
		_particleMeanSwimVels.put("ALL", new HashMap<Integer, Float>());
		_particleMeanRearingHoldings.put("ALL", new HashMap<Integer, Long>());
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
	
 
	private String _fishType = null;
	// group name, swimming velocity parameters[] [0] constSwimmingVelocity; [1]; STD for particles; [2] STD for time steps for an individual particle
	private Map<String, float[]> _swimVelParas=null;
	// Map<ChanGroupName, Map<particleId, meanSwimmingVelocity>>
	private Map<String, Map<Integer, Float>> _particleMeanSwimVels = null;
	// Map<ChanGroupName, Map<particleId, particleMeanRearingHoldingTime>>
	private Map<String, Map<Integer, Long>> _particleMeanRearingHoldings = null;
	private ArrayList<String> _groupNames=null;
	// Channel number (internal), chan group name
	private Map<Integer, String> _channelGroups=null;
	private float _daytimeNotSwimPercent = 0.0f;
	private Pair<Integer, Integer> _sunrise = null;
	private Pair<Integer, Integer> _sunset = null;
	private float _floodHoldVel = -999999.0f;
	private float _constProbConfusion = 0.0f, _maxProbConfusion = 2.0f, _slopeProbConfusion = 0.0f;
	private int _numTidalCycles = 0;
	private boolean _randomAccess;
	private float _accessProb;
}

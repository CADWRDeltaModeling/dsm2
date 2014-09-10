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
			ArrayList<String> sVelInText = PTMUtil.getInputBlock(inText, "CHANNEL_GROUPS", "END_CHANNEL_GROUPS");
			setChannelGroups(sVelInText);
		}
		_fishType = fishType;
		setHelper();
		// TODO Auto-generated constructor stub
		System.out.println("Created SwimHelper...");
	}
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
	
	
	public float getSwimmingVelocityForAll(){
		if (_swimmingVelocities == null || _swimmingVelocities.get("ALL") == null)
			return 0.0f;
		return _swimmingVelocities.get("ALL");
	}
	public void setChannelInfo(Waterbody[] allWbs, Pair<String, Float> commandLineSwimInfo){
		if (commandLineSwimInfo != null){
			boolean find = false;
			for (String groupName: _swimmingVelocities.keySet()){
				if (groupName.equalsIgnoreCase(commandLineSwimInfo.getFirst())){
					_swimmingVelocities.put(groupName, commandLineSwimInfo.getSecond());
					find = true;
				}
				if (!find)
					PTMUtil.systemExit("commandline input channel group name: "+ commandLineSwimInfo.getFirst() + " is not found in the swimming " +
							"velocity section in the behavior input file. ");
			}
		}
		if (_swimmingVelocities != null){
			Float sv = _swimmingVelocities.get("ALL");
			if (sv != null)
				Channel.uSwimmingVelocity = sv;
			if (_channelGroups != null){
				for (int chan: _channelGroups.keySet()){
					//wbArray starts from 1. see PTMFixedInput.java line 180
					Channel c = (Channel) allWbs[chan];
					if (c == null){
						PTMUtil.systemExit("while seting swiming velocity, find no such channel:" + PTMHydroInput.getExtFromIntChan(chan));
					}
					c.setSwimmingVelocity(_swimmingVelocities.get(_channelGroups.get(new Integer(chan))));
				}
			}
				
		}
		
	}
	public void setNodeInfo(Node[] allNodes, int nodeNum){}
	public void updateCurrentInfo(Node[] allNodes, int nodeNum, Waterbody[] allChans, int chanNum, int currentTime){
		
	}
	public SwimHelper getSwimHelper(){return _swimHelper;}
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
		if (_groupNames != null || _swimmingVelocities != null)
			PTMUtil.systemExit("Swimming velocities should not have been set before SwimInputs intialization, system exit");
		_groupNames = new ArrayList<String>();
		_swimmingVelocities = new HashMap<String, Float>();
		for (String line: sVelStrs.subList(1, sVelStrs.size())){
			String [] items = line.trim().split("[,\\s\\t]+");
			// put into the map: group name, survival rate
			try{
				String groupName = items[0].toUpperCase();
				_swimmingVelocities.put(groupName, Float.parseFloat(items[1]));
				_groupNames.add(items[0].toUpperCase());
			}catch(NumberFormatException e){
				PTMUtil.systemExit("expect to read a float in the swimming velocity line, but read: "+items[1]+", System exit.");
			}
		}
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
		if (!title[0].equalsIgnoreCase("Group_Name")
				|| !title[1].equalsIgnoreCase("Swimming_velocity"))		
			PTMUtil.systemExit("SYSTEM EXIT: Expecting Group_Name Survival_Rate but get:"+title[0] + " " +title[1]);
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
	// group name, velocity
	private Map<String, Float> _swimmingVelocities=null;
	private ArrayList<String> _groupNames=null;
	// node, group name
	private Map<Integer, String> _channelGroups=null;
}

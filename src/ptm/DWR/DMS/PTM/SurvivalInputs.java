/**
 * 
 */
package DWR.DMS.PTM;
import java.util.ArrayList;
import java.util.Map;
import java.util.HashMap;
import java.lang.NumberFormatException;


/**
 * @author xwang
 *
 */
public class SurvivalInputs {
	/**
	 * 
	 */
	public SurvivalInputs() {
		// TODO Auto-generated constructor stub
	}
	
	public SurvivalInputs(ArrayList<String> inList) {
		if (inList != null)
			setChannelGroups(PTMUtil.getInputBlock(inList, "CHANNEL_GROUPS", "END_CHANNEL_GROUPS"));
	}
	public Map<String, Pair<Double, Double>> getAllSurvivalParameters() {return _survivalParas;}
	public Pair<Double, Double> getSurvivalParameters(int chanId){
		String groupName = _channelGroups.get(chanId);
		return (groupName != null)?_survivalParas.get(groupName):_survivalParas.get("ALL");
	}
	
	//TODO should allow to set a survival rate for all the channels?
	private void setChannelGroups(ArrayList<String> chanGroups){
		if (chanGroups == null){
			System.err.println("WARNING: No channel groups for suvival rates are defined in behavior input file!");
			return;
		}
		// get survival rates
		ArrayList<String> survivalRateStrs = PTMUtil.getInputBlock(chanGroups, "SURVIVAL_PARAMETERS", "END_SURVIVAL_PARAMETERS");
		if (survivalRateStrs == null)
			PTMUtil.systemExit("No survival parameters found in the Channel_Groups block, system exit");
		checkTitle(survivalRateStrs.get(0));
		_survivalParas = new HashMap<String, Pair<Double, Double>>();
		_groupNames = new ArrayList<String>();
		for (String line: survivalRateStrs.subList(1, survivalRateStrs.size())){
			String [] items = line.trim().split("[,\\s\\t]+");
			if(items.length < 3)
				PTMUtil.systemExit("errors in the survival parameter input line:"+items);
			// put into the map: group name, survival parameters
			try{
				_survivalParas.put(items[0].toUpperCase(), 
						new Pair<Double, Double>(Double.parseDouble(items[1]), Double.parseDouble(items[2])));
				_groupNames.add(items[0].toUpperCase());
			}catch(NumberFormatException e){
				PTMUtil.systemExit("expect to read two doubles in the survival parameter line, but read: "+items+", System exit.");
			}
		}
		//get Channel list
		ArrayList<String> channelListStrs = PTMUtil.getInputBlock(chanGroups, "CHANNEL_LIST", "END_CHANNEL_LIST");
		if (channelListStrs == null)
			PTMUtil.systemExit("No channel list found, system exit");
		_channelGroups = new HashMap<Integer, String>();
		for (String name: _groupNames){
			if(!name.equalsIgnoreCase("ALL")){
				ArrayList<String> chanList = PTMUtil.getInputBlock(channelListStrs, name, "End_".concat(name));
				if (chanList == null)
					PTMUtil.systemExit("expect to get a channel list for group:"+name+", but got none, system exit.");
				for (String line: chanList){
					ArrayList<Integer> chanIds = PTMUtil.getInts(line);
					for (int chanId: chanIds){
						Integer envId = PTMHydroInput.getIntFromExtChan(chanId);
						if (envId <= 0)
							PTMUtil.systemExit("got a wrong channel ID:"+chanId+", system exit.");
						else
							_channelGroups.put(envId, name);
					}
				}
			}
		}
	}
	private void checkTitle(String inTitle){
		String [] title = inTitle.trim().split("[,\\s\\t]+");
		if (!title[0].equalsIgnoreCase("Group_Name")
				|| !title[1].equalsIgnoreCase("Lambda")|| !title[2].equalsIgnoreCase("Omega"))		
			PTMUtil.systemExit("SYSTEM EXIT: Expecting Group_Name Lambda Omega but get:"+title[0] + " " +title[1]+ " " +title[1]);
	}
	// <group name, Pair<Lambda, Omega>>
	private Map<String, Pair<Double, Double>> _survivalParas=null;
	private ArrayList<String> _groupNames=null;
	// <channel number, group name>
	private Map<Integer, String> _channelGroups=null;
}

/*
public void setChannelInfo(Waterbody[] waterbodies){
	if (_channelGroups != null){
		for (Waterbody wb: waterbodies){
			if (wb != null && wb.getType() == Waterbody.CHANNEL){
				Channel chan = (Channel) wb;
				String chanGroup = _channelGroups.get(chan.getEnvIndex());
				//if (chanGroup != null)
					//chan.setChanGroup(chanGroup);
				//else
					//chan.setChanGroup(null);
			}
		}
	}
}
	private SurvivalHelper _survivalHelper = null;
			//setHelper();
*/
//TODO never been used, may be needed later?
//public void setNodeInfo(Node[] allNodes){}
//public void updateCurrentInfo(Node[] allNodes, Waterbody[] allChans, int currentTime){}
//public void addSpecialBehaviors(SurvivalHelper sh, String particleType){}
//public SurvivalHelper getSurvivalHelper(){ return _survivalHelper;}
/*
private void setHelper(){
	//TODO particle should have its own basic behavior???
	if(_fishType.equalsIgnoreCase("PARTICLE"))
		_survivalHelper = new ParticleSurvivalHelper(new ParticleBasicSurvivalBehavior());
	else if(_fishType.equalsIgnoreCase("SALMON"))
		_survivalHelper = new SalmonSurvivalHelper(new SalmonBasicSurvivalBehavior(_survivalRates));
	else if (_fishType.equalsIgnoreCase("SMELT"))
		PTMUtil.systemExit("the special help for smelt has not been defined yet");
	else
		PTMUtil.systemExit("the special help for smelt has not been defined yet");
}
*/
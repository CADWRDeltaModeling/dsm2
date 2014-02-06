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
	
	public SurvivalInputs(ArrayList<String> inList, String fishType) {
		_fishType = fishType;
		if (inList != null)
			setChannelGroups(PTMUtil.getInputBlock(inList, "CHANNEL_GROUPS", "END_CHANNEL_GROUPS"));
		setHelper();
		System.out.println("Created Survival Helper...");
	}
	
	public void addSpecialBehaviors(SurvivalHelper sh, String particleType){}
	
	public void setChannelInfo(Waterbody[] waterbodies){
		for (Waterbody wb: waterbodies){
			if (wb != null && wb.getType() == Waterbody.CHANNEL){
				Channel chan = (Channel) wb;
				String chanGroup = _channelGroups.get(chan.getEnvIndex());
				if (chanGroup != null)
					chan.setChanGroup(chanGroup);
				else
					chan.setChanGroup(null);
			}
		}
	}
	
	public void setNodeInfo(Node[] allNodes, int nodeNum){}
	public void updateCurrentInfo(Node[] allNodes, int nodeNum, Waterbody[] allChans, int chanNum, int currentTime){
		
	}
	public SurvivalHelper getSurvivalHelper(){ return _survivalHelper;}
	
	private void setChannelGroups(ArrayList<String> chanGroups){
		if (chanGroups == null){
			System.err.println("WARNING: No channel group defined!");
			return;
		}
		// get survival rates
		ArrayList<String> survivalRateStrs = PTMUtil.getInputBlock(chanGroups, "SURVIVAL_RATES", "END_SURVIVAL_RATES");
		if (survivalRateStrs == null)
			PTMUtil.systemExit("No survival rates found in the Channel_Groups block, system exit");
		checkTitle(survivalRateStrs.get(0));
		_survivalRates = new HashMap<String, Double>();
		_groupNames = new ArrayList<String>();
		for (String line: survivalRateStrs.subList(1, survivalRateStrs.size())){
			String [] items = line.trim().split("[,\\s\\t]+");
			// put into the map: group name, survival rate
			try{
				_survivalRates.put(items[0].toUpperCase(), Double.parseDouble(items[1]));
				_groupNames.add(items[0].toUpperCase());
			}catch(NumberFormatException e){
				PTMUtil.systemExit("expect to read a double in the survival rate line, but read: "+items[1]+", System exit.");
			}
		}
		//get Channel list
		ArrayList<String> channelListStrs = PTMUtil.getInputBlock(chanGroups, "CHANNEL_LIST", "END_CHANNEL_LIST");
		if (channelListStrs == null || (_groupNames.size() != channelListStrs.size()/2))
			PTMUtil.systemExit("No channel list found or some list missing in the Channel_Groups block, system exit");
		_channelGroups = new HashMap<Integer, String>();
		for (int i = 0; i < channelListStrs.size()/2; i++){
			String []  items = channelListStrs.get(2*i).trim().split("[:,\\s\\t]+");
			String groupName = items[0].toUpperCase();
			if (!_groupNames.contains(groupName))
				PTMUtil.systemExit("got a wrong group name:"+groupName+", system exit.");
			ArrayList<Integer> channelList = PTMUtil.getInts(channelListStrs.get(2*i+1));
			for (Integer chanId: channelList){
				Integer envId = PTMHydroInput.getIntFromExtChan(chanId);
				if (envId <= 0)
					PTMUtil.systemExit("got a wrong channel ID:"+chanId+", system exit.");
				else
					_channelGroups.put(envId, groupName);
			}
		}
	}
	private void checkTitle(String inTitle){
		String [] title = inTitle.trim().split("[,\\s\\t]+");
		if (!title[0].equalsIgnoreCase("Group_Name")
				|| !title[1].equalsIgnoreCase("Survival_Rate"))		
			PTMUtil.systemExit("SYSTEM EXIT: Expecting Group_Name Survival_Rate but get:"+title[0] + " " +title[1]);
	}
	private void setHelper(){
		if(_fishType.equalsIgnoreCase("SALMON"))
			_survivalHelper = new SalmonSurvivalHelper(new SalmonBasicSurvivalBehavior(_survivalRates));
		else if (_fishType.equalsIgnoreCase("SMELT"))
			PTMUtil.systemExit("the special help for smelt has not been defined yet");
		else
			PTMUtil.systemExit("the special help for smelt has not been defined yet");
	}
	private String _fishType = null;
	private SurvivalHelper _survivalHelper = null;
	private Map<String, Double> _survivalRates=null;
	private ArrayList<String> _groupNames=null;
	private Map<Integer, String> _channelGroups=null;
}


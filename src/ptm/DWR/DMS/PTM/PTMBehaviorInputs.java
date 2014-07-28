/**
 * 
 */
package DWR.DMS.PTM;
import java.io.BufferedReader;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;
import java.util.Calendar;
/**
 * @author xwang
 *
 */
public class PTMBehaviorInputs {
	private String _fishType = null;
	private SurvivalInputs _survivalInputs=null;
	private SwimInputs _swimInputs=null;
	private RouteInputs _routeInputs=null;
	private boolean _barrierInstalled = true;
	private int _totalParticlesReleased = 0;
	// Map<NodeId, FishReleaseGroup>
	private Map<Integer, FishReleaseGroup> _fishGroups = null; 
	
	private void extractReleaseInputs(ArrayList<String> releaseInputText){
		if (releaseInputText.size()< 7)
			PTMUtil.systemExit("Errors in Fish_Release_Inputs, system exit.");
		int numOfGroups = PTMUtil.getInt(releaseInputText.get(0));
		for (int i = 1; i< numOfGroups + 1; i++){
			ArrayList<String> groupText = PTMUtil.getInputBlock(releaseInputText, "GROUP_"+i, "END_GROUP_"+i);
			if (groupText.size()<4)
				PTMUtil.systemExit("Errors in Fish_Release_Inputs Group_"+i+" system exit.");
			//TODO clean up
			/*
			String [] title = groupText.get(0).trim().split("[,\\s\\t]+");
			String shouldBe[] = {"NODEID", "RELEASE_START_DATE", "RELEASE_START_TIME", "TOTAL_RELEASE_HOURS", "NUMBER_OF_FISH"};
			if (!PTMUtil.check(title, shouldBe))
				PTMUtil.systemExit("SYSTEM EXIT: Title line is wrong:"+releaseInputText.get(0));
			else{
				ArrayList<String> groupInfo = PTMUtil.getInts(groupText.get(1));
				FishReleaseGroup group = new FishReleaseGroup(groupInfo.get(0), );
			*/
			Integer nodeId = PTMUtil.getInt(groupText.get(0));
			String [] title = groupText.get(1).trim().split("[,\\s\\t]+");
			String shouldBe[] = {"RELEASE_DATE", "RELEASE_TIME", "FISH_NUMBER", "RELEASE_STYLE"};
			if (!PTMUtil.check(title, shouldBe))
				PTMUtil.systemExit("SYSTEM EXIT: Title line is wrong while reading particle release info: "+groupText.get(1));
			else{
				for (String rline: groupText.subList(2, groupText.size())){
					String [] oneRelease = rline.trim().split("[,\\s\\t]+");
					
					if (oneRelease.length<4)
						PTMUtil.systemExit("Errors in Fish_Release_Inputs Group_"+i+": " +rline+" system exit.");
					
					Calendar releaseTime = PTMUtil.getDateTime(oneRelease[0], oneRelease[1]);
					int fishNumber = Integer.parseInt(oneRelease[2].trim());
					
					int releaseStyle = FishRelease.RANDOM;
					if(oneRelease[3].equalsIgnoreCase("CENTER"))
						releaseStyle = FishRelease.CENTER;
					else if (oneRelease[3].equalsIgnoreCase("RANDOM"))
						releaseStyle = FishRelease.RANDOM;
					else
						PTMUtil.systemExit("Errors in Fish_Release_Inputs Group_"+i+": " +rline+" system exit.");
					
					if (_fishGroups == null)
						_fishGroups = new HashMap<Integer, FishReleaseGroup>();
					
					if (_fishGroups.get(nodeId) == null){
						ArrayList<FishRelease> frList = new ArrayList<FishRelease>();
						frList.add(new FishRelease(releaseTime, fishNumber, releaseStyle));
						_fishGroups.put(nodeId, new FishReleaseGroup(nodeId, frList));
					}
					else
						_fishGroups.get(nodeId).addFishRelease(new FishRelease(releaseTime, fishNumber, releaseStyle));
					_totalParticlesReleased += fishNumber; 
				}
			}
		}
			
	}
	
	/**
	 * 
	 */
	public PTMBehaviorInputs() {
		PTMUtil.systemExit("should not be here, system exit.");
	}
	public PTMBehaviorInputs(String inputFileName) {
		if (inputFileName == null || inputFileName.length() == 0)
			PTMUtil.systemExit("Behavior input file not found, system exit");
		BufferedReader inputText = PTMUtil.getInputBuffer(inputFileName);
		// PTMUtil.getInputBlock(...) returns an ArrayList
		ArrayList<String> fishTypeList = PTMUtil.getInputBlock(inputText, "FISH_TYPE_INPUTS", "END_FISH_TYPE_INPUTS");
		if (fishTypeList==null || fishTypeList.size()==0) 
			PTMUtil.systemExit("No Fish Type found, exit from PTMBehaviorInput line 61");
		_fishType = fishTypeList.get(0).trim();		
		
		ArrayList<String> releaseInputs = PTMUtil.getInputBlock(inputText, "FISH_RELEASE_INPUTS", "END_FISH_RELEASE_INPUTS");
		if (releaseInputs==null || releaseInputs.size()==0)
			System.err.println("No fish release timeseries found!");
		else
			extractReleaseInputs(releaseInputs);
		ArrayList<String> survivalInputText = PTMUtil.getInputBlock(inputText, "SURVIVAL_INPUTS", "END_SURVIVAL_INPUTS");
		if (survivalInputText == null)
			System.err.println("WARNING: no survival behavior input found!");
		_survivalInputs = new SurvivalInputs(survivalInputText,  _fishType);
		ArrayList<String> swimInputText = PTMUtil.getInputBlock(inputText, "SWIM_INPUTS", "END_SWIM_INPUTS");
		if (swimInputText == null)
			System.err.println("WARNING: no swim behavior input found!");
		_swimInputs = new SwimInputs(swimInputText,  _fishType);
		ArrayList<String> routeInputText = PTMUtil.getInputBlock(inputText, "ROUTE_INPUTS", "END_ROUTE_INPUTS");
		if (routeInputText == null)
			System.err.println("WARNING: no route behavior input found!");
		_routeInputs = new RouteInputs(routeInputText, _fishType);
			PTMUtil.closeBuffer(inputText);
		
	}
	public void setWaterbodyInfo(Waterbody[] allWbs){
		if (_routeInputs != null){
			_routeInputs.setBarrierWbInfo(allWbs);
			_routeInputs.setFishScreenWbInfo(allWbs);
		}
		if (_survivalInputs != null){
			_survivalInputs.setChannelInfo(allWbs);
		}
		if (_swimInputs != null){
			_swimInputs.setChannelInfo(allWbs);
			//TODO need finish this
			//_swimInputs.setWaterbodyInfo(allWbs, reserviorObj2ObjNameID);
		}
		
	}
	public void setNodeInfo(Node[] allNodes){
		if (_routeInputs != null){
			_routeInputs.setBarrierNodeInfo(allNodes);
			_routeInputs.setFishScreenNodeInfo(allNodes);
		}
		//TODO not needed now, maybe later
		//_survivalInputs.setNodeInfo(allNodes);
		//_swimInputs.setNodeInfo(allNodes);
	}
	public void updateCurrentInfo(Node[] allNodes, Waterbody[] allWbs, int currentTime){
		if (_barrierInstalled && _routeInputs != null)
			_barrierInstalled = _routeInputs.updateCurrentBarrierInfo(allWbs, currentTime);
		//TODO not needed now, maybe later
		//_survivalInputs.updateCurrentInfo(allNodes, allWbs, currentTime);
		//_swimInputs.updateCurrentInfo(allNodes, allWbs, currentTime);
	}
	public String getFishType(){return _fishType;}
	public SwimInputs getSwimInputs(){ return _swimInputs;}
	public SurvivalInputs getSurvivalInputs(){ return _survivalInputs;}
	public RouteInputs getRouteInputs(){ return _routeInputs;}
	public Map<Integer, FishReleaseGroup> getFishReleaseGroups() {return _fishGroups;}
	public int getTotalParticlesReleased() {return _totalParticlesReleased;}
}

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
	private TravelTimeOutput _travelTimeOutput = null;
	// Map<NodeId, FishReleaseGroup>
	private Map<Integer, FishReleaseGroup> _fishGroups = null; 
	
	private void extractReleaseInputs(ArrayList<String> releaseInputText){
		if (releaseInputText.size()< 6)
			PTMUtil.systemExit("Errors in Fish_Release_Inputs, system exit.");
		int numOfGroups = PTMUtil.getInt(releaseInputText.get(0));
		for (int i = 1; i< numOfGroups + 1; i++){
			ArrayList<String> groupText = PTMUtil.getInputBlock(releaseInputText, "GROUP_"+i, "END_GROUP_"+i);
			if (groupText == null)
				PTMUtil.systemExit("No Fish_Release_Inputs Group_"+i+" inputs, system exit.");
			else if (groupText.size()<3)
				PTMUtil.systemExit("Errors in Fish_Release_Inputs Group_"+i+" system exit.");
			Integer nodeId = PTMHydroInput.getIntFromExtNode(PTMUtil.getInt(groupText.get(0)));  // convert to internal id system
			String [] title = groupText.get(1).trim().split("[,\\s\\t]+");
			String shouldBe[] = {"RELEASE_DATE", "RELEASE_TIME", "PARTICLE_NUMBER", "RELEASE_STYLE"};
			if (!PTMUtil.check(title, shouldBe))
				PTMUtil.systemExit("SYSTEM EXIT: Title line is wrong while reading particle release info: "+groupText.get(1));
			else{
				for (String rline: groupText.subList(2, groupText.size())){
					String [] oneRelease = rline.trim().split("[,\\s\\t]+");
					
					if (oneRelease.length<4)
						PTMUtil.systemExit("Errors in Fish_Release_Inputs Group_"+i+": " +rline+" system exit.");
					
					Calendar releaseTime = PTMUtil.getDateTime(oneRelease[0], oneRelease[1]);
					int particleNumber = Integer.parseInt(oneRelease[2].trim());
					
					int releaseStyle = FishRelease.RANDOM;
					if(oneRelease[3].equalsIgnoreCase("CENTER"))
						releaseStyle = FishRelease.CENTER;
					else if (oneRelease[3].equalsIgnoreCase("RANDOM"))
						releaseStyle = FishRelease.RANDOM;
					else
						PTMUtil.systemExit("Errors in Fish_Release_Inputs Group_"+i+": " +rline+" system exit.");
					
					if (_fishGroups == null)
						// map key: node id
						_fishGroups = new HashMap<Integer, FishReleaseGroup>();
					
					if (_fishGroups.get(nodeId) == null){
						ArrayList<FishRelease> frList = new ArrayList<FishRelease>();
						frList.add(new FishRelease(releaseTime, particleNumber, releaseStyle));
						_fishGroups.put(nodeId, new FishReleaseGroup(nodeId, frList));
					}
					else
						_fishGroups.get(nodeId).addFishRelease(new FishRelease(releaseTime, particleNumber, releaseStyle));
					_totalParticlesReleased += particleNumber; 
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
		BufferedReader inputTextBuff = PTMUtil.getInputBuffer(inputFileName);

		ArrayList<String> inputText = PTMUtil.getInputs(inputTextBuff);
		ArrayList<String> fishTypeList = PTMUtil.getInputBlock(inputText, "FISH_TYPE_INPUTS", "END_FISH_TYPE_INPUTS");
		if (fishTypeList==null || fishTypeList.size()==0) 
			PTMUtil.systemExit("No Fish Type found, exit from PTMBehaviorInput line 61");
		_fishType = fishTypeList.get(0).trim();	
		
		ArrayList<String> travelTimeOutputInfo = PTMUtil.getInputBlock(inputText, "TRAVEL_TIME_OUTPUT", "END_TRAVEL_TIME_OUTPUT");
		if (travelTimeOutputInfo==null || travelTimeOutputInfo.size()==0) 
			System.err.println("Warning: no travel time output info defined in behavior input file");
		_travelTimeOutput = new TravelTimeOutput(travelTimeOutputInfo);	
		
		ArrayList<String> releaseInputs = PTMUtil.getInputBlock(inputText, "FISH_RELEASE_INPUTS", "END_FISH_RELEASE_INPUTS");
		if (releaseInputs==null || releaseInputs.size()==0)
			System.err.println("No fish release timeseries found!");
		else
			extractReleaseInputs(releaseInputs);
		ArrayList<String> survivalInputText = PTMUtil.getInputBlock(inputText, "SURVIVAL_INPUTS", "END_SURVIVAL_INPUTS");
		if (survivalInputText == null)
			//TODO comment out for USGS, restore warning later
			System.err.println("");
			//System.err.println("WARNING: no survival behavior input found!");
		_survivalInputs = new SurvivalInputs(survivalInputText,  _fishType);
		ArrayList<String> swimInputText = PTMUtil.getInputBlock(inputText, "SWIM_INPUTS", "END_SWIM_INPUTS");
		if (swimInputText == null)
			System.err.println("WARNING: no swim behavior input found!");
		_swimInputs = new SwimInputs(swimInputText,  _fishType);
		ArrayList<String> routeInputText = PTMUtil.getInputBlock(inputText, "ROUTE_INPUTS", "END_ROUTE_INPUTS");
		if (routeInputText == null)
			System.err.println("WARNING: no route behavior input found!");
		_routeInputs = new RouteInputs(routeInputText, _fishType);
			PTMUtil.closeBuffer(inputTextBuff);
		
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
		if (_travelTimeOutput != null){
			_travelTimeOutput.setOutputWbInfo(allWbs);
		}
	}
	public void setNodeInfo(Node[] allNodes){
		if (_routeInputs != null){
			_routeInputs.setBarrierNodeInfo(allNodes);
			_routeInputs.setFishScreenNodeInfo(allNodes);
		}
		if (_travelTimeOutput != null){
			_travelTimeOutput.setOutputNodeInfo(allNodes);
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
	// map key: node Id (internal Id system)
	public Map<Integer, FishReleaseGroup> getFishReleaseGroups() {return _fishGroups;}
	public int getTotalParticlesReleased() {return _totalParticlesReleased;}
	public TravelTimeOutput getTravelTimeOutput(){return _travelTimeOutput;}
}

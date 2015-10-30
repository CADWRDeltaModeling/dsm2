/**
 * 
 */
package DWR.DMS.PTM;
import java.io.BufferedReader;
import java.nio.IntBuffer;
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
	private int _totalParticlesReleased = 0;
	private TravelTimeOutput _travelTimeOutput = null;
	// Map<name of release station, FishReleaseGroup>
	private Map<String, FishReleaseGroup> _fishGroups = null; 
	// Map<insert station(node id, wb id, and distance), station name> 
	private Node[] _nodeArray=null;
	private Waterbody[] _wbArray=null;
	
	private void extractReleaseInputs(ArrayList<String> releaseInputText){
		if (releaseInputText.size()< 6)
			PTMUtil.systemExit("Errors in Fish_Release_Inputs, system exit.");
		int numOfGroups = PTMUtil.getIntFromLine(releaseInputText.get(0), "NUMBER_OF_RELEASE_GROUPS");
		for (int i = 1; i< numOfGroups + 1; i++){
			ArrayList<String> groupText = PTMUtil.getInputBlock(releaseInputText, "GROUP_"+i, "END_GROUP_"+i);
			if (groupText == null)
				PTMUtil.systemExit("No Fish_Release_Inputs Group_"+i+" inputs, system exit.");
			else if (groupText.size()<4)
				PTMUtil.systemExit("Errors in Fish_Release_Inputs Group_"+i+" system exit.");
			String stationShouldBe[] = {"NODEID", "CHANNELID/RESERVOIRNAME/OBJ2OBJNAME", "DISTANCE", "STATION_NAME"};
			PTMUtil.checkTitle(groupText.get(0), stationShouldBe);
			Pair<String, IntBuffer> stationName = setIdsDistance(groupText.get(1)); //PTMHydroInput.getIntFromExtNode(PTMUtil.getInt(groupText.get(0)));  // convert to internal id system
			IntBuffer station = stationName.getSecond();
			String name = stationName.getFirst();
			String [] title = groupText.get(1).trim().split("[,\\s\\t]+");
			String[] releaseShouldBe = {"RELEASE_DATE", "RELEASE_TIME", "PARTICLE_NUMBER", "RELEASE_STYLE"};
			if (PTMUtil.check(title, releaseShouldBe))
				PTMUtil.systemExit("SYSTEM EXIT: Title line is wrong while reading particle release info: "+groupText.get(2));
			else{
				for (String rline: groupText.subList(3, groupText.size())){
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
	private Pair<String, IntBuffer> setIdsDistance(String stationLine){
		int[] station = new int[3];
		String[] items = stationLine.trim().split("[,\\s\\t]+");
		if (items.length<4)
			PTMUtil.systemExit("expect at least 4 items in paticle release line in behavior input file, system exit ");
		if (items[3] == null)
			PTMUtil.systemExit("expect a release station name, but found none, system exit. ");
		try{
			// nodeId
			station[0] = PTMHydroInput.getIntFromExtNode(Integer.parseInt(items[0]));
		}catch(NumberFormatException e){
				e.printStackTrace();
				PTMUtil.systemExit("node id:" + items[0]+ " in the travel time output line is wrong, please check");
		}
		try{
			// wbId
			station[1] = PTMHydroInput.getIntFromExtChan(Integer.parseInt(items[1]));
		}catch(NumberFormatException e){
			if (PTMEnv.getReservoirObj2ObjEnvId(items[1]) == null){
				PTMUtil.systemExit("channel/reservior/obj2obj id:" + items[1] + " in the travel time output line is wrong, please check");
			}
			else
				station[1] = PTMEnv.getReservoirObj2ObjEnvId(items[1]);
		}
		try{
			//distance
			station[2] = Integer.parseInt(items[2]);
		}catch(NumberFormatException e){
			if (items[2].equalsIgnoreCase("LENGTH"))
				station[2] = -999999;
			else{
				PTMUtil.systemExit("distance input:" + items[2] +" for channel:" + items[0] + " and node:"+items[1] + " in travel time output line is wrong, please check." );
			}
		}
		return new Pair<String, IntBuffer>(items[3], IntBuffer.wrap(station));
	}
	
	/**
	 * 
	 */
	public PTMBehaviorInputs() {
		PTMUtil.systemExit("should not be here, system exit.");
	}
	public PTMBehaviorInputs(String inputFileName, Node[] nodeArray, Waterbody[] wbArray) {
		System.out.println("");
		if (inputFileName == null || inputFileName.length() == 0)
			PTMUtil.systemExit("Behavior input file not found, system exit");
		_nodeArray = nodeArray;
		_wbArray = wbArray;
		
		BufferedReader inputTextBuff = PTMUtil.getInputBuffer(inputFileName);

		ArrayList<String> inputText = PTMUtil.getInputs(inputTextBuff);
		ArrayList<String> fishTypeList = PTMUtil.getInputBlock(inputText, "FISH_TYPE_INPUTS", "END_FISH_TYPE_INPUTS");
		if (fishTypeList==null || fishTypeList.size()==0) 
			PTMUtil.systemExit("No Fish Type found, exit.");
		_fishType = fishTypeList.get(0).trim();	
		
		ArrayList<String> randomSequence = PTMUtil.getInputBlock(inputText, "RANDOM_SEQUENCE_INPUTS", "END_RANDOM_SEQUENCE_INPUTS");
		if (randomSequence==null || randomSequence.size()==0) 
			System.err.println("Warning: No random sequence input found!");
		if (PTMUtil.getStringFromLine(randomSequence.get(0).trim(), "Use_New_Random_Seed").equalsIgnoreCase("YES"))
			PTMUtil.setRandomNumber();	
		
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
			System.out.println("add special items later.");
			//System.err.println("WARNING: no survival behavior input found!");
		_survivalInputs = new SurvivalInputs(survivalInputText);
		ArrayList<String> swimInputText = PTMUtil.getInputBlock(inputText, "SWIM_INPUTS", "END_SWIM_INPUTS");
		if (swimInputText == null)
			System.err.println("WARNING: no swim behavior input found!");
		_swimInputs = new SwimInputs(swimInputText,  _fishType);
		ArrayList<String> routeInputText = PTMUtil.getInputBlock(inputText, "ROUTE_INPUTS", "END_ROUTE_INPUTS");
		if (routeInputText == null)
			System.err.println("WARNING: no route behavior input found!");
		_routeInputs = new RouteInputs(routeInputText, _fishType);
		PTMUtil.closeBuffer(inputTextBuff);
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
	public SwimInputs getSwimInputs(){ return _swimInputs;}
	public SurvivalInputs getSurvivalInputs(){ return _survivalInputs;}
	public RouteInputs getRouteInputs(){ return _routeInputs;}
	// map key: node Id (internal Id system)
	public Map<String, FishReleaseGroup> getFishReleaseGroups() {return _fishGroups;}
	public int getTotalParticlesReleased() {return _totalParticlesReleased;}
	public TravelTimeOutput getTravelTimeOutput(){return _travelTimeOutput;}
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
/**
 * 
 */
package DWR.DMS.PTM;
import java.io.BufferedReader;
import java.util.ArrayList;
/**
 * @author xwang
 *
 */
public class PTMBehaviorInputs {
	private String _fishType;
	private SurvivalInputs _survivalInputs;
	private SwimInputs _swimInputs;
	private RouteInputs _routeInputs;
	
	private void extractFishType(ArrayList<String> fishTypeText){
		if (fishTypeText==null || fishTypeText.size()==0) 
				PTMUtil.systemExit("No Fish Type found, exit from PTMBehaviorInput line 61");
		_fishType = fishTypeText.get(0).trim();		 	
	}
	
	
	/**
	 * 
	 */
	public PTMBehaviorInputs() {
		PTMUtil.systemExit("no input behavior file found, system exit.");
	}
	public PTMBehaviorInputs(String inputFileName) {
		BufferedReader inputText = PTMUtil.getInputBuffer(inputFileName);
		ArrayList<String> fishTypeList = PTMUtil.getInputBlock(inputText, "FISH_TYPE_INPUTS", "END_FISH_TYPE_INPUTS");
		extractFishType(fishTypeList);
		ArrayList<String> survivalInputText = PTMUtil.getInputBlock(inputText, "SURVIVAL_INPUTS", "END_SURVIVAL_INPUTS");
		_survivalInputs = new SurvivalInputs(survivalInputText,  _fishType);
		ArrayList<String> swimInputText = PTMUtil.getInputBlock(inputText, "SWIM_INPUTS", "END_SWIM_INPUTS");
		_swimInputs = new SwimInputs(swimInputText,  _fishType);
		ArrayList<String> routeInputText = PTMUtil.getInputBlock(inputText, "ROUTE_INPUTS", "END_ROUTE_INPUTS");
		_routeInputs = new RouteInputs(routeInputText, _fishType);
		PTMUtil.closeBuffer(inputText);
	}
	public void setWaterbodyInfo(Waterbody[] allWbs){
		_routeInputs.setBarrierWbInfo(allWbs);
		_routeInputs.setFishScreenWbInfo(allWbs);
		//_survivalInputs.setWaterbodyInfo(allWbs, reserviorObj2ObjNameID);
		//_swimInputs.setWaterbodyInfo(allWbs, reserviorObj2ObjNameID);
		
	}
	public void setNodeInfo(Node[] allNodes){
		_routeInputs.setBarrierNodeInfo(allNodes);
		_routeInputs.setFishScreenNodeInfo(allNodes);
		//_survivalInputs.setNodeInfo(allNodes);
		//_swimInputs.setNodeInfo(allNodes);
	}
	public void updateCurrentInfo(Node[] allNodes, Waterbody[] allChans, int currentTime){
		_routeInputs.updateCurrentBarrierInfo(allChans, currentTime);
		//_survivalInputs.updateCurrentInfo(allNodes, allChans, currentTime);
		//_swimInputs.updateCurrentInfo(allNodes, allChans, currentTime);
	}
	public String getFishType(){return _fishType;}
	public SwimInputs getSwimInputs(){ return _swimInputs;}
	public SurvivalInputs getSurvivalInputs(){ return _survivalInputs;}
	public RouteInputs getRouteInputs(){ return _routeInputs;}
}

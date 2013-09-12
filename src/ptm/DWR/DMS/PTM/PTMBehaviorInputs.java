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
		if (fishTypeText==null) 
				PTMUtil.systemExit("No Fish Type found, exit from PTMBehaviorInput line 61");
		_fishType = fishTypeText.get(0).trim();		 	
	}
	
	
	/**
	 * 
	 */
	public PTMBehaviorInputs() {
		// TODO Auto-generated constructor stub
	}
	public PTMBehaviorInputs(String inputFileName) {
		BufferedReader inputText = PTMUtil.getInputBuffer(inputFileName);
		ArrayList<String> fishTypeList = PTMUtil.getInputBlock(inputText, "FISH_TYPE_INPUTS", "END_FISH_TYPE_INPUTS");
		extractFishType(fishTypeList);
		ArrayList<String> survivalInputText = PTMUtil.getInputBlock(inputText, "SURVIVAL_INPUTS", "END_SURVIVAL_INPUTS");
		_survivalInputs = new SurvivalInputs(survivalInputText);
		ArrayList<String> swimInputText = PTMUtil.getInputBlock(inputText, "SWIM_INPUTS", "END_SWIM_INPUTS");
		_swimInputs = new SwimInputs(swimInputText);
		ArrayList<String> routeInputText = PTMUtil.getInputBlock(inputText, "ROUTE_INPUTS", "END_ROUTE_INPUTS");
		_routeInputs = new RouteInputs(routeInputText);
		PTMUtil.closeBuffer(inputText);
	}
	public void setChannelInfo(Waterbody[] allChans, int chanNum){
		_routeInputs.setChannelInfo(allChans, chanNum);
		_survivalInputs.setChannelInfo(allChans, chanNum);
		_swimInputs.setChannelInfo(allChans, chanNum);
		
	}
	public void setNodeInfo(Node[] allNodes, int nodeNum){
		_routeInputs.setNodeInfo(allNodes, nodeNum);
		_survivalInputs.setNodeInfo(allNodes, nodeNum);
		_swimInputs.setNodeInfo(allNodes, nodeNum);
	}
	public void updateCurrentInfo(Node[] allNodes, int nodeNum, Waterbody[] allChans, int chanNum, int currentTime){
		_routeInputs.updateCurrentInfo(allNodes, nodeNum, allChans, chanNum,currentTime);
		_survivalInputs.updateCurrentInfo(allNodes, nodeNum, allChans, chanNum,currentTime);
		_swimInputs.updateCurrentInfo(allNodes, nodeNum, allChans, chanNum,currentTime);
	}
	public String getFishType(){return _fishType;}
	public SwimInputs getSwimInputs(){ return _swimInputs;}
	public SurvivalInputs getSurvivalInputs(){ return _survivalInputs;}
	public RouteInputs getRouteInputs(){ return _routeInputs;}
}

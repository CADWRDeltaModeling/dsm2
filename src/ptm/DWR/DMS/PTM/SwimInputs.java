/**
 * 
 */
package DWR.DMS.PTM;
import java.util.ArrayList;

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
		_fishType = fishType;
		setHelper();
		// TODO Auto-generated constructor stub
		System.out.println("Created SwimHelper...");
	}
	public void setChannelInfo(Waterbody[] allChans, int chanNum){}
	public void setNodeInfo(Node[] allNodes, int nodeNum){}
	public void updateCurrentInfo(Node[] allNodes, int nodeNum, Waterbody[] allChans, int chanNum, int currentTime){
		
	}
	public SwimHelper getSwimHelper(){return _swimHelper;}
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

}

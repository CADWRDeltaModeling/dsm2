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
	public SwimInputs(ArrayList<String> inTextLines) {
		// TODO Auto-generated constructor stub
	}
	public void addSpecialBehaviors(SwimHelper sh, String particleType){}
	public void setChannelInfo(Waterbody[] allChans, int chanNum){}
	public void setNodeInfo(Node[] allNodes, int nodeNum){}
	public void updateCurrentInfo(Node[] allNodes, int nodeNum, Waterbody[] allChans, int chanNum, int currentTime){
		
	}

}

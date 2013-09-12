/**
 * 
 */
package DWR.DMS.PTM;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashSet;
import java.util.Set;


/**
 * @author xwang
 *
 */
public class SurvivalInputs {
	  private static final Set<Integer> _sacChannels = new HashSet<Integer>(Arrays.asList(new Integer[]{
			  //TODO all the sac channels
			  
	  }));
	  private static final Set<Integer> _interiorChannels = new HashSet<Integer>(Arrays.asList(new Integer[]{
			  //TODO all the interior channels
			  
	  }));
	  private static final Set<Integer> _sjrChannels = new HashSet<Integer>(Arrays.asList(new Integer[]{
			  //TODO all the sjr channels
			  
	  }));

	/**
	 * 
	 */
	public SurvivalInputs() {
		// TODO Auto-generated constructor stub
	}
	public SurvivalInputs(ArrayList<String> inList) {
		
	}
	public void addSpecialBehaviors(SurvivalHelper sh, String particleType){}
	public void setChannelInfo(Waterbody[] allChans, int chanNum){
		for (int i = 1; i<=chanNum; i++){
			  Channel aChan = (Channel) allChans[i];
			  if ( aChan == null ){
		        	System.err.println("Channel,"+ i + "is null!");
		        	continue;
		      }
			  if (_sacChannels.contains((Integer) PTMHydroInput.getExtFromIntChan(i)))
				  aChan.setChanGroup(1);
			  else if (_interiorChannels.contains((Integer) PTMHydroInput.getExtFromIntChan(i)))
				  aChan.setChanGroup(2);
			  else if (_sjrChannels.contains((Integer) PTMHydroInput.getExtFromIntChan(i)))
				  aChan.setChanGroup(3);
			  else 
				  aChan.setChanGroup(8);
		}
	}
	public void setNodeInfo(Node[] allNodes, int nodeNum){}
	public void updateCurrentInfo(Node[] allNodes, int nodeNum, Waterbody[] allChans, int chanNum, int currentTime){
		
	}
}

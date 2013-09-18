/**
 * 
 */
package DWR.DMS.PTM;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.Set;
import java.lang.NumberFormatException;


/**
 * @author xwang
 *
 */
public class SurvivalInputs {
	  private Set<Integer> _sacChannels = null;
	  private Set<Integer> _interiorChannels = null;
	  private Set<Integer> _sjrChannels = null;
	  //private static final Set<Integer> _sjrChannels = new HashSet<Integer>(Arrays.asList(new Integer[]{}));
	  private void setChannelGroups(ArrayList<String> chanGroups){
		  if (chanGroups == null){
			  System.err.println("No channel group defined!");
			  return;
		  }
		  _sacChannels = addChannelGroup(PTMUtil.getInputBlock(chanGroups, "SACRAMENTO_RIVER", "END_SACRAMENTO_RIVER"));
		  _sjrChannels = addChannelGroup(PTMUtil.getInputBlock(chanGroups, "SAN_JOAQUIN_RIVER", "END_SAN_JOAQUIN_RIVER"));
		  _interiorChannels = addChannelGroup(PTMUtil.getInputBlock(chanGroups, "INTERIOR", "END_INTERIOR"));
	  }
	  private Set<Integer> addChannelGroup(ArrayList<String> inText){
		  if (inText == null)
			  return null;
		  Set<Integer> chanGroup = new HashSet<Integer>();
		  for (int i = 0; i<inText.size();i++){
			  String[] items = inText.get(i).trim().split("[,\\s\\t]+");
			  for (int j= 0; j<items.length;j++){
				  try{
					  chanGroup.add(PTMHydroInput.getIntFromExtChan(Integer.parseInt(items[j])));
				  }catch(NumberFormatException e){
					  PTMUtil.systemExit("Channel numbers in Survival inputs has wrong format: "+items[j]);
				  }
			  }
		  }
		  return chanGroup;
	  }

	/**
	 * 
	 */
	public SurvivalInputs() {
		// TODO Auto-generated constructor stub
	}
	public SurvivalInputs(ArrayList<String> inList) {
		setChannelGroups(PTMUtil.getInputBlock(inList, "CHANNEL_GROUPS", "END_CHANNEL_GROUPS"));
	}
	public void addSpecialBehaviors(SurvivalHelper sh, String particleType){}
	public void setChannelInfo(Waterbody[] allChans, int chanNum){
		for (int i = 1; i<=chanNum; i++){
			  Channel aChan = (Channel) allChans[i];
			  if ( aChan == null ){
		        	System.err.println("Channel,"+ i + "is null!");
		        	continue;
		      }
			  if (_sacChannels!=null && _sacChannels.contains(i))
				  aChan.setChanGroup(1);
			  else if (_interiorChannels!=null &&_interiorChannels.contains(i))
				  aChan.setChanGroup(2);
			  else if (_sjrChannels!=null && _sjrChannels.contains(i))
				  aChan.setChanGroup(3);
			  else 
				  aChan.setChanGroup(8);
		}
	}
	public void setNodeInfo(Node[] allNodes, int nodeNum){}
	public void updateCurrentInfo(Node[] allNodes, int nodeNum, Waterbody[] allChans, int chanNum, int currentTime){
		
	}
}


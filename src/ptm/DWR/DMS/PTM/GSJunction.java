/**
 * 
 */
package DWR.DMS.PTM;

import java.util.Calendar;

/**
 * @author xwang
 *
 */
public class GSJunction{
	private Node _node;
	private NonPhysicalBarrier _barrier;
	private Channel _gs;
	private Channel _sacUp;
	private Channel _sacDown;

	private void setUp(int nodeEnvId){
		try{
			_node = Globals.Environment.getNode(nodeEnvId);
			if (_node ==null)
				throw new ClassCastException("Georgianna Slough node id is wrong!");
			if (_node.getNumberOfChannels() != 3)
				throw new ClassCastException("Georgianna Slough node id is wrong! channel number != 3");
			// look up methods converted all external ids to internal env ids
			_sacUp = (Channel) _node.getChannel(Globals.Environment.lookUpChannelId("sac_gs_up"));
			_sacDown = (Channel) _node.getChannel(Globals.Environment.lookUpChannelId("sac_gs_down"));
			_gs = (Channel) _node.getChannel(Globals.Environment.lookUpChannelId("sac_gs_gs"));
			if (_sacUp == null || _sacDown == null || _gs == null)
				throw new ClassCastException("Sac River or GS channel Id at the junction is wrong!");
		}catch(ClassCastException cce){
			System.err.println("Error: " + cce.getMessage());
			System.err.println("exit from GSJunction line 43.");
			System.exit(-1);
		} 
	}
	
	public GSJunction(int nodeEnvId){
		/**
		 * nodeId is internal envid
		 * 
		 */
		setUp(nodeEnvId);
		  
	}
	
	public GSJunction(){
		// nodeId here is internal EnvId
		setUp(Globals.Environment.lookUpNodeId("GS"));
	}
	
	
	public Node getNode() {
		return _node;
	}
	
	public NonPhysicalBarrier getBarrier(){
		return _barrier;
	}
		
	public Channel getGSChannel(){
		return _gs;
	}
	
	public Channel getUpSacRiverChannel(){
		return _sacUp;
	}
	
	public Channel getDownSacRiverChannel(){
		return _sacDown;
	}
	
	public int getCurrentBarrierOp(Calendar currentModelTime){
		return _barrier.getBarrierOp(currentModelTime);
	}
	
	public float getUpSacRiverFlow(){
		return _sacUp.getFlow(0.0f);
	}
	public float getDownSacRiverFlow(){
		return _sacDown.getFlow(0.0f);
	}
	public float getGSFlow(){
		return _gs.getFlow(0.0f);
	}

}

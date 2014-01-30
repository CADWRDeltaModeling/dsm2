/**
 * 
 */
package DWR.DMS.PTM;
import java.util.ArrayList;

//import java.util.ArrayList;

/**
 * @author xwang
 *
 */
public class SalmonGSJRouteBehavior extends SalmonBasicRouteBehavior {
	private int _nodeId,_gsWbId; 
	/**
	 * 
	 */
	public SalmonGSJRouteBehavior(Integer nodeId, Integer gsWbId) {
		_nodeId = nodeId;
		_gsWbId = gsWbId;
		System.out.println("Created SalmonGSJRouteBehavior...");
	}
	public SalmonGSJRouteBehavior() {
		// instantiate a junction with internal node number
	}
	
	private Channel[] getChannels(Particle p){
		Node curNode = p.nd;
		if (curNode == null)
			PTMUtil.systemExit("p.nd is null, exit");
		if (curNode.getEnvIndex()!=_nodeId)
			PTMUtil.systemExit("in Georgiana Slough Junction, node id in input file is different from node id the particle encountered, exit.");
		ArrayList<Channel> chans = curNode.getChannels();
		if (chans == null || chans.size() != 3)
			PTMUtil.systemExit("in Georgiana Slough Junction, channel number is not 3, exit.");
		
		Channel sacUp = null, sacDown = null;
		Channel gs = (Channel) curNode.getChannel(_gsWbId);
		if (gs == null)
			PTMUtil.systemExit("could not found Georgiana Slough channel at Georgiana Slough Junction, exit.");
		for (int i = 0; i< 3; i++){
			Channel ch = chans.get(i);
			if (ch.getDownNode().equals(curNode))
				sacUp = ch;
			else if (ch.getUpNode().equals(curNode)){
				if (!ch.equals(gs))
					sacDown = ch;
			}
		}
		if (sacUp == null || sacDown == null)
			PTMUtil.systemExit("could not found channels at Georgiana Slough Junction, exit.");
		return new Channel[] {sacUp, sacDown, gs};
	}

	/* (non-Javadoc)
	 * @see DWR.DMS.PTM.JunctionHandler#execute(DWR.DMS.PTM.Junction, DWR.DMS.PTM.Particle)
	 */
	// this one will be called as long as a SalmonGSJRouteBehavior is passed
	public void makeRouteDecision(Particle p) {
		try{
			if (p.nd.getNumberOfChannels() != 3)
				throw new ClassCastException("particle is in wrong a junction! channels != 3");
			else if (!Globals.Environment.getParticleType().equalsIgnoreCase("SALMON"))
				throw new ClassCastException("particle is not SALMON!");
		}
		catch(ClassCastException cce){
			cce.printStackTrace();
			PTMUtil.systemExit("Error: " + cce.getMessage());
		} 
				
		Channel[] channels = getChannels(p);
		Channel sacUp = channels[0]; // first sac up second sac down third gs
		Channel sacDown = channels[1];
		Channel gs = channels[2];
		// flow units are cfs*1000
		float qUpSac = sacUp.getFlow(0.0f)/1000;
		float qDownSac = sacDown.getFlow(0.0f)/1000;
		float qGs = gs.getFlow(0.0f)/1000;

		// if flow in the range of the regression model do the regression model
		//assume unidirectional flow and less than 50000cfs
		// else do the regular super.makeRouteDecision(...)
		if (qUpSac > 0 && qGs > 0 && qDownSac > 0 && qUpSac < 50 ){
			// day d=1 night d=0
		    int d = 1;
		    // unit of channel width is meter and all special units are meter		
		    float w = sacUp.getWidth(sacUp.getLength())*0.3048f;
		    float s = (qGs/(qGs+qDownSac) - 37.5f/144.8f)*w;
		    float pos = (p.y*0.3048f+0.241022f*w);  // pos y starts from center
		    float multi_v = -2.104f-0.531f*d-1.7f*(gs.getCurrentBarrierOp(p.nd.getEnvIndex()))
		    			+0.082f*s+0.068f*qUpSac+0.045f*pos-0.006f*qUpSac*pos;
		    double possibility = Math.exp(multi_v)/(1+Math.exp(multi_v));
		    //TODO clean up
		    //System.out.println(possibility+" "+w+" "+s+" "+pos+" "+qUpSac+" "+qGs+" "+qDownSac+" "+gs.getBarrierAtUpNodeOp());
		    if (possibility < p.nd.getRandomNumber()){
		    	p.wb = sacDown;
		    }
		    else
		    	p.wb = gs;						    
		}
		else
			super.makeRouteDecision(p);
	}

}

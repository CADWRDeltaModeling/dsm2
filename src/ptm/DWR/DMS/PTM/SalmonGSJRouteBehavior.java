/**
 * 
 */
package DWR.DMS.PTM;

//import java.util.ArrayList;

/**
 * @author xwang
 *
 */
public class SalmonGSJRouteBehavior extends SalmonBasicRouteBehavior {
	// should a junction check if the node number incoming particle
	// encounted is the same as the junction node?
	
	private Particle _p;
	private GSJunction _gsj;

	/**
	 * 
	 */
	public SalmonGSJRouteBehavior(int nodeId) {
		_gsj = new GSJunction(nodeId);
	}
	public SalmonGSJRouteBehavior() {
		// instantiate a junction with internal node number
		_gsj = new GSJunction(Globals.Environment.lookUpNodeId("GS"));
	}
	

	/* (non-Javadoc)
	 * @see DWR.DMS.PTM.JunctionHandler#execute(DWR.DMS.PTM.Junction, DWR.DMS.PTM.Particle)
	 */
	// this one will be called as long as a SalmonGSJRouteBehavior is passed
	public void makeRouteDecision(Particle p) {
		try{
			if (_gsj == null)
				throw new ClassCastException("Initialize GSJunction!");
			else if (!Globals.Environment.getParticleType().equalsIgnoreCase("SALMON"))
				throw new ClassCastException("particle is not SALMON!");
			else if (p.nd.getEnvIndex() != _gsj.getNode().getEnvIndex())
				throw new ClassCastException("particle is in wrong junction!");
			else if (p.nd.getNumberOfChannels() != 3){
				throw new ClassCastException("particle is in wrong junction! channels != 3");
			}
			else
				_p = p;
		}
		catch(ClassCastException cce){
			System.err.println("Error: " + cce.getMessage());
			System.exit(-1);
		} 
		// if flow in the range of the regression model do the regression model
		// else do the regular super.makeRouteDecision(...)
		
		// flow units are cfs*1000
		float qUpSac = _gsj.getUpSacRiverFlow()/1000.0f;
		float qDownSac = _gsj.getDownSacRiverFlow()/1000.0f;
		float qGs = _gsj.getGSFlow()/1000.0f;
		// assume unidirectional flow and less than 8000cfs
		if (_p.x>0 && qUpSac > 0 && qGs > 0 && qDownSac > 0 && qUpSac < 50 ){ 
			// day d=1 night d=0
		    int d = 1;
		    Channel sacUp = _gsj.getUpSacRiverChannel();
		    Channel sacDown = _gsj.getDownSacRiverChannel();
		    Channel gs = _gsj.getGSChannel();
		    // unit of channel width is meter and all special units are meter		
		    float w = sacUp.getWidth(sacUp.getLength())*0.3048f;
		    float s = (qGs/(qGs+qDownSac) - 37.5f/144.8f)*w;
		    float pos = (_p.y*0.3048f+0.241022f*w);  // pos y starts from center
		    float multi_v = -2.104f-0.531f*d-1.7f*(gs.getBarrierAtUpNodeOp())
		    			+0.082f*s+0.068f*qUpSac+0.045f*pos-0.006f*qUpSac*pos;
		    double possibility = Math.exp(multi_v)/(1+Math.exp(multi_v));
		    //TODO clean up
		    //System.out.println(possibility+" "+w+" "+s+" "+pos+" "+qUpSac+" "+qGs+" "+qDownSac+" "+gs.getBarrierAtUpNodeOp());
		    if (possibility < _gsj.getNode().getRandomNumber()){
		    	p.wb = sacDown;
		        //System.out.println("main route wb_id:"+p.wb.getEnvIndex());
		    }
		    else{
		    	p.wb = gs;
		    	//System.out.println("barrier route wb_id:"+p.wb.getEnvIndex());
		    }
		    						    
		}
		else{
			super.makeRouteDecision(p);
			System.out.println("called super");
		}
	}

}

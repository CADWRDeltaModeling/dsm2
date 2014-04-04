/**
 * 
 */
package DWR.DMS.PTM;

/**
 * @author xwang
 *
 */
public class ParticleRouteHelper extends Helper<Integer, BasicRouteBehavior> implements RouteHelper {

	public ParticleRouteHelper(BasicRouteBehavior basic) {
		super(basic);
	}
	/**
	 * 
	 */
	public ParticleRouteHelper() {
		super();
	}

	/* (non-Javadoc)
	 * @see DWR.DMS.PTM.RouteHelper#helpSelectRoute(DWR.DMS.PTM.Particle)
	 */
	@Override
	public void helpSelectRoute(Particle p) {
		super.getBehavior(p).makeRouteDecision(p);
	}

	/* (non-Javadoc)
	 * @see DWR.DMS.PTM.RouteHelper#setRouteHelperForParticle(DWR.DMS.PTM.Particle)
	 */
	@Override
	public void setRouteHelperForParticle(Particle p) {
		p.installRouteHelper(this);
	}
	
	public Integer getKey(Particle p){
		return p.nd.getEnvIndex();
			
	}

}

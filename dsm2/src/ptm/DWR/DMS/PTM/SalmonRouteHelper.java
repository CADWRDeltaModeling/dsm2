/**
 *
 */
package DWR.DMS.PTM;

import java.util.Map;

/**
 * @author xwang
 *
 */
public class SalmonRouteHelper extends Helper<Integer, SalmonRouteBehavior> implements RouteHelper {

	/**
	 * I could use only one RouteHelper for all fish species.  But the reason I separate for different species is
	 * for type safe so other species behavior objects will not be accidently mixed up.
	 */
	public SalmonRouteHelper(SalmonRouteBehavior basic, Map<Integer, SalmonRouteBehavior> specialBehaviors) {
		super(basic, specialBehaviors);
	}
	public SalmonRouteHelper(SalmonRouteBehavior basic) {
		super(basic);
	}

	/**
	 *
	 */
	public SalmonRouteHelper() {
		super();
	}
	/*
	 * (non-Javadoc)
	 * @see DWR.DMS.PTM.Helper#getKey(DWR.DMS.PTM.Particle)
	 * method call to get a junction indicator.
	 * here to get a junction node number
	 */
	public Integer getKey(Particle p){
		return p.nd.getEnvIndex();

	}
	public void helpSelectRoute(Particle p){
		super.getBehavior(p).makeRouteDecision(p);

	}
	public void setRouteHelperForParticle(Particle p){
		p.installRouteHelper(this);
	}
	public void updateCurrentInfo(Waterbody[] allWbs, int currentTime){super.getBasicBehavior().updateCurrentInfo(allWbs, currentTime);}
}

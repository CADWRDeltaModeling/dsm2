/**
 * 
 */
package DWR.DMS.PTM;

/**
 * @author xwang
 *
 */
public interface RouteHelper {
	public void helpSelectRoute(Particle p);
	public void setRouteHelperForParticle(Particle p);
	public void updateCurrentInfo(Waterbody[] allWbs, int currentTime);
}

/**
 * 
 */
package DWR.DMS.PTM;

/**
 * @author xwang
 *
 */
public interface SurvivalBehavior {
	static final String behaviorType = "SURVIVAL";
	public void isSurvived(Particle p, float timeToAdvance);

}

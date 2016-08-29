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
	public void isSurvived(Particle p, float x, float t);
	public float getXofXTSurvival(Channel ch, Node nd, float x, float currX);

}

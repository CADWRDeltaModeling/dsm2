/**
 * 
 */
package DWR.DMS.PTM;

/**
 * @author xwang
 *
 */
public interface SurvivalHelper {
	public void helpSurvival(Particle p, float x, float t);
	public void setSurvivalHelperForParticle(Particle p);
	public float getXofXTSurvival(Channel ch, Node nd, float x, float currX);
}

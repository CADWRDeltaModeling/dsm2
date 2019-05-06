/**
 * 
 */
package DWR.DMS.PTM;

/**
 * @author xwang
 *
 */
public interface SwimHelper {
	public void helpSwim(Particle p, float deltaT);
	public void setSwimHelperForParticle(Particle p);
	public void setSwimmingTime(Particle p, int chId);
	public void setMeanSwimmingVelocity(int pId, int chId);
	public long getSwimmingTime(int pId, int chId);
	public float getSwimmingVelocity (Particle p, int chId); 
	// confusion factor returned include the channel direction
	public int getConfusionFactor(int chId);
	public void setXYZLocationInChannel(Particle p);
	public void insert(Particle p);
	public void updateCurrentInfo(Waterbody[] allWbs);
}

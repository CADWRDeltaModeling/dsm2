/**
 * 
 */
package DWR.DMS.PTM;

/**
 * @author xwang
 *
 */
public interface SwimBehavior {
	static final String behaviorType = "SWIM";
	public void updatePosition(Particle p, float deltaT);
	public void setXYZLocationInChannel(Particle p);
	public void insert(Particle p);
	public float[] getChannelInfo(int particleId);
}

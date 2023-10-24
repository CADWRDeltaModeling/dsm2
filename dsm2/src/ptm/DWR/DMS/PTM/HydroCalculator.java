/**
 *
 */
package DWR.DMS.PTM;

/**
 * @author xwang
 *
 */
public interface HydroCalculator {
	public void updateDiffusion(Particle p);
	public void updateChannelParameters(Particle p);
	public float[] getChannelInfo(int pId);
	public void mapYZ(Particle p);
	public int getSubTimeSteps(float timeStep, Particle p);
	public float getMinTimeStep(float timeStep, Particle p);
	public float calcXAdvectionVelocity(int id, float x, float y, float z, Channel c);
	public float calcXAdvection(float XAdvectionVelocity, float deltaT);
	public float getZPosition(Particle p, float z, float timeStep, double gaussian);
	public float getYPosition(Particle p, float y, float timeStep, double gaussian);
	public void setYZLocationInChannel(Particle p);
	public float getXLocationInChannel(Particle p);
	public float calcTimeToNode(Channel c, float advVel, float swimVel, float x, float xPos);
	public float calcDistanceToNode(Channel c, float x, float xPos);
	public float getTerminalVelocity();
}

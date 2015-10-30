package DWR.DMS.PTM;

public interface SalmonSwimBehavior extends SwimBehavior {
	String particleType = "SALMON";
	// swimming velocity here doesn't include the confusion factor.
	public float getSwimmingVelocity(int pId, int chanId);
	public void setMeanSwimmingVelocity(int pId, int chanId);
	public void setSwimmingTime(Particle p, int chanId);
	public int getSwimmingTime(int pId, int chanId);
	public int getConfusionFactor(int chanId);
	public void updatePosition(Particle p, float deltaT);
	public void setXYZLocationInChannel(Particle p);
	public void insert(Particle p);
	public void updateCurrentInfo(Waterbody[] allWbs);
}

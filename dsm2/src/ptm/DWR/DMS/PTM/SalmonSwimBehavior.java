package DWR.DMS.PTM;

public interface SalmonSwimBehavior extends SwimBehavior {
	String particleType = "SALMON";
	// swimming velocity here doesn't include the confusion factor.
	public float getSwimmingVelocity(Particle p, int chanId);
	public void setMeanSwimmingVelocity(int pId, int chanId);
	public void setSwimmingTime(Particle p, int chanId);
	public long getSwimmingTime(int pId, int chanId);
	// confusion factor returned include the channel direction
	public int getConfusionFactor(int chanId);
	public void updateCurrentInfo(Waterbody[] allWbs);
}

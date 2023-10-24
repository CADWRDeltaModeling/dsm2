/**
 *
 */
package DWR.DMS.PTM;

import java.util.Map;

/**
 * @author xwang
 *
 */
public class SmeltSwimHelper extends Helper<Integer, SmeltBasicSwimBehavior> implements SwimHelper {

	/**
	 */
	public SmeltSwimHelper(SmeltBasicSwimBehavior basic,
			Map<Integer, SmeltBasicSwimBehavior> specialBehaviors) {
		super(basic, specialBehaviors);
	}

	/**
	 *
	 */
	public SmeltSwimHelper(SmeltBasicSwimBehavior basic) {
		super(basic);
	}
	public SmeltSwimHelper() {
		super();
	}

	/* (non-Javadoc)
	 * @see DWR.DMS.PTM.SwimHelper#helpSwim(DWR.DMS.PTM.Particle, float)
	 */
	@Override
	public void helpSwim(Particle p, float deltaT) {
		super.getBehavior(p).updatePosition(p, deltaT);

	}

	/* (non-Javadoc)
	 * @see DWR.DMS.PTM.SwimHelper#setSwimHelperForParticle(DWR.DMS.PTM.Particle)
	 */
	@Override
	public void setSwimHelperForParticle(Particle p) {
		p.installSwimHelper(this);

	}

	/* (non-Javadoc)
	 * @see DWR.DMS.PTM.SwimHelper#setSwimmingTime(DWR.DMS.PTM.Particle, int)
	 */
	@Override
	public void setSwimmingTime(Particle p, int chId) {
		PTMUtil.systemExit("A smelt simulation should never call setSwimmingTime(Particle p, int chId), system exit.");

	}

	/* (non-Javadoc)
	 * @see DWR.DMS.PTM.SwimHelper#setMeanSwimmingVelocity(int, int)
	 */
	@Override
	public void setMeanSwimmingVelocity(int pId, int chId) {
		PTMUtil.systemExit("A smelt simulation should never call setMeanSwimmingVelocity(int pId, int chId), system exit.");

	}

	/* (non-Javadoc)
	 * @see DWR.DMS.PTM.SwimHelper#getSwimmingTime(int, int)
	 */
	@Override
	public long getSwimmingTime(int pId, int chId) {
		PTMUtil.systemExit("A smelt simulation should never call getSwimmingTime(int pId, int chId), system exit.");
		return 0;
	}

	/* (non-Javadoc)
	 * @see DWR.DMS.PTM.SwimHelper#getSwimmingVelocity(Particle, int)
	 */
	@Override
	public float getSwimmingVelocity(Particle p, int chId) {
		PTMUtil.systemExit("A smelt simulation should never call getSwimmingVelocity(Particle p, int chId), system exit.");
		return 0;
	}

	/* (non-Javadoc)
	 * @see DWR.DMS.PTM.SwimHelper#getConfusionFactor(int)
	 */
	@Override
	public int getConfusionFactor(int chId) {
		PTMUtil.systemExit("A smelt simulation should never call getConfusionFactor(int chId), system exit.");
		return 0;
	}

	/* (non-Javadoc)
	 * @see DWR.DMS.PTM.SwimHelper#setXYZLocationInChannel(DWR.DMS.PTM.Particle)
	 */
	@Override
	public void setXYZLocationInChannel(Particle p) {
		super.getBasicBehavior().setXYZLocationInChannel(p);

	}

	/* (non-Javadoc)
	 * @see DWR.DMS.PTM.SwimHelper#insert(DWR.DMS.PTM.Particle)
	 */
	@Override
	public void insert(Particle p) {
		super.getBasicBehavior().insert(p);

	}

	/* (non-Javadoc)
	 * @see DWR.DMS.PTM.SwimHelper#updateCurrentInfo(DWR.DMS.PTM.Waterbody[])
	 */
	@Override
	public void updateCurrentInfo(Waterbody[] allWbs) {
		// no need to update any swim related info for smelt

	}

	/* (non-Javadoc)
	 * @see DWR.DMS.PTM.SwimHelper#getChannelInfo(int)
	 */
	@Override
	public float[] getChannelInfo(int particleId) {return super.getBasicBehavior().getChannelInfo(particleId);}
	public Integer getKey(Particle p){
		return p.wb.getEnvIndex();
	}

}

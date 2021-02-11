/**
 * 
 */
package DWR.DMS.PTM;

import java.util.Map;

/**
 * @author xwang
 *
 */
public class SalmonSwimHelper extends Helper<Integer, SalmonSwimBehavior> implements SwimHelper {

	/**
	 */
	public SalmonSwimHelper(SalmonSwimBehavior basic,
			Map<Integer, SalmonSwimBehavior> specialBehaviors) {
		super(basic, specialBehaviors);
		// TODO Auto-generated constructor stub
	}

	/**
	 * 
	 */
	public SalmonSwimHelper(SalmonSwimBehavior basic) {
		super(basic);
	}
	public SalmonSwimHelper() {
		super();
	}

	/**
	 * 
	 */
	public Integer getKey(Particle p) {
		return p.wb.getEnvIndex();

	}
	public void helpSwim(Particle p, float deltaT){
		super.getBehavior(p).updatePosition(p, deltaT);
		//Commented out because it needs to be consistent with other helpers,i.e., using the call getBehavior method 
		//super.getBasicBehavior().updatePosition(p, deltaT);
	}

	@Override
	public void setSwimHelperForParticle(Particle p) {
		p.installSwimHelper(this);	
	}
	//TODO this is not good, get rid of it.  why???
	//public SwimInputs getSwimInputs(){ return Globals.Environment.getBehaviorInputs().getSwimInputs();}
	
	public void setSwimmingTime(Particle p, int chanId){super.getBasicBehavior().setSwimmingTime(p, chanId);}
	public void setMeanSwimmingVelocity(int pId, int chanId){super.getBasicBehavior().setMeanSwimmingVelocity(pId, chanId);}
	// swimming velocity here does not include confusion factor
	public float getSwimmingVelocity(Particle p, int chanId) {return super.getBasicBehavior().getSwimmingVelocity(p, chanId);}
	// confusion factor returned include the channel direction
	public int getConfusionFactor(int chanId) {return super.getBasicBehavior().getConfusionFactor(chanId);}
	public long getSwimmingTime(int pId, int chanId){return super.getBasicBehavior().getSwimmingTime(pId, chanId);}
	public void setXYZLocationInChannel(Particle p){super.getBasicBehavior().setXYZLocationInChannel(p);}
	public void insert(Particle p){super.getBasicBehavior().insert(p);}
	public void updateCurrentInfo(Waterbody[] allWbs){super.getBasicBehavior().updateCurrentInfo(allWbs);}
	public float[] getChannelInfo(int particleId){return super.getBasicBehavior().getChannelInfo(particleId);}
}

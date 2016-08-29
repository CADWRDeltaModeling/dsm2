/**
 * 
 */
package DWR.DMS.PTM;

import java.util.Map;

/**
 * @author xwang
 *
 */
public class ParticleSurvivalHelper extends Helper<Integer, ParticleBasicSurvivalBehavior> implements SurvivalHelper {

	/**
	 * 
	 */
	public ParticleSurvivalHelper() {
		super();
	}
	
	public ParticleSurvivalHelper(ParticleBasicSurvivalBehavior basic) {
		super(basic);
	}

	/* (non-Javadoc)
	 * @see DWR.DMS.PTM.SurvivalHelper#helpSurvival(DWR.DMS.PTM.Particle, float)
	 */
	@Override
	public void helpSurvival(Particle p, float x, float t) {
		super.getBehavior(p).isSurvived(p, x, t);;
	}
	
	public float getXofXTSurvival(Channel ch, Node nd, float x, float currX){
		PTMUtil.systemExit("Why am I here? in ParticleSurvivalHelper.java");
		return 0.0f;
	}

	/* (non-Javadoc)
	 * @see DWR.DMS.PTM.SurvivalHelper#setSurvivalHelperForParticle(DWR.DMS.PTM.Particle)
	 */
	@Override
	public void setSurvivalHelperForParticle(Particle p) {
		p.installSurvivalHelper(this);

	}
	/* (non-Javadoc)
	 * @see DWR.DMS.PTM.Helper#getKey(DWR.DMS.PTM.Particle)
	 */
	@Override
	public Integer getKey(Particle p) {
			// currently doing nothing
			return null;
	}

}

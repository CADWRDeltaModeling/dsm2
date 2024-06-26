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
	public void helpSurvival(Particle p) {
		super.getBehavior(p).isSurvived(p);;
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

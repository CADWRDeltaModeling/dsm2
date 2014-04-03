/**
 * 
 */
package DWR.DMS.PTM;

import java.util.Map;

/**
 * @author xwang
 *
 */
public class SalmonSurvivalHelper extends Helper<Integer, SalmonSurvivalBehavior> implements SurvivalHelper{

	/**
	 * 
	 */
	public SalmonSurvivalHelper(SalmonSurvivalBehavior basic,
			Map<Integer, SalmonSurvivalBehavior> specialBehaviors) {
		super(basic, specialBehaviors);
	}
	public SalmonSurvivalHelper(SalmonSurvivalBehavior basic) {
		super(basic);
	}

	/**
	 * 
	 */
	public SalmonSurvivalHelper() {
		super();
	}

	/* (non-Javadoc)
	 * @see DWR.DMS.PTM.Helper#getKey(DWR.DMS.PTM.Particle)
	 */
	@Override
	public Integer getKey(Particle p) {
		if (p != null && p.wb != null)
			return p.wb.getEnvIndex();
		else
			return null;
	}
	
	public void helpSurvival(Particle p, float timeToAdvance){
		SalmonSurvivalBehavior b = super.getBehavior(p);
		if (b != null)
			b.isSurvived(p, timeToAdvance);
	};
	public void setSurvivalHelperForParticle(Particle p){
		p.installSurvivalHelper(this);
	};

}

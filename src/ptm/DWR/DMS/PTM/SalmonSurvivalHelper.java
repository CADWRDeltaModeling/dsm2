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
		return p.wb.getEnvIndex();
	}
	//public void help(Particle p){}
	public void helpSurvival(Particle p){
		super.getBehavior(p).isSurvived(p);
	};
	public void setSurvivalHelperForParticle(Particle p){
		p.installSurvivalHelper(this);
	};

}

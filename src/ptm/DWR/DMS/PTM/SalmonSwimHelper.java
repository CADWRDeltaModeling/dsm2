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
	public void helpSwim(Particle p){}

	@Override
	public void setSwimHelperForParticle(Particle p) {
		// TODO Auto-generated method stub
		
	}
	public SwimInputs getSwimInputs(){ return Globals.Environment.getBehaviorInputs().getSwimInputs();}
}

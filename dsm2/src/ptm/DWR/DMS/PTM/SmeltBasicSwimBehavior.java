/**
 * 
 */
package DWR.DMS.PTM;

/**
 * @author xwang
 *
 */
public class SmeltBasicSwimBehavior extends BasicSwimBehavior implements
		SmeltSwimBehavior {

	/**
	 * @param si
	 */
	public SmeltBasicSwimBehavior(SwimInputs si) {
		super(si);
		super.setHydroCalculator(new SmeltHydroCalculator());
		((SmeltHydroCalculator) super.getHydroCalculator()).setSmeltBehaviorData(si.getSmeltBehavior());
	}
	public void updatePosition(Particle p, float delT){
		if(p.wb.getPTMType() == Waterbody.CHANNEL)
			((SmeltHydroCalculator) super.getHydroCalculator()).updateStageInfo(p, delT);
		super.updatePosition(p, delT);		
	}
	
}
